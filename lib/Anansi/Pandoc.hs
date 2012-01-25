{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Anansi.Pandoc
	( loomPandoc
	, looms
	) where

import           Control.Monad.Reader (asks)
import           Control.Monad.Writer (tell)
import qualified Data.Map
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)

import           Anansi hiding (looms)
import           Text.Pandoc hiding (readers, writers)
import qualified Text.Pandoc as Pandoc

-- | Looms which use Pandoc to parse and produce a variety of formats.
--
-- Use this with 'Anansi.defaultMain':
--
-- > #!/usr/bin/env runhaskell
-- > import Anansi
-- > import Anansi.Pandoc
-- > import Data.Map
-- >
-- > main = defaultMain (unions [Anansi.looms, Anansi.Pandoc.looms])
looms :: Data.Map.Map Text Loom
looms = Data.Map.fromList
	[ ("anansi-pandoc.pandoc", loomPandoc Pandoc.readers Pandoc.writers)
	]

-- | A loom which uses Pandoc to parse and produce a variety of formats.
--
-- Documents using this loom should set the options
-- @anansi-pandoc.reader@ and @anansi-pandoc.writer@, to control how the
-- markup is parsed, and what output format is produced. Both settings
-- default to @\"html\"@.
--
-- > :# mydocument.anansi
-- > :option anansi-pandoc.reader=markdown
-- > :option anansi-pandoc.writer=latex
-- >
-- > My Document
-- > ===========
-- > ...
--
-- The /readers/ and /writers/ parameters allow the user to define a custom
-- set of Pandoc formats.
--
-- To work around a limitation in Pandoc, the name specified for
-- @anansi&#x2011;pandoc.reader@ must also be present in
-- @anansi&#x2011;pandoc.writer@. That is, if
-- @anansi&#x2011;pandoc.reader=foo@, then there must be both a reader and
-- writer named @\"foo\"@.
loomPandoc :: [(String, ParserState -> String -> Pandoc)] -- ^ Readers
           -> [(String, WriterOptions -> Pandoc -> String)] -- ^ Writers
           -> Loom
loomPandoc readers writers doc = do
	let readerName = case Data.Map.lookup "anansi-pandoc.reader" (documentOptions doc) of
		Nothing -> "html"
		Just x -> unpack x
	let writerName = case Data.Map.lookup "anansi-pandoc.writer" (documentOptions doc) of
		Nothing -> "html"
		Just x -> unpack x
	
	let reader = case lookup readerName readers of
		Just x -> x
		Nothing -> error ("Unknown Pandoc reader " ++ show readerName)
	let codeWriter = case lookup readerName writers of
		Just x -> x
		Nothing -> error ("Unknown Pandoc writer " ++ show readerName)
	let outputWriter = case lookup writerName writers of
		Just x -> x
		Nothing -> error ("Unknown Pandoc writer " ++ show writerName)
	
	let stringContents = concatMap (stringifyBlock codeWriter) (documentBlocks doc)
	let pandoc = reader defaultParserState stringContents
	
	let outputOptions = defaultWriterOptions
		{ writerStandalone = False
		}
	tell (encodeUtf8 (pack (outputWriter outputOptions pandoc)))
	tell "\n"

stringifyBlock :: (WriterOptions -> Pandoc -> String) -> Anansi.Block -> String
stringifyBlock writer block = case block of
	BlockText text -> unpack text
	BlockFile path content -> writer defaultWriterOptions (Pandoc (Meta [] [] []) [
		BlockQuote [Para [Strong [Str "\xBB", Space, Str (unpack path)]], pandocContent content]
		])
	BlockDefine name content -> writer defaultWriterOptions (Pandoc (Meta [] [] []) [
		BlockQuote [Para [Strong [Str "\xAB", Space, Str (unpack name), Str "\xBB"]], pandocContent content]
		])

pandocContent :: [Content] -> Pandoc.Block
pandocContent = CodeBlock nullAttr . concatMap strContent where
	strContent (ContentText _ text) = unpack text ++ "\n"
	strContent (ContentMacro _ indent name) = unpack indent ++ unpack name ++ "\n"
