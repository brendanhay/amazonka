{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.Types.NS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.NS where

import           Data.Aeson
import           Data.Monoid
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path

newtype NS = NS [Text]
    deriving (Eq, Ord, Show)

textToNS :: Text -> NS
textToNS = NS . Text.splitOn "."

nsToPath :: NS -> Path.FilePath
nsToPath (NS xs) = Path.fromText (Text.intercalate "/" xs) Path.<.> "hs"

instance IsString NS where
    fromString "" = mempty
    fromString s  = textToNS (fromString s)

instance Monoid NS where
    mempty = NS []
    mappend (NS xs) (NS ys)
        | null xs   = NS ys
        | null ys   = NS xs
        | otherwise = NS (xs <> ys)

instance FromJSON NS where
    parseJSON = withText "namespace" (pure . textToNS)

instance ToJSON NS where
    toJSON (NS xs) = toJSON (Text.intercalate "." xs)
