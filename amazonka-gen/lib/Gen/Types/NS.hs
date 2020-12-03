-- |
-- Module      : Gen.Types.NS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject Lens.to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.NS where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Gen.Prelude
import qualified System.FilePath as FilePath

newtype NS = NS [Text]
  deriving stock (Eq, Ord, Show)

mkNS :: Text -> NS
mkNS = NS . Text.splitOn "."

nsToPath :: NS -> FilePath
nsToPath (NS xs) = Text.unpack (Text.intercalate "/" xs) FilePath.<.> "hs"

nsHyphenate :: NS -> Text
nsHyphenate (NS xs) = Text.intercalate "-" xs

instance IsString NS where
  fromString "" = mempty
  fromString s = mkNS (fromString s)

instance Semigroup NS where
  (NS xs) <> (NS ys)
    | null xs = NS ys
    | null ys = NS xs
    | otherwise = NS (xs <> ys)

instance Monoid NS where
  mempty = NS []
  mappend = (<>)

instance FromJSON NS where
  parseJSON = Aeson.withText "namespace" (pure . mkNS)

instance ToJSON NS where
  toJSON (NS xs) = Aeson.toJSON (Text.intercalate "." xs)
