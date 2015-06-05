{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Module      : Compiler.Types.Help
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Help
    ( Help
    , asDesc
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Char          (isSpace)
import           Data.String
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Text.Pandoc
import           Text.Pandoc.Pretty

data Help
    = Help [Help]
    | Pan  Pandoc
    | Raw  Text

instance Monoid Help where
    mempty      = Help []
    mappend x y =
        case (x, y) of
            (Help a, Help b) -> Help (a <> b)
            (Pan  a, Pan  b) -> Pan  (a <> b)
            (Raw  a, Raw  b) -> Raw  (a <> b)
            (Help a, b)      -> Help (a <> [b])
            (a,      Help b) -> Help (a : b)
            (a,      b)      -> Help [a, b]

-- | Empty Show instance to avoid verbose debugging output.
instance Show Help where
    show = const mempty

instance IsString Help where
    fromString = Raw . fromString

instance FromJSON Help where
    parseJSON = withText "help" (pure . Raw)
    -- --either (fail . show) pure
    --     . fmap Pan
    --     . readHtml def
    --     . Text.unpack

instance ToJSON Help where
    toJSON = toJSON . mappend "-- |" . Text.drop 2 . wrap "-- " . flatten

newtype Desc = Desc Help

instance ToJSON Desc where
    toJSON (Desc h) = toJSON . wrap "    " $ flatten h

asDesc :: Getter Help Desc
asDesc = to Desc

flatten :: Help -> String
flatten = \case
    Help xs -> foldMap flatten xs
    Pan  d  -> writeHaddock def d
    Raw  t  -> Text.unpack t

wrap :: String -> String -> Text
wrap sep =
      Text.dropWhileEnd isSpace
    . render (Just 76)
    . prefixed sep
    . fromString
