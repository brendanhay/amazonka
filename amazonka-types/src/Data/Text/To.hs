-- Module      : Data.Text.To
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.To
    (
    -- * Text classes
      ToText (..)

    -- * Class instance helpers
    , showText
    , integral
    , float
    , fromBuilder
    ) where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LText
import qualified Data.Text.Lazy.Builder           as LText
import qualified Data.Text.Lazy.Builder.Int       as LText
import qualified Data.Text.Lazy.Builder.RealFloat as LText

showText :: ToText a => a -> String
showText = Text.unpack . toText

integral :: Integral a => a -> Text
integral = fromBuilder . LText.decimal

float :: RealFloat a => a -> Text
float = fromBuilder . LText.realFloat

fromBuilder :: LText.Builder -> Text
fromBuilder = LText.toStrict . LText.toLazyText

class ToText a where
    toText :: a -> Text

instance ToText Text    where toText = id
instance ToText Int     where toText = integral
instance ToText Integer where toText = integral
instance ToText Float   where toText = float
instance ToText Double  where toText = float
