{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Compiler.Types.Help where

import           Control.Lens
import           Data.Aeson         (ToJSON (..))
import           Data.Jason         hiding (ToJSON (..))
import           Data.String
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Text.Pandoc
import           Text.Pandoc.Pretty

newtype Help = Help Pandoc
    deriving (Monoid)

instance Show Help where
    show (Help p) = writeHaddock def p

instance IsString Help where
    fromString = Help . readHaddock def

instance FromJSON Help where
    parseJSON = withText "help" (pure . Help . readHtml def . Text.unpack)

instance ToJSON Help where
    toJSON = toJSON . mappend "-- |" . Text.drop 2 . helpToHaddock "-- "

newtype Desc = Desc Help

instance ToJSON Desc where
    toJSON (Desc h) = toJSON (helpToHaddock "" h)

asDesc :: Getter Help Desc
asDesc = to Desc

helpToHaddock :: Text -> Help -> Text
helpToHaddock sep (Help h) = Text.pack
    . render (Just 76)
    . prefixed (Text.unpack sep)
    . fromString
    $ writeHaddock def h
