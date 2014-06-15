{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Generator.Stage2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Stage2 where

import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Lazy.Builder
import qualified Network.AWS.Generator.Stage1 as Stage1
import           Network.AWS.Generator.Stage1 hiding (Service)
import           Network.AWS.Generator.Types
import           Text.Shakespeare.Text

transform :: Stage1.Service -> Service
transform = trans

class Transform a where
    type Trans a :: *
    trans :: Trans a -> a

data Service = Service
    { namespace :: Module
    , abbrev    :: Abbrev
    }

instance Transform Service where
    type Trans Service = Stage1.Service
    trans s = Service
        { moduleName = trans s
        , abbrev     = trans s
        }

newtype Module = Module { unModule :: Text }

instance Transform Module where
    type Trans Module = Stage1.Service
    trans = Module
        . mappend "Network.AWS."
        . unAbbrev
        . trans

instance ToText Module where
    toText = fromText . unModule

newtype Abbrev = Abbrev { unAbbrev :: Text }

instance Transform Abbrev where
    type Trans Abbrev = Stage1.Service
    trans = Abbrev
        . mconcat
        . Text.words
        . strip "AWS"
        . strip "Amazon"
        . sServiceAbbreviation

instance ToText Abbrev where
    toText = fromText . unAbbrev

strip :: Text -> Text -> Text
strip delim = f Text.stripSuffix . f Text.stripPrefix
  where
    f g x = fromMaybe x $ g delim x
