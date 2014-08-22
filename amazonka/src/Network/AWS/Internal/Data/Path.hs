-- Module      : Network.AWS.Internal.Data.Path
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Data.Path
    ( ToPath (..)
    ) where

import           Data.ByteString.Char8 (ByteString)
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as Text

class ToPath a where
    toPath :: a -> ByteString
    toPath = const mempty

instance ToPath ByteString where
    toPath = id

instance ToPath Text where
    toPath = Text.encodeUtf8
