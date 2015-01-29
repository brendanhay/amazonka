-- Module      : Network.AWS.Data.Internal.Path
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Path
    ( ToPath (..)
    ) where

import Data.Monoid
import Data.Text   (Text)

class ToPath a where
    toPath :: a -> Text
    toPath = const mempty

instance ToPath Text where
    toPath = id
