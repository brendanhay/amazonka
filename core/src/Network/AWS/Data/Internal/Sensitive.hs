{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : Network.AWS.Data.Internal.Sensitive
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Sensitive where

import Control.Lens
import Data.Monoid
import Data.String
import Network.AWS.Data.Internal.ByteString
import Network.AWS.Data.Internal.JSON
import Network.AWS.Data.Internal.Query
import Network.AWS.Data.Internal.Text
import Network.AWS.Data.Internal.XML

-- | read . show /= isomorphic
newtype Sensitive a = Sensitive { desensitise :: a }
    deriving
        ( Eq
        , Ord
        , Read
        , IsString
        , Monoid
        , ToByteString
        , FromText
        , ToText
        , FromXML
        , ToXML
        , ToQuery
        , ToJSON
        , FromJSON
        )

instance Show (Sensitive a) where
    show = const "******"

_Sensitive :: Iso' (Sensitive a) a
_Sensitive = iso desensitise Sensitive
