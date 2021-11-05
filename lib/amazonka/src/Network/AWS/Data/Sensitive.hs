-- |
-- Module      : Network.AWS.Data.Sensitive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Sensitive where

import Network.AWS.Data.Body
import Network.AWS.Data.ByteString
import Network.AWS.Data.Headers
import Network.AWS.Data.JSON
import Network.AWS.Data.Log
import Network.AWS.Data.Query
import Network.AWS.Data.Text
import Network.AWS.Data.XML
import Network.AWS.Lens (iso)
import Network.AWS.Prelude

-- | /Note/: read . show /= isomorphic
newtype Sensitive a = Sensitive {fromSensitive :: a}
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( IsString,
      Semigroup,
      Monoid,
      ToByteString,
      FromText,
      ToText,
      FromXML,
      ToXML,
      ToQuery,
      ToJSON,
      FromJSON,
      ToHeader,
      ToBody,
      Hashable,
      NFData,
      IsList
    )

instance Show (Sensitive a) where
  show = const "******"

instance ToLog (Sensitive a) where
  build = const "******"

_Sensitive :: Iso' (Sensitive a) a
_Sensitive = iso fromSensitive Sensitive
