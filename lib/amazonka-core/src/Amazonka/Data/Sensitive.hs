-- |
-- Module      : Amazonka.Data.Sensitive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Sensitive where

import Amazonka.Core.Lens.Internal (iso)
import Amazonka.Data.Body
import Amazonka.Data.ByteString
import Amazonka.Data.Headers
import Amazonka.Data.JSON
import Amazonka.Data.Log
import Amazonka.Data.Query
import Amazonka.Data.Text
import Amazonka.Data.XML
import Amazonka.Prelude

-- | /Note/: read . show /= isomorphic
newtype Sensitive a = Sensitive {fromSensitive :: a}
  deriving stock (Eq, Ord, Generic, Functor)
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
