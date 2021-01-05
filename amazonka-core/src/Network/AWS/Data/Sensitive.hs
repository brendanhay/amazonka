-- |
-- Module      : Network.AWS.Data.Sensitive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Sensitive
  ( Sensitive (..),
  )
where

import Network.AWS.Data.JSON (FromJSON, ToJSON)
import Network.AWS.Data.Query (ToQuery)
import Network.AWS.Data.Text (FromText, ToText)
import Network.AWS.Data.XML (FromXML, ToXML)
import Network.AWS.Prelude

newtype Sensitive a = Sensitive {fromSensitive :: a}
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( IsString,
      Hashable,
      NFData,
      Semigroup,
      Monoid,
      ToText,
      FromText,
      ToQuery,
      ToXML,
      FromXML,
      ToJSON,
      FromJSON
    )

instance Show (Sensitive a) where
  showsPrec _ _ =
    showString "Sensitive {fromSensitive = ******}"
