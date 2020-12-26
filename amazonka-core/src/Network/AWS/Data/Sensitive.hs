{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Data.Sensitive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Sensitive where

import Control.DeepSeq
import qualified Control.Lens as Lens
import Data.Hashable
import Data.String
import GHC.Generics (Generic)
import Network.AWS.Data.ByteString
import Network.AWS.Data.JSON
import Network.AWS.Data.Query
import Network.AWS.Data.Text as AWS.Text
import Network.AWS.Data.XML
import Network.AWS.Prelude

newtype Sensitive a = Sensitive {fromSensitive :: a}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype
    ( IsString,
      Hashable,
      NFData,
      Semigroup,
      Monoid
        ToText,
      FromText,
      ToQuery,
      ToXML,
      FromXML,
      ToJSON,
      FromJSON
    )

instance Show (Sensitive a) where
  showsPrec _ = showString "Sensitive {fromSensitive = ******}"

_Sensitive :: Iso' (Sensitive a) a
_Sensitive = Lens.iso fromSensitive Sensitive
