-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Type
  ( Type
      ( Type',
        AmazonCustomerByEmail,
        CanonicalUser,
        Group
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype Type = Type' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AmazonCustomerByEmail :: Type
pattern AmazonCustomerByEmail = Type' "AmazonCustomerByEmail"

pattern CanonicalUser :: Type
pattern CanonicalUser = Type' "CanonicalUser"

pattern Group :: Type
pattern Group = Type' "Group"

{-# COMPLETE
  AmazonCustomerByEmail,
  CanonicalUser,
  Group,
  Type'
  #-}
