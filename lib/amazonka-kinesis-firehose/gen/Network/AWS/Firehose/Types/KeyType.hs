{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KeyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KeyType
  ( KeyType
      ( KeyType',
        KeyTypeAwsOwnedCmk,
        KeyTypeCustomerManagedCmk,
        fromKeyType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype KeyType = KeyType' {fromKeyType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern KeyTypeAwsOwnedCmk :: KeyType
pattern KeyTypeAwsOwnedCmk = KeyType' "AWS_OWNED_CMK"

pattern KeyTypeCustomerManagedCmk :: KeyType
pattern KeyTypeCustomerManagedCmk = KeyType' "CUSTOMER_MANAGED_CMK"

{-# COMPLETE
  KeyTypeAwsOwnedCmk,
  KeyTypeCustomerManagedCmk,
  KeyType'
  #-}
