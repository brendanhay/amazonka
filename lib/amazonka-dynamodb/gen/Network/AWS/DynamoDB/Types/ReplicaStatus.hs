{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaStatus
  ( ReplicaStatus
      ( ReplicaStatus',
        RSCreating,
        RSCreationFailed,
        RSUpdating,
        RSDeleting,
        RSActive,
        RSRegionDisabled,
        RSInaccessibleEncryptionCredentials
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReplicaStatus = ReplicaStatus' Lude.Text
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

pattern RSCreating :: ReplicaStatus
pattern RSCreating = ReplicaStatus' "CREATING"

pattern RSCreationFailed :: ReplicaStatus
pattern RSCreationFailed = ReplicaStatus' "CREATION_FAILED"

pattern RSUpdating :: ReplicaStatus
pattern RSUpdating = ReplicaStatus' "UPDATING"

pattern RSDeleting :: ReplicaStatus
pattern RSDeleting = ReplicaStatus' "DELETING"

pattern RSActive :: ReplicaStatus
pattern RSActive = ReplicaStatus' "ACTIVE"

pattern RSRegionDisabled :: ReplicaStatus
pattern RSRegionDisabled = ReplicaStatus' "REGION_DISABLED"

pattern RSInaccessibleEncryptionCredentials :: ReplicaStatus
pattern RSInaccessibleEncryptionCredentials = ReplicaStatus' "INACCESSIBLE_ENCRYPTION_CREDENTIALS"

{-# COMPLETE
  RSCreating,
  RSCreationFailed,
  RSUpdating,
  RSDeleting,
  RSActive,
  RSRegionDisabled,
  RSInaccessibleEncryptionCredentials,
  ReplicaStatus'
  #-}
