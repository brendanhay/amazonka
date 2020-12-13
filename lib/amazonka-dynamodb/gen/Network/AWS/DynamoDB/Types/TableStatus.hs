{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TableStatus
  ( TableStatus
      ( TableStatus',
        TSCreating,
        TSUpdating,
        TSDeleting,
        TSActive,
        TSInaccessibleEncryptionCredentials,
        TSArchiving,
        TSArchived
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TableStatus = TableStatus' Lude.Text
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

pattern TSCreating :: TableStatus
pattern TSCreating = TableStatus' "CREATING"

pattern TSUpdating :: TableStatus
pattern TSUpdating = TableStatus' "UPDATING"

pattern TSDeleting :: TableStatus
pattern TSDeleting = TableStatus' "DELETING"

pattern TSActive :: TableStatus
pattern TSActive = TableStatus' "ACTIVE"

pattern TSInaccessibleEncryptionCredentials :: TableStatus
pattern TSInaccessibleEncryptionCredentials = TableStatus' "INACCESSIBLE_ENCRYPTION_CREDENTIALS"

pattern TSArchiving :: TableStatus
pattern TSArchiving = TableStatus' "ARCHIVING"

pattern TSArchived :: TableStatus
pattern TSArchived = TableStatus' "ARCHIVED"

{-# COMPLETE
  TSCreating,
  TSUpdating,
  TSDeleting,
  TSActive,
  TSInaccessibleEncryptionCredentials,
  TSArchiving,
  TSArchived,
  TableStatus'
  #-}
