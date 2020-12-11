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
        TSActive,
        TSArchived,
        TSArchiving,
        TSCreating,
        TSDeleting,
        TSInaccessibleEncryptionCredentials,
        TSUpdating
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

pattern TSActive :: TableStatus
pattern TSActive = TableStatus' "ACTIVE"

pattern TSArchived :: TableStatus
pattern TSArchived = TableStatus' "ARCHIVED"

pattern TSArchiving :: TableStatus
pattern TSArchiving = TableStatus' "ARCHIVING"

pattern TSCreating :: TableStatus
pattern TSCreating = TableStatus' "CREATING"

pattern TSDeleting :: TableStatus
pattern TSDeleting = TableStatus' "DELETING"

pattern TSInaccessibleEncryptionCredentials :: TableStatus
pattern TSInaccessibleEncryptionCredentials = TableStatus' "INACCESSIBLE_ENCRYPTION_CREDENTIALS"

pattern TSUpdating :: TableStatus
pattern TSUpdating = TableStatus' "UPDATING"

{-# COMPLETE
  TSActive,
  TSArchived,
  TSArchiving,
  TSCreating,
  TSDeleting,
  TSInaccessibleEncryptionCredentials,
  TSUpdating,
  TableStatus'
  #-}
