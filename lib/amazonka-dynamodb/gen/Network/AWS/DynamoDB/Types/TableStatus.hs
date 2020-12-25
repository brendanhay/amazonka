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
        TableStatusCreating,
        TableStatusUpdating,
        TableStatusDeleting,
        TableStatusActive,
        TableStatusInaccessibleEncryptionCredentials,
        TableStatusArchiving,
        TableStatusArchived,
        fromTableStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TableStatus = TableStatus' {fromTableStatus :: Core.Text}
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

pattern TableStatusCreating :: TableStatus
pattern TableStatusCreating = TableStatus' "CREATING"

pattern TableStatusUpdating :: TableStatus
pattern TableStatusUpdating = TableStatus' "UPDATING"

pattern TableStatusDeleting :: TableStatus
pattern TableStatusDeleting = TableStatus' "DELETING"

pattern TableStatusActive :: TableStatus
pattern TableStatusActive = TableStatus' "ACTIVE"

pattern TableStatusInaccessibleEncryptionCredentials :: TableStatus
pattern TableStatusInaccessibleEncryptionCredentials = TableStatus' "INACCESSIBLE_ENCRYPTION_CREDENTIALS"

pattern TableStatusArchiving :: TableStatus
pattern TableStatusArchiving = TableStatus' "ARCHIVING"

pattern TableStatusArchived :: TableStatus
pattern TableStatusArchived = TableStatus' "ARCHIVED"

{-# COMPLETE
  TableStatusCreating,
  TableStatusUpdating,
  TableStatusDeleting,
  TableStatusActive,
  TableStatusInaccessibleEncryptionCredentials,
  TableStatusArchiving,
  TableStatusArchived,
  TableStatus'
  #-}
