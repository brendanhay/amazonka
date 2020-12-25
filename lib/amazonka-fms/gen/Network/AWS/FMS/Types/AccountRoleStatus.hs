{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AccountRoleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AccountRoleStatus
  ( AccountRoleStatus
      ( AccountRoleStatus',
        AccountRoleStatusReady,
        AccountRoleStatusCreating,
        AccountRoleStatusPendingDeletion,
        AccountRoleStatusDeleting,
        AccountRoleStatusDeleted,
        fromAccountRoleStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AccountRoleStatus = AccountRoleStatus'
  { fromAccountRoleStatus ::
      Core.Text
  }
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

pattern AccountRoleStatusReady :: AccountRoleStatus
pattern AccountRoleStatusReady = AccountRoleStatus' "READY"

pattern AccountRoleStatusCreating :: AccountRoleStatus
pattern AccountRoleStatusCreating = AccountRoleStatus' "CREATING"

pattern AccountRoleStatusPendingDeletion :: AccountRoleStatus
pattern AccountRoleStatusPendingDeletion = AccountRoleStatus' "PENDING_DELETION"

pattern AccountRoleStatusDeleting :: AccountRoleStatus
pattern AccountRoleStatusDeleting = AccountRoleStatus' "DELETING"

pattern AccountRoleStatusDeleted :: AccountRoleStatus
pattern AccountRoleStatusDeleted = AccountRoleStatus' "DELETED"

{-# COMPLETE
  AccountRoleStatusReady,
  AccountRoleStatusCreating,
  AccountRoleStatusPendingDeletion,
  AccountRoleStatusDeleting,
  AccountRoleStatusDeleted,
  AccountRoleStatus'
  #-}
