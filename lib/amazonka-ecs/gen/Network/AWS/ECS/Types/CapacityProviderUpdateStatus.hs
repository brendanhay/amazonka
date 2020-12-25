{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderUpdateStatus
  ( CapacityProviderUpdateStatus
      ( CapacityProviderUpdateStatus',
        CapacityProviderUpdateStatusDeleteInProgress,
        CapacityProviderUpdateStatusDeleteComplete,
        CapacityProviderUpdateStatusDeleteFailed,
        CapacityProviderUpdateStatusUpdateInProgress,
        CapacityProviderUpdateStatusUpdateComplete,
        CapacityProviderUpdateStatusUpdateFailed,
        fromCapacityProviderUpdateStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CapacityProviderUpdateStatus = CapacityProviderUpdateStatus'
  { fromCapacityProviderUpdateStatus ::
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

pattern CapacityProviderUpdateStatusDeleteInProgress :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatusDeleteInProgress = CapacityProviderUpdateStatus' "DELETE_IN_PROGRESS"

pattern CapacityProviderUpdateStatusDeleteComplete :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatusDeleteComplete = CapacityProviderUpdateStatus' "DELETE_COMPLETE"

pattern CapacityProviderUpdateStatusDeleteFailed :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatusDeleteFailed = CapacityProviderUpdateStatus' "DELETE_FAILED"

pattern CapacityProviderUpdateStatusUpdateInProgress :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatusUpdateInProgress = CapacityProviderUpdateStatus' "UPDATE_IN_PROGRESS"

pattern CapacityProviderUpdateStatusUpdateComplete :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatusUpdateComplete = CapacityProviderUpdateStatus' "UPDATE_COMPLETE"

pattern CapacityProviderUpdateStatusUpdateFailed :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatusUpdateFailed = CapacityProviderUpdateStatus' "UPDATE_FAILED"

{-# COMPLETE
  CapacityProviderUpdateStatusDeleteInProgress,
  CapacityProviderUpdateStatusDeleteComplete,
  CapacityProviderUpdateStatusDeleteFailed,
  CapacityProviderUpdateStatusUpdateInProgress,
  CapacityProviderUpdateStatusUpdateComplete,
  CapacityProviderUpdateStatusUpdateFailed,
  CapacityProviderUpdateStatus'
  #-}
