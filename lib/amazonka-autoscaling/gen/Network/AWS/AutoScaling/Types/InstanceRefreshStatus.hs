{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceRefreshStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceRefreshStatus
  ( InstanceRefreshStatus
      ( InstanceRefreshStatus',
        InstanceRefreshStatusPending,
        InstanceRefreshStatusInProgress,
        InstanceRefreshStatusSuccessful,
        InstanceRefreshStatusFailed,
        InstanceRefreshStatusCancelling,
        InstanceRefreshStatusCancelled,
        fromInstanceRefreshStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InstanceRefreshStatus = InstanceRefreshStatus'
  { fromInstanceRefreshStatus ::
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

pattern InstanceRefreshStatusPending :: InstanceRefreshStatus
pattern InstanceRefreshStatusPending = InstanceRefreshStatus' "Pending"

pattern InstanceRefreshStatusInProgress :: InstanceRefreshStatus
pattern InstanceRefreshStatusInProgress = InstanceRefreshStatus' "InProgress"

pattern InstanceRefreshStatusSuccessful :: InstanceRefreshStatus
pattern InstanceRefreshStatusSuccessful = InstanceRefreshStatus' "Successful"

pattern InstanceRefreshStatusFailed :: InstanceRefreshStatus
pattern InstanceRefreshStatusFailed = InstanceRefreshStatus' "Failed"

pattern InstanceRefreshStatusCancelling :: InstanceRefreshStatus
pattern InstanceRefreshStatusCancelling = InstanceRefreshStatus' "Cancelling"

pattern InstanceRefreshStatusCancelled :: InstanceRefreshStatus
pattern InstanceRefreshStatusCancelled = InstanceRefreshStatus' "Cancelled"

{-# COMPLETE
  InstanceRefreshStatusPending,
  InstanceRefreshStatusInProgress,
  InstanceRefreshStatusSuccessful,
  InstanceRefreshStatusFailed,
  InstanceRefreshStatusCancelling,
  InstanceRefreshStatusCancelled,
  InstanceRefreshStatus'
  #-}
