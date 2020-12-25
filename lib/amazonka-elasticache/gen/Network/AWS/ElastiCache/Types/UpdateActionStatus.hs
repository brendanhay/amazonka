{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UpdateActionStatus
  ( UpdateActionStatus
      ( UpdateActionStatus',
        UpdateActionStatusNotApplied,
        UpdateActionStatusWaitingToStart,
        UpdateActionStatusInProgress,
        UpdateActionStatusStopping,
        UpdateActionStatusStopped,
        UpdateActionStatusComplete,
        UpdateActionStatusScheduling,
        UpdateActionStatusScheduled,
        UpdateActionStatusNotApplicable,
        fromUpdateActionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UpdateActionStatus = UpdateActionStatus'
  { fromUpdateActionStatus ::
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

pattern UpdateActionStatusNotApplied :: UpdateActionStatus
pattern UpdateActionStatusNotApplied = UpdateActionStatus' "not-applied"

pattern UpdateActionStatusWaitingToStart :: UpdateActionStatus
pattern UpdateActionStatusWaitingToStart = UpdateActionStatus' "waiting-to-start"

pattern UpdateActionStatusInProgress :: UpdateActionStatus
pattern UpdateActionStatusInProgress = UpdateActionStatus' "in-progress"

pattern UpdateActionStatusStopping :: UpdateActionStatus
pattern UpdateActionStatusStopping = UpdateActionStatus' "stopping"

pattern UpdateActionStatusStopped :: UpdateActionStatus
pattern UpdateActionStatusStopped = UpdateActionStatus' "stopped"

pattern UpdateActionStatusComplete :: UpdateActionStatus
pattern UpdateActionStatusComplete = UpdateActionStatus' "complete"

pattern UpdateActionStatusScheduling :: UpdateActionStatus
pattern UpdateActionStatusScheduling = UpdateActionStatus' "scheduling"

pattern UpdateActionStatusScheduled :: UpdateActionStatus
pattern UpdateActionStatusScheduled = UpdateActionStatus' "scheduled"

pattern UpdateActionStatusNotApplicable :: UpdateActionStatus
pattern UpdateActionStatusNotApplicable = UpdateActionStatus' "not-applicable"

{-# COMPLETE
  UpdateActionStatusNotApplied,
  UpdateActionStatusWaitingToStart,
  UpdateActionStatusInProgress,
  UpdateActionStatusStopping,
  UpdateActionStatusStopped,
  UpdateActionStatusComplete,
  UpdateActionStatusScheduling,
  UpdateActionStatusScheduled,
  UpdateActionStatusNotApplicable,
  UpdateActionStatus'
  #-}
