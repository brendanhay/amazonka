{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AgentUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AgentUpdateStatus
  ( AgentUpdateStatus
      ( AgentUpdateStatus',
        AgentUpdateStatusPending,
        AgentUpdateStatusStaging,
        AgentUpdateStatusStaged,
        AgentUpdateStatusUpdating,
        AgentUpdateStatusUpdated,
        AgentUpdateStatusFailed,
        fromAgentUpdateStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AgentUpdateStatus = AgentUpdateStatus'
  { fromAgentUpdateStatus ::
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

pattern AgentUpdateStatusPending :: AgentUpdateStatus
pattern AgentUpdateStatusPending = AgentUpdateStatus' "PENDING"

pattern AgentUpdateStatusStaging :: AgentUpdateStatus
pattern AgentUpdateStatusStaging = AgentUpdateStatus' "STAGING"

pattern AgentUpdateStatusStaged :: AgentUpdateStatus
pattern AgentUpdateStatusStaged = AgentUpdateStatus' "STAGED"

pattern AgentUpdateStatusUpdating :: AgentUpdateStatus
pattern AgentUpdateStatusUpdating = AgentUpdateStatus' "UPDATING"

pattern AgentUpdateStatusUpdated :: AgentUpdateStatus
pattern AgentUpdateStatusUpdated = AgentUpdateStatus' "UPDATED"

pattern AgentUpdateStatusFailed :: AgentUpdateStatus
pattern AgentUpdateStatusFailed = AgentUpdateStatus' "FAILED"

{-# COMPLETE
  AgentUpdateStatusPending,
  AgentUpdateStatusStaging,
  AgentUpdateStatusStaged,
  AgentUpdateStatusUpdating,
  AgentUpdateStatusUpdated,
  AgentUpdateStatusFailed,
  AgentUpdateStatus'
  #-}
