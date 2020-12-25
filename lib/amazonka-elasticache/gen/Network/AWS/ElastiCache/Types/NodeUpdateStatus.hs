{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeUpdateStatus
  ( NodeUpdateStatus
      ( NodeUpdateStatus',
        NodeUpdateStatusNotApplied,
        NodeUpdateStatusWaitingToStart,
        NodeUpdateStatusInProgress,
        NodeUpdateStatusStopping,
        NodeUpdateStatusStopped,
        NodeUpdateStatusComplete,
        fromNodeUpdateStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype NodeUpdateStatus = NodeUpdateStatus'
  { fromNodeUpdateStatus ::
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

pattern NodeUpdateStatusNotApplied :: NodeUpdateStatus
pattern NodeUpdateStatusNotApplied = NodeUpdateStatus' "not-applied"

pattern NodeUpdateStatusWaitingToStart :: NodeUpdateStatus
pattern NodeUpdateStatusWaitingToStart = NodeUpdateStatus' "waiting-to-start"

pattern NodeUpdateStatusInProgress :: NodeUpdateStatus
pattern NodeUpdateStatusInProgress = NodeUpdateStatus' "in-progress"

pattern NodeUpdateStatusStopping :: NodeUpdateStatus
pattern NodeUpdateStatusStopping = NodeUpdateStatus' "stopping"

pattern NodeUpdateStatusStopped :: NodeUpdateStatus
pattern NodeUpdateStatusStopped = NodeUpdateStatus' "stopped"

pattern NodeUpdateStatusComplete :: NodeUpdateStatus
pattern NodeUpdateStatusComplete = NodeUpdateStatus' "complete"

{-# COMPLETE
  NodeUpdateStatusNotApplied,
  NodeUpdateStatusWaitingToStart,
  NodeUpdateStatusInProgress,
  NodeUpdateStatusStopping,
  NodeUpdateStatusStopped,
  NodeUpdateStatusComplete,
  NodeUpdateStatus'
  #-}
