{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationJobState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ReplicationJobState
  ( ReplicationJobState
    ( ReplicationJobState'
    , ReplicationJobStatePending
    , ReplicationJobStateActive
    , ReplicationJobStateFailed
    , ReplicationJobStateDeleting
    , ReplicationJobStateDeleted
    , ReplicationJobStateCompleted
    , ReplicationJobStatePausedOnFailure
    , ReplicationJobStateFailing
    , fromReplicationJobState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ReplicationJobState = ReplicationJobState'{fromReplicationJobState
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern ReplicationJobStatePending :: ReplicationJobState
pattern ReplicationJobStatePending = ReplicationJobState' "PENDING"

pattern ReplicationJobStateActive :: ReplicationJobState
pattern ReplicationJobStateActive = ReplicationJobState' "ACTIVE"

pattern ReplicationJobStateFailed :: ReplicationJobState
pattern ReplicationJobStateFailed = ReplicationJobState' "FAILED"

pattern ReplicationJobStateDeleting :: ReplicationJobState
pattern ReplicationJobStateDeleting = ReplicationJobState' "DELETING"

pattern ReplicationJobStateDeleted :: ReplicationJobState
pattern ReplicationJobStateDeleted = ReplicationJobState' "DELETED"

pattern ReplicationJobStateCompleted :: ReplicationJobState
pattern ReplicationJobStateCompleted = ReplicationJobState' "COMPLETED"

pattern ReplicationJobStatePausedOnFailure :: ReplicationJobState
pattern ReplicationJobStatePausedOnFailure = ReplicationJobState' "PAUSED_ON_FAILURE"

pattern ReplicationJobStateFailing :: ReplicationJobState
pattern ReplicationJobStateFailing = ReplicationJobState' "FAILING"

{-# COMPLETE 
  ReplicationJobStatePending,

  ReplicationJobStateActive,

  ReplicationJobStateFailed,

  ReplicationJobStateDeleting,

  ReplicationJobStateDeleted,

  ReplicationJobStateCompleted,

  ReplicationJobStatePausedOnFailure,

  ReplicationJobStateFailing,
  ReplicationJobState'
  #-}
