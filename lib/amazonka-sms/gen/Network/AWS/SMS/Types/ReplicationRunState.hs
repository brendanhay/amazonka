{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRunState
  ( ReplicationRunState
      ( ReplicationRunState',
        ReplicationRunStatePending,
        ReplicationRunStateMissed,
        ReplicationRunStateActive,
        ReplicationRunStateFailed,
        ReplicationRunStateCompleted,
        ReplicationRunStateDeleting,
        ReplicationRunStateDeleted,
        fromReplicationRunState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ReplicationRunState = ReplicationRunState'
  { fromReplicationRunState ::
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

pattern ReplicationRunStatePending :: ReplicationRunState
pattern ReplicationRunStatePending = ReplicationRunState' "PENDING"

pattern ReplicationRunStateMissed :: ReplicationRunState
pattern ReplicationRunStateMissed = ReplicationRunState' "MISSED"

pattern ReplicationRunStateActive :: ReplicationRunState
pattern ReplicationRunStateActive = ReplicationRunState' "ACTIVE"

pattern ReplicationRunStateFailed :: ReplicationRunState
pattern ReplicationRunStateFailed = ReplicationRunState' "FAILED"

pattern ReplicationRunStateCompleted :: ReplicationRunState
pattern ReplicationRunStateCompleted = ReplicationRunState' "COMPLETED"

pattern ReplicationRunStateDeleting :: ReplicationRunState
pattern ReplicationRunStateDeleting = ReplicationRunState' "DELETING"

pattern ReplicationRunStateDeleted :: ReplicationRunState
pattern ReplicationRunStateDeleted = ReplicationRunState' "DELETED"

{-# COMPLETE
  ReplicationRunStatePending,
  ReplicationRunStateMissed,
  ReplicationRunStateActive,
  ReplicationRunStateFailed,
  ReplicationRunStateCompleted,
  ReplicationRunStateDeleting,
  ReplicationRunStateDeleted,
  ReplicationRunState'
  #-}
