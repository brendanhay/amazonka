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
        RRSActive,
        RRSCompleted,
        RRSDeleted,
        RRSDeleting,
        RRSFailed,
        RRSMissed,
        RRSPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReplicationRunState = ReplicationRunState' Lude.Text
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

pattern RRSActive :: ReplicationRunState
pattern RRSActive = ReplicationRunState' "ACTIVE"

pattern RRSCompleted :: ReplicationRunState
pattern RRSCompleted = ReplicationRunState' "COMPLETED"

pattern RRSDeleted :: ReplicationRunState
pattern RRSDeleted = ReplicationRunState' "DELETED"

pattern RRSDeleting :: ReplicationRunState
pattern RRSDeleting = ReplicationRunState' "DELETING"

pattern RRSFailed :: ReplicationRunState
pattern RRSFailed = ReplicationRunState' "FAILED"

pattern RRSMissed :: ReplicationRunState
pattern RRSMissed = ReplicationRunState' "MISSED"

pattern RRSPending :: ReplicationRunState
pattern RRSPending = ReplicationRunState' "PENDING"

{-# COMPLETE
  RRSActive,
  RRSCompleted,
  RRSDeleted,
  RRSDeleting,
  RRSFailed,
  RRSMissed,
  RRSPending,
  ReplicationRunState'
  #-}
