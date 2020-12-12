{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStateName
  ( InstanceStateName
      ( InstanceStateName',
        ISNPending,
        ISNRunning,
        ISNShuttingDown,
        ISNStopped,
        ISNStopping,
        ISNTerminated
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceStateName = InstanceStateName' Lude.Text
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

pattern ISNPending :: InstanceStateName
pattern ISNPending = InstanceStateName' "pending"

pattern ISNRunning :: InstanceStateName
pattern ISNRunning = InstanceStateName' "running"

pattern ISNShuttingDown :: InstanceStateName
pattern ISNShuttingDown = InstanceStateName' "shutting-down"

pattern ISNStopped :: InstanceStateName
pattern ISNStopped = InstanceStateName' "stopped"

pattern ISNStopping :: InstanceStateName
pattern ISNStopping = InstanceStateName' "stopping"

pattern ISNTerminated :: InstanceStateName
pattern ISNTerminated = InstanceStateName' "terminated"

{-# COMPLETE
  ISNPending,
  ISNRunning,
  ISNShuttingDown,
  ISNStopped,
  ISNStopping,
  ISNTerminated,
  InstanceStateName'
  #-}
