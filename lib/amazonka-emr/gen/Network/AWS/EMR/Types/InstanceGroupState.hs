{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupState
  ( InstanceGroupState
      ( InstanceGroupState',
        Provisioning,
        Bootstrapping,
        Running,
        Reconfiguring,
        Resizing,
        Suspended,
        Terminating,
        Terminated,
        Arrested,
        ShuttingDown,
        Ended
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceGroupState = InstanceGroupState' Lude.Text
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

pattern Provisioning :: InstanceGroupState
pattern Provisioning = InstanceGroupState' "PROVISIONING"

pattern Bootstrapping :: InstanceGroupState
pattern Bootstrapping = InstanceGroupState' "BOOTSTRAPPING"

pattern Running :: InstanceGroupState
pattern Running = InstanceGroupState' "RUNNING"

pattern Reconfiguring :: InstanceGroupState
pattern Reconfiguring = InstanceGroupState' "RECONFIGURING"

pattern Resizing :: InstanceGroupState
pattern Resizing = InstanceGroupState' "RESIZING"

pattern Suspended :: InstanceGroupState
pattern Suspended = InstanceGroupState' "SUSPENDED"

pattern Terminating :: InstanceGroupState
pattern Terminating = InstanceGroupState' "TERMINATING"

pattern Terminated :: InstanceGroupState
pattern Terminated = InstanceGroupState' "TERMINATED"

pattern Arrested :: InstanceGroupState
pattern Arrested = InstanceGroupState' "ARRESTED"

pattern ShuttingDown :: InstanceGroupState
pattern ShuttingDown = InstanceGroupState' "SHUTTING_DOWN"

pattern Ended :: InstanceGroupState
pattern Ended = InstanceGroupState' "ENDED"

{-# COMPLETE
  Provisioning,
  Bootstrapping,
  Running,
  Reconfiguring,
  Resizing,
  Suspended,
  Terminating,
  Terminated,
  Arrested,
  ShuttingDown,
  Ended,
  InstanceGroupState'
  #-}
