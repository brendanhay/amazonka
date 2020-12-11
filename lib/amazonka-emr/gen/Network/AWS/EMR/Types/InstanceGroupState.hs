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
        Arrested,
        Bootstrapping,
        Ended,
        Provisioning,
        Reconfiguring,
        Resizing,
        Running,
        ShuttingDown,
        Suspended,
        Terminated,
        Terminating
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

pattern Arrested :: InstanceGroupState
pattern Arrested = InstanceGroupState' "ARRESTED"

pattern Bootstrapping :: InstanceGroupState
pattern Bootstrapping = InstanceGroupState' "BOOTSTRAPPING"

pattern Ended :: InstanceGroupState
pattern Ended = InstanceGroupState' "ENDED"

pattern Provisioning :: InstanceGroupState
pattern Provisioning = InstanceGroupState' "PROVISIONING"

pattern Reconfiguring :: InstanceGroupState
pattern Reconfiguring = InstanceGroupState' "RECONFIGURING"

pattern Resizing :: InstanceGroupState
pattern Resizing = InstanceGroupState' "RESIZING"

pattern Running :: InstanceGroupState
pattern Running = InstanceGroupState' "RUNNING"

pattern ShuttingDown :: InstanceGroupState
pattern ShuttingDown = InstanceGroupState' "SHUTTING_DOWN"

pattern Suspended :: InstanceGroupState
pattern Suspended = InstanceGroupState' "SUSPENDED"

pattern Terminated :: InstanceGroupState
pattern Terminated = InstanceGroupState' "TERMINATED"

pattern Terminating :: InstanceGroupState
pattern Terminating = InstanceGroupState' "TERMINATING"

{-# COMPLETE
  Arrested,
  Bootstrapping,
  Ended,
  Provisioning,
  Reconfiguring,
  Resizing,
  Running,
  ShuttingDown,
  Suspended,
  Terminated,
  Terminating,
  InstanceGroupState'
  #-}
