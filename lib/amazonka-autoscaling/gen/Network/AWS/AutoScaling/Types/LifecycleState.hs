-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleState
  ( LifecycleState
      ( LifecycleState',
        Detached,
        Detaching,
        EnteringStandby,
        InService,
        Pending,
        PendingProceed,
        PendingWait,
        Quarantined,
        Standby,
        Terminated,
        Terminating,
        TerminatingProceed,
        TerminatingWait
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LifecycleState = LifecycleState' Lude.Text
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

pattern Detached :: LifecycleState
pattern Detached = LifecycleState' "Detached"

pattern Detaching :: LifecycleState
pattern Detaching = LifecycleState' "Detaching"

pattern EnteringStandby :: LifecycleState
pattern EnteringStandby = LifecycleState' "EnteringStandby"

pattern InService :: LifecycleState
pattern InService = LifecycleState' "InService"

pattern Pending :: LifecycleState
pattern Pending = LifecycleState' "Pending"

pattern PendingProceed :: LifecycleState
pattern PendingProceed = LifecycleState' "Pending:Proceed"

pattern PendingWait :: LifecycleState
pattern PendingWait = LifecycleState' "Pending:Wait"

pattern Quarantined :: LifecycleState
pattern Quarantined = LifecycleState' "Quarantined"

pattern Standby :: LifecycleState
pattern Standby = LifecycleState' "Standby"

pattern Terminated :: LifecycleState
pattern Terminated = LifecycleState' "Terminated"

pattern Terminating :: LifecycleState
pattern Terminating = LifecycleState' "Terminating"

pattern TerminatingProceed :: LifecycleState
pattern TerminatingProceed = LifecycleState' "Terminating:Proceed"

pattern TerminatingWait :: LifecycleState
pattern TerminatingWait = LifecycleState' "Terminating:Wait"

{-# COMPLETE
  Detached,
  Detaching,
  EnteringStandby,
  InService,
  Pending,
  PendingProceed,
  PendingWait,
  Quarantined,
  Standby,
  Terminated,
  Terminating,
  TerminatingProceed,
  TerminatingWait,
  LifecycleState'
  #-}
