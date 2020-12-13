{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        LSPending,
        LSPendingWait,
        LSPendingProceed,
        LSQuarantined,
        LSInService,
        LSTerminating,
        LSTerminatingWait,
        LSTerminatingProceed,
        LSTerminated,
        LSDetaching,
        LSDetached,
        LSEnteringStandby,
        LSStandby
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

pattern LSPending :: LifecycleState
pattern LSPending = LifecycleState' "Pending"

pattern LSPendingWait :: LifecycleState
pattern LSPendingWait = LifecycleState' "Pending:Wait"

pattern LSPendingProceed :: LifecycleState
pattern LSPendingProceed = LifecycleState' "Pending:Proceed"

pattern LSQuarantined :: LifecycleState
pattern LSQuarantined = LifecycleState' "Quarantined"

pattern LSInService :: LifecycleState
pattern LSInService = LifecycleState' "InService"

pattern LSTerminating :: LifecycleState
pattern LSTerminating = LifecycleState' "Terminating"

pattern LSTerminatingWait :: LifecycleState
pattern LSTerminatingWait = LifecycleState' "Terminating:Wait"

pattern LSTerminatingProceed :: LifecycleState
pattern LSTerminatingProceed = LifecycleState' "Terminating:Proceed"

pattern LSTerminated :: LifecycleState
pattern LSTerminated = LifecycleState' "Terminated"

pattern LSDetaching :: LifecycleState
pattern LSDetaching = LifecycleState' "Detaching"

pattern LSDetached :: LifecycleState
pattern LSDetached = LifecycleState' "Detached"

pattern LSEnteringStandby :: LifecycleState
pattern LSEnteringStandby = LifecycleState' "EnteringStandby"

pattern LSStandby :: LifecycleState
pattern LSStandby = LifecycleState' "Standby"

{-# COMPLETE
  LSPending,
  LSPendingWait,
  LSPendingProceed,
  LSQuarantined,
  LSInService,
  LSTerminating,
  LSTerminatingWait,
  LSTerminatingProceed,
  LSTerminated,
  LSDetaching,
  LSDetached,
  LSEnteringStandby,
  LSStandby,
  LifecycleState'
  #-}
