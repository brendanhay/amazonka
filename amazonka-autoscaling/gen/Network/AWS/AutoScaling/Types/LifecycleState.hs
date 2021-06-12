{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LifecycleState
  ( LifecycleState
      ( ..,
        LifecycleState_Detached,
        LifecycleState_Detaching,
        LifecycleState_EnteringStandby,
        LifecycleState_InService,
        LifecycleState_Pending,
        LifecycleState_Pending_Proceed,
        LifecycleState_Pending_Wait,
        LifecycleState_Quarantined,
        LifecycleState_Standby,
        LifecycleState_Terminated,
        LifecycleState_Terminating,
        LifecycleState_Terminating_Proceed,
        LifecycleState_Terminating_Wait
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LifecycleState = LifecycleState'
  { fromLifecycleState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern LifecycleState_Detached :: LifecycleState
pattern LifecycleState_Detached = LifecycleState' "Detached"

pattern LifecycleState_Detaching :: LifecycleState
pattern LifecycleState_Detaching = LifecycleState' "Detaching"

pattern LifecycleState_EnteringStandby :: LifecycleState
pattern LifecycleState_EnteringStandby = LifecycleState' "EnteringStandby"

pattern LifecycleState_InService :: LifecycleState
pattern LifecycleState_InService = LifecycleState' "InService"

pattern LifecycleState_Pending :: LifecycleState
pattern LifecycleState_Pending = LifecycleState' "Pending"

pattern LifecycleState_Pending_Proceed :: LifecycleState
pattern LifecycleState_Pending_Proceed = LifecycleState' "Pending:Proceed"

pattern LifecycleState_Pending_Wait :: LifecycleState
pattern LifecycleState_Pending_Wait = LifecycleState' "Pending:Wait"

pattern LifecycleState_Quarantined :: LifecycleState
pattern LifecycleState_Quarantined = LifecycleState' "Quarantined"

pattern LifecycleState_Standby :: LifecycleState
pattern LifecycleState_Standby = LifecycleState' "Standby"

pattern LifecycleState_Terminated :: LifecycleState
pattern LifecycleState_Terminated = LifecycleState' "Terminated"

pattern LifecycleState_Terminating :: LifecycleState
pattern LifecycleState_Terminating = LifecycleState' "Terminating"

pattern LifecycleState_Terminating_Proceed :: LifecycleState
pattern LifecycleState_Terminating_Proceed = LifecycleState' "Terminating:Proceed"

pattern LifecycleState_Terminating_Wait :: LifecycleState
pattern LifecycleState_Terminating_Wait = LifecycleState' "Terminating:Wait"

{-# COMPLETE
  LifecycleState_Detached,
  LifecycleState_Detaching,
  LifecycleState_EnteringStandby,
  LifecycleState_InService,
  LifecycleState_Pending,
  LifecycleState_Pending_Proceed,
  LifecycleState_Pending_Wait,
  LifecycleState_Quarantined,
  LifecycleState_Standby,
  LifecycleState_Terminated,
  LifecycleState_Terminating,
  LifecycleState_Terminating_Proceed,
  LifecycleState_Terminating_Wait,
  LifecycleState'
  #-}
