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
-- Module      : Amazonka.AutoScaling.Types.LifecycleState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LifecycleState
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
        LifecycleState_Terminating_Wait,
        LifecycleState_Warmed_Hibernated,
        LifecycleState_Warmed_Pending,
        LifecycleState_Warmed_Pending_Proceed,
        LifecycleState_Warmed_Pending_Wait,
        LifecycleState_Warmed_Running,
        LifecycleState_Warmed_Stopped,
        LifecycleState_Warmed_Terminated,
        LifecycleState_Warmed_Terminating,
        LifecycleState_Warmed_Terminating_Proceed,
        LifecycleState_Warmed_Terminating_Wait
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LifecycleState = LifecycleState'
  { fromLifecycleState ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

pattern LifecycleState_Warmed_Hibernated :: LifecycleState
pattern LifecycleState_Warmed_Hibernated = LifecycleState' "Warmed:Hibernated"

pattern LifecycleState_Warmed_Pending :: LifecycleState
pattern LifecycleState_Warmed_Pending = LifecycleState' "Warmed:Pending"

pattern LifecycleState_Warmed_Pending_Proceed :: LifecycleState
pattern LifecycleState_Warmed_Pending_Proceed = LifecycleState' "Warmed:Pending:Proceed"

pattern LifecycleState_Warmed_Pending_Wait :: LifecycleState
pattern LifecycleState_Warmed_Pending_Wait = LifecycleState' "Warmed:Pending:Wait"

pattern LifecycleState_Warmed_Running :: LifecycleState
pattern LifecycleState_Warmed_Running = LifecycleState' "Warmed:Running"

pattern LifecycleState_Warmed_Stopped :: LifecycleState
pattern LifecycleState_Warmed_Stopped = LifecycleState' "Warmed:Stopped"

pattern LifecycleState_Warmed_Terminated :: LifecycleState
pattern LifecycleState_Warmed_Terminated = LifecycleState' "Warmed:Terminated"

pattern LifecycleState_Warmed_Terminating :: LifecycleState
pattern LifecycleState_Warmed_Terminating = LifecycleState' "Warmed:Terminating"

pattern LifecycleState_Warmed_Terminating_Proceed :: LifecycleState
pattern LifecycleState_Warmed_Terminating_Proceed = LifecycleState' "Warmed:Terminating:Proceed"

pattern LifecycleState_Warmed_Terminating_Wait :: LifecycleState
pattern LifecycleState_Warmed_Terminating_Wait = LifecycleState' "Warmed:Terminating:Wait"

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
  LifecycleState_Warmed_Hibernated,
  LifecycleState_Warmed_Pending,
  LifecycleState_Warmed_Pending_Proceed,
  LifecycleState_Warmed_Pending_Wait,
  LifecycleState_Warmed_Running,
  LifecycleState_Warmed_Stopped,
  LifecycleState_Warmed_Terminated,
  LifecycleState_Warmed_Terminating,
  LifecycleState_Warmed_Terminating_Proceed,
  LifecycleState_Warmed_Terminating_Wait,
  LifecycleState'
  #-}
