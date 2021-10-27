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
-- Module      : Network.AWS.RobOMaker.Types.SimulationJobBatchStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.SimulationJobBatchStatus
  ( SimulationJobBatchStatus
      ( ..,
        SimulationJobBatchStatus_Canceled,
        SimulationJobBatchStatus_Canceling,
        SimulationJobBatchStatus_Completed,
        SimulationJobBatchStatus_Completing,
        SimulationJobBatchStatus_Failed,
        SimulationJobBatchStatus_InProgress,
        SimulationJobBatchStatus_Pending,
        SimulationJobBatchStatus_TimedOut,
        SimulationJobBatchStatus_TimingOut
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SimulationJobBatchStatus = SimulationJobBatchStatus'
  { fromSimulationJobBatchStatus ::
      Core.Text
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

pattern SimulationJobBatchStatus_Canceled :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_Canceled = SimulationJobBatchStatus' "Canceled"

pattern SimulationJobBatchStatus_Canceling :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_Canceling = SimulationJobBatchStatus' "Canceling"

pattern SimulationJobBatchStatus_Completed :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_Completed = SimulationJobBatchStatus' "Completed"

pattern SimulationJobBatchStatus_Completing :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_Completing = SimulationJobBatchStatus' "Completing"

pattern SimulationJobBatchStatus_Failed :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_Failed = SimulationJobBatchStatus' "Failed"

pattern SimulationJobBatchStatus_InProgress :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_InProgress = SimulationJobBatchStatus' "InProgress"

pattern SimulationJobBatchStatus_Pending :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_Pending = SimulationJobBatchStatus' "Pending"

pattern SimulationJobBatchStatus_TimedOut :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_TimedOut = SimulationJobBatchStatus' "TimedOut"

pattern SimulationJobBatchStatus_TimingOut :: SimulationJobBatchStatus
pattern SimulationJobBatchStatus_TimingOut = SimulationJobBatchStatus' "TimingOut"

{-# COMPLETE
  SimulationJobBatchStatus_Canceled,
  SimulationJobBatchStatus_Canceling,
  SimulationJobBatchStatus_Completed,
  SimulationJobBatchStatus_Completing,
  SimulationJobBatchStatus_Failed,
  SimulationJobBatchStatus_InProgress,
  SimulationJobBatchStatus_Pending,
  SimulationJobBatchStatus_TimedOut,
  SimulationJobBatchStatus_TimingOut,
  SimulationJobBatchStatus'
  #-}
