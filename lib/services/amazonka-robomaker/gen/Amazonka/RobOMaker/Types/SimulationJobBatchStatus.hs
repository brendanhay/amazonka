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
-- Module      : Amazonka.RobOMaker.Types.SimulationJobBatchStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationJobBatchStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SimulationJobBatchStatus = SimulationJobBatchStatus'
  { fromSimulationJobBatchStatus ::
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
