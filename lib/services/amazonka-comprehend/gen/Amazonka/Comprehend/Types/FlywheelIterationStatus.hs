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
-- Module      : Amazonka.Comprehend.Types.FlywheelIterationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelIterationStatus
  ( FlywheelIterationStatus
      ( ..,
        FlywheelIterationStatus_COMPLETED,
        FlywheelIterationStatus_EVALUATING,
        FlywheelIterationStatus_FAILED,
        FlywheelIterationStatus_STOPPED,
        FlywheelIterationStatus_STOP_REQUESTED,
        FlywheelIterationStatus_TRAINING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FlywheelIterationStatus = FlywheelIterationStatus'
  { fromFlywheelIterationStatus ::
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

pattern FlywheelIterationStatus_COMPLETED :: FlywheelIterationStatus
pattern FlywheelIterationStatus_COMPLETED = FlywheelIterationStatus' "COMPLETED"

pattern FlywheelIterationStatus_EVALUATING :: FlywheelIterationStatus
pattern FlywheelIterationStatus_EVALUATING = FlywheelIterationStatus' "EVALUATING"

pattern FlywheelIterationStatus_FAILED :: FlywheelIterationStatus
pattern FlywheelIterationStatus_FAILED = FlywheelIterationStatus' "FAILED"

pattern FlywheelIterationStatus_STOPPED :: FlywheelIterationStatus
pattern FlywheelIterationStatus_STOPPED = FlywheelIterationStatus' "STOPPED"

pattern FlywheelIterationStatus_STOP_REQUESTED :: FlywheelIterationStatus
pattern FlywheelIterationStatus_STOP_REQUESTED = FlywheelIterationStatus' "STOP_REQUESTED"

pattern FlywheelIterationStatus_TRAINING :: FlywheelIterationStatus
pattern FlywheelIterationStatus_TRAINING = FlywheelIterationStatus' "TRAINING"

{-# COMPLETE
  FlywheelIterationStatus_COMPLETED,
  FlywheelIterationStatus_EVALUATING,
  FlywheelIterationStatus_FAILED,
  FlywheelIterationStatus_STOPPED,
  FlywheelIterationStatus_STOP_REQUESTED,
  FlywheelIterationStatus_TRAINING,
  FlywheelIterationStatus'
  #-}
