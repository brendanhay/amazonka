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
-- Module      : Amazonka.Athena.Types.CalculationExecutionState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CalculationExecutionState
  ( CalculationExecutionState
      ( ..,
        CalculationExecutionState_CANCELED,
        CalculationExecutionState_CANCELING,
        CalculationExecutionState_COMPLETED,
        CalculationExecutionState_CREATED,
        CalculationExecutionState_CREATING,
        CalculationExecutionState_FAILED,
        CalculationExecutionState_QUEUED,
        CalculationExecutionState_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CalculationExecutionState = CalculationExecutionState'
  { fromCalculationExecutionState ::
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

pattern CalculationExecutionState_CANCELED :: CalculationExecutionState
pattern CalculationExecutionState_CANCELED = CalculationExecutionState' "CANCELED"

pattern CalculationExecutionState_CANCELING :: CalculationExecutionState
pattern CalculationExecutionState_CANCELING = CalculationExecutionState' "CANCELING"

pattern CalculationExecutionState_COMPLETED :: CalculationExecutionState
pattern CalculationExecutionState_COMPLETED = CalculationExecutionState' "COMPLETED"

pattern CalculationExecutionState_CREATED :: CalculationExecutionState
pattern CalculationExecutionState_CREATED = CalculationExecutionState' "CREATED"

pattern CalculationExecutionState_CREATING :: CalculationExecutionState
pattern CalculationExecutionState_CREATING = CalculationExecutionState' "CREATING"

pattern CalculationExecutionState_FAILED :: CalculationExecutionState
pattern CalculationExecutionState_FAILED = CalculationExecutionState' "FAILED"

pattern CalculationExecutionState_QUEUED :: CalculationExecutionState
pattern CalculationExecutionState_QUEUED = CalculationExecutionState' "QUEUED"

pattern CalculationExecutionState_RUNNING :: CalculationExecutionState
pattern CalculationExecutionState_RUNNING = CalculationExecutionState' "RUNNING"

{-# COMPLETE
  CalculationExecutionState_CANCELED,
  CalculationExecutionState_CANCELING,
  CalculationExecutionState_COMPLETED,
  CalculationExecutionState_CREATED,
  CalculationExecutionState_CREATING,
  CalculationExecutionState_FAILED,
  CalculationExecutionState_QUEUED,
  CalculationExecutionState_RUNNING,
  CalculationExecutionState'
  #-}
