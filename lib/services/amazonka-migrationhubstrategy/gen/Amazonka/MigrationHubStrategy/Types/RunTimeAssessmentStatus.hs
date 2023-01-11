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
-- Module      : Amazonka.MigrationHubStrategy.Types.RunTimeAssessmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.RunTimeAssessmentStatus
  ( RunTimeAssessmentStatus
      ( ..,
        RunTimeAssessmentStatus_DataCollectionTaskFailed,
        RunTimeAssessmentStatus_DataCollectionTaskPartialSuccess,
        RunTimeAssessmentStatus_DataCollectionTaskScheduled,
        RunTimeAssessmentStatus_DataCollectionTaskStarted,
        RunTimeAssessmentStatus_DataCollectionTaskStopped,
        RunTimeAssessmentStatus_DataCollectionTaskSuccess,
        RunTimeAssessmentStatus_DataCollectionTaskToBeScheduled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RunTimeAssessmentStatus = RunTimeAssessmentStatus'
  { fromRunTimeAssessmentStatus ::
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

pattern RunTimeAssessmentStatus_DataCollectionTaskFailed :: RunTimeAssessmentStatus
pattern RunTimeAssessmentStatus_DataCollectionTaskFailed = RunTimeAssessmentStatus' "dataCollectionTaskFailed"

pattern RunTimeAssessmentStatus_DataCollectionTaskPartialSuccess :: RunTimeAssessmentStatus
pattern RunTimeAssessmentStatus_DataCollectionTaskPartialSuccess = RunTimeAssessmentStatus' "dataCollectionTaskPartialSuccess"

pattern RunTimeAssessmentStatus_DataCollectionTaskScheduled :: RunTimeAssessmentStatus
pattern RunTimeAssessmentStatus_DataCollectionTaskScheduled = RunTimeAssessmentStatus' "dataCollectionTaskScheduled"

pattern RunTimeAssessmentStatus_DataCollectionTaskStarted :: RunTimeAssessmentStatus
pattern RunTimeAssessmentStatus_DataCollectionTaskStarted = RunTimeAssessmentStatus' "dataCollectionTaskStarted"

pattern RunTimeAssessmentStatus_DataCollectionTaskStopped :: RunTimeAssessmentStatus
pattern RunTimeAssessmentStatus_DataCollectionTaskStopped = RunTimeAssessmentStatus' "dataCollectionTaskStopped"

pattern RunTimeAssessmentStatus_DataCollectionTaskSuccess :: RunTimeAssessmentStatus
pattern RunTimeAssessmentStatus_DataCollectionTaskSuccess = RunTimeAssessmentStatus' "dataCollectionTaskSuccess"

pattern RunTimeAssessmentStatus_DataCollectionTaskToBeScheduled :: RunTimeAssessmentStatus
pattern RunTimeAssessmentStatus_DataCollectionTaskToBeScheduled = RunTimeAssessmentStatus' "dataCollectionTaskToBeScheduled"

{-# COMPLETE
  RunTimeAssessmentStatus_DataCollectionTaskFailed,
  RunTimeAssessmentStatus_DataCollectionTaskPartialSuccess,
  RunTimeAssessmentStatus_DataCollectionTaskScheduled,
  RunTimeAssessmentStatus_DataCollectionTaskStarted,
  RunTimeAssessmentStatus_DataCollectionTaskStopped,
  RunTimeAssessmentStatus_DataCollectionTaskSuccess,
  RunTimeAssessmentStatus_DataCollectionTaskToBeScheduled,
  RunTimeAssessmentStatus'
  #-}
