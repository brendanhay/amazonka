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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyDetectionTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectionTaskStatus
  ( AnomalyDetectionTaskStatus
      ( ..,
        AnomalyDetectionTaskStatus_COMPLETED,
        AnomalyDetectionTaskStatus_FAILED,
        AnomalyDetectionTaskStatus_FAILED_TO_SCHEDULE,
        AnomalyDetectionTaskStatus_IN_PROGRESS,
        AnomalyDetectionTaskStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnomalyDetectionTaskStatus = AnomalyDetectionTaskStatus'
  { fromAnomalyDetectionTaskStatus ::
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

pattern AnomalyDetectionTaskStatus_COMPLETED :: AnomalyDetectionTaskStatus
pattern AnomalyDetectionTaskStatus_COMPLETED = AnomalyDetectionTaskStatus' "COMPLETED"

pattern AnomalyDetectionTaskStatus_FAILED :: AnomalyDetectionTaskStatus
pattern AnomalyDetectionTaskStatus_FAILED = AnomalyDetectionTaskStatus' "FAILED"

pattern AnomalyDetectionTaskStatus_FAILED_TO_SCHEDULE :: AnomalyDetectionTaskStatus
pattern AnomalyDetectionTaskStatus_FAILED_TO_SCHEDULE = AnomalyDetectionTaskStatus' "FAILED_TO_SCHEDULE"

pattern AnomalyDetectionTaskStatus_IN_PROGRESS :: AnomalyDetectionTaskStatus
pattern AnomalyDetectionTaskStatus_IN_PROGRESS = AnomalyDetectionTaskStatus' "IN_PROGRESS"

pattern AnomalyDetectionTaskStatus_PENDING :: AnomalyDetectionTaskStatus
pattern AnomalyDetectionTaskStatus_PENDING = AnomalyDetectionTaskStatus' "PENDING"

{-# COMPLETE
  AnomalyDetectionTaskStatus_COMPLETED,
  AnomalyDetectionTaskStatus_FAILED,
  AnomalyDetectionTaskStatus_FAILED_TO_SCHEDULE,
  AnomalyDetectionTaskStatus_IN_PROGRESS,
  AnomalyDetectionTaskStatus_PENDING,
  AnomalyDetectionTaskStatus'
  #-}
