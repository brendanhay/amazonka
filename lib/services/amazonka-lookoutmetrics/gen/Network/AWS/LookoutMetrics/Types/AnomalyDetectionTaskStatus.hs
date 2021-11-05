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
-- Module      : Network.AWS.LookoutMetrics.Types.AnomalyDetectionTaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutMetrics.Types.AnomalyDetectionTaskStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AnomalyDetectionTaskStatus = AnomalyDetectionTaskStatus'
  { fromAnomalyDetectionTaskStatus ::
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
