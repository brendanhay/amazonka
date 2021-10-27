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
-- Module      : Network.AWS.WellArchitected.Types.WorkloadImprovementStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.WorkloadImprovementStatus
  ( WorkloadImprovementStatus
      ( ..,
        WorkloadImprovementStatus_COMPLETE,
        WorkloadImprovementStatus_IN_PROGRESS,
        WorkloadImprovementStatus_NOT_APPLICABLE,
        WorkloadImprovementStatus_NOT_STARTED,
        WorkloadImprovementStatus_RISK_ACKNOWLEDGED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The improvement status for a workload.
newtype WorkloadImprovementStatus = WorkloadImprovementStatus'
  { fromWorkloadImprovementStatus ::
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

pattern WorkloadImprovementStatus_COMPLETE :: WorkloadImprovementStatus
pattern WorkloadImprovementStatus_COMPLETE = WorkloadImprovementStatus' "COMPLETE"

pattern WorkloadImprovementStatus_IN_PROGRESS :: WorkloadImprovementStatus
pattern WorkloadImprovementStatus_IN_PROGRESS = WorkloadImprovementStatus' "IN_PROGRESS"

pattern WorkloadImprovementStatus_NOT_APPLICABLE :: WorkloadImprovementStatus
pattern WorkloadImprovementStatus_NOT_APPLICABLE = WorkloadImprovementStatus' "NOT_APPLICABLE"

pattern WorkloadImprovementStatus_NOT_STARTED :: WorkloadImprovementStatus
pattern WorkloadImprovementStatus_NOT_STARTED = WorkloadImprovementStatus' "NOT_STARTED"

pattern WorkloadImprovementStatus_RISK_ACKNOWLEDGED :: WorkloadImprovementStatus
pattern WorkloadImprovementStatus_RISK_ACKNOWLEDGED = WorkloadImprovementStatus' "RISK_ACKNOWLEDGED"

{-# COMPLETE
  WorkloadImprovementStatus_COMPLETE,
  WorkloadImprovementStatus_IN_PROGRESS,
  WorkloadImprovementStatus_NOT_APPLICABLE,
  WorkloadImprovementStatus_NOT_STARTED,
  WorkloadImprovementStatus_RISK_ACKNOWLEDGED,
  WorkloadImprovementStatus'
  #-}
