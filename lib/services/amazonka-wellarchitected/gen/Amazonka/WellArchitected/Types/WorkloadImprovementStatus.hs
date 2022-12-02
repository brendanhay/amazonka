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
-- Module      : Amazonka.WellArchitected.Types.WorkloadImprovementStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.WorkloadImprovementStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The improvement status for a workload.
newtype WorkloadImprovementStatus = WorkloadImprovementStatus'
  { fromWorkloadImprovementStatus ::
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
