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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFindingReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFindingReasonCode
  ( ECSServiceRecommendationFindingReasonCode
      ( ..,
        ECSServiceRecommendationFindingReasonCode_CPUOverprovisioned,
        ECSServiceRecommendationFindingReasonCode_CPUUnderprovisioned,
        ECSServiceRecommendationFindingReasonCode_MemoryOverprovisioned,
        ECSServiceRecommendationFindingReasonCode_MemoryUnderprovisioned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ECSServiceRecommendationFindingReasonCode = ECSServiceRecommendationFindingReasonCode'
  { fromECSServiceRecommendationFindingReasonCode ::
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

pattern ECSServiceRecommendationFindingReasonCode_CPUOverprovisioned :: ECSServiceRecommendationFindingReasonCode
pattern ECSServiceRecommendationFindingReasonCode_CPUOverprovisioned = ECSServiceRecommendationFindingReasonCode' "CPUOverprovisioned"

pattern ECSServiceRecommendationFindingReasonCode_CPUUnderprovisioned :: ECSServiceRecommendationFindingReasonCode
pattern ECSServiceRecommendationFindingReasonCode_CPUUnderprovisioned = ECSServiceRecommendationFindingReasonCode' "CPUUnderprovisioned"

pattern ECSServiceRecommendationFindingReasonCode_MemoryOverprovisioned :: ECSServiceRecommendationFindingReasonCode
pattern ECSServiceRecommendationFindingReasonCode_MemoryOverprovisioned = ECSServiceRecommendationFindingReasonCode' "MemoryOverprovisioned"

pattern ECSServiceRecommendationFindingReasonCode_MemoryUnderprovisioned :: ECSServiceRecommendationFindingReasonCode
pattern ECSServiceRecommendationFindingReasonCode_MemoryUnderprovisioned = ECSServiceRecommendationFindingReasonCode' "MemoryUnderprovisioned"

{-# COMPLETE
  ECSServiceRecommendationFindingReasonCode_CPUOverprovisioned,
  ECSServiceRecommendationFindingReasonCode_CPUUnderprovisioned,
  ECSServiceRecommendationFindingReasonCode_MemoryOverprovisioned,
  ECSServiceRecommendationFindingReasonCode_MemoryUnderprovisioned,
  ECSServiceRecommendationFindingReasonCode'
  #-}
