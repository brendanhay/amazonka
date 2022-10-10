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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFindingReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFindingReasonCode
  ( LambdaFunctionRecommendationFindingReasonCode
      ( ..,
        LambdaFunctionRecommendationFindingReasonCode_Inconclusive,
        LambdaFunctionRecommendationFindingReasonCode_InsufficientData,
        LambdaFunctionRecommendationFindingReasonCode_MemoryOverprovisioned,
        LambdaFunctionRecommendationFindingReasonCode_MemoryUnderprovisioned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LambdaFunctionRecommendationFindingReasonCode = LambdaFunctionRecommendationFindingReasonCode'
  { fromLambdaFunctionRecommendationFindingReasonCode ::
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

pattern LambdaFunctionRecommendationFindingReasonCode_Inconclusive :: LambdaFunctionRecommendationFindingReasonCode
pattern LambdaFunctionRecommendationFindingReasonCode_Inconclusive = LambdaFunctionRecommendationFindingReasonCode' "Inconclusive"

pattern LambdaFunctionRecommendationFindingReasonCode_InsufficientData :: LambdaFunctionRecommendationFindingReasonCode
pattern LambdaFunctionRecommendationFindingReasonCode_InsufficientData = LambdaFunctionRecommendationFindingReasonCode' "InsufficientData"

pattern LambdaFunctionRecommendationFindingReasonCode_MemoryOverprovisioned :: LambdaFunctionRecommendationFindingReasonCode
pattern LambdaFunctionRecommendationFindingReasonCode_MemoryOverprovisioned = LambdaFunctionRecommendationFindingReasonCode' "MemoryOverprovisioned"

pattern LambdaFunctionRecommendationFindingReasonCode_MemoryUnderprovisioned :: LambdaFunctionRecommendationFindingReasonCode
pattern LambdaFunctionRecommendationFindingReasonCode_MemoryUnderprovisioned = LambdaFunctionRecommendationFindingReasonCode' "MemoryUnderprovisioned"

{-# COMPLETE
  LambdaFunctionRecommendationFindingReasonCode_Inconclusive,
  LambdaFunctionRecommendationFindingReasonCode_InsufficientData,
  LambdaFunctionRecommendationFindingReasonCode_MemoryOverprovisioned,
  LambdaFunctionRecommendationFindingReasonCode_MemoryUnderprovisioned,
  LambdaFunctionRecommendationFindingReasonCode'
  #-}
