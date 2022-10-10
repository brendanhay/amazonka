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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilterName
  ( LambdaFunctionRecommendationFilterName
      ( ..,
        LambdaFunctionRecommendationFilterName_Finding,
        LambdaFunctionRecommendationFilterName_FindingReasonCode
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LambdaFunctionRecommendationFilterName = LambdaFunctionRecommendationFilterName'
  { fromLambdaFunctionRecommendationFilterName ::
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

pattern LambdaFunctionRecommendationFilterName_Finding :: LambdaFunctionRecommendationFilterName
pattern LambdaFunctionRecommendationFilterName_Finding = LambdaFunctionRecommendationFilterName' "Finding"

pattern LambdaFunctionRecommendationFilterName_FindingReasonCode :: LambdaFunctionRecommendationFilterName
pattern LambdaFunctionRecommendationFilterName_FindingReasonCode = LambdaFunctionRecommendationFilterName' "FindingReasonCode"

{-# COMPLETE
  LambdaFunctionRecommendationFilterName_Finding,
  LambdaFunctionRecommendationFilterName_FindingReasonCode,
  LambdaFunctionRecommendationFilterName'
  #-}
