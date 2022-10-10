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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFinding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFinding
  ( LambdaFunctionRecommendationFinding
      ( ..,
        LambdaFunctionRecommendationFinding_NotOptimized,
        LambdaFunctionRecommendationFinding_Optimized,
        LambdaFunctionRecommendationFinding_Unavailable
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LambdaFunctionRecommendationFinding = LambdaFunctionRecommendationFinding'
  { fromLambdaFunctionRecommendationFinding ::
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

pattern LambdaFunctionRecommendationFinding_NotOptimized :: LambdaFunctionRecommendationFinding
pattern LambdaFunctionRecommendationFinding_NotOptimized = LambdaFunctionRecommendationFinding' "NotOptimized"

pattern LambdaFunctionRecommendationFinding_Optimized :: LambdaFunctionRecommendationFinding
pattern LambdaFunctionRecommendationFinding_Optimized = LambdaFunctionRecommendationFinding' "Optimized"

pattern LambdaFunctionRecommendationFinding_Unavailable :: LambdaFunctionRecommendationFinding
pattern LambdaFunctionRecommendationFinding_Unavailable = LambdaFunctionRecommendationFinding' "Unavailable"

{-# COMPLETE
  LambdaFunctionRecommendationFinding_NotOptimized,
  LambdaFunctionRecommendationFinding_Optimized,
  LambdaFunctionRecommendationFinding_Unavailable,
  LambdaFunctionRecommendationFinding'
  #-}
