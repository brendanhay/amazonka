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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFinding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFinding
  ( ECSServiceRecommendationFinding
      ( ..,
        ECSServiceRecommendationFinding_Optimized,
        ECSServiceRecommendationFinding_Overprovisioned,
        ECSServiceRecommendationFinding_Underprovisioned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ECSServiceRecommendationFinding = ECSServiceRecommendationFinding'
  { fromECSServiceRecommendationFinding ::
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

pattern ECSServiceRecommendationFinding_Optimized :: ECSServiceRecommendationFinding
pattern ECSServiceRecommendationFinding_Optimized = ECSServiceRecommendationFinding' "Optimized"

pattern ECSServiceRecommendationFinding_Overprovisioned :: ECSServiceRecommendationFinding
pattern ECSServiceRecommendationFinding_Overprovisioned = ECSServiceRecommendationFinding' "Overprovisioned"

pattern ECSServiceRecommendationFinding_Underprovisioned :: ECSServiceRecommendationFinding
pattern ECSServiceRecommendationFinding_Underprovisioned = ECSServiceRecommendationFinding' "Underprovisioned"

{-# COMPLETE
  ECSServiceRecommendationFinding_Optimized,
  ECSServiceRecommendationFinding_Overprovisioned,
  ECSServiceRecommendationFinding_Underprovisioned,
  ECSServiceRecommendationFinding'
  #-}
