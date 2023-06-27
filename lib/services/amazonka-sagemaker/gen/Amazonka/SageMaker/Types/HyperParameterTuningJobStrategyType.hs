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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyType
  ( HyperParameterTuningJobStrategyType
      ( ..,
        HyperParameterTuningJobStrategyType_Bayesian,
        HyperParameterTuningJobStrategyType_Grid,
        HyperParameterTuningJobStrategyType_Hyperband,
        HyperParameterTuningJobStrategyType_Random
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The strategy hyperparameter tuning uses to find the best combination of
-- hyperparameters for your model.
newtype HyperParameterTuningJobStrategyType = HyperParameterTuningJobStrategyType'
  { fromHyperParameterTuningJobStrategyType ::
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

pattern HyperParameterTuningJobStrategyType_Bayesian :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Bayesian = HyperParameterTuningJobStrategyType' "Bayesian"

pattern HyperParameterTuningJobStrategyType_Grid :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Grid = HyperParameterTuningJobStrategyType' "Grid"

pattern HyperParameterTuningJobStrategyType_Hyperband :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Hyperband = HyperParameterTuningJobStrategyType' "Hyperband"

pattern HyperParameterTuningJobStrategyType_Random :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Random = HyperParameterTuningJobStrategyType' "Random"

{-# COMPLETE
  HyperParameterTuningJobStrategyType_Bayesian,
  HyperParameterTuningJobStrategyType_Grid,
  HyperParameterTuningJobStrategyType_Hyperband,
  HyperParameterTuningJobStrategyType_Random,
  HyperParameterTuningJobStrategyType'
  #-}
