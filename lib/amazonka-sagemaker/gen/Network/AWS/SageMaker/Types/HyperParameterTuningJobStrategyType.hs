{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
  ( HyperParameterTuningJobStrategyType
      ( HyperParameterTuningJobStrategyType',
        HyperParameterTuningJobStrategyTypeBayesian,
        HyperParameterTuningJobStrategyTypeRandom,
        fromHyperParameterTuningJobStrategyType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The strategy hyperparameter tuning uses to find the best combination of hyperparameters for your model. Currently, the only supported value is @Bayesian@ .
newtype HyperParameterTuningJobStrategyType = HyperParameterTuningJobStrategyType'
  { fromHyperParameterTuningJobStrategyType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HyperParameterTuningJobStrategyTypeBayesian :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyTypeBayesian = HyperParameterTuningJobStrategyType' "Bayesian"

pattern HyperParameterTuningJobStrategyTypeRandom :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyTypeRandom = HyperParameterTuningJobStrategyType' "Random"

{-# COMPLETE
  HyperParameterTuningJobStrategyTypeBayesian,
  HyperParameterTuningJobStrategyTypeRandom,
  HyperParameterTuningJobStrategyType'
  #-}
