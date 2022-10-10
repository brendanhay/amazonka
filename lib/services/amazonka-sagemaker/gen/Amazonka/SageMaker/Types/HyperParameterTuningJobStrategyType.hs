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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyType
  ( HyperParameterTuningJobStrategyType
      ( ..,
        HyperParameterTuningJobStrategyType_Bayesian,
        HyperParameterTuningJobStrategyType_Hyperband,
        HyperParameterTuningJobStrategyType_Random
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The strategy hyperparameter tuning uses to find the best combination of
-- hyperparameters for your model. Currently, the only supported value is
-- @Bayesian@.
newtype HyperParameterTuningJobStrategyType = HyperParameterTuningJobStrategyType'
  { fromHyperParameterTuningJobStrategyType ::
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

pattern HyperParameterTuningJobStrategyType_Bayesian :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Bayesian = HyperParameterTuningJobStrategyType' "Bayesian"

pattern HyperParameterTuningJobStrategyType_Hyperband :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Hyperband = HyperParameterTuningJobStrategyType' "Hyperband"

pattern HyperParameterTuningJobStrategyType_Random :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Random = HyperParameterTuningJobStrategyType' "Random"

{-# COMPLETE
  HyperParameterTuningJobStrategyType_Bayesian,
  HyperParameterTuningJobStrategyType_Hyperband,
  HyperParameterTuningJobStrategyType_Random,
  HyperParameterTuningJobStrategyType'
  #-}
