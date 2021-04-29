{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
  ( HyperParameterTuningJobStrategyType
      ( ..,
        HyperParameterTuningJobStrategyType_Bayesian,
        HyperParameterTuningJobStrategyType_Random
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The strategy hyperparameter tuning uses to find the best combination of
-- hyperparameters for your model. Currently, the only supported value is
-- @Bayesian@.
newtype HyperParameterTuningJobStrategyType = HyperParameterTuningJobStrategyType'
  { fromHyperParameterTuningJobStrategyType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern HyperParameterTuningJobStrategyType_Bayesian :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Bayesian = HyperParameterTuningJobStrategyType' "Bayesian"

pattern HyperParameterTuningJobStrategyType_Random :: HyperParameterTuningJobStrategyType
pattern HyperParameterTuningJobStrategyType_Random = HyperParameterTuningJobStrategyType' "Random"

{-# COMPLETE
  HyperParameterTuningJobStrategyType_Bayesian,
  HyperParameterTuningJobStrategyType_Random,
  HyperParameterTuningJobStrategyType'
  #-}
