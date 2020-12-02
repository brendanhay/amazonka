{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType where

import Network.AWS.Prelude

-- | The strategy hyperparameter tuning uses to find the best combination of hyperparameters for your model. Currently, the only supported value is @Bayesian@ .
data HyperParameterTuningJobStrategyType
  = Bayesian
  | Random
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HyperParameterTuningJobStrategyType where
  parser =
    takeLowerText >>= \case
      "bayesian" -> pure Bayesian
      "random" -> pure Random
      e ->
        fromTextError $
          "Failure parsing HyperParameterTuningJobStrategyType from value: '" <> e
            <> "'. Accepted values: bayesian, random"

instance ToText HyperParameterTuningJobStrategyType where
  toText = \case
    Bayesian -> "Bayesian"
    Random -> "Random"

instance Hashable HyperParameterTuningJobStrategyType

instance NFData HyperParameterTuningJobStrategyType

instance ToByteString HyperParameterTuningJobStrategyType

instance ToQuery HyperParameterTuningJobStrategyType

instance ToHeader HyperParameterTuningJobStrategyType

instance ToJSON HyperParameterTuningJobStrategyType where
  toJSON = toJSONText

instance FromJSON HyperParameterTuningJobStrategyType where
  parseJSON = parseJSONText "HyperParameterTuningJobStrategyType"
