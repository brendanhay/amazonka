{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType where

import Network.AWS.Prelude

data HyperParameterTuningJobWarmStartType
  = IdenticalDataAndAlgorithm
  | TransferLearning
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

instance FromText HyperParameterTuningJobWarmStartType where
  parser =
    takeLowerText >>= \case
      "identicaldataandalgorithm" -> pure IdenticalDataAndAlgorithm
      "transferlearning" -> pure TransferLearning
      e ->
        fromTextError $
          "Failure parsing HyperParameterTuningJobWarmStartType from value: '" <> e
            <> "'. Accepted values: identicaldataandalgorithm, transferlearning"

instance ToText HyperParameterTuningJobWarmStartType where
  toText = \case
    IdenticalDataAndAlgorithm -> "IdenticalDataAndAlgorithm"
    TransferLearning -> "TransferLearning"

instance Hashable HyperParameterTuningJobWarmStartType

instance NFData HyperParameterTuningJobWarmStartType

instance ToByteString HyperParameterTuningJobWarmStartType

instance ToQuery HyperParameterTuningJobWarmStartType

instance ToHeader HyperParameterTuningJobWarmStartType

instance ToJSON HyperParameterTuningJobWarmStartType where
  toJSON = toJSONText

instance FromJSON HyperParameterTuningJobWarmStartType where
  parseJSON = parseJSONText "HyperParameterTuningJobWarmStartType"
