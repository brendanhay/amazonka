{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLMetricEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLMetricEnum where

import Network.AWS.Prelude

data AutoMLMetricEnum
  = Accuracy
  | Auc
  | F1
  | F1macro
  | Mse
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

instance FromText AutoMLMetricEnum where
  parser =
    takeLowerText >>= \case
      "accuracy" -> pure Accuracy
      "auc" -> pure Auc
      "f1" -> pure F1
      "f1macro" -> pure F1macro
      "mse" -> pure Mse
      e ->
        fromTextError $
          "Failure parsing AutoMLMetricEnum from value: '" <> e
            <> "'. Accepted values: accuracy, auc, f1, f1macro, mse"

instance ToText AutoMLMetricEnum where
  toText = \case
    Accuracy -> "Accuracy"
    Auc -> "AUC"
    F1 -> "F1"
    F1macro -> "F1macro"
    Mse -> "MSE"

instance Hashable AutoMLMetricEnum

instance NFData AutoMLMetricEnum

instance ToByteString AutoMLMetricEnum

instance ToQuery AutoMLMetricEnum

instance ToHeader AutoMLMetricEnum

instance ToJSON AutoMLMetricEnum where
  toJSON = toJSONText

instance FromJSON AutoMLMetricEnum where
  parseJSON = parseJSONText "AutoMLMetricEnum"
