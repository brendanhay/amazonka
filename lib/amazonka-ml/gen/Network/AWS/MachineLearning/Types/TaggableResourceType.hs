{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.TaggableResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.TaggableResourceType where

import Network.AWS.Prelude

data TaggableResourceType
  = BatchPrediction
  | DataSource
  | Evaluation
  | MLModel
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

instance FromText TaggableResourceType where
  parser =
    takeLowerText >>= \case
      "batchprediction" -> pure BatchPrediction
      "datasource" -> pure DataSource
      "evaluation" -> pure Evaluation
      "mlmodel" -> pure MLModel
      e ->
        fromTextError $
          "Failure parsing TaggableResourceType from value: '" <> e
            <> "'. Accepted values: batchprediction, datasource, evaluation, mlmodel"

instance ToText TaggableResourceType where
  toText = \case
    BatchPrediction -> "BatchPrediction"
    DataSource -> "DataSource"
    Evaluation -> "Evaluation"
    MLModel -> "MLModel"

instance Hashable TaggableResourceType

instance NFData TaggableResourceType

instance ToByteString TaggableResourceType

instance ToQuery TaggableResourceType

instance ToHeader TaggableResourceType

instance ToJSON TaggableResourceType where
  toJSON = toJSONText

instance FromJSON TaggableResourceType where
  parseJSON = parseJSONText "TaggableResourceType"
