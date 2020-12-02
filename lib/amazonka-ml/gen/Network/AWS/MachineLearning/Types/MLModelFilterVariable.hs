{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.MLModelFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.MLModelFilterVariable where

import Network.AWS.Prelude

data MLModelFilterVariable
  = MLMFVAlgorithm
  | MLMFVCreatedAt
  | MLMFVIAMUser
  | MLMFVLastUpdatedAt
  | MLMFVMLModelType
  | MLMFVName
  | MLMFVRealtimeEndpointStatus
  | MLMFVStatus
  | MLMFVTrainingDataSourceId
  | MLMFVTrainingDataURI
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

instance FromText MLModelFilterVariable where
  parser =
    takeLowerText >>= \case
      "algorithm" -> pure MLMFVAlgorithm
      "createdat" -> pure MLMFVCreatedAt
      "iamuser" -> pure MLMFVIAMUser
      "lastupdatedat" -> pure MLMFVLastUpdatedAt
      "mlmodeltype" -> pure MLMFVMLModelType
      "name" -> pure MLMFVName
      "realtimeendpointstatus" -> pure MLMFVRealtimeEndpointStatus
      "status" -> pure MLMFVStatus
      "trainingdatasourceid" -> pure MLMFVTrainingDataSourceId
      "trainingdatauri" -> pure MLMFVTrainingDataURI
      e ->
        fromTextError $
          "Failure parsing MLModelFilterVariable from value: '" <> e
            <> "'. Accepted values: algorithm, createdat, iamuser, lastupdatedat, mlmodeltype, name, realtimeendpointstatus, status, trainingdatasourceid, trainingdatauri"

instance ToText MLModelFilterVariable where
  toText = \case
    MLMFVAlgorithm -> "Algorithm"
    MLMFVCreatedAt -> "CreatedAt"
    MLMFVIAMUser -> "IAMUser"
    MLMFVLastUpdatedAt -> "LastUpdatedAt"
    MLMFVMLModelType -> "MLModelType"
    MLMFVName -> "Name"
    MLMFVRealtimeEndpointStatus -> "RealtimeEndpointStatus"
    MLMFVStatus -> "Status"
    MLMFVTrainingDataSourceId -> "TrainingDataSourceId"
    MLMFVTrainingDataURI -> "TrainingDataURI"

instance Hashable MLModelFilterVariable

instance NFData MLModelFilterVariable

instance ToByteString MLModelFilterVariable

instance ToQuery MLModelFilterVariable

instance ToHeader MLModelFilterVariable

instance ToJSON MLModelFilterVariable where
  toJSON = toJSONText
