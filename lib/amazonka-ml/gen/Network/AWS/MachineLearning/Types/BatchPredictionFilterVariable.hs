{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable where

import Network.AWS.Prelude

-- | A list of the variables to use in searching or filtering @BatchPrediction@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @BatchPrediction@ creation date.    * @Status@ - Sets the search criteria to @BatchPrediction@ status.    * @Name@ - Sets the search criteria to the contents of @BatchPrediction@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked the @BatchPrediction@ creation.    * @MLModelId@ - Sets the search criteria to the @MLModel@ used in the @BatchPrediction@ .    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in the @BatchPrediction@ .    * @DataURI@ - Sets the search criteria to the data file(s) used in the @BatchPrediction@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
data BatchPredictionFilterVariable
  = BatchCreatedAt
  | BatchDataSourceId
  | BatchDataURI
  | BatchIAMUser
  | BatchLastUpdatedAt
  | BatchMLModelId
  | BatchName
  | BatchStatus
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

instance FromText BatchPredictionFilterVariable where
  parser =
    takeLowerText >>= \case
      "createdat" -> pure BatchCreatedAt
      "datasourceid" -> pure BatchDataSourceId
      "datauri" -> pure BatchDataURI
      "iamuser" -> pure BatchIAMUser
      "lastupdatedat" -> pure BatchLastUpdatedAt
      "mlmodelid" -> pure BatchMLModelId
      "name" -> pure BatchName
      "status" -> pure BatchStatus
      e ->
        fromTextError $
          "Failure parsing BatchPredictionFilterVariable from value: '" <> e
            <> "'. Accepted values: createdat, datasourceid, datauri, iamuser, lastupdatedat, mlmodelid, name, status"

instance ToText BatchPredictionFilterVariable where
  toText = \case
    BatchCreatedAt -> "CreatedAt"
    BatchDataSourceId -> "DataSourceId"
    BatchDataURI -> "DataURI"
    BatchIAMUser -> "IAMUser"
    BatchLastUpdatedAt -> "LastUpdatedAt"
    BatchMLModelId -> "MLModelId"
    BatchName -> "Name"
    BatchStatus -> "Status"

instance Hashable BatchPredictionFilterVariable

instance NFData BatchPredictionFilterVariable

instance ToByteString BatchPredictionFilterVariable

instance ToQuery BatchPredictionFilterVariable

instance ToHeader BatchPredictionFilterVariable

instance ToJSON BatchPredictionFilterVariable where
  toJSON = toJSONText
