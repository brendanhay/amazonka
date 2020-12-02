{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.EvaluationFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.EvaluationFilterVariable where

import Network.AWS.Prelude

-- | A list of the variables to use in searching or filtering @Evaluation@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @Evaluation@ creation date.    * @Status@ - Sets the search criteria to @Evaluation@ status.    * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked an evaluation.    * @MLModelId@ - Sets the search criteria to the @Predictor@ that was evaluated.    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in evaluation.    * @DataUri@ - Sets the search criteria to the data file(s) used in evaluation. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
data EvaluationFilterVariable
  = EvalCreatedAt
  | EvalDataSourceId
  | EvalDataURI
  | EvalIAMUser
  | EvalLastUpdatedAt
  | EvalMLModelId
  | EvalName
  | EvalStatus
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

instance FromText EvaluationFilterVariable where
  parser =
    takeLowerText >>= \case
      "createdat" -> pure EvalCreatedAt
      "datasourceid" -> pure EvalDataSourceId
      "datauri" -> pure EvalDataURI
      "iamuser" -> pure EvalIAMUser
      "lastupdatedat" -> pure EvalLastUpdatedAt
      "mlmodelid" -> pure EvalMLModelId
      "name" -> pure EvalName
      "status" -> pure EvalStatus
      e ->
        fromTextError $
          "Failure parsing EvaluationFilterVariable from value: '" <> e
            <> "'. Accepted values: createdat, datasourceid, datauri, iamuser, lastupdatedat, mlmodelid, name, status"

instance ToText EvaluationFilterVariable where
  toText = \case
    EvalCreatedAt -> "CreatedAt"
    EvalDataSourceId -> "DataSourceId"
    EvalDataURI -> "DataURI"
    EvalIAMUser -> "IAMUser"
    EvalLastUpdatedAt -> "LastUpdatedAt"
    EvalMLModelId -> "MLModelId"
    EvalName -> "Name"
    EvalStatus -> "Status"

instance Hashable EvaluationFilterVariable

instance NFData EvaluationFilterVariable

instance ToByteString EvaluationFilterVariable

instance ToQuery EvaluationFilterVariable

instance ToHeader EvaluationFilterVariable

instance ToJSON EvaluationFilterVariable where
  toJSON = toJSONText
