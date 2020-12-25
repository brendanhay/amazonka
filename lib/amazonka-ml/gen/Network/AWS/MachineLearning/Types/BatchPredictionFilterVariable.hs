{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
  ( BatchPredictionFilterVariable
      ( BatchPredictionFilterVariable',
        BatchPredictionFilterVariableBatchCreatedAt,
        BatchPredictionFilterVariableBatchLastUpdatedAt,
        BatchPredictionFilterVariableBatchStatus,
        BatchPredictionFilterVariableBatchName,
        BatchPredictionFilterVariableBatchIAMUser,
        BatchPredictionFilterVariableBatchMLModelId,
        BatchPredictionFilterVariableBatchDataSourceId,
        BatchPredictionFilterVariableBatchDataURI,
        fromBatchPredictionFilterVariable
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | A list of the variables to use in searching or filtering @BatchPrediction@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @BatchPrediction@ creation date.
--
--     * @Status@ - Sets the search criteria to @BatchPrediction@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @BatchPrediction@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @BatchPrediction@ creation.
--
--     * @MLModelId@ - Sets the search criteria to the @MLModel@ used in the @BatchPrediction@ .
--
--     * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in the @BatchPrediction@ .
--
--     * @DataURI@ - Sets the search criteria to the data file(s) used in the @BatchPrediction@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
newtype BatchPredictionFilterVariable = BatchPredictionFilterVariable'
  { fromBatchPredictionFilterVariable ::
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

pattern BatchPredictionFilterVariableBatchCreatedAt :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchCreatedAt = BatchPredictionFilterVariable' "CreatedAt"

pattern BatchPredictionFilterVariableBatchLastUpdatedAt :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchLastUpdatedAt = BatchPredictionFilterVariable' "LastUpdatedAt"

pattern BatchPredictionFilterVariableBatchStatus :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchStatus = BatchPredictionFilterVariable' "Status"

pattern BatchPredictionFilterVariableBatchName :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchName = BatchPredictionFilterVariable' "Name"

pattern BatchPredictionFilterVariableBatchIAMUser :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchIAMUser = BatchPredictionFilterVariable' "IAMUser"

pattern BatchPredictionFilterVariableBatchMLModelId :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchMLModelId = BatchPredictionFilterVariable' "MLModelId"

pattern BatchPredictionFilterVariableBatchDataSourceId :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchDataSourceId = BatchPredictionFilterVariable' "DataSourceId"

pattern BatchPredictionFilterVariableBatchDataURI :: BatchPredictionFilterVariable
pattern BatchPredictionFilterVariableBatchDataURI = BatchPredictionFilterVariable' "DataURI"

{-# COMPLETE
  BatchPredictionFilterVariableBatchCreatedAt,
  BatchPredictionFilterVariableBatchLastUpdatedAt,
  BatchPredictionFilterVariableBatchStatus,
  BatchPredictionFilterVariableBatchName,
  BatchPredictionFilterVariableBatchIAMUser,
  BatchPredictionFilterVariableBatchMLModelId,
  BatchPredictionFilterVariableBatchDataSourceId,
  BatchPredictionFilterVariableBatchDataURI,
  BatchPredictionFilterVariable'
  #-}
