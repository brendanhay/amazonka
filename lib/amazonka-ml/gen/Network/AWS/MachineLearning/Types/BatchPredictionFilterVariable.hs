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
        BatchCreatedAt,
        BatchLastUpdatedAt,
        BatchStatus,
        BatchName,
        BatchIAMUser,
        BatchMLModelId,
        BatchDataSourceId,
        BatchDataURI
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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
newtype BatchPredictionFilterVariable = BatchPredictionFilterVariable' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern BatchCreatedAt :: BatchPredictionFilterVariable
pattern BatchCreatedAt = BatchPredictionFilterVariable' "CreatedAt"

pattern BatchLastUpdatedAt :: BatchPredictionFilterVariable
pattern BatchLastUpdatedAt = BatchPredictionFilterVariable' "LastUpdatedAt"

pattern BatchStatus :: BatchPredictionFilterVariable
pattern BatchStatus = BatchPredictionFilterVariable' "Status"

pattern BatchName :: BatchPredictionFilterVariable
pattern BatchName = BatchPredictionFilterVariable' "Name"

pattern BatchIAMUser :: BatchPredictionFilterVariable
pattern BatchIAMUser = BatchPredictionFilterVariable' "IAMUser"

pattern BatchMLModelId :: BatchPredictionFilterVariable
pattern BatchMLModelId = BatchPredictionFilterVariable' "MLModelId"

pattern BatchDataSourceId :: BatchPredictionFilterVariable
pattern BatchDataSourceId = BatchPredictionFilterVariable' "DataSourceId"

pattern BatchDataURI :: BatchPredictionFilterVariable
pattern BatchDataURI = BatchPredictionFilterVariable' "DataURI"

{-# COMPLETE
  BatchCreatedAt,
  BatchLastUpdatedAt,
  BatchStatus,
  BatchName,
  BatchIAMUser,
  BatchMLModelId,
  BatchDataSourceId,
  BatchDataURI,
  BatchPredictionFilterVariable'
  #-}
