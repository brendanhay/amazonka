{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.S3DataSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.S3DataSpec
  ( S3DataSpec (..),

    -- * Smart constructor
    mkS3DataSpec,

    -- * Lenses
    sdsDataLocationS3,
    sdsDataRearrangement,
    sdsDataSchema,
    sdsDataSchemaLocationS3,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.DataLocationS3 as Types
import qualified Network.AWS.MachineLearning.Types.DataRearrangement as Types
import qualified Network.AWS.MachineLearning.Types.DataSchema as Types
import qualified Network.AWS.MachineLearning.Types.DataSchemaLocationS3 as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the data specification of a @DataSource@ .
--
-- /See:/ 'mkS3DataSpec' smart constructor.
data S3DataSpec = S3DataSpec'
  { -- | The location of the data file(s) used by a @DataSource@ . The URI specifies a data file or an Amazon Simple Storage Service (Amazon S3) directory or bucket containing data files.
    dataLocationS3 :: Types.DataLocationS3,
    -- | A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ .
    --
    -- There are multiple parameters that control what data is used to create a datasource:
    --
    --     * __@percentBegin@ __
    -- Use @percentBegin@ to indicate the beginning of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.
    --
    --
    --     * __@percentEnd@ __
    -- Use @percentEnd@ to indicate the end of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.
    --
    --
    --     * __@complement@ __
    -- The @complement@ parameter instructs Amazon ML to use the data that is not included in the range of @percentBegin@ to @percentEnd@ to create a datasource. The @complement@ parameter is useful if you need to create complementary datasources for training and evaluation. To create a complementary datasource, use the same values for @percentBegin@ and @percentEnd@ , along with the @complement@ parameter.
    -- For example, the following two datasources do not share any data, and can be used to train and evaluate a model. The first datasource has 25 percent of the data, and the second one has 75 percent of the data.
    -- Datasource for evaluation: @{"splitting":{"percentBegin":0, "percentEnd":25}}@
    -- Datasource for training: @{"splitting":{"percentBegin":0, "percentEnd":25, "complement":"true"}}@
    --
    --
    --     * __@strategy@ __
    -- To change how Amazon ML splits the data for a datasource, use the @strategy@ parameter.
    -- The default value for the @strategy@ parameter is @sequential@ , meaning that Amazon ML takes all of the data records between the @percentBegin@ and @percentEnd@ parameters for the datasource, in the order that the records appear in the input data.
    -- The following two @DataRearrangement@ lines are examples of sequentially ordered training and evaluation datasources:
    -- Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential"}}@
    -- Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential", "complement":"true"}}@
    -- To randomly split the input data into the proportions indicated by the percentBegin and percentEnd parameters, set the @strategy@ parameter to @random@ and provide a string that is used as the seed value for the random data splitting (for example, you can use the S3 path to your data as the random seed string). If you choose the random split strategy, Amazon ML assigns each row of data a pseudo-random number between 0 and 100, and then selects the rows that have an assigned number between @percentBegin@ and @percentEnd@ . Pseudo-random numbers are assigned using both the input seed string value and the byte offset as a seed, so changing the data results in a different split. Any existing ordering is preserved. The random splitting strategy ensures that variables in the training and evaluation data are distributed similarly. It is useful in the cases where the input data may have an implicit sort order, which would otherwise result in training and evaluation datasources containing non-similar data records.
    -- The following two @DataRearrangement@ lines are examples of non-sequentially ordered training and evaluation datasources:
    -- Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv"}}@
    -- Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv", "complement":"true"}}@
    dataRearrangement :: Core.Maybe Types.DataRearrangement,
    -- | A JSON string that represents the schema for an Amazon S3 @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
    --
    -- You must provide either the @DataSchema@ or the @DataSchemaLocationS3@ .
    -- Define your @DataSchema@ as a series of key-value pairs. @attributes@ and @excludedVariableNames@ have an array of key-value pairs for their value. Use the following format to define your @DataSchema@ .
    -- { "version": "1.0",
    -- "recordAnnotationFieldName": "F1",
    -- "recordWeightFieldName": "F2",
    -- "targetFieldName": "F3",
    -- "dataFormat": "CSV",
    -- "dataFileContainsHeader": true,
    -- "attributes": [
    -- { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ],
    -- "excludedVariableNames": [ "F6" ] }
    dataSchema :: Core.Maybe Types.DataSchema,
    -- | Describes the schema location in Amazon S3. You must provide either the @DataSchema@ or the @DataSchemaLocationS3@ .
    dataSchemaLocationS3 :: Core.Maybe Types.DataSchemaLocationS3
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3DataSpec' value with any optional fields omitted.
mkS3DataSpec ::
  -- | 'dataLocationS3'
  Types.DataLocationS3 ->
  S3DataSpec
mkS3DataSpec dataLocationS3 =
  S3DataSpec'
    { dataLocationS3,
      dataRearrangement = Core.Nothing,
      dataSchema = Core.Nothing,
      dataSchemaLocationS3 = Core.Nothing
    }

-- | The location of the data file(s) used by a @DataSource@ . The URI specifies a data file or an Amazon Simple Storage Service (Amazon S3) directory or bucket containing data files.
--
-- /Note:/ Consider using 'dataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsDataLocationS3 :: Lens.Lens' S3DataSpec Types.DataLocationS3
sdsDataLocationS3 = Lens.field @"dataLocationS3"
{-# DEPRECATED sdsDataLocationS3 "Use generic-lens or generic-optics with 'dataLocationS3' instead." #-}

-- | A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ .
--
-- There are multiple parameters that control what data is used to create a datasource:
--
--     * __@percentBegin@ __
-- Use @percentBegin@ to indicate the beginning of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.
--
--
--     * __@percentEnd@ __
-- Use @percentEnd@ to indicate the end of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.
--
--
--     * __@complement@ __
-- The @complement@ parameter instructs Amazon ML to use the data that is not included in the range of @percentBegin@ to @percentEnd@ to create a datasource. The @complement@ parameter is useful if you need to create complementary datasources for training and evaluation. To create a complementary datasource, use the same values for @percentBegin@ and @percentEnd@ , along with the @complement@ parameter.
-- For example, the following two datasources do not share any data, and can be used to train and evaluate a model. The first datasource has 25 percent of the data, and the second one has 75 percent of the data.
-- Datasource for evaluation: @{"splitting":{"percentBegin":0, "percentEnd":25}}@
-- Datasource for training: @{"splitting":{"percentBegin":0, "percentEnd":25, "complement":"true"}}@
--
--
--     * __@strategy@ __
-- To change how Amazon ML splits the data for a datasource, use the @strategy@ parameter.
-- The default value for the @strategy@ parameter is @sequential@ , meaning that Amazon ML takes all of the data records between the @percentBegin@ and @percentEnd@ parameters for the datasource, in the order that the records appear in the input data.
-- The following two @DataRearrangement@ lines are examples of sequentially ordered training and evaluation datasources:
-- Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential"}}@
-- Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential", "complement":"true"}}@
-- To randomly split the input data into the proportions indicated by the percentBegin and percentEnd parameters, set the @strategy@ parameter to @random@ and provide a string that is used as the seed value for the random data splitting (for example, you can use the S3 path to your data as the random seed string). If you choose the random split strategy, Amazon ML assigns each row of data a pseudo-random number between 0 and 100, and then selects the rows that have an assigned number between @percentBegin@ and @percentEnd@ . Pseudo-random numbers are assigned using both the input seed string value and the byte offset as a seed, so changing the data results in a different split. Any existing ordering is preserved. The random splitting strategy ensures that variables in the training and evaluation data are distributed similarly. It is useful in the cases where the input data may have an implicit sort order, which would otherwise result in training and evaluation datasources containing non-similar data records.
-- The following two @DataRearrangement@ lines are examples of non-sequentially ordered training and evaluation datasources:
-- Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv"}}@
-- Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv", "complement":"true"}}@
--
--
--
-- /Note:/ Consider using 'dataRearrangement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsDataRearrangement :: Lens.Lens' S3DataSpec (Core.Maybe Types.DataRearrangement)
sdsDataRearrangement = Lens.field @"dataRearrangement"
{-# DEPRECATED sdsDataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead." #-}

-- | A JSON string that represents the schema for an Amazon S3 @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
--
-- You must provide either the @DataSchema@ or the @DataSchemaLocationS3@ .
-- Define your @DataSchema@ as a series of key-value pairs. @attributes@ and @excludedVariableNames@ have an array of key-value pairs for their value. Use the following format to define your @DataSchema@ .
-- { "version": "1.0",
-- "recordAnnotationFieldName": "F1",
-- "recordWeightFieldName": "F2",
-- "targetFieldName": "F3",
-- "dataFormat": "CSV",
-- "dataFileContainsHeader": true,
-- "attributes": [
-- { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ],
-- "excludedVariableNames": [ "F6" ] }
--
-- /Note:/ Consider using 'dataSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsDataSchema :: Lens.Lens' S3DataSpec (Core.Maybe Types.DataSchema)
sdsDataSchema = Lens.field @"dataSchema"
{-# DEPRECATED sdsDataSchema "Use generic-lens or generic-optics with 'dataSchema' instead." #-}

-- | Describes the schema location in Amazon S3. You must provide either the @DataSchema@ or the @DataSchemaLocationS3@ .
--
-- /Note:/ Consider using 'dataSchemaLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsDataSchemaLocationS3 :: Lens.Lens' S3DataSpec (Core.Maybe Types.DataSchemaLocationS3)
sdsDataSchemaLocationS3 = Lens.field @"dataSchemaLocationS3"
{-# DEPRECATED sdsDataSchemaLocationS3 "Use generic-lens or generic-optics with 'dataSchemaLocationS3' instead." #-}

instance Core.FromJSON S3DataSpec where
  toJSON S3DataSpec {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DataLocationS3" Core..= dataLocationS3),
            ("DataRearrangement" Core..=) Core.<$> dataRearrangement,
            ("DataSchema" Core..=) Core.<$> dataSchema,
            ("DataSchemaLocationS3" Core..=) Core.<$> dataSchemaLocationS3
          ]
      )
