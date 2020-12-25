{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDataSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDataSpec
  ( RedshiftDataSpec (..),

    -- * Smart constructor
    mkRedshiftDataSpec,

    -- * Lenses
    rdsDatabaseInformation,
    rdsSelectSqlQuery,
    rdsDatabaseCredentials,
    rdsS3StagingLocation,
    rdsDataRearrangement,
    rdsDataSchema,
    rdsDataSchemaUri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.DataRearrangement as Types
import qualified Network.AWS.MachineLearning.Types.DataSchema as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftDatabase as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftSelectSqlQuery as Types
import qualified Network.AWS.MachineLearning.Types.S3Url as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the data specification of an Amazon Redshift @DataSource@ .
--
-- /See:/ 'mkRedshiftDataSpec' smart constructor.
data RedshiftDataSpec = RedshiftDataSpec'
  { -- | Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon Redshift @DataSource@ .
    databaseInformation :: Types.RedshiftDatabase,
    -- | Describes the SQL Query to execute on an Amazon Redshift database for an Amazon Redshift @DataSource@ .
    selectSqlQuery :: Types.RedshiftSelectSqlQuery,
    -- | Describes AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon Redshift database.
    databaseCredentials :: Types.RedshiftDatabaseCredentials,
    -- | Describes an Amazon S3 location to store the result set of the @SelectSqlQuery@ query.
    s3StagingLocation :: Types.S3Url,
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
    -- | A JSON string that represents the schema for an Amazon Redshift @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
    --
    -- A @DataSchema@ is not required if you specify a @DataSchemaUri@ .
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
    -- | Describes the schema location for an Amazon Redshift @DataSource@ .
    dataSchemaUri :: Core.Maybe Types.S3Url
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftDataSpec' value with any optional fields omitted.
mkRedshiftDataSpec ::
  -- | 'databaseInformation'
  Types.RedshiftDatabase ->
  -- | 'selectSqlQuery'
  Types.RedshiftSelectSqlQuery ->
  -- | 'databaseCredentials'
  Types.RedshiftDatabaseCredentials ->
  -- | 's3StagingLocation'
  Types.S3Url ->
  RedshiftDataSpec
mkRedshiftDataSpec
  databaseInformation
  selectSqlQuery
  databaseCredentials
  s3StagingLocation =
    RedshiftDataSpec'
      { databaseInformation,
        selectSqlQuery,
        databaseCredentials,
        s3StagingLocation,
        dataRearrangement = Core.Nothing,
        dataSchema = Core.Nothing,
        dataSchemaUri = Core.Nothing
      }

-- | Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon Redshift @DataSource@ .
--
-- /Note:/ Consider using 'databaseInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsDatabaseInformation :: Lens.Lens' RedshiftDataSpec Types.RedshiftDatabase
rdsDatabaseInformation = Lens.field @"databaseInformation"
{-# DEPRECATED rdsDatabaseInformation "Use generic-lens or generic-optics with 'databaseInformation' instead." #-}

-- | Describes the SQL Query to execute on an Amazon Redshift database for an Amazon Redshift @DataSource@ .
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsSelectSqlQuery :: Lens.Lens' RedshiftDataSpec Types.RedshiftSelectSqlQuery
rdsSelectSqlQuery = Lens.field @"selectSqlQuery"
{-# DEPRECATED rdsSelectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead." #-}

-- | Describes AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon Redshift database.
--
-- /Note:/ Consider using 'databaseCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsDatabaseCredentials :: Lens.Lens' RedshiftDataSpec Types.RedshiftDatabaseCredentials
rdsDatabaseCredentials = Lens.field @"databaseCredentials"
{-# DEPRECATED rdsDatabaseCredentials "Use generic-lens or generic-optics with 'databaseCredentials' instead." #-}

-- | Describes an Amazon S3 location to store the result set of the @SelectSqlQuery@ query.
--
-- /Note:/ Consider using 's3StagingLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsS3StagingLocation :: Lens.Lens' RedshiftDataSpec Types.S3Url
rdsS3StagingLocation = Lens.field @"s3StagingLocation"
{-# DEPRECATED rdsS3StagingLocation "Use generic-lens or generic-optics with 's3StagingLocation' instead." #-}

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
rdsDataRearrangement :: Lens.Lens' RedshiftDataSpec (Core.Maybe Types.DataRearrangement)
rdsDataRearrangement = Lens.field @"dataRearrangement"
{-# DEPRECATED rdsDataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead." #-}

-- | A JSON string that represents the schema for an Amazon Redshift @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
--
-- A @DataSchema@ is not required if you specify a @DataSchemaUri@ .
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
rdsDataSchema :: Lens.Lens' RedshiftDataSpec (Core.Maybe Types.DataSchema)
rdsDataSchema = Lens.field @"dataSchema"
{-# DEPRECATED rdsDataSchema "Use generic-lens or generic-optics with 'dataSchema' instead." #-}

-- | Describes the schema location for an Amazon Redshift @DataSource@ .
--
-- /Note:/ Consider using 'dataSchemaUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsDataSchemaUri :: Lens.Lens' RedshiftDataSpec (Core.Maybe Types.S3Url)
rdsDataSchemaUri = Lens.field @"dataSchemaUri"
{-# DEPRECATED rdsDataSchemaUri "Use generic-lens or generic-optics with 'dataSchemaUri' instead." #-}

instance Core.FromJSON RedshiftDataSpec where
  toJSON RedshiftDataSpec {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DatabaseInformation" Core..= databaseInformation),
            Core.Just ("SelectSqlQuery" Core..= selectSqlQuery),
            Core.Just ("DatabaseCredentials" Core..= databaseCredentials),
            Core.Just ("S3StagingLocation" Core..= s3StagingLocation),
            ("DataRearrangement" Core..=) Core.<$> dataRearrangement,
            ("DataSchema" Core..=) Core.<$> dataSchema,
            ("DataSchemaUri" Core..=) Core.<$> dataSchemaUri
          ]
      )
