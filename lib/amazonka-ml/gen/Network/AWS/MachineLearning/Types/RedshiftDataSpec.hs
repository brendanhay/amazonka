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
    rdsDataSchemaURI,
    rdsDataSchema,
    rdsDataRearrangement,
    rdsDatabaseInformation,
    rdsSelectSqlQuery,
    rdsDatabaseCredentials,
    rdsS3StagingLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
import qualified Network.AWS.Prelude as Lude

-- | Describes the data specification of an Amazon Redshift @DataSource@ .
--
-- /See:/ 'mkRedshiftDataSpec' smart constructor.
data RedshiftDataSpec = RedshiftDataSpec'
  { dataSchemaURI ::
      Lude.Maybe Lude.Text,
    dataSchema :: Lude.Maybe Lude.Text,
    dataRearrangement :: Lude.Maybe Lude.Text,
    databaseInformation :: RedshiftDatabase,
    selectSqlQuery :: Lude.Text,
    databaseCredentials :: RedshiftDatabaseCredentials,
    s3StagingLocation :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftDataSpec' with the minimum fields required to make a request.
--
-- * 'dataRearrangement' - A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ .
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
-- * 'dataSchema' - A JSON string that represents the schema for an Amazon Redshift @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
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
-- * 'dataSchemaURI' - Describes the schema location for an Amazon Redshift @DataSource@ .
-- * 'databaseCredentials' - Describes AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon Redshift database.
-- * 'databaseInformation' - Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon Redshift @DataSource@ .
-- * 's3StagingLocation' - Describes an Amazon S3 location to store the result set of the @SelectSqlQuery@ query.
-- * 'selectSqlQuery' - Describes the SQL Query to execute on an Amazon Redshift database for an Amazon Redshift @DataSource@ .
mkRedshiftDataSpec ::
  -- | 'databaseInformation'
  RedshiftDatabase ->
  -- | 'selectSqlQuery'
  Lude.Text ->
  -- | 'databaseCredentials'
  RedshiftDatabaseCredentials ->
  -- | 's3StagingLocation'
  Lude.Text ->
  RedshiftDataSpec
mkRedshiftDataSpec
  pDatabaseInformation_
  pSelectSqlQuery_
  pDatabaseCredentials_
  pS3StagingLocation_ =
    RedshiftDataSpec'
      { dataSchemaURI = Lude.Nothing,
        dataSchema = Lude.Nothing,
        dataRearrangement = Lude.Nothing,
        databaseInformation = pDatabaseInformation_,
        selectSqlQuery = pSelectSqlQuery_,
        databaseCredentials = pDatabaseCredentials_,
        s3StagingLocation = pS3StagingLocation_
      }

-- | Describes the schema location for an Amazon Redshift @DataSource@ .
--
-- /Note:/ Consider using 'dataSchemaURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsDataSchemaURI :: Lens.Lens' RedshiftDataSpec (Lude.Maybe Lude.Text)
rdsDataSchemaURI = Lens.lens (dataSchemaURI :: RedshiftDataSpec -> Lude.Maybe Lude.Text) (\s a -> s {dataSchemaURI = a} :: RedshiftDataSpec)
{-# DEPRECATED rdsDataSchemaURI "Use generic-lens or generic-optics with 'dataSchemaURI' instead." #-}

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
rdsDataSchema :: Lens.Lens' RedshiftDataSpec (Lude.Maybe Lude.Text)
rdsDataSchema = Lens.lens (dataSchema :: RedshiftDataSpec -> Lude.Maybe Lude.Text) (\s a -> s {dataSchema = a} :: RedshiftDataSpec)
{-# DEPRECATED rdsDataSchema "Use generic-lens or generic-optics with 'dataSchema' instead." #-}

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
rdsDataRearrangement :: Lens.Lens' RedshiftDataSpec (Lude.Maybe Lude.Text)
rdsDataRearrangement = Lens.lens (dataRearrangement :: RedshiftDataSpec -> Lude.Maybe Lude.Text) (\s a -> s {dataRearrangement = a} :: RedshiftDataSpec)
{-# DEPRECATED rdsDataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead." #-}

-- | Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon Redshift @DataSource@ .
--
-- /Note:/ Consider using 'databaseInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsDatabaseInformation :: Lens.Lens' RedshiftDataSpec RedshiftDatabase
rdsDatabaseInformation = Lens.lens (databaseInformation :: RedshiftDataSpec -> RedshiftDatabase) (\s a -> s {databaseInformation = a} :: RedshiftDataSpec)
{-# DEPRECATED rdsDatabaseInformation "Use generic-lens or generic-optics with 'databaseInformation' instead." #-}

-- | Describes the SQL Query to execute on an Amazon Redshift database for an Amazon Redshift @DataSource@ .
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsSelectSqlQuery :: Lens.Lens' RedshiftDataSpec Lude.Text
rdsSelectSqlQuery = Lens.lens (selectSqlQuery :: RedshiftDataSpec -> Lude.Text) (\s a -> s {selectSqlQuery = a} :: RedshiftDataSpec)
{-# DEPRECATED rdsSelectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead." #-}

-- | Describes AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon Redshift database.
--
-- /Note:/ Consider using 'databaseCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsDatabaseCredentials :: Lens.Lens' RedshiftDataSpec RedshiftDatabaseCredentials
rdsDatabaseCredentials = Lens.lens (databaseCredentials :: RedshiftDataSpec -> RedshiftDatabaseCredentials) (\s a -> s {databaseCredentials = a} :: RedshiftDataSpec)
{-# DEPRECATED rdsDatabaseCredentials "Use generic-lens or generic-optics with 'databaseCredentials' instead." #-}

-- | Describes an Amazon S3 location to store the result set of the @SelectSqlQuery@ query.
--
-- /Note:/ Consider using 's3StagingLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsS3StagingLocation :: Lens.Lens' RedshiftDataSpec Lude.Text
rdsS3StagingLocation = Lens.lens (s3StagingLocation :: RedshiftDataSpec -> Lude.Text) (\s a -> s {s3StagingLocation = a} :: RedshiftDataSpec)
{-# DEPRECATED rdsS3StagingLocation "Use generic-lens or generic-optics with 's3StagingLocation' instead." #-}

instance Lude.ToJSON RedshiftDataSpec where
  toJSON RedshiftDataSpec' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataSchemaUri" Lude..=) Lude.<$> dataSchemaURI,
            ("DataSchema" Lude..=) Lude.<$> dataSchema,
            ("DataRearrangement" Lude..=) Lude.<$> dataRearrangement,
            Lude.Just ("DatabaseInformation" Lude..= databaseInformation),
            Lude.Just ("SelectSqlQuery" Lude..= selectSqlQuery),
            Lude.Just ("DatabaseCredentials" Lude..= databaseCredentials),
            Lude.Just ("S3StagingLocation" Lude..= s3StagingLocation)
          ]
      )
