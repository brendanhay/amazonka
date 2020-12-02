{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDataSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDataSpec where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
import Network.AWS.Prelude

-- | Describes the data specification of an Amazon Redshift @DataSource@ .
--
--
--
-- /See:/ 'redshiftDataSpec' smart constructor.
data RedshiftDataSpec = RedshiftDataSpec'
  { _rDataSchemaURI ::
      !(Maybe Text),
    _rDataSchema :: !(Maybe Text),
    _rDataRearrangement :: !(Maybe Text),
    _rDatabaseInformation :: !RedshiftDatabase,
    _rSelectSqlQuery :: !Text,
    _rDatabaseCredentials :: !RedshiftDatabaseCredentials,
    _rS3StagingLocation :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftDataSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rDataSchemaURI' - Describes the schema location for an Amazon Redshift @DataSource@ .
--
-- * 'rDataSchema' - A JSON string that represents the schema for an Amazon Redshift @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ . A @DataSchema@ is not required if you specify a @DataSchemaUri@ . Define your @DataSchema@ as a series of key-value pairs. @attributes@ and @excludedVariableNames@ have an array of key-value pairs for their value. Use the following format to define your @DataSchema@ . { "version": "1.0", "recordAnnotationFieldName": "F1", "recordWeightFieldName": "F2", "targetFieldName": "F3", "dataFormat": "CSV", "dataFileContainsHeader": true, "attributes": [ { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ], "excludedVariableNames": [ "F6" ] }
--
-- * 'rDataRearrangement' - A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ . There are multiple parameters that control what data is used to create a datasource:     * __@percentBegin@ __  Use @percentBegin@ to indicate the beginning of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@percentEnd@ __  Use @percentEnd@ to indicate the end of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@complement@ __  The @complement@ parameter instructs Amazon ML to use the data that is not included in the range of @percentBegin@ to @percentEnd@ to create a datasource. The @complement@ parameter is useful if you need to create complementary datasources for training and evaluation. To create a complementary datasource, use the same values for @percentBegin@ and @percentEnd@ , along with the @complement@ parameter. For example, the following two datasources do not share any data, and can be used to train and evaluate a model. The first datasource has 25 percent of the data, and the second one has 75 percent of the data. Datasource for evaluation: @{"splitting":{"percentBegin":0, "percentEnd":25}}@  Datasource for training: @{"splitting":{"percentBegin":0, "percentEnd":25, "complement":"true"}}@      * __@strategy@ __  To change how Amazon ML splits the data for a datasource, use the @strategy@ parameter. The default value for the @strategy@ parameter is @sequential@ , meaning that Amazon ML takes all of the data records between the @percentBegin@ and @percentEnd@ parameters for the datasource, in the order that the records appear in the input data. The following two @DataRearrangement@ lines are examples of sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential", "complement":"true"}}@  To randomly split the input data into the proportions indicated by the percentBegin and percentEnd parameters, set the @strategy@ parameter to @random@ and provide a string that is used as the seed value for the random data splitting (for example, you can use the S3 path to your data as the random seed string). If you choose the random split strategy, Amazon ML assigns each row of data a pseudo-random number between 0 and 100, and then selects the rows that have an assigned number between @percentBegin@ and @percentEnd@ . Pseudo-random numbers are assigned using both the input seed string value and the byte offset as a seed, so changing the data results in a different split. Any existing ordering is preserved. The random splitting strategy ensures that variables in the training and evaluation data are distributed similarly. It is useful in the cases where the input data may have an implicit sort order, which would otherwise result in training and evaluation datasources containing non-similar data records. The following two @DataRearrangement@ lines are examples of non-sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv", "complement":"true"}}@
--
-- * 'rDatabaseInformation' - Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon Redshift @DataSource@ .
--
-- * 'rSelectSqlQuery' - Describes the SQL Query to execute on an Amazon Redshift database for an Amazon Redshift @DataSource@ .
--
-- * 'rDatabaseCredentials' - Describes AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon Redshift database.
--
-- * 'rS3StagingLocation' - Describes an Amazon S3 location to store the result set of the @SelectSqlQuery@ query.
redshiftDataSpec ::
  -- | 'rDatabaseInformation'
  RedshiftDatabase ->
  -- | 'rSelectSqlQuery'
  Text ->
  -- | 'rDatabaseCredentials'
  RedshiftDatabaseCredentials ->
  -- | 'rS3StagingLocation'
  Text ->
  RedshiftDataSpec
redshiftDataSpec
  pDatabaseInformation_
  pSelectSqlQuery_
  pDatabaseCredentials_
  pS3StagingLocation_ =
    RedshiftDataSpec'
      { _rDataSchemaURI = Nothing,
        _rDataSchema = Nothing,
        _rDataRearrangement = Nothing,
        _rDatabaseInformation = pDatabaseInformation_,
        _rSelectSqlQuery = pSelectSqlQuery_,
        _rDatabaseCredentials = pDatabaseCredentials_,
        _rS3StagingLocation = pS3StagingLocation_
      }

-- | Describes the schema location for an Amazon Redshift @DataSource@ .
rDataSchemaURI :: Lens' RedshiftDataSpec (Maybe Text)
rDataSchemaURI = lens _rDataSchemaURI (\s a -> s {_rDataSchemaURI = a})

-- | A JSON string that represents the schema for an Amazon Redshift @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ . A @DataSchema@ is not required if you specify a @DataSchemaUri@ . Define your @DataSchema@ as a series of key-value pairs. @attributes@ and @excludedVariableNames@ have an array of key-value pairs for their value. Use the following format to define your @DataSchema@ . { "version": "1.0", "recordAnnotationFieldName": "F1", "recordWeightFieldName": "F2", "targetFieldName": "F3", "dataFormat": "CSV", "dataFileContainsHeader": true, "attributes": [ { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ], "excludedVariableNames": [ "F6" ] }
rDataSchema :: Lens' RedshiftDataSpec (Maybe Text)
rDataSchema = lens _rDataSchema (\s a -> s {_rDataSchema = a})

-- | A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ . There are multiple parameters that control what data is used to create a datasource:     * __@percentBegin@ __  Use @percentBegin@ to indicate the beginning of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@percentEnd@ __  Use @percentEnd@ to indicate the end of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@complement@ __  The @complement@ parameter instructs Amazon ML to use the data that is not included in the range of @percentBegin@ to @percentEnd@ to create a datasource. The @complement@ parameter is useful if you need to create complementary datasources for training and evaluation. To create a complementary datasource, use the same values for @percentBegin@ and @percentEnd@ , along with the @complement@ parameter. For example, the following two datasources do not share any data, and can be used to train and evaluate a model. The first datasource has 25 percent of the data, and the second one has 75 percent of the data. Datasource for evaluation: @{"splitting":{"percentBegin":0, "percentEnd":25}}@  Datasource for training: @{"splitting":{"percentBegin":0, "percentEnd":25, "complement":"true"}}@      * __@strategy@ __  To change how Amazon ML splits the data for a datasource, use the @strategy@ parameter. The default value for the @strategy@ parameter is @sequential@ , meaning that Amazon ML takes all of the data records between the @percentBegin@ and @percentEnd@ parameters for the datasource, in the order that the records appear in the input data. The following two @DataRearrangement@ lines are examples of sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential", "complement":"true"}}@  To randomly split the input data into the proportions indicated by the percentBegin and percentEnd parameters, set the @strategy@ parameter to @random@ and provide a string that is used as the seed value for the random data splitting (for example, you can use the S3 path to your data as the random seed string). If you choose the random split strategy, Amazon ML assigns each row of data a pseudo-random number between 0 and 100, and then selects the rows that have an assigned number between @percentBegin@ and @percentEnd@ . Pseudo-random numbers are assigned using both the input seed string value and the byte offset as a seed, so changing the data results in a different split. Any existing ordering is preserved. The random splitting strategy ensures that variables in the training and evaluation data are distributed similarly. It is useful in the cases where the input data may have an implicit sort order, which would otherwise result in training and evaluation datasources containing non-similar data records. The following two @DataRearrangement@ lines are examples of non-sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv", "complement":"true"}}@
rDataRearrangement :: Lens' RedshiftDataSpec (Maybe Text)
rDataRearrangement = lens _rDataRearrangement (\s a -> s {_rDataRearrangement = a})

-- | Describes the @DatabaseName@ and @ClusterIdentifier@ for an Amazon Redshift @DataSource@ .
rDatabaseInformation :: Lens' RedshiftDataSpec RedshiftDatabase
rDatabaseInformation = lens _rDatabaseInformation (\s a -> s {_rDatabaseInformation = a})

-- | Describes the SQL Query to execute on an Amazon Redshift database for an Amazon Redshift @DataSource@ .
rSelectSqlQuery :: Lens' RedshiftDataSpec Text
rSelectSqlQuery = lens _rSelectSqlQuery (\s a -> s {_rSelectSqlQuery = a})

-- | Describes AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon Redshift database.
rDatabaseCredentials :: Lens' RedshiftDataSpec RedshiftDatabaseCredentials
rDatabaseCredentials = lens _rDatabaseCredentials (\s a -> s {_rDatabaseCredentials = a})

-- | Describes an Amazon S3 location to store the result set of the @SelectSqlQuery@ query.
rS3StagingLocation :: Lens' RedshiftDataSpec Text
rS3StagingLocation = lens _rS3StagingLocation (\s a -> s {_rS3StagingLocation = a})

instance Hashable RedshiftDataSpec

instance NFData RedshiftDataSpec

instance ToJSON RedshiftDataSpec where
  toJSON RedshiftDataSpec' {..} =
    object
      ( catMaybes
          [ ("DataSchemaUri" .=) <$> _rDataSchemaURI,
            ("DataSchema" .=) <$> _rDataSchema,
            ("DataRearrangement" .=) <$> _rDataRearrangement,
            Just ("DatabaseInformation" .= _rDatabaseInformation),
            Just ("SelectSqlQuery" .= _rSelectSqlQuery),
            Just ("DatabaseCredentials" .= _rDatabaseCredentials),
            Just ("S3StagingLocation" .= _rS3StagingLocation)
          ]
      )
