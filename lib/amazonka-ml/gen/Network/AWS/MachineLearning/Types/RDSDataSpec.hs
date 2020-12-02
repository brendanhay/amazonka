{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDataSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDataSpec where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.RDSDatabase
import Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
import Network.AWS.Prelude

-- | The data specification of an Amazon Relational Database Service (Amazon RDS) @DataSource@ .
--
--
--
-- /See:/ 'rdsDataSpec' smart constructor.
data RDSDataSpec = RDSDataSpec'
  { _rdsdsDataSchemaURI ::
      !(Maybe Text),
    _rdsdsDataSchema :: !(Maybe Text),
    _rdsdsDataRearrangement :: !(Maybe Text),
    _rdsdsDatabaseInformation :: !RDSDatabase,
    _rdsdsSelectSqlQuery :: !Text,
    _rdsdsDatabaseCredentials :: !RDSDatabaseCredentials,
    _rdsdsS3StagingLocation :: !Text,
    _rdsdsResourceRole :: !Text,
    _rdsdsServiceRole :: !Text,
    _rdsdsSubnetId :: !Text,
    _rdsdsSecurityGroupIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RDSDataSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdsDataSchemaURI' - The Amazon S3 location of the @DataSchema@ .
--
-- * 'rdsdsDataSchema' - A JSON string that represents the schema for an Amazon RDS @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ . A @DataSchema@ is not required if you specify a @DataSchemaUri@  Define your @DataSchema@ as a series of key-value pairs. @attributes@ and @excludedVariableNames@ have an array of key-value pairs for their value. Use the following format to define your @DataSchema@ . { "version": "1.0", "recordAnnotationFieldName": "F1", "recordWeightFieldName": "F2", "targetFieldName": "F3", "dataFormat": "CSV", "dataFileContainsHeader": true, "attributes": [ { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ], "excludedVariableNames": [ "F6" ] }
--
-- * 'rdsdsDataRearrangement' - A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ . There are multiple parameters that control what data is used to create a datasource:     * __@percentBegin@ __  Use @percentBegin@ to indicate the beginning of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@percentEnd@ __  Use @percentEnd@ to indicate the end of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@complement@ __  The @complement@ parameter instructs Amazon ML to use the data that is not included in the range of @percentBegin@ to @percentEnd@ to create a datasource. The @complement@ parameter is useful if you need to create complementary datasources for training and evaluation. To create a complementary datasource, use the same values for @percentBegin@ and @percentEnd@ , along with the @complement@ parameter. For example, the following two datasources do not share any data, and can be used to train and evaluate a model. The first datasource has 25 percent of the data, and the second one has 75 percent of the data. Datasource for evaluation: @{"splitting":{"percentBegin":0, "percentEnd":25}}@  Datasource for training: @{"splitting":{"percentBegin":0, "percentEnd":25, "complement":"true"}}@      * __@strategy@ __  To change how Amazon ML splits the data for a datasource, use the @strategy@ parameter. The default value for the @strategy@ parameter is @sequential@ , meaning that Amazon ML takes all of the data records between the @percentBegin@ and @percentEnd@ parameters for the datasource, in the order that the records appear in the input data. The following two @DataRearrangement@ lines are examples of sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential", "complement":"true"}}@  To randomly split the input data into the proportions indicated by the percentBegin and percentEnd parameters, set the @strategy@ parameter to @random@ and provide a string that is used as the seed value for the random data splitting (for example, you can use the S3 path to your data as the random seed string). If you choose the random split strategy, Amazon ML assigns each row of data a pseudo-random number between 0 and 100, and then selects the rows that have an assigned number between @percentBegin@ and @percentEnd@ . Pseudo-random numbers are assigned using both the input seed string value and the byte offset as a seed, so changing the data results in a different split. Any existing ordering is preserved. The random splitting strategy ensures that variables in the training and evaluation data are distributed similarly. It is useful in the cases where the input data may have an implicit sort order, which would otherwise result in training and evaluation datasources containing non-similar data records. The following two @DataRearrangement@ lines are examples of non-sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv", "complement":"true"}}@
--
-- * 'rdsdsDatabaseInformation' - Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS database.
--
-- * 'rdsdsSelectSqlQuery' - The query that is used to retrieve the observation data for the @DataSource@ .
--
-- * 'rdsdsDatabaseCredentials' - The AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon RDS database.
--
-- * 'rdsdsS3StagingLocation' - The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
--
-- * 'rdsdsResourceRole' - The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic Compute Cloud (Amazon EC2) instance to carry out the copy operation from Amazon RDS to an Amazon S3 task. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- * 'rdsdsServiceRole' - The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- * 'rdsdsSubnetId' - The subnet ID to be used to access a VPC-based RDS DB instance. This attribute is used by Data Pipeline to carry out the copy task from Amazon RDS to Amazon S3.
--
-- * 'rdsdsSecurityGroupIds' - The security group IDs to be used to access a VPC-based RDS DB instance. Ensure that there are appropriate ingress rules set up to allow access to the RDS DB instance. This attribute is used by Data Pipeline to carry out the copy operation from Amazon RDS to an Amazon S3 task.
rdsDataSpec ::
  -- | 'rdsdsDatabaseInformation'
  RDSDatabase ->
  -- | 'rdsdsSelectSqlQuery'
  Text ->
  -- | 'rdsdsDatabaseCredentials'
  RDSDatabaseCredentials ->
  -- | 'rdsdsS3StagingLocation'
  Text ->
  -- | 'rdsdsResourceRole'
  Text ->
  -- | 'rdsdsServiceRole'
  Text ->
  -- | 'rdsdsSubnetId'
  Text ->
  RDSDataSpec
rdsDataSpec
  pDatabaseInformation_
  pSelectSqlQuery_
  pDatabaseCredentials_
  pS3StagingLocation_
  pResourceRole_
  pServiceRole_
  pSubnetId_ =
    RDSDataSpec'
      { _rdsdsDataSchemaURI = Nothing,
        _rdsdsDataSchema = Nothing,
        _rdsdsDataRearrangement = Nothing,
        _rdsdsDatabaseInformation = pDatabaseInformation_,
        _rdsdsSelectSqlQuery = pSelectSqlQuery_,
        _rdsdsDatabaseCredentials = pDatabaseCredentials_,
        _rdsdsS3StagingLocation = pS3StagingLocation_,
        _rdsdsResourceRole = pResourceRole_,
        _rdsdsServiceRole = pServiceRole_,
        _rdsdsSubnetId = pSubnetId_,
        _rdsdsSecurityGroupIds = mempty
      }

-- | The Amazon S3 location of the @DataSchema@ .
rdsdsDataSchemaURI :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataSchemaURI = lens _rdsdsDataSchemaURI (\s a -> s {_rdsdsDataSchemaURI = a})

-- | A JSON string that represents the schema for an Amazon RDS @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ . A @DataSchema@ is not required if you specify a @DataSchemaUri@  Define your @DataSchema@ as a series of key-value pairs. @attributes@ and @excludedVariableNames@ have an array of key-value pairs for their value. Use the following format to define your @DataSchema@ . { "version": "1.0", "recordAnnotationFieldName": "F1", "recordWeightFieldName": "F2", "targetFieldName": "F3", "dataFormat": "CSV", "dataFileContainsHeader": true, "attributes": [ { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ], "excludedVariableNames": [ "F6" ] }
rdsdsDataSchema :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataSchema = lens _rdsdsDataSchema (\s a -> s {_rdsdsDataSchema = a})

-- | A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ . There are multiple parameters that control what data is used to create a datasource:     * __@percentBegin@ __  Use @percentBegin@ to indicate the beginning of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@percentEnd@ __  Use @percentEnd@ to indicate the end of the range of the data used to create the Datasource. If you do not include @percentBegin@ and @percentEnd@ , Amazon ML includes all of the data when creating the datasource.     * __@complement@ __  The @complement@ parameter instructs Amazon ML to use the data that is not included in the range of @percentBegin@ to @percentEnd@ to create a datasource. The @complement@ parameter is useful if you need to create complementary datasources for training and evaluation. To create a complementary datasource, use the same values for @percentBegin@ and @percentEnd@ , along with the @complement@ parameter. For example, the following two datasources do not share any data, and can be used to train and evaluate a model. The first datasource has 25 percent of the data, and the second one has 75 percent of the data. Datasource for evaluation: @{"splitting":{"percentBegin":0, "percentEnd":25}}@  Datasource for training: @{"splitting":{"percentBegin":0, "percentEnd":25, "complement":"true"}}@      * __@strategy@ __  To change how Amazon ML splits the data for a datasource, use the @strategy@ parameter. The default value for the @strategy@ parameter is @sequential@ , meaning that Amazon ML takes all of the data records between the @percentBegin@ and @percentEnd@ parameters for the datasource, in the order that the records appear in the input data. The following two @DataRearrangement@ lines are examples of sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"sequential", "complement":"true"}}@  To randomly split the input data into the proportions indicated by the percentBegin and percentEnd parameters, set the @strategy@ parameter to @random@ and provide a string that is used as the seed value for the random data splitting (for example, you can use the S3 path to your data as the random seed string). If you choose the random split strategy, Amazon ML assigns each row of data a pseudo-random number between 0 and 100, and then selects the rows that have an assigned number between @percentBegin@ and @percentEnd@ . Pseudo-random numbers are assigned using both the input seed string value and the byte offset as a seed, so changing the data results in a different split. Any existing ordering is preserved. The random splitting strategy ensures that variables in the training and evaluation data are distributed similarly. It is useful in the cases where the input data may have an implicit sort order, which would otherwise result in training and evaluation datasources containing non-similar data records. The following two @DataRearrangement@ lines are examples of non-sequentially ordered training and evaluation datasources: Datasource for evaluation: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv"}}@  Datasource for training: @{"splitting":{"percentBegin":70, "percentEnd":100, "strategy":"random", "randomSeed"="s3://my_s3_path/bucket/file.csv", "complement":"true"}}@
rdsdsDataRearrangement :: Lens' RDSDataSpec (Maybe Text)
rdsdsDataRearrangement = lens _rdsdsDataRearrangement (\s a -> s {_rdsdsDataRearrangement = a})

-- | Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS database.
rdsdsDatabaseInformation :: Lens' RDSDataSpec RDSDatabase
rdsdsDatabaseInformation = lens _rdsdsDatabaseInformation (\s a -> s {_rdsdsDatabaseInformation = a})

-- | The query that is used to retrieve the observation data for the @DataSource@ .
rdsdsSelectSqlQuery :: Lens' RDSDataSpec Text
rdsdsSelectSqlQuery = lens _rdsdsSelectSqlQuery (\s a -> s {_rdsdsSelectSqlQuery = a})

-- | The AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon RDS database.
rdsdsDatabaseCredentials :: Lens' RDSDataSpec RDSDatabaseCredentials
rdsdsDatabaseCredentials = lens _rdsdsDatabaseCredentials (\s a -> s {_rdsdsDatabaseCredentials = a})

-- | The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
rdsdsS3StagingLocation :: Lens' RDSDataSpec Text
rdsdsS3StagingLocation = lens _rdsdsS3StagingLocation (\s a -> s {_rdsdsS3StagingLocation = a})

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic Compute Cloud (Amazon EC2) instance to carry out the copy operation from Amazon RDS to an Amazon S3 task. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rdsdsResourceRole :: Lens' RDSDataSpec Text
rdsdsResourceRole = lens _rdsdsResourceRole (\s a -> s {_rdsdsResourceRole = a})

-- | The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
rdsdsServiceRole :: Lens' RDSDataSpec Text
rdsdsServiceRole = lens _rdsdsServiceRole (\s a -> s {_rdsdsServiceRole = a})

-- | The subnet ID to be used to access a VPC-based RDS DB instance. This attribute is used by Data Pipeline to carry out the copy task from Amazon RDS to Amazon S3.
rdsdsSubnetId :: Lens' RDSDataSpec Text
rdsdsSubnetId = lens _rdsdsSubnetId (\s a -> s {_rdsdsSubnetId = a})

-- | The security group IDs to be used to access a VPC-based RDS DB instance. Ensure that there are appropriate ingress rules set up to allow access to the RDS DB instance. This attribute is used by Data Pipeline to carry out the copy operation from Amazon RDS to an Amazon S3 task.
rdsdsSecurityGroupIds :: Lens' RDSDataSpec [Text]
rdsdsSecurityGroupIds = lens _rdsdsSecurityGroupIds (\s a -> s {_rdsdsSecurityGroupIds = a}) . _Coerce

instance Hashable RDSDataSpec

instance NFData RDSDataSpec

instance ToJSON RDSDataSpec where
  toJSON RDSDataSpec' {..} =
    object
      ( catMaybes
          [ ("DataSchemaUri" .=) <$> _rdsdsDataSchemaURI,
            ("DataSchema" .=) <$> _rdsdsDataSchema,
            ("DataRearrangement" .=) <$> _rdsdsDataRearrangement,
            Just ("DatabaseInformation" .= _rdsdsDatabaseInformation),
            Just ("SelectSqlQuery" .= _rdsdsSelectSqlQuery),
            Just ("DatabaseCredentials" .= _rdsdsDatabaseCredentials),
            Just ("S3StagingLocation" .= _rdsdsS3StagingLocation),
            Just ("ResourceRole" .= _rdsdsResourceRole),
            Just ("ServiceRole" .= _rdsdsServiceRole),
            Just ("SubnetId" .= _rdsdsSubnetId),
            Just ("SecurityGroupIds" .= _rdsdsSecurityGroupIds)
          ]
      )
