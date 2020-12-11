-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDataSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDataSpec
  ( RDSDataSpec (..),

    -- * Smart constructor
    mkRDSDataSpec,

    -- * Lenses
    rdsdsDataSchemaURI,
    rdsdsDataSchema,
    rdsdsDataRearrangement,
    rdsdsDatabaseInformation,
    rdsdsSelectSqlQuery,
    rdsdsDatabaseCredentials,
    rdsdsS3StagingLocation,
    rdsdsResourceRole,
    rdsdsServiceRole,
    rdsdsSubnetId,
    rdsdsSecurityGroupIds,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RDSDatabase
import Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
import qualified Network.AWS.Prelude as Lude

-- | The data specification of an Amazon Relational Database Service (Amazon RDS) @DataSource@ .
--
-- /See:/ 'mkRDSDataSpec' smart constructor.
data RDSDataSpec = RDSDataSpec'
  { dataSchemaURI ::
      Lude.Maybe Lude.Text,
    dataSchema :: Lude.Maybe Lude.Text,
    dataRearrangement :: Lude.Maybe Lude.Text,
    databaseInformation :: RDSDatabase,
    selectSqlQuery :: Lude.Text,
    databaseCredentials :: RDSDatabaseCredentials,
    s3StagingLocation :: Lude.Text,
    resourceRole :: Lude.Text,
    serviceRole :: Lude.Text,
    subnetId :: Lude.Text,
    securityGroupIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RDSDataSpec' with the minimum fields required to make a request.
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
-- * 'dataSchema' - A JSON string that represents the schema for an Amazon RDS @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
--
-- A @DataSchema@ is not required if you specify a @DataSchemaUri@
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
-- * 'dataSchemaURI' - The Amazon S3 location of the @DataSchema@ .
-- * 'databaseCredentials' - The AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon RDS database.
-- * 'databaseInformation' - Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS database.
-- * 'resourceRole' - The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic Compute Cloud (Amazon EC2) instance to carry out the copy operation from Amazon RDS to an Amazon S3 task. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
-- * 's3StagingLocation' - The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
-- * 'securityGroupIds' - The security group IDs to be used to access a VPC-based RDS DB instance. Ensure that there are appropriate ingress rules set up to allow access to the RDS DB instance. This attribute is used by Data Pipeline to carry out the copy operation from Amazon RDS to an Amazon S3 task.
-- * 'selectSqlQuery' - The query that is used to retrieve the observation data for the @DataSource@ .
-- * 'serviceRole' - The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
-- * 'subnetId' - The subnet ID to be used to access a VPC-based RDS DB instance. This attribute is used by Data Pipeline to carry out the copy task from Amazon RDS to Amazon S3.
mkRDSDataSpec ::
  -- | 'databaseInformation'
  RDSDatabase ->
  -- | 'selectSqlQuery'
  Lude.Text ->
  -- | 'databaseCredentials'
  RDSDatabaseCredentials ->
  -- | 's3StagingLocation'
  Lude.Text ->
  -- | 'resourceRole'
  Lude.Text ->
  -- | 'serviceRole'
  Lude.Text ->
  -- | 'subnetId'
  Lude.Text ->
  RDSDataSpec
mkRDSDataSpec
  pDatabaseInformation_
  pSelectSqlQuery_
  pDatabaseCredentials_
  pS3StagingLocation_
  pResourceRole_
  pServiceRole_
  pSubnetId_ =
    RDSDataSpec'
      { dataSchemaURI = Lude.Nothing,
        dataSchema = Lude.Nothing,
        dataRearrangement = Lude.Nothing,
        databaseInformation = pDatabaseInformation_,
        selectSqlQuery = pSelectSqlQuery_,
        databaseCredentials = pDatabaseCredentials_,
        s3StagingLocation = pS3StagingLocation_,
        resourceRole = pResourceRole_,
        serviceRole = pServiceRole_,
        subnetId = pSubnetId_,
        securityGroupIds = Lude.mempty
      }

-- | The Amazon S3 location of the @DataSchema@ .
--
-- /Note:/ Consider using 'dataSchemaURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsDataSchemaURI :: Lens.Lens' RDSDataSpec (Lude.Maybe Lude.Text)
rdsdsDataSchemaURI = Lens.lens (dataSchemaURI :: RDSDataSpec -> Lude.Maybe Lude.Text) (\s a -> s {dataSchemaURI = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsDataSchemaURI "Use generic-lens or generic-optics with 'dataSchemaURI' instead." #-}

-- | A JSON string that represents the schema for an Amazon RDS @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
--
-- A @DataSchema@ is not required if you specify a @DataSchemaUri@
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
rdsdsDataSchema :: Lens.Lens' RDSDataSpec (Lude.Maybe Lude.Text)
rdsdsDataSchema = Lens.lens (dataSchema :: RDSDataSpec -> Lude.Maybe Lude.Text) (\s a -> s {dataSchema = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsDataSchema "Use generic-lens or generic-optics with 'dataSchema' instead." #-}

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
rdsdsDataRearrangement :: Lens.Lens' RDSDataSpec (Lude.Maybe Lude.Text)
rdsdsDataRearrangement = Lens.lens (dataRearrangement :: RDSDataSpec -> Lude.Maybe Lude.Text) (\s a -> s {dataRearrangement = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsDataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead." #-}

-- | Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS database.
--
-- /Note:/ Consider using 'databaseInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsDatabaseInformation :: Lens.Lens' RDSDataSpec RDSDatabase
rdsdsDatabaseInformation = Lens.lens (databaseInformation :: RDSDataSpec -> RDSDatabase) (\s a -> s {databaseInformation = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsDatabaseInformation "Use generic-lens or generic-optics with 'databaseInformation' instead." #-}

-- | The query that is used to retrieve the observation data for the @DataSource@ .
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsSelectSqlQuery :: Lens.Lens' RDSDataSpec Lude.Text
rdsdsSelectSqlQuery = Lens.lens (selectSqlQuery :: RDSDataSpec -> Lude.Text) (\s a -> s {selectSqlQuery = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsSelectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead." #-}

-- | The AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon RDS database.
--
-- /Note:/ Consider using 'databaseCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsDatabaseCredentials :: Lens.Lens' RDSDataSpec RDSDatabaseCredentials
rdsdsDatabaseCredentials = Lens.lens (databaseCredentials :: RDSDataSpec -> RDSDatabaseCredentials) (\s a -> s {databaseCredentials = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsDatabaseCredentials "Use generic-lens or generic-optics with 'databaseCredentials' instead." #-}

-- | The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
--
-- /Note:/ Consider using 's3StagingLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsS3StagingLocation :: Lens.Lens' RDSDataSpec Lude.Text
rdsdsS3StagingLocation = Lens.lens (s3StagingLocation :: RDSDataSpec -> Lude.Text) (\s a -> s {s3StagingLocation = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsS3StagingLocation "Use generic-lens or generic-optics with 's3StagingLocation' instead." #-}

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic Compute Cloud (Amazon EC2) instance to carry out the copy operation from Amazon RDS to an Amazon S3 task. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'resourceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsResourceRole :: Lens.Lens' RDSDataSpec Lude.Text
rdsdsResourceRole = Lens.lens (resourceRole :: RDSDataSpec -> Lude.Text) (\s a -> s {resourceRole = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsResourceRole "Use generic-lens or generic-optics with 'resourceRole' instead." #-}

-- | The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsServiceRole :: Lens.Lens' RDSDataSpec Lude.Text
rdsdsServiceRole = Lens.lens (serviceRole :: RDSDataSpec -> Lude.Text) (\s a -> s {serviceRole = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The subnet ID to be used to access a VPC-based RDS DB instance. This attribute is used by Data Pipeline to carry out the copy task from Amazon RDS to Amazon S3.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsSubnetId :: Lens.Lens' RDSDataSpec Lude.Text
rdsdsSubnetId = Lens.lens (subnetId :: RDSDataSpec -> Lude.Text) (\s a -> s {subnetId = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The security group IDs to be used to access a VPC-based RDS DB instance. Ensure that there are appropriate ingress rules set up to allow access to the RDS DB instance. This attribute is used by Data Pipeline to carry out the copy operation from Amazon RDS to an Amazon S3 task.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsSecurityGroupIds :: Lens.Lens' RDSDataSpec [Lude.Text]
rdsdsSecurityGroupIds = Lens.lens (securityGroupIds :: RDSDataSpec -> [Lude.Text]) (\s a -> s {securityGroupIds = a} :: RDSDataSpec)
{-# DEPRECATED rdsdsSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

instance Lude.ToJSON RDSDataSpec where
  toJSON RDSDataSpec' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataSchemaUri" Lude..=) Lude.<$> dataSchemaURI,
            ("DataSchema" Lude..=) Lude.<$> dataSchema,
            ("DataRearrangement" Lude..=) Lude.<$> dataRearrangement,
            Lude.Just ("DatabaseInformation" Lude..= databaseInformation),
            Lude.Just ("SelectSqlQuery" Lude..= selectSqlQuery),
            Lude.Just ("DatabaseCredentials" Lude..= databaseCredentials),
            Lude.Just ("S3StagingLocation" Lude..= s3StagingLocation),
            Lude.Just ("ResourceRole" Lude..= resourceRole),
            Lude.Just ("ServiceRole" Lude..= serviceRole),
            Lude.Just ("SubnetId" Lude..= subnetId),
            Lude.Just ("SecurityGroupIds" Lude..= securityGroupIds)
          ]
      )
