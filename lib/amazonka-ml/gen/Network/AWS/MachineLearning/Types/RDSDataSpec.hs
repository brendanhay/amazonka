{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDataSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.RDSDataSpec
  ( RDSDataSpec (..)
  -- * Smart constructor
  , mkRDSDataSpec
  -- * Lenses
  , rdsdsDatabaseInformation
  , rdsdsSelectSqlQuery
  , rdsdsDatabaseCredentials
  , rdsdsS3StagingLocation
  , rdsdsResourceRole
  , rdsdsServiceRole
  , rdsdsSubnetId
  , rdsdsSecurityGroupIds
  , rdsdsDataRearrangement
  , rdsdsDataSchema
  , rdsdsDataSchemaUri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.DataRearrangement as Types
import qualified Network.AWS.MachineLearning.Types.DataSchema as Types
import qualified Network.AWS.MachineLearning.Types.DataSchemaUri as Types
import qualified Network.AWS.MachineLearning.Types.EDPResourceRole as Types
import qualified Network.AWS.MachineLearning.Types.EDPSecurityGroupId as Types
import qualified Network.AWS.MachineLearning.Types.RDSDatabase as Types
import qualified Network.AWS.MachineLearning.Types.RDSDatabaseCredentials as Types
import qualified Network.AWS.MachineLearning.Types.S3StagingLocation as Types
import qualified Network.AWS.MachineLearning.Types.SelectSqlQuery as Types
import qualified Network.AWS.MachineLearning.Types.ServiceRole as Types
import qualified Network.AWS.MachineLearning.Types.SubnetId as Types
import qualified Network.AWS.Prelude as Core

-- | The data specification of an Amazon Relational Database Service (Amazon RDS) @DataSource@ .
--
-- /See:/ 'mkRDSDataSpec' smart constructor.
data RDSDataSpec = RDSDataSpec'
  { databaseInformation :: Types.RDSDatabase
    -- ^ Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS database.
  , selectSqlQuery :: Types.SelectSqlQuery
    -- ^ The query that is used to retrieve the observation data for the @DataSource@ .
  , databaseCredentials :: Types.RDSDatabaseCredentials
    -- ^ The AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon RDS database.
  , s3StagingLocation :: Types.S3StagingLocation
    -- ^ The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
  , resourceRole :: Types.EDPResourceRole
    -- ^ The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic Compute Cloud (Amazon EC2) instance to carry out the copy operation from Amazon RDS to an Amazon S3 task. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
  , serviceRole :: Types.ServiceRole
    -- ^ The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
  , subnetId :: Types.SubnetId
    -- ^ The subnet ID to be used to access a VPC-based RDS DB instance. This attribute is used by Data Pipeline to carry out the copy task from Amazon RDS to Amazon S3.
  , securityGroupIds :: [Types.EDPSecurityGroupId]
    -- ^ The security group IDs to be used to access a VPC-based RDS DB instance. Ensure that there are appropriate ingress rules set up to allow access to the RDS DB instance. This attribute is used by Data Pipeline to carry out the copy operation from Amazon RDS to an Amazon S3 task.
  , dataRearrangement :: Core.Maybe Types.DataRearrangement
    -- ^ A JSON string that represents the splitting and rearrangement processing to be applied to a @DataSource@ . If the @DataRearrangement@ parameter is not provided, all of the input data is used to create the @Datasource@ .
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
  , dataSchema :: Core.Maybe Types.DataSchema
    -- ^ A JSON string that represents the schema for an Amazon RDS @DataSource@ . The @DataSchema@ defines the structure of the observation data in the data file(s) referenced in the @DataSource@ .
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
  , dataSchemaUri :: Core.Maybe Types.DataSchemaUri
    -- ^ The Amazon S3 location of the @DataSchema@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RDSDataSpec' value with any optional fields omitted.
mkRDSDataSpec
    :: Types.RDSDatabase -- ^ 'databaseInformation'
    -> Types.SelectSqlQuery -- ^ 'selectSqlQuery'
    -> Types.RDSDatabaseCredentials -- ^ 'databaseCredentials'
    -> Types.S3StagingLocation -- ^ 's3StagingLocation'
    -> Types.EDPResourceRole -- ^ 'resourceRole'
    -> Types.ServiceRole -- ^ 'serviceRole'
    -> Types.SubnetId -- ^ 'subnetId'
    -> RDSDataSpec
mkRDSDataSpec databaseInformation selectSqlQuery
  databaseCredentials s3StagingLocation resourceRole serviceRole
  subnetId
  = RDSDataSpec'{databaseInformation, selectSqlQuery,
                 databaseCredentials, s3StagingLocation, resourceRole, serviceRole,
                 subnetId, securityGroupIds = Core.mempty,
                 dataRearrangement = Core.Nothing, dataSchema = Core.Nothing,
                 dataSchemaUri = Core.Nothing}

-- | Describes the @DatabaseName@ and @InstanceIdentifier@ of an Amazon RDS database.
--
-- /Note:/ Consider using 'databaseInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsDatabaseInformation :: Lens.Lens' RDSDataSpec Types.RDSDatabase
rdsdsDatabaseInformation = Lens.field @"databaseInformation"
{-# INLINEABLE rdsdsDatabaseInformation #-}
{-# DEPRECATED databaseInformation "Use generic-lens or generic-optics with 'databaseInformation' instead"  #-}

-- | The query that is used to retrieve the observation data for the @DataSource@ .
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsSelectSqlQuery :: Lens.Lens' RDSDataSpec Types.SelectSqlQuery
rdsdsSelectSqlQuery = Lens.field @"selectSqlQuery"
{-# INLINEABLE rdsdsSelectSqlQuery #-}
{-# DEPRECATED selectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead"  #-}

-- | The AWS Identity and Access Management (IAM) credentials that are used connect to the Amazon RDS database.
--
-- /Note:/ Consider using 'databaseCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsDatabaseCredentials :: Lens.Lens' RDSDataSpec Types.RDSDatabaseCredentials
rdsdsDatabaseCredentials = Lens.field @"databaseCredentials"
{-# INLINEABLE rdsdsDatabaseCredentials #-}
{-# DEPRECATED databaseCredentials "Use generic-lens or generic-optics with 'databaseCredentials' instead"  #-}

-- | The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
--
-- /Note:/ Consider using 's3StagingLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsS3StagingLocation :: Lens.Lens' RDSDataSpec Types.S3StagingLocation
rdsdsS3StagingLocation = Lens.field @"s3StagingLocation"
{-# INLINEABLE rdsdsS3StagingLocation #-}
{-# DEPRECATED s3StagingLocation "Use generic-lens or generic-optics with 's3StagingLocation' instead"  #-}

-- | The role (DataPipelineDefaultResourceRole) assumed by an Amazon Elastic Compute Cloud (Amazon EC2) instance to carry out the copy operation from Amazon RDS to an Amazon S3 task. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'resourceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsResourceRole :: Lens.Lens' RDSDataSpec Types.EDPResourceRole
rdsdsResourceRole = Lens.field @"resourceRole"
{-# INLINEABLE rdsdsResourceRole #-}
{-# DEPRECATED resourceRole "Use generic-lens or generic-optics with 'resourceRole' instead"  #-}

-- | The role (DataPipelineDefaultRole) assumed by AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsServiceRole :: Lens.Lens' RDSDataSpec Types.ServiceRole
rdsdsServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE rdsdsServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | The subnet ID to be used to access a VPC-based RDS DB instance. This attribute is used by Data Pipeline to carry out the copy task from Amazon RDS to Amazon S3.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsSubnetId :: Lens.Lens' RDSDataSpec Types.SubnetId
rdsdsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE rdsdsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The security group IDs to be used to access a VPC-based RDS DB instance. Ensure that there are appropriate ingress rules set up to allow access to the RDS DB instance. This attribute is used by Data Pipeline to carry out the copy operation from Amazon RDS to an Amazon S3 task.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsSecurityGroupIds :: Lens.Lens' RDSDataSpec [Types.EDPSecurityGroupId]
rdsdsSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE rdsdsSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

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
rdsdsDataRearrangement :: Lens.Lens' RDSDataSpec (Core.Maybe Types.DataRearrangement)
rdsdsDataRearrangement = Lens.field @"dataRearrangement"
{-# INLINEABLE rdsdsDataRearrangement #-}
{-# DEPRECATED dataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead"  #-}

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
rdsdsDataSchema :: Lens.Lens' RDSDataSpec (Core.Maybe Types.DataSchema)
rdsdsDataSchema = Lens.field @"dataSchema"
{-# INLINEABLE rdsdsDataSchema #-}
{-# DEPRECATED dataSchema "Use generic-lens or generic-optics with 'dataSchema' instead"  #-}

-- | The Amazon S3 location of the @DataSchema@ . 
--
-- /Note:/ Consider using 'dataSchemaUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdsDataSchemaUri :: Lens.Lens' RDSDataSpec (Core.Maybe Types.DataSchemaUri)
rdsdsDataSchemaUri = Lens.field @"dataSchemaUri"
{-# INLINEABLE rdsdsDataSchemaUri #-}
{-# DEPRECATED dataSchemaUri "Use generic-lens or generic-optics with 'dataSchemaUri' instead"  #-}

instance Core.FromJSON RDSDataSpec where
        toJSON RDSDataSpec{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseInformation" Core..= databaseInformation),
                  Core.Just ("SelectSqlQuery" Core..= selectSqlQuery),
                  Core.Just ("DatabaseCredentials" Core..= databaseCredentials),
                  Core.Just ("S3StagingLocation" Core..= s3StagingLocation),
                  Core.Just ("ResourceRole" Core..= resourceRole),
                  Core.Just ("ServiceRole" Core..= serviceRole),
                  Core.Just ("SubnetId" Core..= subnetId),
                  Core.Just ("SecurityGroupIds" Core..= securityGroupIds),
                  ("DataRearrangement" Core..=) Core.<$> dataRearrangement,
                  ("DataSchema" Core..=) Core.<$> dataSchema,
                  ("DataSchemaUri" Core..=) Core.<$> dataSchemaUri])
