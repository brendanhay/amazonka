{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRDS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object from an <http://aws.amazon.com/rds/ Amazon Relational Database Service> (Amazon RDS). A @DataSource@ references data that can be used to perform @CreateMLModel@ , @CreateEvaluation@ , or @CreateBatchPrediction@ operations.
--
-- @CreateDataSourceFromRDS@ is an asynchronous operation. In response to @CreateDataSourceFromRDS@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @DataSource@ status to @PENDING@ . After the @DataSource@ is created and ready for use, Amazon ML sets the @Status@ parameter to @COMPLETED@ . @DataSource@ in the @COMPLETED@ or @PENDING@ state can be used only to perform @>CreateMLModel@ >, @CreateEvaluation@ , or @CreateBatchPrediction@ operations. 
-- If Amazon ML cannot accept the input source, it sets the @Status@ parameter to @FAILED@ and includes an error message in the @Message@ attribute of the @GetDataSource@ operation response. 
module Network.AWS.MachineLearning.CreateDataSourceFromRDS
    (
    -- * Creating a request
      CreateDataSourceFromRDS (..)
    , mkCreateDataSourceFromRDS
    -- ** Request lenses
    , cdsfrdsDataSourceId
    , cdsfrdsRDSData
    , cdsfrdsRoleARN
    , cdsfrdsComputeStatistics
    , cdsfrdsDataSourceName

    -- * Destructuring the response
    , CreateDataSourceFromRDSResponse (..)
    , mkCreateDataSourceFromRDSResponse
    -- ** Response lenses
    , cdsfrdsrrsDataSourceId
    , cdsfrdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDataSourceFromRDS' smart constructor.
data CreateDataSourceFromRDS = CreateDataSourceFromRDS'
  { dataSourceId :: Types.DataSourceId
    -- ^ A user-supplied ID that uniquely identifies the @DataSource@ . Typically, an Amazon Resource Number (ARN) becomes the ID for a @DataSource@ .
  , rDSData :: Types.RDSDataSpec
    -- ^ The data specification of an Amazon RDS @DataSource@ :
--
--
--     * DatabaseInformation - 
--     * @DatabaseName@ - The name of the Amazon RDS database.
--
--     * @InstanceIdentifier @ - A unique identifier for the Amazon RDS database instance.
--
--
--
--
--     * DatabaseCredentials - AWS Identity and Access Management (IAM) credentials that are used to connect to the Amazon RDS database.
--
--
--     * ResourceRole - A role (DataPipelineDefaultResourceRole) assumed by an EC2 instance to carry out the copy task from Amazon RDS to Amazon Simple Storage Service (Amazon S3). For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
--
--     * ServiceRole - A role (DataPipelineDefaultRole) assumed by the AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
--
--     * SecurityInfo - The security information to use to access an RDS DB instance. You need to set up appropriate ingress rules for the security entity IDs provided to allow access to the Amazon RDS instance. Specify a [@SubnetId@ , @SecurityGroupIds@ ] pair for a VPC-based RDS DB instance.
--
--
--     * SelectSqlQuery - A query that is used to retrieve the observation data for the @Datasource@ .
--
--
--     * S3StagingLocation - The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
--
--
--     * DataSchemaUri - The Amazon S3 location of the @DataSchema@ .
--
--
--     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified. 
--
--
--     * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @Datasource@ . 
--
-- Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@ 
--
--
  , roleARN :: Types.RoleARN
    -- ^ The role that Amazon ML assumes on behalf of the user to create and activate a data pipeline in the user's account and copy data using the @SelectSqlQuery@ query from Amazon RDS to Amazon S3.
--
--
  , computeStatistics :: Core.Maybe Core.Bool
    -- ^ The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training. 
  , dataSourceName :: Core.Maybe Types.DataSourceName
    -- ^ A user-supplied name or description of the @DataSource@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSourceFromRDS' value with any optional fields omitted.
mkCreateDataSourceFromRDS
    :: Types.DataSourceId -- ^ 'dataSourceId'
    -> Types.RDSDataSpec -- ^ 'rDSData'
    -> Types.RoleARN -- ^ 'roleARN'
    -> CreateDataSourceFromRDS
mkCreateDataSourceFromRDS dataSourceId rDSData roleARN
  = CreateDataSourceFromRDS'{dataSourceId, rDSData, roleARN,
                             computeStatistics = Core.Nothing, dataSourceName = Core.Nothing}

-- | A user-supplied ID that uniquely identifies the @DataSource@ . Typically, an Amazon Resource Number (ARN) becomes the ID for a @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsDataSourceId :: Lens.Lens' CreateDataSourceFromRDS Types.DataSourceId
cdsfrdsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE cdsfrdsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | The data specification of an Amazon RDS @DataSource@ :
--
--
--     * DatabaseInformation - 
--     * @DatabaseName@ - The name of the Amazon RDS database.
--
--     * @InstanceIdentifier @ - A unique identifier for the Amazon RDS database instance.
--
--
--
--
--     * DatabaseCredentials - AWS Identity and Access Management (IAM) credentials that are used to connect to the Amazon RDS database.
--
--
--     * ResourceRole - A role (DataPipelineDefaultResourceRole) assumed by an EC2 instance to carry out the copy task from Amazon RDS to Amazon Simple Storage Service (Amazon S3). For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
--
--     * ServiceRole - A role (DataPipelineDefaultRole) assumed by the AWS Data Pipeline service to monitor the progress of the copy task from Amazon RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
--
--     * SecurityInfo - The security information to use to access an RDS DB instance. You need to set up appropriate ingress rules for the security entity IDs provided to allow access to the Amazon RDS instance. Specify a [@SubnetId@ , @SecurityGroupIds@ ] pair for a VPC-based RDS DB instance.
--
--
--     * SelectSqlQuery - A query that is used to retrieve the observation data for the @Datasource@ .
--
--
--     * S3StagingLocation - The Amazon S3 location for staging Amazon RDS data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is stored in this location.
--
--
--     * DataSchemaUri - The Amazon S3 location of the @DataSchema@ .
--
--
--     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified. 
--
--
--     * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @Datasource@ . 
--
-- Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@ 
--
--
--
-- /Note:/ Consider using 'rDSData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsRDSData :: Lens.Lens' CreateDataSourceFromRDS Types.RDSDataSpec
cdsfrdsRDSData = Lens.field @"rDSData"
{-# INLINEABLE cdsfrdsRDSData #-}
{-# DEPRECATED rDSData "Use generic-lens or generic-optics with 'rDSData' instead"  #-}

-- | The role that Amazon ML assumes on behalf of the user to create and activate a data pipeline in the user's account and copy data using the @SelectSqlQuery@ query from Amazon RDS to Amazon S3.
--
--
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsRoleARN :: Lens.Lens' CreateDataSourceFromRDS Types.RoleARN
cdsfrdsRoleARN = Lens.field @"roleARN"
{-# INLINEABLE cdsfrdsRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training. 
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsComputeStatistics :: Lens.Lens' CreateDataSourceFromRDS (Core.Maybe Core.Bool)
cdsfrdsComputeStatistics = Lens.field @"computeStatistics"
{-# INLINEABLE cdsfrdsComputeStatistics #-}
{-# DEPRECATED computeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead"  #-}

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsDataSourceName :: Lens.Lens' CreateDataSourceFromRDS (Core.Maybe Types.DataSourceName)
cdsfrdsDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE cdsfrdsDataSourceName #-}
{-# DEPRECATED dataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead"  #-}

instance Core.ToQuery CreateDataSourceFromRDS where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDataSourceFromRDS where
        toHeaders CreateDataSourceFromRDS{..}
          = Core.pure
              ("X-Amz-Target", "AmazonML_20141212.CreateDataSourceFromRDS")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDataSourceFromRDS where
        toJSON CreateDataSourceFromRDS{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DataSourceId" Core..= dataSourceId),
                  Core.Just ("RDSData" Core..= rDSData),
                  Core.Just ("RoleARN" Core..= roleARN),
                  ("ComputeStatistics" Core..=) Core.<$> computeStatistics,
                  ("DataSourceName" Core..=) Core.<$> dataSourceName])

instance Core.AWSRequest CreateDataSourceFromRDS where
        type Rs CreateDataSourceFromRDS = CreateDataSourceFromRDSResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDataSourceFromRDSResponse' Core.<$>
                   (x Core..:? "DataSourceId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @CreateDataSourceFromRDS@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromRDS@ > operation is asynchronous. You can poll for updates by using the @GetBatchPrediction@ operation and checking the @Status@ parameter. You can inspect the @Message@ when @Status@ shows up as @FAILED@ . You can also check the progress of the copy operation by going to the @DataPipeline@ console and looking up the pipeline using the @pipelineId @ from the describe call.
--
-- /See:/ 'mkCreateDataSourceFromRDSResponse' smart constructor.
data CreateDataSourceFromRDSResponse = CreateDataSourceFromRDSResponse'
  { dataSourceId :: Core.Maybe Types.DataSourceId
    -- ^ A user-supplied ID that uniquely identifies the datasource. This value should be identical to the value of the @DataSourceID@ in the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSourceFromRDSResponse' value with any optional fields omitted.
mkCreateDataSourceFromRDSResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDataSourceFromRDSResponse
mkCreateDataSourceFromRDSResponse responseStatus
  = CreateDataSourceFromRDSResponse'{dataSourceId = Core.Nothing,
                                     responseStatus}

-- | A user-supplied ID that uniquely identifies the datasource. This value should be identical to the value of the @DataSourceID@ in the request. 
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsrrsDataSourceId :: Lens.Lens' CreateDataSourceFromRDSResponse (Core.Maybe Types.DataSourceId)
cdsfrdsrrsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE cdsfrdsrrsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsrrsResponseStatus :: Lens.Lens' CreateDataSourceFromRDSResponse Core.Int
cdsfrdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdsfrdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
