{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDataSourceFromRDS (..),
    mkCreateDataSourceFromRDS,

    -- ** Request lenses
    cdsfrdsDataSourceName,
    cdsfrdsComputeStatistics,
    cdsfrdsDataSourceId,
    cdsfrdsRDSData,
    cdsfrdsRoleARN,

    -- * Destructuring the response
    CreateDataSourceFromRDSResponse (..),
    mkCreateDataSourceFromRDSResponse,

    -- ** Response lenses
    cdsfrdsrsDataSourceId,
    cdsfrdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDataSourceFromRDS' smart constructor.
data CreateDataSourceFromRDS = CreateDataSourceFromRDS'
  { dataSourceName ::
      Lude.Maybe Lude.Text,
    computeStatistics :: Lude.Maybe Lude.Bool,
    dataSourceId :: Lude.Text,
    rdsData :: RDSDataSpec,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataSourceFromRDS' with the minimum fields required to make a request.
--
-- * 'computeStatistics' - The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
-- * 'dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@ . Typically, an Amazon Resource Number (ARN) becomes the ID for a @DataSource@ .
-- * 'dataSourceName' - A user-supplied name or description of the @DataSource@ .
-- * 'rdsData' - The data specification of an Amazon RDS @DataSource@ :
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
-- * 'roleARN' - The role that Amazon ML assumes on behalf of the user to create and activate a data pipeline in the user's account and copy data using the @SelectSqlQuery@ query from Amazon RDS to Amazon S3.
mkCreateDataSourceFromRDS ::
  -- | 'dataSourceId'
  Lude.Text ->
  -- | 'rdsData'
  RDSDataSpec ->
  -- | 'roleARN'
  Lude.Text ->
  CreateDataSourceFromRDS
mkCreateDataSourceFromRDS pDataSourceId_ pRDSData_ pRoleARN_ =
  CreateDataSourceFromRDS'
    { dataSourceName = Lude.Nothing,
      computeStatistics = Lude.Nothing,
      dataSourceId = pDataSourceId_,
      rdsData = pRDSData_,
      roleARN = pRoleARN_
    }

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsDataSourceName :: Lens.Lens' CreateDataSourceFromRDS (Lude.Maybe Lude.Text)
cdsfrdsDataSourceName = Lens.lens (dataSourceName :: CreateDataSourceFromRDS -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceName = a} :: CreateDataSourceFromRDS)
{-# DEPRECATED cdsfrdsDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsComputeStatistics :: Lens.Lens' CreateDataSourceFromRDS (Lude.Maybe Lude.Bool)
cdsfrdsComputeStatistics = Lens.lens (computeStatistics :: CreateDataSourceFromRDS -> Lude.Maybe Lude.Bool) (\s a -> s {computeStatistics = a} :: CreateDataSourceFromRDS)
{-# DEPRECATED cdsfrdsComputeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead." #-}

-- | A user-supplied ID that uniquely identifies the @DataSource@ . Typically, an Amazon Resource Number (ARN) becomes the ID for a @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsDataSourceId :: Lens.Lens' CreateDataSourceFromRDS Lude.Text
cdsfrdsDataSourceId = Lens.lens (dataSourceId :: CreateDataSourceFromRDS -> Lude.Text) (\s a -> s {dataSourceId = a} :: CreateDataSourceFromRDS)
{-# DEPRECATED cdsfrdsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

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
-- /Note:/ Consider using 'rdsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsRDSData :: Lens.Lens' CreateDataSourceFromRDS RDSDataSpec
cdsfrdsRDSData = Lens.lens (rdsData :: CreateDataSourceFromRDS -> RDSDataSpec) (\s a -> s {rdsData = a} :: CreateDataSourceFromRDS)
{-# DEPRECATED cdsfrdsRDSData "Use generic-lens or generic-optics with 'rdsData' instead." #-}

-- | The role that Amazon ML assumes on behalf of the user to create and activate a data pipeline in the user's account and copy data using the @SelectSqlQuery@ query from Amazon RDS to Amazon S3.
--
--
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsRoleARN :: Lens.Lens' CreateDataSourceFromRDS Lude.Text
cdsfrdsRoleARN = Lens.lens (roleARN :: CreateDataSourceFromRDS -> Lude.Text) (\s a -> s {roleARN = a} :: CreateDataSourceFromRDS)
{-# DEPRECATED cdsfrdsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateDataSourceFromRDS where
  type Rs CreateDataSourceFromRDS = CreateDataSourceFromRDSResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDataSourceFromRDSResponse'
            Lude.<$> (x Lude..?> "DataSourceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDataSourceFromRDS where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.CreateDataSourceFromRDS" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDataSourceFromRDS where
  toJSON CreateDataSourceFromRDS' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataSourceName" Lude..=) Lude.<$> dataSourceName,
            ("ComputeStatistics" Lude..=) Lude.<$> computeStatistics,
            Lude.Just ("DataSourceId" Lude..= dataSourceId),
            Lude.Just ("RDSData" Lude..= rdsData),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateDataSourceFromRDS where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDataSourceFromRDS where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateDataSourceFromRDS@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromRDS@ > operation is asynchronous. You can poll for updates by using the @GetBatchPrediction@ operation and checking the @Status@ parameter. You can inspect the @Message@ when @Status@ shows up as @FAILED@ . You can also check the progress of the copy operation by going to the @DataPipeline@ console and looking up the pipeline using the @pipelineId @ from the describe call.
--
-- /See:/ 'mkCreateDataSourceFromRDSResponse' smart constructor.
data CreateDataSourceFromRDSResponse = CreateDataSourceFromRDSResponse'
  { dataSourceId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataSourceFromRDSResponse' with the minimum fields required to make a request.
--
-- * 'dataSourceId' - A user-supplied ID that uniquely identifies the datasource. This value should be identical to the value of the @DataSourceID@ in the request.
-- * 'responseStatus' - The response status code.
mkCreateDataSourceFromRDSResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDataSourceFromRDSResponse
mkCreateDataSourceFromRDSResponse pResponseStatus_ =
  CreateDataSourceFromRDSResponse'
    { dataSourceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value should be identical to the value of the @DataSourceID@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsrsDataSourceId :: Lens.Lens' CreateDataSourceFromRDSResponse (Lude.Maybe Lude.Text)
cdsfrdsrsDataSourceId = Lens.lens (dataSourceId :: CreateDataSourceFromRDSResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceId = a} :: CreateDataSourceFromRDSResponse)
{-# DEPRECATED cdsfrdsrsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrdsrsResponseStatus :: Lens.Lens' CreateDataSourceFromRDSResponse Lude.Int
cdsfrdsrsResponseStatus = Lens.lens (responseStatus :: CreateDataSourceFromRDSResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDataSourceFromRDSResponse)
{-# DEPRECATED cdsfrdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
