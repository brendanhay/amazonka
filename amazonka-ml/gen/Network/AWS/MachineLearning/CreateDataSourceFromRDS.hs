{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRDS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object from an
-- <http://aws.amazon.com/rds/ Amazon Relational Database Service> (Amazon
-- RDS). A @DataSource@ references data that can be used to perform
-- @CreateMLModel@, @CreateEvaluation@, or @CreateBatchPrediction@
-- operations.
--
-- @CreateDataSourceFromRDS@ is an asynchronous operation. In response to
-- @CreateDataSourceFromRDS@, Amazon Machine Learning (Amazon ML)
-- immediately returns and sets the @DataSource@ status to @PENDING@. After
-- the @DataSource@ is created and ready for use, Amazon ML sets the
-- @Status@ parameter to @COMPLETED@. @DataSource@ in the @COMPLETED@ or
-- @PENDING@ state can be used only to perform @>CreateMLModel@>,
-- @CreateEvaluation@, or @CreateBatchPrediction@ operations.
--
-- If Amazon ML cannot accept the input source, it sets the @Status@
-- parameter to @FAILED@ and includes an error message in the @Message@
-- attribute of the @GetDataSource@ operation response.
module Network.AWS.MachineLearning.CreateDataSourceFromRDS
  ( -- * Creating a Request
    CreateDataSourceFromRDS (..),
    newCreateDataSourceFromRDS,

    -- * Request Lenses
    createDataSourceFromRDS_computeStatistics,
    createDataSourceFromRDS_dataSourceName,
    createDataSourceFromRDS_dataSourceId,
    createDataSourceFromRDS_rDSData,
    createDataSourceFromRDS_roleARN,

    -- * Destructuring the Response
    CreateDataSourceFromRDSResponse (..),
    newCreateDataSourceFromRDSResponse,

    -- * Response Lenses
    createDataSourceFromRDSResponse_dataSourceId,
    createDataSourceFromRDSResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataSourceFromRDS' smart constructor.
data CreateDataSourceFromRDS = CreateDataSourceFromRDS'
  { -- | The compute statistics for a @DataSource@. The statistics are generated
    -- from the observation data referenced by a @DataSource@. Amazon ML uses
    -- the statistics internally during @MLModel@ training. This parameter must
    -- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
    -- training.
    computeStatistics :: Prelude.Maybe Prelude.Bool,
    -- | A user-supplied name or description of the @DataSource@.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied ID that uniquely identifies the @DataSource@. Typically,
    -- an Amazon Resource Number (ARN) becomes the ID for a @DataSource@.
    dataSourceId :: Prelude.Text,
    -- | The data specification of an Amazon RDS @DataSource@:
    --
    -- -   DatabaseInformation -
    --
    --     -   @DatabaseName@ - The name of the Amazon RDS database.
    --     -   @InstanceIdentifier @ - A unique identifier for the Amazon RDS
    --         database instance.
    --
    -- -   DatabaseCredentials - AWS Identity and Access Management (IAM)
    --     credentials that are used to connect to the Amazon RDS database.
    --
    -- -   ResourceRole - A role (DataPipelineDefaultResourceRole) assumed by
    --     an EC2 instance to carry out the copy task from Amazon RDS to Amazon
    --     Simple Storage Service (Amazon S3). For more information, see
    --     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
    --     for data pipelines.
    --
    -- -   ServiceRole - A role (DataPipelineDefaultRole) assumed by the AWS
    --     Data Pipeline service to monitor the progress of the copy task from
    --     Amazon RDS to Amazon S3. For more information, see
    --     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
    --     for data pipelines.
    --
    -- -   SecurityInfo - The security information to use to access an RDS DB
    --     instance. You need to set up appropriate ingress rules for the
    --     security entity IDs provided to allow access to the Amazon RDS
    --     instance. Specify a [@SubnetId@, @SecurityGroupIds@] pair for a
    --     VPC-based RDS DB instance.
    --
    -- -   SelectSqlQuery - A query that is used to retrieve the observation
    --     data for the @Datasource@.
    --
    -- -   S3StagingLocation - The Amazon S3 location for staging Amazon RDS
    --     data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is
    --     stored in this location.
    --
    -- -   DataSchemaUri - The Amazon S3 location of the @DataSchema@.
    --
    -- -   DataSchema - A JSON string representing the schema. This is not
    --     required if @DataSchemaUri@ is specified.
    --
    -- -   DataRearrangement - A JSON string that represents the splitting and
    --     rearrangement requirements for the @Datasource@.
    --
    --     Sample -
    --     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
    rDSData :: RDSDataSpec,
    -- | The role that Amazon ML assumes on behalf of the user to create and
    -- activate a data pipeline in the user\'s account and copy data using the
    -- @SelectSqlQuery@ query from Amazon RDS to Amazon S3.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromRDS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeStatistics', 'createDataSourceFromRDS_computeStatistics' - The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
-- training.
--
-- 'dataSourceName', 'createDataSourceFromRDS_dataSourceName' - A user-supplied name or description of the @DataSource@.
--
-- 'dataSourceId', 'createDataSourceFromRDS_dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@. Typically,
-- an Amazon Resource Number (ARN) becomes the ID for a @DataSource@.
--
-- 'rDSData', 'createDataSourceFromRDS_rDSData' - The data specification of an Amazon RDS @DataSource@:
--
-- -   DatabaseInformation -
--
--     -   @DatabaseName@ - The name of the Amazon RDS database.
--     -   @InstanceIdentifier @ - A unique identifier for the Amazon RDS
--         database instance.
--
-- -   DatabaseCredentials - AWS Identity and Access Management (IAM)
--     credentials that are used to connect to the Amazon RDS database.
--
-- -   ResourceRole - A role (DataPipelineDefaultResourceRole) assumed by
--     an EC2 instance to carry out the copy task from Amazon RDS to Amazon
--     Simple Storage Service (Amazon S3). For more information, see
--     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
--     for data pipelines.
--
-- -   ServiceRole - A role (DataPipelineDefaultRole) assumed by the AWS
--     Data Pipeline service to monitor the progress of the copy task from
--     Amazon RDS to Amazon S3. For more information, see
--     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
--     for data pipelines.
--
-- -   SecurityInfo - The security information to use to access an RDS DB
--     instance. You need to set up appropriate ingress rules for the
--     security entity IDs provided to allow access to the Amazon RDS
--     instance. Specify a [@SubnetId@, @SecurityGroupIds@] pair for a
--     VPC-based RDS DB instance.
--
-- -   SelectSqlQuery - A query that is used to retrieve the observation
--     data for the @Datasource@.
--
-- -   S3StagingLocation - The Amazon S3 location for staging Amazon RDS
--     data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is
--     stored in this location.
--
-- -   DataSchemaUri - The Amazon S3 location of the @DataSchema@.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if @DataSchemaUri@ is specified.
--
-- -   DataRearrangement - A JSON string that represents the splitting and
--     rearrangement requirements for the @Datasource@.
--
--     Sample -
--     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
--
-- 'roleARN', 'createDataSourceFromRDS_roleARN' - The role that Amazon ML assumes on behalf of the user to create and
-- activate a data pipeline in the user\'s account and copy data using the
-- @SelectSqlQuery@ query from Amazon RDS to Amazon S3.
newCreateDataSourceFromRDS ::
  -- | 'dataSourceId'
  Prelude.Text ->
  -- | 'rDSData'
  RDSDataSpec ->
  -- | 'roleARN'
  Prelude.Text ->
  CreateDataSourceFromRDS
newCreateDataSourceFromRDS
  pDataSourceId_
  pRDSData_
  pRoleARN_ =
    CreateDataSourceFromRDS'
      { computeStatistics =
          Prelude.Nothing,
        dataSourceName = Prelude.Nothing,
        dataSourceId = pDataSourceId_,
        rDSData = pRDSData_,
        roleARN = pRoleARN_
      }

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
-- training.
createDataSourceFromRDS_computeStatistics :: Lens.Lens' CreateDataSourceFromRDS (Prelude.Maybe Prelude.Bool)
createDataSourceFromRDS_computeStatistics = Lens.lens (\CreateDataSourceFromRDS' {computeStatistics} -> computeStatistics) (\s@CreateDataSourceFromRDS' {} a -> s {computeStatistics = a} :: CreateDataSourceFromRDS)

-- | A user-supplied name or description of the @DataSource@.
createDataSourceFromRDS_dataSourceName :: Lens.Lens' CreateDataSourceFromRDS (Prelude.Maybe Prelude.Text)
createDataSourceFromRDS_dataSourceName = Lens.lens (\CreateDataSourceFromRDS' {dataSourceName} -> dataSourceName) (\s@CreateDataSourceFromRDS' {} a -> s {dataSourceName = a} :: CreateDataSourceFromRDS)

-- | A user-supplied ID that uniquely identifies the @DataSource@. Typically,
-- an Amazon Resource Number (ARN) becomes the ID for a @DataSource@.
createDataSourceFromRDS_dataSourceId :: Lens.Lens' CreateDataSourceFromRDS Prelude.Text
createDataSourceFromRDS_dataSourceId = Lens.lens (\CreateDataSourceFromRDS' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromRDS' {} a -> s {dataSourceId = a} :: CreateDataSourceFromRDS)

-- | The data specification of an Amazon RDS @DataSource@:
--
-- -   DatabaseInformation -
--
--     -   @DatabaseName@ - The name of the Amazon RDS database.
--     -   @InstanceIdentifier @ - A unique identifier for the Amazon RDS
--         database instance.
--
-- -   DatabaseCredentials - AWS Identity and Access Management (IAM)
--     credentials that are used to connect to the Amazon RDS database.
--
-- -   ResourceRole - A role (DataPipelineDefaultResourceRole) assumed by
--     an EC2 instance to carry out the copy task from Amazon RDS to Amazon
--     Simple Storage Service (Amazon S3). For more information, see
--     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
--     for data pipelines.
--
-- -   ServiceRole - A role (DataPipelineDefaultRole) assumed by the AWS
--     Data Pipeline service to monitor the progress of the copy task from
--     Amazon RDS to Amazon S3. For more information, see
--     <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
--     for data pipelines.
--
-- -   SecurityInfo - The security information to use to access an RDS DB
--     instance. You need to set up appropriate ingress rules for the
--     security entity IDs provided to allow access to the Amazon RDS
--     instance. Specify a [@SubnetId@, @SecurityGroupIds@] pair for a
--     VPC-based RDS DB instance.
--
-- -   SelectSqlQuery - A query that is used to retrieve the observation
--     data for the @Datasource@.
--
-- -   S3StagingLocation - The Amazon S3 location for staging Amazon RDS
--     data. The data retrieved from Amazon RDS using @SelectSqlQuery@ is
--     stored in this location.
--
-- -   DataSchemaUri - The Amazon S3 location of the @DataSchema@.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if @DataSchemaUri@ is specified.
--
-- -   DataRearrangement - A JSON string that represents the splitting and
--     rearrangement requirements for the @Datasource@.
--
--     Sample -
--     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
createDataSourceFromRDS_rDSData :: Lens.Lens' CreateDataSourceFromRDS RDSDataSpec
createDataSourceFromRDS_rDSData = Lens.lens (\CreateDataSourceFromRDS' {rDSData} -> rDSData) (\s@CreateDataSourceFromRDS' {} a -> s {rDSData = a} :: CreateDataSourceFromRDS)

-- | The role that Amazon ML assumes on behalf of the user to create and
-- activate a data pipeline in the user\'s account and copy data using the
-- @SelectSqlQuery@ query from Amazon RDS to Amazon S3.
createDataSourceFromRDS_roleARN :: Lens.Lens' CreateDataSourceFromRDS Prelude.Text
createDataSourceFromRDS_roleARN = Lens.lens (\CreateDataSourceFromRDS' {roleARN} -> roleARN) (\s@CreateDataSourceFromRDS' {} a -> s {roleARN = a} :: CreateDataSourceFromRDS)

instance Prelude.AWSRequest CreateDataSourceFromRDS where
  type
    Rs CreateDataSourceFromRDS =
      CreateDataSourceFromRDSResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceFromRDSResponse'
            Prelude.<$> (x Prelude..?> "DataSourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataSourceFromRDS

instance Prelude.NFData CreateDataSourceFromRDS

instance Prelude.ToHeaders CreateDataSourceFromRDS where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.CreateDataSourceFromRDS" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateDataSourceFromRDS where
  toJSON CreateDataSourceFromRDS' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ComputeStatistics" Prelude..=)
              Prelude.<$> computeStatistics,
            ("DataSourceName" Prelude..=)
              Prelude.<$> dataSourceName,
            Prelude.Just
              ("DataSourceId" Prelude..= dataSourceId),
            Prelude.Just ("RDSData" Prelude..= rDSData),
            Prelude.Just ("RoleARN" Prelude..= roleARN)
          ]
      )

instance Prelude.ToPath CreateDataSourceFromRDS where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDataSourceFromRDS where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateDataSourceFromRDS@ operation, and is
-- an acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromRDS@> operation is asynchronous. You can poll
-- for updates by using the @GetBatchPrediction@ operation and checking the
-- @Status@ parameter. You can inspect the @Message@ when @Status@ shows up
-- as @FAILED@. You can also check the progress of the copy operation by
-- going to the @DataPipeline@ console and looking up the pipeline using
-- the @pipelineId @ from the describe call.
--
-- /See:/ 'newCreateDataSourceFromRDSResponse' smart constructor.
data CreateDataSourceFromRDSResponse = CreateDataSourceFromRDSResponse'
  { -- | A user-supplied ID that uniquely identifies the datasource. This value
    -- should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromRDSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'createDataSourceFromRDSResponse_dataSourceId' - A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
--
-- 'httpStatus', 'createDataSourceFromRDSResponse_httpStatus' - The response's http status code.
newCreateDataSourceFromRDSResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataSourceFromRDSResponse
newCreateDataSourceFromRDSResponse pHttpStatus_ =
  CreateDataSourceFromRDSResponse'
    { dataSourceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
createDataSourceFromRDSResponse_dataSourceId :: Lens.Lens' CreateDataSourceFromRDSResponse (Prelude.Maybe Prelude.Text)
createDataSourceFromRDSResponse_dataSourceId = Lens.lens (\CreateDataSourceFromRDSResponse' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromRDSResponse' {} a -> s {dataSourceId = a} :: CreateDataSourceFromRDSResponse)

-- | The response's http status code.
createDataSourceFromRDSResponse_httpStatus :: Lens.Lens' CreateDataSourceFromRDSResponse Prelude.Int
createDataSourceFromRDSResponse_httpStatus = Lens.lens (\CreateDataSourceFromRDSResponse' {httpStatus} -> httpStatus) (\s@CreateDataSourceFromRDSResponse' {} a -> s {httpStatus = a} :: CreateDataSourceFromRDSResponse)

instance
  Prelude.NFData
    CreateDataSourceFromRDSResponse
