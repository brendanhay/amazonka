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
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRedshift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ from a database hosted on an Amazon Redshift
-- cluster. A @DataSource@ references data that can be used to perform
-- either @CreateMLModel@, @CreateEvaluation@, or @CreateBatchPrediction@
-- operations.
--
-- @CreateDataSourceFromRedshift@ is an asynchronous operation. In response
-- to @CreateDataSourceFromRedshift@, Amazon Machine Learning (Amazon ML)
-- immediately returns and sets the @DataSource@ status to @PENDING@. After
-- the @DataSource@ is created and ready for use, Amazon ML sets the
-- @Status@ parameter to @COMPLETED@. @DataSource@ in @COMPLETED@ or
-- @PENDING@ states can be used to perform only @CreateMLModel@,
-- @CreateEvaluation@, or @CreateBatchPrediction@ operations.
--
-- If Amazon ML can\'t accept the input source, it sets the @Status@
-- parameter to @FAILED@ and includes an error message in the @Message@
-- attribute of the @GetDataSource@ operation response.
--
-- The observations should be contained in the database hosted on an Amazon
-- Redshift cluster and should be specified by a @SelectSqlQuery@ query.
-- Amazon ML executes an @Unload@ command in Amazon Redshift to transfer
-- the result set of the @SelectSqlQuery@ query to @S3StagingLocation@.
--
-- After the @DataSource@ has been created, it\'s ready for use in
-- evaluations and batch predictions. If you plan to use the @DataSource@
-- to train an @MLModel@, the @DataSource@ also requires a recipe. A recipe
-- describes how each input variable will be used in training an @MLModel@.
-- Will the variable be included or excluded from training? Will the
-- variable be manipulated; for example, will it be combined with another
-- variable or will it be split apart into word combinations? The recipe
-- provides answers to these questions.
--
-- You can\'t change an existing datasource, but you can copy and modify
-- the settings from an existing Amazon Redshift datasource to create a new
-- datasource. To do so, call @GetDataSource@ for an existing datasource
-- and copy the values to a @CreateDataSource@ call. Change the settings
-- that you want to change and make sure that all required fields have the
-- appropriate values.
module Network.AWS.MachineLearning.CreateDataSourceFromRedshift
  ( -- * Creating a Request
    CreateDataSourceFromRedshift (..),
    newCreateDataSourceFromRedshift,

    -- * Request Lenses
    createDataSourceFromRedshift_computeStatistics,
    createDataSourceFromRedshift_dataSourceName,
    createDataSourceFromRedshift_dataSourceId,
    createDataSourceFromRedshift_dataSpec,
    createDataSourceFromRedshift_roleARN,

    -- * Destructuring the Response
    CreateDataSourceFromRedshiftResponse (..),
    newCreateDataSourceFromRedshiftResponse,

    -- * Response Lenses
    createDataSourceFromRedshiftResponse_dataSourceId,
    createDataSourceFromRedshiftResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataSourceFromRedshift' smart constructor.
data CreateDataSourceFromRedshift = CreateDataSourceFromRedshift'
  { -- | The compute statistics for a @DataSource@. The statistics are generated
    -- from the observation data referenced by a @DataSource@. Amazon ML uses
    -- the statistics internally during @MLModel@ training. This parameter must
    -- be set to @true@ if the @DataSource@ needs to be used for @MLModel@
    -- training.
    computeStatistics :: Prelude.Maybe Prelude.Bool,
    -- | A user-supplied name or description of the @DataSource@.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied ID that uniquely identifies the @DataSource@.
    dataSourceId :: Prelude.Text,
    -- | The data specification of an Amazon Redshift @DataSource@:
    --
    -- -   DatabaseInformation -
    --
    --     -   @DatabaseName@ - The name of the Amazon Redshift database.
    --     -   @ ClusterIdentifier@ - The unique ID for the Amazon Redshift
    --         cluster.
    --
    -- -   DatabaseCredentials - The AWS Identity and Access Management (IAM)
    --     credentials that are used to connect to the Amazon Redshift
    --     database.
    --
    -- -   SelectSqlQuery - The query that is used to retrieve the observation
    --     data for the @Datasource@.
    --
    -- -   S3StagingLocation - The Amazon Simple Storage Service (Amazon S3)
    --     location for staging Amazon Redshift data. The data retrieved from
    --     Amazon Redshift using the @SelectSqlQuery@ query is stored in this
    --     location.
    --
    -- -   DataSchemaUri - The Amazon S3 location of the @DataSchema@.
    --
    -- -   DataSchema - A JSON string representing the schema. This is not
    --     required if @DataSchemaUri@ is specified.
    --
    -- -   DataRearrangement - A JSON string that represents the splitting and
    --     rearrangement requirements for the @DataSource@.
    --
    --     Sample -
    --     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
    dataSpec :: RedshiftDataSpec,
    -- | A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the
    -- role on behalf of the user to create the following:
    --
    -- -   A security group to allow Amazon ML to execute the @SelectSqlQuery@
    --     query on an Amazon Redshift cluster
    --
    -- -   An Amazon S3 bucket policy to grant Amazon ML read\/write
    --     permissions on the @S3StagingLocation@
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromRedshift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeStatistics', 'createDataSourceFromRedshift_computeStatistics' - The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @DataSource@ needs to be used for @MLModel@
-- training.
--
-- 'dataSourceName', 'createDataSourceFromRedshift_dataSourceName' - A user-supplied name or description of the @DataSource@.
--
-- 'dataSourceId', 'createDataSourceFromRedshift_dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@.
--
-- 'dataSpec', 'createDataSourceFromRedshift_dataSpec' - The data specification of an Amazon Redshift @DataSource@:
--
-- -   DatabaseInformation -
--
--     -   @DatabaseName@ - The name of the Amazon Redshift database.
--     -   @ ClusterIdentifier@ - The unique ID for the Amazon Redshift
--         cluster.
--
-- -   DatabaseCredentials - The AWS Identity and Access Management (IAM)
--     credentials that are used to connect to the Amazon Redshift
--     database.
--
-- -   SelectSqlQuery - The query that is used to retrieve the observation
--     data for the @Datasource@.
--
-- -   S3StagingLocation - The Amazon Simple Storage Service (Amazon S3)
--     location for staging Amazon Redshift data. The data retrieved from
--     Amazon Redshift using the @SelectSqlQuery@ query is stored in this
--     location.
--
-- -   DataSchemaUri - The Amazon S3 location of the @DataSchema@.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if @DataSchemaUri@ is specified.
--
-- -   DataRearrangement - A JSON string that represents the splitting and
--     rearrangement requirements for the @DataSource@.
--
--     Sample -
--     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
--
-- 'roleARN', 'createDataSourceFromRedshift_roleARN' - A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the
-- role on behalf of the user to create the following:
--
-- -   A security group to allow Amazon ML to execute the @SelectSqlQuery@
--     query on an Amazon Redshift cluster
--
-- -   An Amazon S3 bucket policy to grant Amazon ML read\/write
--     permissions on the @S3StagingLocation@
newCreateDataSourceFromRedshift ::
  -- | 'dataSourceId'
  Prelude.Text ->
  -- | 'dataSpec'
  RedshiftDataSpec ->
  -- | 'roleARN'
  Prelude.Text ->
  CreateDataSourceFromRedshift
newCreateDataSourceFromRedshift
  pDataSourceId_
  pDataSpec_
  pRoleARN_ =
    CreateDataSourceFromRedshift'
      { computeStatistics =
          Prelude.Nothing,
        dataSourceName = Prelude.Nothing,
        dataSourceId = pDataSourceId_,
        dataSpec = pDataSpec_,
        roleARN = pRoleARN_
      }

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @DataSource@ needs to be used for @MLModel@
-- training.
createDataSourceFromRedshift_computeStatistics :: Lens.Lens' CreateDataSourceFromRedshift (Prelude.Maybe Prelude.Bool)
createDataSourceFromRedshift_computeStatistics = Lens.lens (\CreateDataSourceFromRedshift' {computeStatistics} -> computeStatistics) (\s@CreateDataSourceFromRedshift' {} a -> s {computeStatistics = a} :: CreateDataSourceFromRedshift)

-- | A user-supplied name or description of the @DataSource@.
createDataSourceFromRedshift_dataSourceName :: Lens.Lens' CreateDataSourceFromRedshift (Prelude.Maybe Prelude.Text)
createDataSourceFromRedshift_dataSourceName = Lens.lens (\CreateDataSourceFromRedshift' {dataSourceName} -> dataSourceName) (\s@CreateDataSourceFromRedshift' {} a -> s {dataSourceName = a} :: CreateDataSourceFromRedshift)

-- | A user-supplied ID that uniquely identifies the @DataSource@.
createDataSourceFromRedshift_dataSourceId :: Lens.Lens' CreateDataSourceFromRedshift Prelude.Text
createDataSourceFromRedshift_dataSourceId = Lens.lens (\CreateDataSourceFromRedshift' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromRedshift' {} a -> s {dataSourceId = a} :: CreateDataSourceFromRedshift)

-- | The data specification of an Amazon Redshift @DataSource@:
--
-- -   DatabaseInformation -
--
--     -   @DatabaseName@ - The name of the Amazon Redshift database.
--     -   @ ClusterIdentifier@ - The unique ID for the Amazon Redshift
--         cluster.
--
-- -   DatabaseCredentials - The AWS Identity and Access Management (IAM)
--     credentials that are used to connect to the Amazon Redshift
--     database.
--
-- -   SelectSqlQuery - The query that is used to retrieve the observation
--     data for the @Datasource@.
--
-- -   S3StagingLocation - The Amazon Simple Storage Service (Amazon S3)
--     location for staging Amazon Redshift data. The data retrieved from
--     Amazon Redshift using the @SelectSqlQuery@ query is stored in this
--     location.
--
-- -   DataSchemaUri - The Amazon S3 location of the @DataSchema@.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if @DataSchemaUri@ is specified.
--
-- -   DataRearrangement - A JSON string that represents the splitting and
--     rearrangement requirements for the @DataSource@.
--
--     Sample -
--     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
createDataSourceFromRedshift_dataSpec :: Lens.Lens' CreateDataSourceFromRedshift RedshiftDataSpec
createDataSourceFromRedshift_dataSpec = Lens.lens (\CreateDataSourceFromRedshift' {dataSpec} -> dataSpec) (\s@CreateDataSourceFromRedshift' {} a -> s {dataSpec = a} :: CreateDataSourceFromRedshift)

-- | A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the
-- role on behalf of the user to create the following:
--
-- -   A security group to allow Amazon ML to execute the @SelectSqlQuery@
--     query on an Amazon Redshift cluster
--
-- -   An Amazon S3 bucket policy to grant Amazon ML read\/write
--     permissions on the @S3StagingLocation@
createDataSourceFromRedshift_roleARN :: Lens.Lens' CreateDataSourceFromRedshift Prelude.Text
createDataSourceFromRedshift_roleARN = Lens.lens (\CreateDataSourceFromRedshift' {roleARN} -> roleARN) (\s@CreateDataSourceFromRedshift' {} a -> s {roleARN = a} :: CreateDataSourceFromRedshift)

instance
  Prelude.AWSRequest
    CreateDataSourceFromRedshift
  where
  type
    Rs CreateDataSourceFromRedshift =
      CreateDataSourceFromRedshiftResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceFromRedshiftResponse'
            Prelude.<$> (x Prelude..?> "DataSourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDataSourceFromRedshift

instance Prelude.NFData CreateDataSourceFromRedshift

instance
  Prelude.ToHeaders
    CreateDataSourceFromRedshift
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.CreateDataSourceFromRedshift" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateDataSourceFromRedshift where
  toJSON CreateDataSourceFromRedshift' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ComputeStatistics" Prelude..=)
              Prelude.<$> computeStatistics,
            ("DataSourceName" Prelude..=)
              Prelude.<$> dataSourceName,
            Prelude.Just
              ("DataSourceId" Prelude..= dataSourceId),
            Prelude.Just ("DataSpec" Prelude..= dataSpec),
            Prelude.Just ("RoleARN" Prelude..= roleARN)
          ]
      )

instance Prelude.ToPath CreateDataSourceFromRedshift where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDataSourceFromRedshift where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateDataSourceFromRedshift@ operation, and
-- is an acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromRedshift@ operation is asynchronous. You can
-- poll for updates by using the @GetBatchPrediction@ operation and
-- checking the @Status@ parameter.
--
-- /See:/ 'newCreateDataSourceFromRedshiftResponse' smart constructor.
data CreateDataSourceFromRedshiftResponse = CreateDataSourceFromRedshiftResponse'
  { -- | A user-supplied ID that uniquely identifies the datasource. This value
    -- should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromRedshiftResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'createDataSourceFromRedshiftResponse_dataSourceId' - A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
--
-- 'httpStatus', 'createDataSourceFromRedshiftResponse_httpStatus' - The response's http status code.
newCreateDataSourceFromRedshiftResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataSourceFromRedshiftResponse
newCreateDataSourceFromRedshiftResponse pHttpStatus_ =
  CreateDataSourceFromRedshiftResponse'
    { dataSourceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value
-- should be identical to the value of the @DataSourceID@ in the request.
createDataSourceFromRedshiftResponse_dataSourceId :: Lens.Lens' CreateDataSourceFromRedshiftResponse (Prelude.Maybe Prelude.Text)
createDataSourceFromRedshiftResponse_dataSourceId = Lens.lens (\CreateDataSourceFromRedshiftResponse' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromRedshiftResponse' {} a -> s {dataSourceId = a} :: CreateDataSourceFromRedshiftResponse)

-- | The response's http status code.
createDataSourceFromRedshiftResponse_httpStatus :: Lens.Lens' CreateDataSourceFromRedshiftResponse Prelude.Int
createDataSourceFromRedshiftResponse_httpStatus = Lens.lens (\CreateDataSourceFromRedshiftResponse' {httpStatus} -> httpStatus) (\s@CreateDataSourceFromRedshiftResponse' {} a -> s {httpStatus = a} :: CreateDataSourceFromRedshiftResponse)

instance
  Prelude.NFData
    CreateDataSourceFromRedshiftResponse
