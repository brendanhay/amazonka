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
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object. A @DataSource@ references data that can
-- be used to perform @CreateMLModel@, @CreateEvaluation@, or
-- @CreateBatchPrediction@ operations.
--
-- @CreateDataSourceFromS3@ is an asynchronous operation. In response to
-- @CreateDataSourceFromS3@, Amazon Machine Learning (Amazon ML)
-- immediately returns and sets the @DataSource@ status to @PENDING@. After
-- the @DataSource@ has been created and is ready for use, Amazon ML sets
-- the @Status@ parameter to @COMPLETED@. @DataSource@ in the @COMPLETED@
-- or @PENDING@ state can be used to perform only @CreateMLModel@,
-- @CreateEvaluation@ or @CreateBatchPrediction@ operations.
--
-- If Amazon ML can\'t accept the input source, it sets the @Status@
-- parameter to @FAILED@ and includes an error message in the @Message@
-- attribute of the @GetDataSource@ operation response.
--
-- The observation data used in a @DataSource@ should be ready to use; that
-- is, it should have a consistent structure, and missing data values
-- should be kept to a minimum. The observation data must reside in one or
-- more .csv files in an Amazon Simple Storage Service (Amazon S3)
-- location, along with a schema that describes the data items by name and
-- type. The same schema must be used for all of the data files referenced
-- by the @DataSource@.
--
-- After the @DataSource@ has been created, it\'s ready to use in
-- evaluations and batch predictions. If you plan to use the @DataSource@
-- to train an @MLModel@, the @DataSource@ also needs a recipe. A recipe
-- describes how each input variable will be used in training an @MLModel@.
-- Will the variable be included or excluded from training? Will the
-- variable be manipulated; for example, will it be combined with another
-- variable or will it be split apart into word combinations? The recipe
-- provides answers to these questions.
module Network.AWS.MachineLearning.CreateDataSourceFromS
  ( -- * Creating a Request
    CreateDataSourceFromS (..),
    newCreateDataSourceFromS,

    -- * Request Lenses
    createDataSourceFromS_computeStatistics,
    createDataSourceFromS_dataSourceName,
    createDataSourceFromS_dataSourceId,
    createDataSourceFromS_dataSpec,

    -- * Destructuring the Response
    CreateDataSourceFromSResponse (..),
    newCreateDataSourceFromSResponse,

    -- * Response Lenses
    createDataSourceFromSResponse_dataSourceId,
    createDataSourceFromSResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDataSourceFromS' smart constructor.
data CreateDataSourceFromS = CreateDataSourceFromS'
  { -- | The compute statistics for a @DataSource@. The statistics are generated
    -- from the observation data referenced by a @DataSource@. Amazon ML uses
    -- the statistics internally during @MLModel@ training. This parameter must
    -- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
    -- training.
    computeStatistics :: Prelude.Maybe Prelude.Bool,
    -- | A user-supplied name or description of the @DataSource@.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied identifier that uniquely identifies the @DataSource@.
    dataSourceId :: Prelude.Text,
    -- | The data specification of a @DataSource@:
    --
    -- -   DataLocationS3 - The Amazon S3 location of the observation data.
    --
    -- -   DataSchemaLocationS3 - The Amazon S3 location of the @DataSchema@.
    --
    -- -   DataSchema - A JSON string representing the schema. This is not
    --     required if @DataSchemaUri@ is specified.
    --
    -- -   DataRearrangement - A JSON string that represents the splitting and
    --     rearrangement requirements for the @Datasource@.
    --
    --     Sample -
    --     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
    dataSpec :: S3DataSpec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeStatistics', 'createDataSourceFromS_computeStatistics' - The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
-- training.
--
-- 'dataSourceName', 'createDataSourceFromS_dataSourceName' - A user-supplied name or description of the @DataSource@.
--
-- 'dataSourceId', 'createDataSourceFromS_dataSourceId' - A user-supplied identifier that uniquely identifies the @DataSource@.
--
-- 'dataSpec', 'createDataSourceFromS_dataSpec' - The data specification of a @DataSource@:
--
-- -   DataLocationS3 - The Amazon S3 location of the observation data.
--
-- -   DataSchemaLocationS3 - The Amazon S3 location of the @DataSchema@.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if @DataSchemaUri@ is specified.
--
-- -   DataRearrangement - A JSON string that represents the splitting and
--     rearrangement requirements for the @Datasource@.
--
--     Sample -
--     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
newCreateDataSourceFromS ::
  -- | 'dataSourceId'
  Prelude.Text ->
  -- | 'dataSpec'
  S3DataSpec ->
  CreateDataSourceFromS
newCreateDataSourceFromS pDataSourceId_ pDataSpec_ =
  CreateDataSourceFromS'
    { computeStatistics =
        Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      dataSourceId = pDataSourceId_,
      dataSpec = pDataSpec_
    }

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the @@DataSource@@ needs to be used for @MLModel@
-- training.
createDataSourceFromS_computeStatistics :: Lens.Lens' CreateDataSourceFromS (Prelude.Maybe Prelude.Bool)
createDataSourceFromS_computeStatistics = Lens.lens (\CreateDataSourceFromS' {computeStatistics} -> computeStatistics) (\s@CreateDataSourceFromS' {} a -> s {computeStatistics = a} :: CreateDataSourceFromS)

-- | A user-supplied name or description of the @DataSource@.
createDataSourceFromS_dataSourceName :: Lens.Lens' CreateDataSourceFromS (Prelude.Maybe Prelude.Text)
createDataSourceFromS_dataSourceName = Lens.lens (\CreateDataSourceFromS' {dataSourceName} -> dataSourceName) (\s@CreateDataSourceFromS' {} a -> s {dataSourceName = a} :: CreateDataSourceFromS)

-- | A user-supplied identifier that uniquely identifies the @DataSource@.
createDataSourceFromS_dataSourceId :: Lens.Lens' CreateDataSourceFromS Prelude.Text
createDataSourceFromS_dataSourceId = Lens.lens (\CreateDataSourceFromS' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromS' {} a -> s {dataSourceId = a} :: CreateDataSourceFromS)

-- | The data specification of a @DataSource@:
--
-- -   DataLocationS3 - The Amazon S3 location of the observation data.
--
-- -   DataSchemaLocationS3 - The Amazon S3 location of the @DataSchema@.
--
-- -   DataSchema - A JSON string representing the schema. This is not
--     required if @DataSchemaUri@ is specified.
--
-- -   DataRearrangement - A JSON string that represents the splitting and
--     rearrangement requirements for the @Datasource@.
--
--     Sample -
--     @ \"{\\\"splitting\\\":{\\\"percentBegin\\\":10,\\\"percentEnd\\\":60}}\"@
createDataSourceFromS_dataSpec :: Lens.Lens' CreateDataSourceFromS S3DataSpec
createDataSourceFromS_dataSpec = Lens.lens (\CreateDataSourceFromS' {dataSpec} -> dataSpec) (\s@CreateDataSourceFromS' {} a -> s {dataSpec = a} :: CreateDataSourceFromS)

instance Prelude.AWSRequest CreateDataSourceFromS where
  type
    Rs CreateDataSourceFromS =
      CreateDataSourceFromSResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceFromSResponse'
            Prelude.<$> (x Prelude..?> "DataSourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataSourceFromS

instance Prelude.NFData CreateDataSourceFromS

instance Prelude.ToHeaders CreateDataSourceFromS where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonML_20141212.CreateDataSourceFromS" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateDataSourceFromS where
  toJSON CreateDataSourceFromS' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ComputeStatistics" Prelude..=)
              Prelude.<$> computeStatistics,
            ("DataSourceName" Prelude..=)
              Prelude.<$> dataSourceName,
            Prelude.Just
              ("DataSourceId" Prelude..= dataSourceId),
            Prelude.Just ("DataSpec" Prelude..= dataSpec)
          ]
      )

instance Prelude.ToPath CreateDataSourceFromS where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDataSourceFromS where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateDataSourceFromS3@ operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromS3@ operation is asynchronous. You can poll for
-- updates by using the @GetBatchPrediction@ operation and checking the
-- @Status@ parameter.
--
-- /See:/ 'newCreateDataSourceFromSResponse' smart constructor.
data CreateDataSourceFromSResponse = CreateDataSourceFromSResponse'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@. This value
    -- should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromSResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'createDataSourceFromSResponse_dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
--
-- 'httpStatus', 'createDataSourceFromSResponse_httpStatus' - The response's http status code.
newCreateDataSourceFromSResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataSourceFromSResponse
newCreateDataSourceFromSResponse pHttpStatus_ =
  CreateDataSourceFromSResponse'
    { dataSourceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
createDataSourceFromSResponse_dataSourceId :: Lens.Lens' CreateDataSourceFromSResponse (Prelude.Maybe Prelude.Text)
createDataSourceFromSResponse_dataSourceId = Lens.lens (\CreateDataSourceFromSResponse' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromSResponse' {} a -> s {dataSourceId = a} :: CreateDataSourceFromSResponse)

-- | The response's http status code.
createDataSourceFromSResponse_httpStatus :: Lens.Lens' CreateDataSourceFromSResponse Prelude.Int
createDataSourceFromSResponse_httpStatus = Lens.lens (\CreateDataSourceFromSResponse' {httpStatus} -> httpStatus) (\s@CreateDataSourceFromSResponse' {} a -> s {httpStatus = a} :: CreateDataSourceFromSResponse)

instance Prelude.NFData CreateDataSourceFromSResponse
