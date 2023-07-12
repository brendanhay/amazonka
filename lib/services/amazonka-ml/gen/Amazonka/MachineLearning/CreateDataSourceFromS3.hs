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
-- Module      : Amazonka.MachineLearning.CreateDataSourceFromS3
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.MachineLearning.CreateDataSourceFromS3
  ( -- * Creating a Request
    CreateDataSourceFromS3 (..),
    newCreateDataSourceFromS3,

    -- * Request Lenses
    createDataSourceFromS3_computeStatistics,
    createDataSourceFromS3_dataSourceName,
    createDataSourceFromS3_dataSourceId,
    createDataSourceFromS3_dataSpec,

    -- * Destructuring the Response
    CreateDataSourceFromS3Response (..),
    newCreateDataSourceFromS3Response,

    -- * Response Lenses
    createDataSourceFromS3Response_dataSourceId,
    createDataSourceFromS3Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataSourceFromS3' smart constructor.
data CreateDataSourceFromS3 = CreateDataSourceFromS3'
  { -- | The compute statistics for a @DataSource@. The statistics are generated
    -- from the observation data referenced by a @DataSource@. Amazon ML uses
    -- the statistics internally during @MLModel@ training. This parameter must
    -- be set to @true@ if the DataSource needs to be used for @MLModel@
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromS3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeStatistics', 'createDataSourceFromS3_computeStatistics' - The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the DataSource needs to be used for @MLModel@
-- training.
--
-- 'dataSourceName', 'createDataSourceFromS3_dataSourceName' - A user-supplied name or description of the @DataSource@.
--
-- 'dataSourceId', 'createDataSourceFromS3_dataSourceId' - A user-supplied identifier that uniquely identifies the @DataSource@.
--
-- 'dataSpec', 'createDataSourceFromS3_dataSpec' - The data specification of a @DataSource@:
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
newCreateDataSourceFromS3 ::
  -- | 'dataSourceId'
  Prelude.Text ->
  -- | 'dataSpec'
  S3DataSpec ->
  CreateDataSourceFromS3
newCreateDataSourceFromS3 pDataSourceId_ pDataSpec_ =
  CreateDataSourceFromS3'
    { computeStatistics =
        Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      dataSourceId = pDataSourceId_,
      dataSpec = pDataSpec_
    }

-- | The compute statistics for a @DataSource@. The statistics are generated
-- from the observation data referenced by a @DataSource@. Amazon ML uses
-- the statistics internally during @MLModel@ training. This parameter must
-- be set to @true@ if the DataSource needs to be used for @MLModel@
-- training.
createDataSourceFromS3_computeStatistics :: Lens.Lens' CreateDataSourceFromS3 (Prelude.Maybe Prelude.Bool)
createDataSourceFromS3_computeStatistics = Lens.lens (\CreateDataSourceFromS3' {computeStatistics} -> computeStatistics) (\s@CreateDataSourceFromS3' {} a -> s {computeStatistics = a} :: CreateDataSourceFromS3)

-- | A user-supplied name or description of the @DataSource@.
createDataSourceFromS3_dataSourceName :: Lens.Lens' CreateDataSourceFromS3 (Prelude.Maybe Prelude.Text)
createDataSourceFromS3_dataSourceName = Lens.lens (\CreateDataSourceFromS3' {dataSourceName} -> dataSourceName) (\s@CreateDataSourceFromS3' {} a -> s {dataSourceName = a} :: CreateDataSourceFromS3)

-- | A user-supplied identifier that uniquely identifies the @DataSource@.
createDataSourceFromS3_dataSourceId :: Lens.Lens' CreateDataSourceFromS3 Prelude.Text
createDataSourceFromS3_dataSourceId = Lens.lens (\CreateDataSourceFromS3' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromS3' {} a -> s {dataSourceId = a} :: CreateDataSourceFromS3)

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
createDataSourceFromS3_dataSpec :: Lens.Lens' CreateDataSourceFromS3 S3DataSpec
createDataSourceFromS3_dataSpec = Lens.lens (\CreateDataSourceFromS3' {dataSpec} -> dataSpec) (\s@CreateDataSourceFromS3' {} a -> s {dataSpec = a} :: CreateDataSourceFromS3)

instance Core.AWSRequest CreateDataSourceFromS3 where
  type
    AWSResponse CreateDataSourceFromS3 =
      CreateDataSourceFromS3Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataSourceFromS3Response'
            Prelude.<$> (x Data..?> "DataSourceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataSourceFromS3 where
  hashWithSalt _salt CreateDataSourceFromS3' {..} =
    _salt
      `Prelude.hashWithSalt` computeStatistics
      `Prelude.hashWithSalt` dataSourceName
      `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` dataSpec

instance Prelude.NFData CreateDataSourceFromS3 where
  rnf CreateDataSourceFromS3' {..} =
    Prelude.rnf computeStatistics
      `Prelude.seq` Prelude.rnf dataSourceName
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf dataSpec

instance Data.ToHeaders CreateDataSourceFromS3 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonML_20141212.CreateDataSourceFromS3" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataSourceFromS3 where
  toJSON CreateDataSourceFromS3' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComputeStatistics" Data..=)
              Prelude.<$> computeStatistics,
            ("DataSourceName" Data..=)
              Prelude.<$> dataSourceName,
            Prelude.Just ("DataSourceId" Data..= dataSourceId),
            Prelude.Just ("DataSpec" Data..= dataSpec)
          ]
      )

instance Data.ToPath CreateDataSourceFromS3 where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataSourceFromS3 where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateDataSourceFromS3@ operation, and is an
-- acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromS3@ operation is asynchronous. You can poll for
-- updates by using the @GetBatchPrediction@ operation and checking the
-- @Status@ parameter.
--
-- /See:/ 'newCreateDataSourceFromS3Response' smart constructor.
data CreateDataSourceFromS3Response = CreateDataSourceFromS3Response'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@. This value
    -- should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataSourceFromS3Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'createDataSourceFromS3Response_dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
--
-- 'httpStatus', 'createDataSourceFromS3Response_httpStatus' - The response's http status code.
newCreateDataSourceFromS3Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataSourceFromS3Response
newCreateDataSourceFromS3Response pHttpStatus_ =
  CreateDataSourceFromS3Response'
    { dataSourceId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@. This value
-- should be identical to the value of the @DataSourceID@ in the request.
createDataSourceFromS3Response_dataSourceId :: Lens.Lens' CreateDataSourceFromS3Response (Prelude.Maybe Prelude.Text)
createDataSourceFromS3Response_dataSourceId = Lens.lens (\CreateDataSourceFromS3Response' {dataSourceId} -> dataSourceId) (\s@CreateDataSourceFromS3Response' {} a -> s {dataSourceId = a} :: CreateDataSourceFromS3Response)

-- | The response's http status code.
createDataSourceFromS3Response_httpStatus :: Lens.Lens' CreateDataSourceFromS3Response Prelude.Int
createDataSourceFromS3Response_httpStatus = Lens.lens (\CreateDataSourceFromS3Response' {httpStatus} -> httpStatus) (\s@CreateDataSourceFromS3Response' {} a -> s {httpStatus = a} :: CreateDataSourceFromS3Response)

instance
  Prelude.NFData
    CreateDataSourceFromS3Response
  where
  rnf CreateDataSourceFromS3Response' {..} =
    Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf httpStatus
