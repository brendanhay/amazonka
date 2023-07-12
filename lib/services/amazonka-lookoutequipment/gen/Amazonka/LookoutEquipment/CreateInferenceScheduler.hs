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
-- Module      : Amazonka.LookoutEquipment.CreateInferenceScheduler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled inference. Scheduling an inference is setting up a
-- continuous real-time inference plan to analyze new measurement data.
-- When setting up the schedule, you provide an S3 bucket location for the
-- input data, assign it a delimiter between separate entries in the data,
-- set an offset delay if desired, and set the frequency of inferencing.
-- You must also provide an S3 bucket location for the output data.
module Amazonka.LookoutEquipment.CreateInferenceScheduler
  ( -- * Creating a Request
    CreateInferenceScheduler (..),
    newCreateInferenceScheduler,

    -- * Request Lenses
    createInferenceScheduler_dataDelayOffsetInMinutes,
    createInferenceScheduler_serverSideKmsKeyId,
    createInferenceScheduler_tags,
    createInferenceScheduler_modelName,
    createInferenceScheduler_inferenceSchedulerName,
    createInferenceScheduler_dataUploadFrequency,
    createInferenceScheduler_dataInputConfiguration,
    createInferenceScheduler_dataOutputConfiguration,
    createInferenceScheduler_roleArn,
    createInferenceScheduler_clientToken,

    -- * Destructuring the Response
    CreateInferenceSchedulerResponse (..),
    newCreateInferenceSchedulerResponse,

    -- * Response Lenses
    createInferenceSchedulerResponse_inferenceSchedulerArn,
    createInferenceSchedulerResponse_inferenceSchedulerName,
    createInferenceSchedulerResponse_status,
    createInferenceSchedulerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInferenceScheduler' smart constructor.
data CreateInferenceScheduler = CreateInferenceScheduler'
  { -- | The interval (in minutes) of planned delay at the start of each
    -- inference segment. For example, if inference is set to run every ten
    -- minutes, the delay is set to five minutes and the time is 09:08. The
    -- inference scheduler will wake up at the configured interval (which,
    -- without a delay configured, would be 09:10) plus the additional five
    -- minute delay time (so 09:15) to check your Amazon S3 bucket. The delay
    -- provides a buffer for you to upload data at the same frequency, so that
    -- you don\'t have to stop and restart the scheduler when uploading new
    -- data.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lookout-for-equipment/latest/ug/understanding-inference-process.html Understanding the inference process>.
    dataDelayOffsetInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Provides the identifier of the KMS key used to encrypt inference
    -- scheduler data by Amazon Lookout for Equipment.
    serverSideKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Any tags associated with the inference scheduler.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the previously trained ML model being used to create the
    -- inference scheduler.
    modelName :: Prelude.Text,
    -- | The name of the inference scheduler being created.
    inferenceSchedulerName :: Prelude.Text,
    -- | How often data is uploaded to the source Amazon S3 bucket for the input
    -- data. The value chosen is the length of time between data uploads. For
    -- instance, if you select 5 minutes, Amazon Lookout for Equipment will
    -- upload the real-time data to the source bucket once every 5 minutes.
    -- This frequency also determines how often Amazon Lookout for Equipment
    -- runs inference on your data.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lookout-for-equipment/latest/ug/understanding-inference-process.html Understanding the inference process>.
    dataUploadFrequency :: DataUploadFrequency,
    -- | Specifies configuration information for the input data for the inference
    -- scheduler, including delimiter, format, and dataset location.
    dataInputConfiguration :: InferenceInputConfiguration,
    -- | Specifies configuration information for the output results for the
    -- inference scheduler, including the S3 location for the output.
    dataOutputConfiguration :: InferenceOutputConfiguration,
    -- | The Amazon Resource Name (ARN) of a role with permission to access the
    -- data source being used for the inference.
    roleArn :: Prelude.Text,
    -- | A unique identifier for the request. If you do not set the client
    -- request token, Amazon Lookout for Equipment generates one.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInferenceScheduler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataDelayOffsetInMinutes', 'createInferenceScheduler_dataDelayOffsetInMinutes' - The interval (in minutes) of planned delay at the start of each
-- inference segment. For example, if inference is set to run every ten
-- minutes, the delay is set to five minutes and the time is 09:08. The
-- inference scheduler will wake up at the configured interval (which,
-- without a delay configured, would be 09:10) plus the additional five
-- minute delay time (so 09:15) to check your Amazon S3 bucket. The delay
-- provides a buffer for you to upload data at the same frequency, so that
-- you don\'t have to stop and restart the scheduler when uploading new
-- data.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lookout-for-equipment/latest/ug/understanding-inference-process.html Understanding the inference process>.
--
-- 'serverSideKmsKeyId', 'createInferenceScheduler_serverSideKmsKeyId' - Provides the identifier of the KMS key used to encrypt inference
-- scheduler data by Amazon Lookout for Equipment.
--
-- 'tags', 'createInferenceScheduler_tags' - Any tags associated with the inference scheduler.
--
-- 'modelName', 'createInferenceScheduler_modelName' - The name of the previously trained ML model being used to create the
-- inference scheduler.
--
-- 'inferenceSchedulerName', 'createInferenceScheduler_inferenceSchedulerName' - The name of the inference scheduler being created.
--
-- 'dataUploadFrequency', 'createInferenceScheduler_dataUploadFrequency' - How often data is uploaded to the source Amazon S3 bucket for the input
-- data. The value chosen is the length of time between data uploads. For
-- instance, if you select 5 minutes, Amazon Lookout for Equipment will
-- upload the real-time data to the source bucket once every 5 minutes.
-- This frequency also determines how often Amazon Lookout for Equipment
-- runs inference on your data.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lookout-for-equipment/latest/ug/understanding-inference-process.html Understanding the inference process>.
--
-- 'dataInputConfiguration', 'createInferenceScheduler_dataInputConfiguration' - Specifies configuration information for the input data for the inference
-- scheduler, including delimiter, format, and dataset location.
--
-- 'dataOutputConfiguration', 'createInferenceScheduler_dataOutputConfiguration' - Specifies configuration information for the output results for the
-- inference scheduler, including the S3 location for the output.
--
-- 'roleArn', 'createInferenceScheduler_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access the
-- data source being used for the inference.
--
-- 'clientToken', 'createInferenceScheduler_clientToken' - A unique identifier for the request. If you do not set the client
-- request token, Amazon Lookout for Equipment generates one.
newCreateInferenceScheduler ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  -- | 'dataUploadFrequency'
  DataUploadFrequency ->
  -- | 'dataInputConfiguration'
  InferenceInputConfiguration ->
  -- | 'dataOutputConfiguration'
  InferenceOutputConfiguration ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateInferenceScheduler
newCreateInferenceScheduler
  pModelName_
  pInferenceSchedulerName_
  pDataUploadFrequency_
  pDataInputConfiguration_
  pDataOutputConfiguration_
  pRoleArn_
  pClientToken_ =
    CreateInferenceScheduler'
      { dataDelayOffsetInMinutes =
          Prelude.Nothing,
        serverSideKmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        modelName = pModelName_,
        inferenceSchedulerName = pInferenceSchedulerName_,
        dataUploadFrequency = pDataUploadFrequency_,
        dataInputConfiguration = pDataInputConfiguration_,
        dataOutputConfiguration =
          pDataOutputConfiguration_,
        roleArn = pRoleArn_,
        clientToken = pClientToken_
      }

-- | The interval (in minutes) of planned delay at the start of each
-- inference segment. For example, if inference is set to run every ten
-- minutes, the delay is set to five minutes and the time is 09:08. The
-- inference scheduler will wake up at the configured interval (which,
-- without a delay configured, would be 09:10) plus the additional five
-- minute delay time (so 09:15) to check your Amazon S3 bucket. The delay
-- provides a buffer for you to upload data at the same frequency, so that
-- you don\'t have to stop and restart the scheduler when uploading new
-- data.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lookout-for-equipment/latest/ug/understanding-inference-process.html Understanding the inference process>.
createInferenceScheduler_dataDelayOffsetInMinutes :: Lens.Lens' CreateInferenceScheduler (Prelude.Maybe Prelude.Natural)
createInferenceScheduler_dataDelayOffsetInMinutes = Lens.lens (\CreateInferenceScheduler' {dataDelayOffsetInMinutes} -> dataDelayOffsetInMinutes) (\s@CreateInferenceScheduler' {} a -> s {dataDelayOffsetInMinutes = a} :: CreateInferenceScheduler)

-- | Provides the identifier of the KMS key used to encrypt inference
-- scheduler data by Amazon Lookout for Equipment.
createInferenceScheduler_serverSideKmsKeyId :: Lens.Lens' CreateInferenceScheduler (Prelude.Maybe Prelude.Text)
createInferenceScheduler_serverSideKmsKeyId = Lens.lens (\CreateInferenceScheduler' {serverSideKmsKeyId} -> serverSideKmsKeyId) (\s@CreateInferenceScheduler' {} a -> s {serverSideKmsKeyId = a} :: CreateInferenceScheduler)

-- | Any tags associated with the inference scheduler.
createInferenceScheduler_tags :: Lens.Lens' CreateInferenceScheduler (Prelude.Maybe [Tag])
createInferenceScheduler_tags = Lens.lens (\CreateInferenceScheduler' {tags} -> tags) (\s@CreateInferenceScheduler' {} a -> s {tags = a} :: CreateInferenceScheduler) Prelude.. Lens.mapping Lens.coerced

-- | The name of the previously trained ML model being used to create the
-- inference scheduler.
createInferenceScheduler_modelName :: Lens.Lens' CreateInferenceScheduler Prelude.Text
createInferenceScheduler_modelName = Lens.lens (\CreateInferenceScheduler' {modelName} -> modelName) (\s@CreateInferenceScheduler' {} a -> s {modelName = a} :: CreateInferenceScheduler)

-- | The name of the inference scheduler being created.
createInferenceScheduler_inferenceSchedulerName :: Lens.Lens' CreateInferenceScheduler Prelude.Text
createInferenceScheduler_inferenceSchedulerName = Lens.lens (\CreateInferenceScheduler' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@CreateInferenceScheduler' {} a -> s {inferenceSchedulerName = a} :: CreateInferenceScheduler)

-- | How often data is uploaded to the source Amazon S3 bucket for the input
-- data. The value chosen is the length of time between data uploads. For
-- instance, if you select 5 minutes, Amazon Lookout for Equipment will
-- upload the real-time data to the source bucket once every 5 minutes.
-- This frequency also determines how often Amazon Lookout for Equipment
-- runs inference on your data.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lookout-for-equipment/latest/ug/understanding-inference-process.html Understanding the inference process>.
createInferenceScheduler_dataUploadFrequency :: Lens.Lens' CreateInferenceScheduler DataUploadFrequency
createInferenceScheduler_dataUploadFrequency = Lens.lens (\CreateInferenceScheduler' {dataUploadFrequency} -> dataUploadFrequency) (\s@CreateInferenceScheduler' {} a -> s {dataUploadFrequency = a} :: CreateInferenceScheduler)

-- | Specifies configuration information for the input data for the inference
-- scheduler, including delimiter, format, and dataset location.
createInferenceScheduler_dataInputConfiguration :: Lens.Lens' CreateInferenceScheduler InferenceInputConfiguration
createInferenceScheduler_dataInputConfiguration = Lens.lens (\CreateInferenceScheduler' {dataInputConfiguration} -> dataInputConfiguration) (\s@CreateInferenceScheduler' {} a -> s {dataInputConfiguration = a} :: CreateInferenceScheduler)

-- | Specifies configuration information for the output results for the
-- inference scheduler, including the S3 location for the output.
createInferenceScheduler_dataOutputConfiguration :: Lens.Lens' CreateInferenceScheduler InferenceOutputConfiguration
createInferenceScheduler_dataOutputConfiguration = Lens.lens (\CreateInferenceScheduler' {dataOutputConfiguration} -> dataOutputConfiguration) (\s@CreateInferenceScheduler' {} a -> s {dataOutputConfiguration = a} :: CreateInferenceScheduler)

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- data source being used for the inference.
createInferenceScheduler_roleArn :: Lens.Lens' CreateInferenceScheduler Prelude.Text
createInferenceScheduler_roleArn = Lens.lens (\CreateInferenceScheduler' {roleArn} -> roleArn) (\s@CreateInferenceScheduler' {} a -> s {roleArn = a} :: CreateInferenceScheduler)

-- | A unique identifier for the request. If you do not set the client
-- request token, Amazon Lookout for Equipment generates one.
createInferenceScheduler_clientToken :: Lens.Lens' CreateInferenceScheduler Prelude.Text
createInferenceScheduler_clientToken = Lens.lens (\CreateInferenceScheduler' {clientToken} -> clientToken) (\s@CreateInferenceScheduler' {} a -> s {clientToken = a} :: CreateInferenceScheduler)

instance Core.AWSRequest CreateInferenceScheduler where
  type
    AWSResponse CreateInferenceScheduler =
      CreateInferenceSchedulerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInferenceSchedulerResponse'
            Prelude.<$> (x Data..?> "InferenceSchedulerArn")
            Prelude.<*> (x Data..?> "InferenceSchedulerName")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInferenceScheduler where
  hashWithSalt _salt CreateInferenceScheduler' {..} =
    _salt
      `Prelude.hashWithSalt` dataDelayOffsetInMinutes
      `Prelude.hashWithSalt` serverSideKmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` inferenceSchedulerName
      `Prelude.hashWithSalt` dataUploadFrequency
      `Prelude.hashWithSalt` dataInputConfiguration
      `Prelude.hashWithSalt` dataOutputConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateInferenceScheduler where
  rnf CreateInferenceScheduler' {..} =
    Prelude.rnf dataDelayOffsetInMinutes
      `Prelude.seq` Prelude.rnf serverSideKmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf inferenceSchedulerName
      `Prelude.seq` Prelude.rnf dataUploadFrequency
      `Prelude.seq` Prelude.rnf dataInputConfiguration
      `Prelude.seq` Prelude.rnf dataOutputConfiguration
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateInferenceScheduler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.CreateInferenceScheduler" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateInferenceScheduler where
  toJSON CreateInferenceScheduler' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataDelayOffsetInMinutes" Data..=)
              Prelude.<$> dataDelayOffsetInMinutes,
            ("ServerSideKmsKeyId" Data..=)
              Prelude.<$> serverSideKmsKeyId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ModelName" Data..= modelName),
            Prelude.Just
              ( "InferenceSchedulerName"
                  Data..= inferenceSchedulerName
              ),
            Prelude.Just
              ("DataUploadFrequency" Data..= dataUploadFrequency),
            Prelude.Just
              ( "DataInputConfiguration"
                  Data..= dataInputConfiguration
              ),
            Prelude.Just
              ( "DataOutputConfiguration"
                  Data..= dataOutputConfiguration
              ),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateInferenceScheduler where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInferenceScheduler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInferenceSchedulerResponse' smart constructor.
data CreateInferenceSchedulerResponse = CreateInferenceSchedulerResponse'
  { -- | The Amazon Resource Name (ARN) of the inference scheduler being created.
    inferenceSchedulerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of inference scheduler being created.
    inferenceSchedulerName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the @CreateInferenceScheduler@ operation.
    status :: Prelude.Maybe InferenceSchedulerStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInferenceSchedulerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceSchedulerArn', 'createInferenceSchedulerResponse_inferenceSchedulerArn' - The Amazon Resource Name (ARN) of the inference scheduler being created.
--
-- 'inferenceSchedulerName', 'createInferenceSchedulerResponse_inferenceSchedulerName' - The name of inference scheduler being created.
--
-- 'status', 'createInferenceSchedulerResponse_status' - Indicates the status of the @CreateInferenceScheduler@ operation.
--
-- 'httpStatus', 'createInferenceSchedulerResponse_httpStatus' - The response's http status code.
newCreateInferenceSchedulerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInferenceSchedulerResponse
newCreateInferenceSchedulerResponse pHttpStatus_ =
  CreateInferenceSchedulerResponse'
    { inferenceSchedulerArn =
        Prelude.Nothing,
      inferenceSchedulerName = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the inference scheduler being created.
createInferenceSchedulerResponse_inferenceSchedulerArn :: Lens.Lens' CreateInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
createInferenceSchedulerResponse_inferenceSchedulerArn = Lens.lens (\CreateInferenceSchedulerResponse' {inferenceSchedulerArn} -> inferenceSchedulerArn) (\s@CreateInferenceSchedulerResponse' {} a -> s {inferenceSchedulerArn = a} :: CreateInferenceSchedulerResponse)

-- | The name of inference scheduler being created.
createInferenceSchedulerResponse_inferenceSchedulerName :: Lens.Lens' CreateInferenceSchedulerResponse (Prelude.Maybe Prelude.Text)
createInferenceSchedulerResponse_inferenceSchedulerName = Lens.lens (\CreateInferenceSchedulerResponse' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@CreateInferenceSchedulerResponse' {} a -> s {inferenceSchedulerName = a} :: CreateInferenceSchedulerResponse)

-- | Indicates the status of the @CreateInferenceScheduler@ operation.
createInferenceSchedulerResponse_status :: Lens.Lens' CreateInferenceSchedulerResponse (Prelude.Maybe InferenceSchedulerStatus)
createInferenceSchedulerResponse_status = Lens.lens (\CreateInferenceSchedulerResponse' {status} -> status) (\s@CreateInferenceSchedulerResponse' {} a -> s {status = a} :: CreateInferenceSchedulerResponse)

-- | The response's http status code.
createInferenceSchedulerResponse_httpStatus :: Lens.Lens' CreateInferenceSchedulerResponse Prelude.Int
createInferenceSchedulerResponse_httpStatus = Lens.lens (\CreateInferenceSchedulerResponse' {httpStatus} -> httpStatus) (\s@CreateInferenceSchedulerResponse' {} a -> s {httpStatus = a} :: CreateInferenceSchedulerResponse)

instance
  Prelude.NFData
    CreateInferenceSchedulerResponse
  where
  rnf CreateInferenceSchedulerResponse' {..} =
    Prelude.rnf inferenceSchedulerArn
      `Prelude.seq` Prelude.rnf inferenceSchedulerName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
