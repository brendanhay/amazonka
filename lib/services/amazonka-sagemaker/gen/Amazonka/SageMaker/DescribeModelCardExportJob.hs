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
-- Module      : Amazonka.SageMaker.DescribeModelCardExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon SageMaker Model Card export job.
module Amazonka.SageMaker.DescribeModelCardExportJob
  ( -- * Creating a Request
    DescribeModelCardExportJob (..),
    newDescribeModelCardExportJob,

    -- * Request Lenses
    describeModelCardExportJob_modelCardExportJobArn,

    -- * Destructuring the Response
    DescribeModelCardExportJobResponse (..),
    newDescribeModelCardExportJobResponse,

    -- * Response Lenses
    describeModelCardExportJobResponse_exportArtifacts,
    describeModelCardExportJobResponse_failureReason,
    describeModelCardExportJobResponse_httpStatus,
    describeModelCardExportJobResponse_modelCardExportJobName,
    describeModelCardExportJobResponse_modelCardExportJobArn,
    describeModelCardExportJobResponse_status,
    describeModelCardExportJobResponse_modelCardName,
    describeModelCardExportJobResponse_modelCardVersion,
    describeModelCardExportJobResponse_outputConfig,
    describeModelCardExportJobResponse_createdAt,
    describeModelCardExportJobResponse_lastModifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeModelCardExportJob' smart constructor.
data DescribeModelCardExportJob = DescribeModelCardExportJob'
  { -- | The Amazon Resource Name (ARN) of the model card export job to describe.
    modelCardExportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelCardExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelCardExportJobArn', 'describeModelCardExportJob_modelCardExportJobArn' - The Amazon Resource Name (ARN) of the model card export job to describe.
newDescribeModelCardExportJob ::
  -- | 'modelCardExportJobArn'
  Prelude.Text ->
  DescribeModelCardExportJob
newDescribeModelCardExportJob pModelCardExportJobArn_ =
  DescribeModelCardExportJob'
    { modelCardExportJobArn =
        pModelCardExportJobArn_
    }

-- | The Amazon Resource Name (ARN) of the model card export job to describe.
describeModelCardExportJob_modelCardExportJobArn :: Lens.Lens' DescribeModelCardExportJob Prelude.Text
describeModelCardExportJob_modelCardExportJobArn = Lens.lens (\DescribeModelCardExportJob' {modelCardExportJobArn} -> modelCardExportJobArn) (\s@DescribeModelCardExportJob' {} a -> s {modelCardExportJobArn = a} :: DescribeModelCardExportJob)

instance Core.AWSRequest DescribeModelCardExportJob where
  type
    AWSResponse DescribeModelCardExportJob =
      DescribeModelCardExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelCardExportJobResponse'
            Prelude.<$> (x Data..?> "ExportArtifacts")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelCardExportJobName")
            Prelude.<*> (x Data..:> "ModelCardExportJobArn")
            Prelude.<*> (x Data..:> "Status")
            Prelude.<*> (x Data..:> "ModelCardName")
            Prelude.<*> (x Data..:> "ModelCardVersion")
            Prelude.<*> (x Data..:> "OutputConfig")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "LastModifiedAt")
      )

instance Prelude.Hashable DescribeModelCardExportJob where
  hashWithSalt _salt DescribeModelCardExportJob' {..} =
    _salt `Prelude.hashWithSalt` modelCardExportJobArn

instance Prelude.NFData DescribeModelCardExportJob where
  rnf DescribeModelCardExportJob' {..} =
    Prelude.rnf modelCardExportJobArn

instance Data.ToHeaders DescribeModelCardExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeModelCardExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeModelCardExportJob where
  toJSON DescribeModelCardExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ModelCardExportJobArn"
                  Data..= modelCardExportJobArn
              )
          ]
      )

instance Data.ToPath DescribeModelCardExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeModelCardExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelCardExportJobResponse' smart constructor.
data DescribeModelCardExportJobResponse = DescribeModelCardExportJobResponse'
  { -- | The exported model card artifacts.
    exportArtifacts :: Prelude.Maybe ModelCardExportArtifacts,
    -- | The failure reason if the model export job fails.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the model card export job to describe.
    modelCardExportJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model card export job.
    modelCardExportJobArn :: Prelude.Text,
    -- | The completion status of the model card export job.
    --
    -- -   @InProgress@: The model card export job is in progress.
    --
    -- -   @Completed@: The model card export job is complete.
    --
    -- -   @Failed@: The model card export job failed. To see the reason for
    --     the failure, see the @FailureReason@ field in the response to a
    --     @DescribeModelCardExportJob@ call.
    status :: ModelCardExportJobStatus,
    -- | The name of the model card that the model export job exports.
    modelCardName :: Prelude.Text,
    -- | The version of the model card that the model export job exports.
    modelCardVersion :: Prelude.Int,
    -- | The export output details for the model card.
    outputConfig :: ModelCardExportOutputConfig,
    -- | The date and time that the model export job was created.
    createdAt :: Data.POSIX,
    -- | The date and time that the model export job was last modified.
    lastModifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelCardExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportArtifacts', 'describeModelCardExportJobResponse_exportArtifacts' - The exported model card artifacts.
--
-- 'failureReason', 'describeModelCardExportJobResponse_failureReason' - The failure reason if the model export job fails.
--
-- 'httpStatus', 'describeModelCardExportJobResponse_httpStatus' - The response's http status code.
--
-- 'modelCardExportJobName', 'describeModelCardExportJobResponse_modelCardExportJobName' - The name of the model card export job to describe.
--
-- 'modelCardExportJobArn', 'describeModelCardExportJobResponse_modelCardExportJobArn' - The Amazon Resource Name (ARN) of the model card export job.
--
-- 'status', 'describeModelCardExportJobResponse_status' - The completion status of the model card export job.
--
-- -   @InProgress@: The model card export job is in progress.
--
-- -   @Completed@: The model card export job is complete.
--
-- -   @Failed@: The model card export job failed. To see the reason for
--     the failure, see the @FailureReason@ field in the response to a
--     @DescribeModelCardExportJob@ call.
--
-- 'modelCardName', 'describeModelCardExportJobResponse_modelCardName' - The name of the model card that the model export job exports.
--
-- 'modelCardVersion', 'describeModelCardExportJobResponse_modelCardVersion' - The version of the model card that the model export job exports.
--
-- 'outputConfig', 'describeModelCardExportJobResponse_outputConfig' - The export output details for the model card.
--
-- 'createdAt', 'describeModelCardExportJobResponse_createdAt' - The date and time that the model export job was created.
--
-- 'lastModifiedAt', 'describeModelCardExportJobResponse_lastModifiedAt' - The date and time that the model export job was last modified.
newDescribeModelCardExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelCardExportJobName'
  Prelude.Text ->
  -- | 'modelCardExportJobArn'
  Prelude.Text ->
  -- | 'status'
  ModelCardExportJobStatus ->
  -- | 'modelCardName'
  Prelude.Text ->
  -- | 'modelCardVersion'
  Prelude.Int ->
  -- | 'outputConfig'
  ModelCardExportOutputConfig ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  DescribeModelCardExportJobResponse
newDescribeModelCardExportJobResponse
  pHttpStatus_
  pModelCardExportJobName_
  pModelCardExportJobArn_
  pStatus_
  pModelCardName_
  pModelCardVersion_
  pOutputConfig_
  pCreatedAt_
  pLastModifiedAt_ =
    DescribeModelCardExportJobResponse'
      { exportArtifacts =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        modelCardExportJobName =
          pModelCardExportJobName_,
        modelCardExportJobArn =
          pModelCardExportJobArn_,
        status = pStatus_,
        modelCardName = pModelCardName_,
        modelCardVersion = pModelCardVersion_,
        outputConfig = pOutputConfig_,
        createdAt =
          Data._Time Lens.# pCreatedAt_,
        lastModifiedAt =
          Data._Time Lens.# pLastModifiedAt_
      }

-- | The exported model card artifacts.
describeModelCardExportJobResponse_exportArtifacts :: Lens.Lens' DescribeModelCardExportJobResponse (Prelude.Maybe ModelCardExportArtifacts)
describeModelCardExportJobResponse_exportArtifacts = Lens.lens (\DescribeModelCardExportJobResponse' {exportArtifacts} -> exportArtifacts) (\s@DescribeModelCardExportJobResponse' {} a -> s {exportArtifacts = a} :: DescribeModelCardExportJobResponse)

-- | The failure reason if the model export job fails.
describeModelCardExportJobResponse_failureReason :: Lens.Lens' DescribeModelCardExportJobResponse (Prelude.Maybe Prelude.Text)
describeModelCardExportJobResponse_failureReason = Lens.lens (\DescribeModelCardExportJobResponse' {failureReason} -> failureReason) (\s@DescribeModelCardExportJobResponse' {} a -> s {failureReason = a} :: DescribeModelCardExportJobResponse)

-- | The response's http status code.
describeModelCardExportJobResponse_httpStatus :: Lens.Lens' DescribeModelCardExportJobResponse Prelude.Int
describeModelCardExportJobResponse_httpStatus = Lens.lens (\DescribeModelCardExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeModelCardExportJobResponse' {} a -> s {httpStatus = a} :: DescribeModelCardExportJobResponse)

-- | The name of the model card export job to describe.
describeModelCardExportJobResponse_modelCardExportJobName :: Lens.Lens' DescribeModelCardExportJobResponse Prelude.Text
describeModelCardExportJobResponse_modelCardExportJobName = Lens.lens (\DescribeModelCardExportJobResponse' {modelCardExportJobName} -> modelCardExportJobName) (\s@DescribeModelCardExportJobResponse' {} a -> s {modelCardExportJobName = a} :: DescribeModelCardExportJobResponse)

-- | The Amazon Resource Name (ARN) of the model card export job.
describeModelCardExportJobResponse_modelCardExportJobArn :: Lens.Lens' DescribeModelCardExportJobResponse Prelude.Text
describeModelCardExportJobResponse_modelCardExportJobArn = Lens.lens (\DescribeModelCardExportJobResponse' {modelCardExportJobArn} -> modelCardExportJobArn) (\s@DescribeModelCardExportJobResponse' {} a -> s {modelCardExportJobArn = a} :: DescribeModelCardExportJobResponse)

-- | The completion status of the model card export job.
--
-- -   @InProgress@: The model card export job is in progress.
--
-- -   @Completed@: The model card export job is complete.
--
-- -   @Failed@: The model card export job failed. To see the reason for
--     the failure, see the @FailureReason@ field in the response to a
--     @DescribeModelCardExportJob@ call.
describeModelCardExportJobResponse_status :: Lens.Lens' DescribeModelCardExportJobResponse ModelCardExportJobStatus
describeModelCardExportJobResponse_status = Lens.lens (\DescribeModelCardExportJobResponse' {status} -> status) (\s@DescribeModelCardExportJobResponse' {} a -> s {status = a} :: DescribeModelCardExportJobResponse)

-- | The name of the model card that the model export job exports.
describeModelCardExportJobResponse_modelCardName :: Lens.Lens' DescribeModelCardExportJobResponse Prelude.Text
describeModelCardExportJobResponse_modelCardName = Lens.lens (\DescribeModelCardExportJobResponse' {modelCardName} -> modelCardName) (\s@DescribeModelCardExportJobResponse' {} a -> s {modelCardName = a} :: DescribeModelCardExportJobResponse)

-- | The version of the model card that the model export job exports.
describeModelCardExportJobResponse_modelCardVersion :: Lens.Lens' DescribeModelCardExportJobResponse Prelude.Int
describeModelCardExportJobResponse_modelCardVersion = Lens.lens (\DescribeModelCardExportJobResponse' {modelCardVersion} -> modelCardVersion) (\s@DescribeModelCardExportJobResponse' {} a -> s {modelCardVersion = a} :: DescribeModelCardExportJobResponse)

-- | The export output details for the model card.
describeModelCardExportJobResponse_outputConfig :: Lens.Lens' DescribeModelCardExportJobResponse ModelCardExportOutputConfig
describeModelCardExportJobResponse_outputConfig = Lens.lens (\DescribeModelCardExportJobResponse' {outputConfig} -> outputConfig) (\s@DescribeModelCardExportJobResponse' {} a -> s {outputConfig = a} :: DescribeModelCardExportJobResponse)

-- | The date and time that the model export job was created.
describeModelCardExportJobResponse_createdAt :: Lens.Lens' DescribeModelCardExportJobResponse Prelude.UTCTime
describeModelCardExportJobResponse_createdAt = Lens.lens (\DescribeModelCardExportJobResponse' {createdAt} -> createdAt) (\s@DescribeModelCardExportJobResponse' {} a -> s {createdAt = a} :: DescribeModelCardExportJobResponse) Prelude.. Data._Time

-- | The date and time that the model export job was last modified.
describeModelCardExportJobResponse_lastModifiedAt :: Lens.Lens' DescribeModelCardExportJobResponse Prelude.UTCTime
describeModelCardExportJobResponse_lastModifiedAt = Lens.lens (\DescribeModelCardExportJobResponse' {lastModifiedAt} -> lastModifiedAt) (\s@DescribeModelCardExportJobResponse' {} a -> s {lastModifiedAt = a} :: DescribeModelCardExportJobResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    DescribeModelCardExportJobResponse
  where
  rnf DescribeModelCardExportJobResponse' {..} =
    Prelude.rnf exportArtifacts
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardExportJobName
      `Prelude.seq` Prelude.rnf modelCardExportJobArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modelCardName
      `Prelude.seq` Prelude.rnf modelCardVersion
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastModifiedAt
