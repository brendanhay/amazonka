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
-- Module      : Network.AWS.Comprehend.StartEventsDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous event detection job for a collection of
-- documents.
module Network.AWS.Comprehend.StartEventsDetectionJob
  ( -- * Creating a Request
    StartEventsDetectionJob (..),
    newStartEventsDetectionJob,

    -- * Request Lenses
    startEventsDetectionJob_clientRequestToken,
    startEventsDetectionJob_jobName,
    startEventsDetectionJob_inputDataConfig,
    startEventsDetectionJob_outputDataConfig,
    startEventsDetectionJob_dataAccessRoleArn,
    startEventsDetectionJob_languageCode,
    startEventsDetectionJob_targetEventTypes,

    -- * Destructuring the Response
    StartEventsDetectionJobResponse (..),
    newStartEventsDetectionJobResponse,

    -- * Response Lenses
    startEventsDetectionJobResponse_jobStatus,
    startEventsDetectionJobResponse_jobId,
    startEventsDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartEventsDetectionJob' smart constructor.
data StartEventsDetectionJob = StartEventsDetectionJob'
  { -- | An unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The identifier of the events detection job.
    jobName :: Core.Maybe Core.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Text,
    -- | The language code of the input documents.
    languageCode :: LanguageCode,
    -- | The types of events to detect in the input documents.
    targetEventTypes :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartEventsDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startEventsDetectionJob_clientRequestToken' - An unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startEventsDetectionJob_jobName' - The identifier of the events detection job.
--
-- 'inputDataConfig', 'startEventsDetectionJob_inputDataConfig' - Specifies the format and location of the input data for the job.
--
-- 'outputDataConfig', 'startEventsDetectionJob_outputDataConfig' - Specifies where to send the output files.
--
-- 'dataAccessRoleArn', 'startEventsDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- 'languageCode', 'startEventsDetectionJob_languageCode' - The language code of the input documents.
--
-- 'targetEventTypes', 'startEventsDetectionJob_targetEventTypes' - The types of events to detect in the input documents.
newStartEventsDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Core.Text ->
  -- | 'languageCode'
  LanguageCode ->
  -- | 'targetEventTypes'
  Core.NonEmpty Core.Text ->
  StartEventsDetectionJob
newStartEventsDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_
  pTargetEventTypes_ =
    StartEventsDetectionJob'
      { clientRequestToken =
          Core.Nothing,
        jobName = Core.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_,
        targetEventTypes =
          Lens._Coerce Lens.# pTargetEventTypes_
      }

-- | An unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startEventsDetectionJob_clientRequestToken :: Lens.Lens' StartEventsDetectionJob (Core.Maybe Core.Text)
startEventsDetectionJob_clientRequestToken = Lens.lens (\StartEventsDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartEventsDetectionJob' {} a -> s {clientRequestToken = a} :: StartEventsDetectionJob)

-- | The identifier of the events detection job.
startEventsDetectionJob_jobName :: Lens.Lens' StartEventsDetectionJob (Core.Maybe Core.Text)
startEventsDetectionJob_jobName = Lens.lens (\StartEventsDetectionJob' {jobName} -> jobName) (\s@StartEventsDetectionJob' {} a -> s {jobName = a} :: StartEventsDetectionJob)

-- | Specifies the format and location of the input data for the job.
startEventsDetectionJob_inputDataConfig :: Lens.Lens' StartEventsDetectionJob InputDataConfig
startEventsDetectionJob_inputDataConfig = Lens.lens (\StartEventsDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartEventsDetectionJob' {} a -> s {inputDataConfig = a} :: StartEventsDetectionJob)

-- | Specifies where to send the output files.
startEventsDetectionJob_outputDataConfig :: Lens.Lens' StartEventsDetectionJob OutputDataConfig
startEventsDetectionJob_outputDataConfig = Lens.lens (\StartEventsDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartEventsDetectionJob' {} a -> s {outputDataConfig = a} :: StartEventsDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
startEventsDetectionJob_dataAccessRoleArn :: Lens.Lens' StartEventsDetectionJob Core.Text
startEventsDetectionJob_dataAccessRoleArn = Lens.lens (\StartEventsDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartEventsDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartEventsDetectionJob)

-- | The language code of the input documents.
startEventsDetectionJob_languageCode :: Lens.Lens' StartEventsDetectionJob LanguageCode
startEventsDetectionJob_languageCode = Lens.lens (\StartEventsDetectionJob' {languageCode} -> languageCode) (\s@StartEventsDetectionJob' {} a -> s {languageCode = a} :: StartEventsDetectionJob)

-- | The types of events to detect in the input documents.
startEventsDetectionJob_targetEventTypes :: Lens.Lens' StartEventsDetectionJob (Core.NonEmpty Core.Text)
startEventsDetectionJob_targetEventTypes = Lens.lens (\StartEventsDetectionJob' {targetEventTypes} -> targetEventTypes) (\s@StartEventsDetectionJob' {} a -> s {targetEventTypes = a} :: StartEventsDetectionJob) Core.. Lens._Coerce

instance Core.AWSRequest StartEventsDetectionJob where
  type
    AWSResponse StartEventsDetectionJob =
      StartEventsDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEventsDetectionJobResponse'
            Core.<$> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartEventsDetectionJob

instance Core.NFData StartEventsDetectionJob

instance Core.ToHeaders StartEventsDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartEventsDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartEventsDetectionJob where
  toJSON StartEventsDetectionJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("JobName" Core..=) Core.<$> jobName,
            Core.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Core.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Core.Just ("LanguageCode" Core..= languageCode),
            Core.Just
              ("TargetEventTypes" Core..= targetEventTypes)
          ]
      )

instance Core.ToPath StartEventsDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery StartEventsDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartEventsDetectionJobResponse' smart constructor.
data StartEventsDetectionJobResponse = StartEventsDetectionJobResponse'
  { -- | The status of the events detection job.
    jobStatus :: Core.Maybe JobStatus,
    -- | An unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartEventsDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startEventsDetectionJobResponse_jobStatus' - The status of the events detection job.
--
-- 'jobId', 'startEventsDetectionJobResponse_jobId' - An unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'httpStatus', 'startEventsDetectionJobResponse_httpStatus' - The response's http status code.
newStartEventsDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartEventsDetectionJobResponse
newStartEventsDetectionJobResponse pHttpStatus_ =
  StartEventsDetectionJobResponse'
    { jobStatus =
        Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the events detection job.
startEventsDetectionJobResponse_jobStatus :: Lens.Lens' StartEventsDetectionJobResponse (Core.Maybe JobStatus)
startEventsDetectionJobResponse_jobStatus = Lens.lens (\StartEventsDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartEventsDetectionJobResponse' {} a -> s {jobStatus = a} :: StartEventsDetectionJobResponse)

-- | An unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startEventsDetectionJobResponse_jobId :: Lens.Lens' StartEventsDetectionJobResponse (Core.Maybe Core.Text)
startEventsDetectionJobResponse_jobId = Lens.lens (\StartEventsDetectionJobResponse' {jobId} -> jobId) (\s@StartEventsDetectionJobResponse' {} a -> s {jobId = a} :: StartEventsDetectionJobResponse)

-- | The response's http status code.
startEventsDetectionJobResponse_httpStatus :: Lens.Lens' StartEventsDetectionJobResponse Core.Int
startEventsDetectionJobResponse_httpStatus = Lens.lens (\StartEventsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartEventsDetectionJobResponse' {} a -> s {httpStatus = a} :: StartEventsDetectionJobResponse)

instance Core.NFData StartEventsDetectionJobResponse
