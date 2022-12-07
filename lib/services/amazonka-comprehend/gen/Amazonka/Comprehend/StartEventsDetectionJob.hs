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
-- Module      : Amazonka.Comprehend.StartEventsDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous event detection job for a collection of
-- documents.
module Amazonka.Comprehend.StartEventsDetectionJob
  ( -- * Creating a Request
    StartEventsDetectionJob (..),
    newStartEventsDetectionJob,

    -- * Request Lenses
    startEventsDetectionJob_tags,
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
    startEventsDetectionJobResponse_jobArn,
    startEventsDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartEventsDetectionJob' smart constructor.
data StartEventsDetectionJob = StartEventsDetectionJob'
  { -- | Tags to be associated with the events detection job. A tag is a
    -- key-value pair that adds metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | An unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the events detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format and location of the input data for the job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies where to send the output files.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language code of the input documents.
    languageCode :: LanguageCode,
    -- | The types of events to detect in the input documents.
    targetEventTypes :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEventsDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startEventsDetectionJob_tags' - Tags to be associated with the events detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
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
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  -- | 'targetEventTypes'
  Prelude.NonEmpty Prelude.Text ->
  StartEventsDetectionJob
newStartEventsDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pLanguageCode_
  pTargetEventTypes_ =
    StartEventsDetectionJob'
      { tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_,
        targetEventTypes =
          Lens.coerced Lens.# pTargetEventTypes_
      }

-- | Tags to be associated with the events detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
startEventsDetectionJob_tags :: Lens.Lens' StartEventsDetectionJob (Prelude.Maybe [Tag])
startEventsDetectionJob_tags = Lens.lens (\StartEventsDetectionJob' {tags} -> tags) (\s@StartEventsDetectionJob' {} a -> s {tags = a} :: StartEventsDetectionJob) Prelude.. Lens.mapping Lens.coerced

-- | An unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startEventsDetectionJob_clientRequestToken :: Lens.Lens' StartEventsDetectionJob (Prelude.Maybe Prelude.Text)
startEventsDetectionJob_clientRequestToken = Lens.lens (\StartEventsDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartEventsDetectionJob' {} a -> s {clientRequestToken = a} :: StartEventsDetectionJob)

-- | The identifier of the events detection job.
startEventsDetectionJob_jobName :: Lens.Lens' StartEventsDetectionJob (Prelude.Maybe Prelude.Text)
startEventsDetectionJob_jobName = Lens.lens (\StartEventsDetectionJob' {jobName} -> jobName) (\s@StartEventsDetectionJob' {} a -> s {jobName = a} :: StartEventsDetectionJob)

-- | Specifies the format and location of the input data for the job.
startEventsDetectionJob_inputDataConfig :: Lens.Lens' StartEventsDetectionJob InputDataConfig
startEventsDetectionJob_inputDataConfig = Lens.lens (\StartEventsDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartEventsDetectionJob' {} a -> s {inputDataConfig = a} :: StartEventsDetectionJob)

-- | Specifies where to send the output files.
startEventsDetectionJob_outputDataConfig :: Lens.Lens' StartEventsDetectionJob OutputDataConfig
startEventsDetectionJob_outputDataConfig = Lens.lens (\StartEventsDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartEventsDetectionJob' {} a -> s {outputDataConfig = a} :: StartEventsDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
startEventsDetectionJob_dataAccessRoleArn :: Lens.Lens' StartEventsDetectionJob Prelude.Text
startEventsDetectionJob_dataAccessRoleArn = Lens.lens (\StartEventsDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartEventsDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartEventsDetectionJob)

-- | The language code of the input documents.
startEventsDetectionJob_languageCode :: Lens.Lens' StartEventsDetectionJob LanguageCode
startEventsDetectionJob_languageCode = Lens.lens (\StartEventsDetectionJob' {languageCode} -> languageCode) (\s@StartEventsDetectionJob' {} a -> s {languageCode = a} :: StartEventsDetectionJob)

-- | The types of events to detect in the input documents.
startEventsDetectionJob_targetEventTypes :: Lens.Lens' StartEventsDetectionJob (Prelude.NonEmpty Prelude.Text)
startEventsDetectionJob_targetEventTypes = Lens.lens (\StartEventsDetectionJob' {targetEventTypes} -> targetEventTypes) (\s@StartEventsDetectionJob' {} a -> s {targetEventTypes = a} :: StartEventsDetectionJob) Prelude.. Lens.coerced

instance Core.AWSRequest StartEventsDetectionJob where
  type
    AWSResponse StartEventsDetectionJob =
      StartEventsDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEventsDetectionJobResponse'
            Prelude.<$> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "JobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartEventsDetectionJob where
  hashWithSalt _salt StartEventsDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` targetEventTypes

instance Prelude.NFData StartEventsDetectionJob where
  rnf StartEventsDetectionJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf targetEventTypes

instance Data.ToHeaders StartEventsDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StartEventsDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartEventsDetectionJob where
  toJSON StartEventsDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Data..=) Prelude.<$> jobName,
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Data..= languageCode),
            Prelude.Just
              ("TargetEventTypes" Data..= targetEventTypes)
          ]
      )

instance Data.ToPath StartEventsDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartEventsDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEventsDetectionJobResponse' smart constructor.
data StartEventsDetectionJobResponse = StartEventsDetectionJobResponse'
  { -- | The status of the events detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | An unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the events detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:events-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:events-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'jobArn', 'startEventsDetectionJobResponse_jobArn' - The Amazon Resource Name (ARN) of the events detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:events-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:events-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'httpStatus', 'startEventsDetectionJobResponse_httpStatus' - The response's http status code.
newStartEventsDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartEventsDetectionJobResponse
newStartEventsDetectionJobResponse pHttpStatus_ =
  StartEventsDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the events detection job.
startEventsDetectionJobResponse_jobStatus :: Lens.Lens' StartEventsDetectionJobResponse (Prelude.Maybe JobStatus)
startEventsDetectionJobResponse_jobStatus = Lens.lens (\StartEventsDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartEventsDetectionJobResponse' {} a -> s {jobStatus = a} :: StartEventsDetectionJobResponse)

-- | An unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startEventsDetectionJobResponse_jobId :: Lens.Lens' StartEventsDetectionJobResponse (Prelude.Maybe Prelude.Text)
startEventsDetectionJobResponse_jobId = Lens.lens (\StartEventsDetectionJobResponse' {jobId} -> jobId) (\s@StartEventsDetectionJobResponse' {} a -> s {jobId = a} :: StartEventsDetectionJobResponse)

-- | The Amazon Resource Name (ARN) of the events detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:events-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:events-detection-job\/1234abcd12ab34cd56ef1234567890ab@
startEventsDetectionJobResponse_jobArn :: Lens.Lens' StartEventsDetectionJobResponse (Prelude.Maybe Prelude.Text)
startEventsDetectionJobResponse_jobArn = Lens.lens (\StartEventsDetectionJobResponse' {jobArn} -> jobArn) (\s@StartEventsDetectionJobResponse' {} a -> s {jobArn = a} :: StartEventsDetectionJobResponse)

-- | The response's http status code.
startEventsDetectionJobResponse_httpStatus :: Lens.Lens' StartEventsDetectionJobResponse Prelude.Int
startEventsDetectionJobResponse_httpStatus = Lens.lens (\StartEventsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartEventsDetectionJobResponse' {} a -> s {httpStatus = a} :: StartEventsDetectionJobResponse)

instance
  Prelude.NFData
    StartEventsDetectionJobResponse
  where
  rnf StartEventsDetectionJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf httpStatus
