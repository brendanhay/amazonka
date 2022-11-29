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
-- Module      : Amazonka.Comprehend.StartPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous PII entity detection job for a collection of
-- documents.
module Amazonka.Comprehend.StartPiiEntitiesDetectionJob
  ( -- * Creating a Request
    StartPiiEntitiesDetectionJob (..),
    newStartPiiEntitiesDetectionJob,

    -- * Request Lenses
    startPiiEntitiesDetectionJob_tags,
    startPiiEntitiesDetectionJob_clientRequestToken,
    startPiiEntitiesDetectionJob_jobName,
    startPiiEntitiesDetectionJob_redactionConfig,
    startPiiEntitiesDetectionJob_inputDataConfig,
    startPiiEntitiesDetectionJob_outputDataConfig,
    startPiiEntitiesDetectionJob_mode,
    startPiiEntitiesDetectionJob_dataAccessRoleArn,
    startPiiEntitiesDetectionJob_languageCode,

    -- * Destructuring the Response
    StartPiiEntitiesDetectionJobResponse (..),
    newStartPiiEntitiesDetectionJobResponse,

    -- * Response Lenses
    startPiiEntitiesDetectionJobResponse_jobStatus,
    startPiiEntitiesDetectionJobResponse_jobId,
    startPiiEntitiesDetectionJobResponse_jobArn,
    startPiiEntitiesDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartPiiEntitiesDetectionJob' smart constructor.
data StartPiiEntitiesDetectionJob = StartPiiEntitiesDetectionJob'
  { -- | Tags to be associated with the PII entities detection job. A tag is a
    -- key-value pair that adds metadata to a resource used by Amazon
    -- Comprehend. For example, a tag with \"Sales\" as the key might be added
    -- to a resource to indicate its use by the sales department.
    tags :: Prelude.Maybe [Tag],
    -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | Provides configuration parameters for PII entity redaction.
    --
    -- This parameter is required if you set the @Mode@ parameter to
    -- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
    -- definition that includes the @PiiEntityTypes@ parameter.
    redactionConfig :: Prelude.Maybe RedactionConfig,
    -- | The input properties for a PII entities detection job.
    inputDataConfig :: InputDataConfig,
    -- | Provides conﬁguration parameters for the output of PII entity detection
    -- jobs.
    outputDataConfig :: OutputDataConfig,
    -- | Specifies whether the output provides the locations (offsets) of PII
    -- entities or a file in which PII entities are redacted.
    mode :: PiiEntitiesDetectionMode,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Text,
    -- | The language of the input documents. Currently, English is the only
    -- valid language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPiiEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startPiiEntitiesDetectionJob_tags' - Tags to be associated with the PII entities detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
--
-- 'clientRequestToken', 'startPiiEntitiesDetectionJob_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'jobName', 'startPiiEntitiesDetectionJob_jobName' - The identifier of the job.
--
-- 'redactionConfig', 'startPiiEntitiesDetectionJob_redactionConfig' - Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
--
-- 'inputDataConfig', 'startPiiEntitiesDetectionJob_inputDataConfig' - The input properties for a PII entities detection job.
--
-- 'outputDataConfig', 'startPiiEntitiesDetectionJob_outputDataConfig' - Provides conﬁguration parameters for the output of PII entity detection
-- jobs.
--
-- 'mode', 'startPiiEntitiesDetectionJob_mode' - Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
--
-- 'dataAccessRoleArn', 'startPiiEntitiesDetectionJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- 'languageCode', 'startPiiEntitiesDetectionJob_languageCode' - The language of the input documents. Currently, English is the only
-- valid language.
newStartPiiEntitiesDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'mode'
  PiiEntitiesDetectionMode ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartPiiEntitiesDetectionJob
newStartPiiEntitiesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pMode_
  pDataAccessRoleArn_
  pLanguageCode_ =
    StartPiiEntitiesDetectionJob'
      { tags =
          Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        jobName = Prelude.Nothing,
        redactionConfig = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        mode = pMode_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        languageCode = pLanguageCode_
      }

-- | Tags to be associated with the PII entities detection job. A tag is a
-- key-value pair that adds metadata to a resource used by Amazon
-- Comprehend. For example, a tag with \"Sales\" as the key might be added
-- to a resource to indicate its use by the sales department.
startPiiEntitiesDetectionJob_tags :: Lens.Lens' StartPiiEntitiesDetectionJob (Prelude.Maybe [Tag])
startPiiEntitiesDetectionJob_tags = Lens.lens (\StartPiiEntitiesDetectionJob' {tags} -> tags) (\s@StartPiiEntitiesDetectionJob' {} a -> s {tags = a} :: StartPiiEntitiesDetectionJob) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startPiiEntitiesDetectionJob_clientRequestToken :: Lens.Lens' StartPiiEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startPiiEntitiesDetectionJob_clientRequestToken = Lens.lens (\StartPiiEntitiesDetectionJob' {clientRequestToken} -> clientRequestToken) (\s@StartPiiEntitiesDetectionJob' {} a -> s {clientRequestToken = a} :: StartPiiEntitiesDetectionJob)

-- | The identifier of the job.
startPiiEntitiesDetectionJob_jobName :: Lens.Lens' StartPiiEntitiesDetectionJob (Prelude.Maybe Prelude.Text)
startPiiEntitiesDetectionJob_jobName = Lens.lens (\StartPiiEntitiesDetectionJob' {jobName} -> jobName) (\s@StartPiiEntitiesDetectionJob' {} a -> s {jobName = a} :: StartPiiEntitiesDetectionJob)

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to
-- @ONLY_REDACTION@. In that case, you must provide a @RedactionConfig@
-- definition that includes the @PiiEntityTypes@ parameter.
startPiiEntitiesDetectionJob_redactionConfig :: Lens.Lens' StartPiiEntitiesDetectionJob (Prelude.Maybe RedactionConfig)
startPiiEntitiesDetectionJob_redactionConfig = Lens.lens (\StartPiiEntitiesDetectionJob' {redactionConfig} -> redactionConfig) (\s@StartPiiEntitiesDetectionJob' {} a -> s {redactionConfig = a} :: StartPiiEntitiesDetectionJob)

-- | The input properties for a PII entities detection job.
startPiiEntitiesDetectionJob_inputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob InputDataConfig
startPiiEntitiesDetectionJob_inputDataConfig = Lens.lens (\StartPiiEntitiesDetectionJob' {inputDataConfig} -> inputDataConfig) (\s@StartPiiEntitiesDetectionJob' {} a -> s {inputDataConfig = a} :: StartPiiEntitiesDetectionJob)

-- | Provides conﬁguration parameters for the output of PII entity detection
-- jobs.
startPiiEntitiesDetectionJob_outputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob OutputDataConfig
startPiiEntitiesDetectionJob_outputDataConfig = Lens.lens (\StartPiiEntitiesDetectionJob' {outputDataConfig} -> outputDataConfig) (\s@StartPiiEntitiesDetectionJob' {} a -> s {outputDataConfig = a} :: StartPiiEntitiesDetectionJob)

-- | Specifies whether the output provides the locations (offsets) of PII
-- entities or a file in which PII entities are redacted.
startPiiEntitiesDetectionJob_mode :: Lens.Lens' StartPiiEntitiesDetectionJob PiiEntitiesDetectionMode
startPiiEntitiesDetectionJob_mode = Lens.lens (\StartPiiEntitiesDetectionJob' {mode} -> mode) (\s@StartPiiEntitiesDetectionJob' {} a -> s {mode = a} :: StartPiiEntitiesDetectionJob)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that grants Amazon Comprehend read access to your input data.
startPiiEntitiesDetectionJob_dataAccessRoleArn :: Lens.Lens' StartPiiEntitiesDetectionJob Prelude.Text
startPiiEntitiesDetectionJob_dataAccessRoleArn = Lens.lens (\StartPiiEntitiesDetectionJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartPiiEntitiesDetectionJob' {} a -> s {dataAccessRoleArn = a} :: StartPiiEntitiesDetectionJob)

-- | The language of the input documents. Currently, English is the only
-- valid language.
startPiiEntitiesDetectionJob_languageCode :: Lens.Lens' StartPiiEntitiesDetectionJob LanguageCode
startPiiEntitiesDetectionJob_languageCode = Lens.lens (\StartPiiEntitiesDetectionJob' {languageCode} -> languageCode) (\s@StartPiiEntitiesDetectionJob' {} a -> s {languageCode = a} :: StartPiiEntitiesDetectionJob)

instance Core.AWSRequest StartPiiEntitiesDetectionJob where
  type
    AWSResponse StartPiiEntitiesDetectionJob =
      StartPiiEntitiesDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPiiEntitiesDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (x Core..?> "JobArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartPiiEntitiesDetectionJob
  where
  hashWithSalt _salt StartPiiEntitiesDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` redactionConfig
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData StartPiiEntitiesDetectionJob where
  rnf StartPiiEntitiesDetectionJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf redactionConfig
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToHeaders StartPiiEntitiesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StartPiiEntitiesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartPiiEntitiesDetectionJob where
  toJSON StartPiiEntitiesDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("JobName" Core..=) Prelude.<$> jobName,
            ("RedactionConfig" Core..=)
              Prelude.<$> redactionConfig,
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just ("Mode" Core..= mode),
            Prelude.Just
              ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath StartPiiEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StartPiiEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPiiEntitiesDetectionJobResponse' smart constructor.
data StartPiiEntitiesDetectionJobResponse = StartPiiEntitiesDetectionJobResponse'
  { -- | The status of the job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier generated for the job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the PII entity detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:pii-entities-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:pii-entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPiiEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startPiiEntitiesDetectionJobResponse_jobStatus' - The status of the job.
--
-- 'jobId', 'startPiiEntitiesDetectionJobResponse_jobId' - The identifier generated for the job.
--
-- 'jobArn', 'startPiiEntitiesDetectionJobResponse_jobArn' - The Amazon Resource Name (ARN) of the PII entity detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:pii-entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:pii-entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'httpStatus', 'startPiiEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newStartPiiEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPiiEntitiesDetectionJobResponse
newStartPiiEntitiesDetectionJobResponse pHttpStatus_ =
  StartPiiEntitiesDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the job.
startPiiEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Prelude.Maybe JobStatus)
startPiiEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StartPiiEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StartPiiEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StartPiiEntitiesDetectionJobResponse)

-- | The identifier generated for the job.
startPiiEntitiesDetectionJobResponse_jobId :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startPiiEntitiesDetectionJobResponse_jobId = Lens.lens (\StartPiiEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StartPiiEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StartPiiEntitiesDetectionJobResponse)

-- | The Amazon Resource Name (ARN) of the PII entity detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:pii-entities-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:pii-entities-detection-job\/1234abcd12ab34cd56ef1234567890ab@
startPiiEntitiesDetectionJobResponse_jobArn :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
startPiiEntitiesDetectionJobResponse_jobArn = Lens.lens (\StartPiiEntitiesDetectionJobResponse' {jobArn} -> jobArn) (\s@StartPiiEntitiesDetectionJobResponse' {} a -> s {jobArn = a} :: StartPiiEntitiesDetectionJobResponse)

-- | The response's http status code.
startPiiEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse Prelude.Int
startPiiEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StartPiiEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StartPiiEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StartPiiEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    StartPiiEntitiesDetectionJobResponse
  where
  rnf StartPiiEntitiesDetectionJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf httpStatus
