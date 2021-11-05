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
-- Module      : Amazonka.DataBrew.UpdateProfileJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the definition of an existing profile job.
module Amazonka.DataBrew.UpdateProfileJob
  ( -- * Creating a Request
    UpdateProfileJob (..),
    newUpdateProfileJob,

    -- * Request Lenses
    updateProfileJob_encryptionMode,
    updateProfileJob_logSubscription,
    updateProfileJob_maxRetries,
    updateProfileJob_encryptionKeyArn,
    updateProfileJob_maxCapacity,
    updateProfileJob_configuration,
    updateProfileJob_timeout,
    updateProfileJob_jobSample,
    updateProfileJob_name,
    updateProfileJob_outputLocation,
    updateProfileJob_roleArn,

    -- * Destructuring the Response
    UpdateProfileJobResponse (..),
    newUpdateProfileJobResponse,

    -- * Response Lenses
    updateProfileJobResponse_httpStatus,
    updateProfileJobResponse_name,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProfileJob' smart constructor.
data UpdateProfileJob = UpdateProfileJob'
  { -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | Enables or disables Amazon CloudWatch logging for the job. If logging is
    -- enabled, CloudWatch writes one log stream for each job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of compute nodes that DataBrew can use when the job
    -- processes data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | Configuration for profile jobs. Used to select columns, do evaluations,
    -- and override default parameters of evaluations. When configuration is
    -- null, the profile job will run with default settings.
    configuration :: Prelude.Maybe ProfileConfiguration,
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Sample configuration for Profile Jobs only. Determines the number of
    -- rows on which the Profile job will be executed. If a JobSample value is
    -- not provided for profile jobs, the default value will be used. The
    -- default value is CUSTOM_ROWS for the mode parameter and 20000 for the
    -- size parameter.
    jobSample :: Prelude.Maybe JobSample,
    -- | The name of the job to be updated.
    name :: Prelude.Text,
    outputLocation :: S3Location,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role to be assumed when DataBrew runs the job.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfileJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionMode', 'updateProfileJob_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
--
-- 'logSubscription', 'updateProfileJob_logSubscription' - Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
--
-- 'maxRetries', 'updateProfileJob_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'encryptionKeyArn', 'updateProfileJob_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
--
-- 'maxCapacity', 'updateProfileJob_maxCapacity' - The maximum number of compute nodes that DataBrew can use when the job
-- processes data.
--
-- 'configuration', 'updateProfileJob_configuration' - Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
--
-- 'timeout', 'updateProfileJob_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'jobSample', 'updateProfileJob_jobSample' - Sample configuration for Profile Jobs only. Determines the number of
-- rows on which the Profile job will be executed. If a JobSample value is
-- not provided for profile jobs, the default value will be used. The
-- default value is CUSTOM_ROWS for the mode parameter and 20000 for the
-- size parameter.
--
-- 'name', 'updateProfileJob_name' - The name of the job to be updated.
--
-- 'outputLocation', 'updateProfileJob_outputLocation' - Undocumented member.
--
-- 'roleArn', 'updateProfileJob_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
newUpdateProfileJob ::
  -- | 'name'
  Prelude.Text ->
  -- | 'outputLocation'
  S3Location ->
  -- | 'roleArn'
  Prelude.Text ->
  UpdateProfileJob
newUpdateProfileJob pName_ pOutputLocation_ pRoleArn_ =
  UpdateProfileJob'
    { encryptionMode = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      configuration = Prelude.Nothing,
      timeout = Prelude.Nothing,
      jobSample = Prelude.Nothing,
      name = pName_,
      outputLocation = pOutputLocation_,
      roleArn = pRoleArn_
    }

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
updateProfileJob_encryptionMode :: Lens.Lens' UpdateProfileJob (Prelude.Maybe EncryptionMode)
updateProfileJob_encryptionMode = Lens.lens (\UpdateProfileJob' {encryptionMode} -> encryptionMode) (\s@UpdateProfileJob' {} a -> s {encryptionMode = a} :: UpdateProfileJob)

-- | Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
updateProfileJob_logSubscription :: Lens.Lens' UpdateProfileJob (Prelude.Maybe LogSubscription)
updateProfileJob_logSubscription = Lens.lens (\UpdateProfileJob' {logSubscription} -> logSubscription) (\s@UpdateProfileJob' {} a -> s {logSubscription = a} :: UpdateProfileJob)

-- | The maximum number of times to retry the job after a job run fails.
updateProfileJob_maxRetries :: Lens.Lens' UpdateProfileJob (Prelude.Maybe Prelude.Natural)
updateProfileJob_maxRetries = Lens.lens (\UpdateProfileJob' {maxRetries} -> maxRetries) (\s@UpdateProfileJob' {} a -> s {maxRetries = a} :: UpdateProfileJob)

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
updateProfileJob_encryptionKeyArn :: Lens.Lens' UpdateProfileJob (Prelude.Maybe Prelude.Text)
updateProfileJob_encryptionKeyArn = Lens.lens (\UpdateProfileJob' {encryptionKeyArn} -> encryptionKeyArn) (\s@UpdateProfileJob' {} a -> s {encryptionKeyArn = a} :: UpdateProfileJob)

-- | The maximum number of compute nodes that DataBrew can use when the job
-- processes data.
updateProfileJob_maxCapacity :: Lens.Lens' UpdateProfileJob (Prelude.Maybe Prelude.Int)
updateProfileJob_maxCapacity = Lens.lens (\UpdateProfileJob' {maxCapacity} -> maxCapacity) (\s@UpdateProfileJob' {} a -> s {maxCapacity = a} :: UpdateProfileJob)

-- | Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
updateProfileJob_configuration :: Lens.Lens' UpdateProfileJob (Prelude.Maybe ProfileConfiguration)
updateProfileJob_configuration = Lens.lens (\UpdateProfileJob' {configuration} -> configuration) (\s@UpdateProfileJob' {} a -> s {configuration = a} :: UpdateProfileJob)

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
updateProfileJob_timeout :: Lens.Lens' UpdateProfileJob (Prelude.Maybe Prelude.Natural)
updateProfileJob_timeout = Lens.lens (\UpdateProfileJob' {timeout} -> timeout) (\s@UpdateProfileJob' {} a -> s {timeout = a} :: UpdateProfileJob)

-- | Sample configuration for Profile Jobs only. Determines the number of
-- rows on which the Profile job will be executed. If a JobSample value is
-- not provided for profile jobs, the default value will be used. The
-- default value is CUSTOM_ROWS for the mode parameter and 20000 for the
-- size parameter.
updateProfileJob_jobSample :: Lens.Lens' UpdateProfileJob (Prelude.Maybe JobSample)
updateProfileJob_jobSample = Lens.lens (\UpdateProfileJob' {jobSample} -> jobSample) (\s@UpdateProfileJob' {} a -> s {jobSample = a} :: UpdateProfileJob)

-- | The name of the job to be updated.
updateProfileJob_name :: Lens.Lens' UpdateProfileJob Prelude.Text
updateProfileJob_name = Lens.lens (\UpdateProfileJob' {name} -> name) (\s@UpdateProfileJob' {} a -> s {name = a} :: UpdateProfileJob)

-- | Undocumented member.
updateProfileJob_outputLocation :: Lens.Lens' UpdateProfileJob S3Location
updateProfileJob_outputLocation = Lens.lens (\UpdateProfileJob' {outputLocation} -> outputLocation) (\s@UpdateProfileJob' {} a -> s {outputLocation = a} :: UpdateProfileJob)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
updateProfileJob_roleArn :: Lens.Lens' UpdateProfileJob Prelude.Text
updateProfileJob_roleArn = Lens.lens (\UpdateProfileJob' {roleArn} -> roleArn) (\s@UpdateProfileJob' {} a -> s {roleArn = a} :: UpdateProfileJob)

instance Core.AWSRequest UpdateProfileJob where
  type
    AWSResponse UpdateProfileJob =
      UpdateProfileJobResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProfileJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Name")
      )

instance Prelude.Hashable UpdateProfileJob

instance Prelude.NFData UpdateProfileJob

instance Core.ToHeaders UpdateProfileJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProfileJob where
  toJSON UpdateProfileJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionMode" Core..=)
              Prelude.<$> encryptionMode,
            ("LogSubscription" Core..=)
              Prelude.<$> logSubscription,
            ("MaxRetries" Core..=) Prelude.<$> maxRetries,
            ("EncryptionKeyArn" Core..=)
              Prelude.<$> encryptionKeyArn,
            ("MaxCapacity" Core..=) Prelude.<$> maxCapacity,
            ("Configuration" Core..=) Prelude.<$> configuration,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("JobSample" Core..=) Prelude.<$> jobSample,
            Prelude.Just
              ("OutputLocation" Core..= outputLocation),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath UpdateProfileJob where
  toPath UpdateProfileJob' {..} =
    Prelude.mconcat ["/profileJobs/", Core.toBS name]

instance Core.ToQuery UpdateProfileJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProfileJobResponse' smart constructor.
data UpdateProfileJobResponse = UpdateProfileJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job that was updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfileJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProfileJobResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateProfileJobResponse_name' - The name of the job that was updated.
newUpdateProfileJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateProfileJobResponse
newUpdateProfileJobResponse pHttpStatus_ pName_ =
  UpdateProfileJobResponse'
    { httpStatus =
        pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateProfileJobResponse_httpStatus :: Lens.Lens' UpdateProfileJobResponse Prelude.Int
updateProfileJobResponse_httpStatus = Lens.lens (\UpdateProfileJobResponse' {httpStatus} -> httpStatus) (\s@UpdateProfileJobResponse' {} a -> s {httpStatus = a} :: UpdateProfileJobResponse)

-- | The name of the job that was updated.
updateProfileJobResponse_name :: Lens.Lens' UpdateProfileJobResponse Prelude.Text
updateProfileJobResponse_name = Lens.lens (\UpdateProfileJobResponse' {name} -> name) (\s@UpdateProfileJobResponse' {} a -> s {name = a} :: UpdateProfileJobResponse)

instance Prelude.NFData UpdateProfileJobResponse
