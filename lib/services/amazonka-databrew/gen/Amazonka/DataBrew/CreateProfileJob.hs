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
-- Module      : Amazonka.DataBrew.CreateProfileJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new job to analyze a dataset and create its data profile.
module Amazonka.DataBrew.CreateProfileJob
  ( -- * Creating a Request
    CreateProfileJob (..),
    newCreateProfileJob,

    -- * Request Lenses
    createProfileJob_configuration,
    createProfileJob_encryptionKeyArn,
    createProfileJob_encryptionMode,
    createProfileJob_jobSample,
    createProfileJob_logSubscription,
    createProfileJob_maxCapacity,
    createProfileJob_maxRetries,
    createProfileJob_tags,
    createProfileJob_timeout,
    createProfileJob_validationConfigurations,
    createProfileJob_datasetName,
    createProfileJob_name,
    createProfileJob_outputLocation,
    createProfileJob_roleArn,

    -- * Destructuring the Response
    CreateProfileJobResponse (..),
    newCreateProfileJobResponse,

    -- * Response Lenses
    createProfileJobResponse_httpStatus,
    createProfileJobResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProfileJob' smart constructor.
data CreateProfileJob = CreateProfileJob'
  { -- | Configuration for profile jobs. Used to select columns, do evaluations,
    -- and override default parameters of evaluations. When configuration is
    -- null, the profile job will run with default settings.
    configuration :: Prelude.Maybe ProfileConfiguration,
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - @SSE-KMS@ - Server-side encryption with KMS-managed
    --     keys.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | Sample configuration for profile jobs only. Determines the number of
    -- rows on which the profile job will be executed. If a JobSample value is
    -- not provided, the default value will be used. The default value is
    -- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
    jobSample :: Prelude.Maybe JobSample,
    -- | Enables or disables Amazon CloudWatch logging for the job. If logging is
    -- enabled, CloudWatch writes one log stream for each job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The maximum number of nodes that DataBrew can use when the job processes
    -- data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    -- | Metadata tags to apply to this job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | List of validation configurations that are applied to the profile job.
    validationConfigurations :: Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration),
    -- | The name of the dataset that this job is to act upon.
    datasetName :: Prelude.Text,
    -- | The name of the job to be created. Valid characters are alphanumeric
    -- (A-Z, a-z, 0-9), hyphen (-), period (.), and space.
    name :: Prelude.Text,
    outputLocation :: S3Location,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role to be assumed when DataBrew runs the job.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfileJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'createProfileJob_configuration' - Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
--
-- 'encryptionKeyArn', 'createProfileJob_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
--
-- 'encryptionMode', 'createProfileJob_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - @SSE-KMS@ - Server-side encryption with KMS-managed
--     keys.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
--
-- 'jobSample', 'createProfileJob_jobSample' - Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed. If a JobSample value is
-- not provided, the default value will be used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
--
-- 'logSubscription', 'createProfileJob_logSubscription' - Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
--
-- 'maxCapacity', 'createProfileJob_maxCapacity' - The maximum number of nodes that DataBrew can use when the job processes
-- data.
--
-- 'maxRetries', 'createProfileJob_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'tags', 'createProfileJob_tags' - Metadata tags to apply to this job.
--
-- 'timeout', 'createProfileJob_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'validationConfigurations', 'createProfileJob_validationConfigurations' - List of validation configurations that are applied to the profile job.
--
-- 'datasetName', 'createProfileJob_datasetName' - The name of the dataset that this job is to act upon.
--
-- 'name', 'createProfileJob_name' - The name of the job to be created. Valid characters are alphanumeric
-- (A-Z, a-z, 0-9), hyphen (-), period (.), and space.
--
-- 'outputLocation', 'createProfileJob_outputLocation' - Undocumented member.
--
-- 'roleArn', 'createProfileJob_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
newCreateProfileJob ::
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'outputLocation'
  S3Location ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateProfileJob
newCreateProfileJob
  pDatasetName_
  pName_
  pOutputLocation_
  pRoleArn_ =
    CreateProfileJob'
      { configuration = Prelude.Nothing,
        encryptionKeyArn = Prelude.Nothing,
        encryptionMode = Prelude.Nothing,
        jobSample = Prelude.Nothing,
        logSubscription = Prelude.Nothing,
        maxCapacity = Prelude.Nothing,
        maxRetries = Prelude.Nothing,
        tags = Prelude.Nothing,
        timeout = Prelude.Nothing,
        validationConfigurations = Prelude.Nothing,
        datasetName = pDatasetName_,
        name = pName_,
        outputLocation = pOutputLocation_,
        roleArn = pRoleArn_
      }

-- | Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
createProfileJob_configuration :: Lens.Lens' CreateProfileJob (Prelude.Maybe ProfileConfiguration)
createProfileJob_configuration = Lens.lens (\CreateProfileJob' {configuration} -> configuration) (\s@CreateProfileJob' {} a -> s {configuration = a} :: CreateProfileJob)

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
createProfileJob_encryptionKeyArn :: Lens.Lens' CreateProfileJob (Prelude.Maybe Prelude.Text)
createProfileJob_encryptionKeyArn = Lens.lens (\CreateProfileJob' {encryptionKeyArn} -> encryptionKeyArn) (\s@CreateProfileJob' {} a -> s {encryptionKeyArn = a} :: CreateProfileJob)

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - @SSE-KMS@ - Server-side encryption with KMS-managed
--     keys.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
createProfileJob_encryptionMode :: Lens.Lens' CreateProfileJob (Prelude.Maybe EncryptionMode)
createProfileJob_encryptionMode = Lens.lens (\CreateProfileJob' {encryptionMode} -> encryptionMode) (\s@CreateProfileJob' {} a -> s {encryptionMode = a} :: CreateProfileJob)

-- | Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed. If a JobSample value is
-- not provided, the default value will be used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20000 for the size parameter.
createProfileJob_jobSample :: Lens.Lens' CreateProfileJob (Prelude.Maybe JobSample)
createProfileJob_jobSample = Lens.lens (\CreateProfileJob' {jobSample} -> jobSample) (\s@CreateProfileJob' {} a -> s {jobSample = a} :: CreateProfileJob)

-- | Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
createProfileJob_logSubscription :: Lens.Lens' CreateProfileJob (Prelude.Maybe LogSubscription)
createProfileJob_logSubscription = Lens.lens (\CreateProfileJob' {logSubscription} -> logSubscription) (\s@CreateProfileJob' {} a -> s {logSubscription = a} :: CreateProfileJob)

-- | The maximum number of nodes that DataBrew can use when the job processes
-- data.
createProfileJob_maxCapacity :: Lens.Lens' CreateProfileJob (Prelude.Maybe Prelude.Int)
createProfileJob_maxCapacity = Lens.lens (\CreateProfileJob' {maxCapacity} -> maxCapacity) (\s@CreateProfileJob' {} a -> s {maxCapacity = a} :: CreateProfileJob)

-- | The maximum number of times to retry the job after a job run fails.
createProfileJob_maxRetries :: Lens.Lens' CreateProfileJob (Prelude.Maybe Prelude.Natural)
createProfileJob_maxRetries = Lens.lens (\CreateProfileJob' {maxRetries} -> maxRetries) (\s@CreateProfileJob' {} a -> s {maxRetries = a} :: CreateProfileJob)

-- | Metadata tags to apply to this job.
createProfileJob_tags :: Lens.Lens' CreateProfileJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProfileJob_tags = Lens.lens (\CreateProfileJob' {tags} -> tags) (\s@CreateProfileJob' {} a -> s {tags = a} :: CreateProfileJob) Prelude.. Lens.mapping Lens.coerced

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
createProfileJob_timeout :: Lens.Lens' CreateProfileJob (Prelude.Maybe Prelude.Natural)
createProfileJob_timeout = Lens.lens (\CreateProfileJob' {timeout} -> timeout) (\s@CreateProfileJob' {} a -> s {timeout = a} :: CreateProfileJob)

-- | List of validation configurations that are applied to the profile job.
createProfileJob_validationConfigurations :: Lens.Lens' CreateProfileJob (Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration))
createProfileJob_validationConfigurations = Lens.lens (\CreateProfileJob' {validationConfigurations} -> validationConfigurations) (\s@CreateProfileJob' {} a -> s {validationConfigurations = a} :: CreateProfileJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset that this job is to act upon.
createProfileJob_datasetName :: Lens.Lens' CreateProfileJob Prelude.Text
createProfileJob_datasetName = Lens.lens (\CreateProfileJob' {datasetName} -> datasetName) (\s@CreateProfileJob' {} a -> s {datasetName = a} :: CreateProfileJob)

-- | The name of the job to be created. Valid characters are alphanumeric
-- (A-Z, a-z, 0-9), hyphen (-), period (.), and space.
createProfileJob_name :: Lens.Lens' CreateProfileJob Prelude.Text
createProfileJob_name = Lens.lens (\CreateProfileJob' {name} -> name) (\s@CreateProfileJob' {} a -> s {name = a} :: CreateProfileJob)

-- | Undocumented member.
createProfileJob_outputLocation :: Lens.Lens' CreateProfileJob S3Location
createProfileJob_outputLocation = Lens.lens (\CreateProfileJob' {outputLocation} -> outputLocation) (\s@CreateProfileJob' {} a -> s {outputLocation = a} :: CreateProfileJob)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
createProfileJob_roleArn :: Lens.Lens' CreateProfileJob Prelude.Text
createProfileJob_roleArn = Lens.lens (\CreateProfileJob' {roleArn} -> roleArn) (\s@CreateProfileJob' {} a -> s {roleArn = a} :: CreateProfileJob)

instance Core.AWSRequest CreateProfileJob where
  type
    AWSResponse CreateProfileJob =
      CreateProfileJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable CreateProfileJob where
  hashWithSalt _salt CreateProfileJob' {..} =
    _salt `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` jobSample
      `Prelude.hashWithSalt` logSubscription
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` validationConfigurations
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateProfileJob where
  rnf CreateProfileJob' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf jobSample
      `Prelude.seq` Prelude.rnf logSubscription
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf validationConfigurations
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateProfileJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProfileJob where
  toJSON CreateProfileJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Configuration" Data..=) Prelude.<$> configuration,
            ("EncryptionKeyArn" Data..=)
              Prelude.<$> encryptionKeyArn,
            ("EncryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            ("JobSample" Data..=) Prelude.<$> jobSample,
            ("LogSubscription" Data..=)
              Prelude.<$> logSubscription,
            ("MaxCapacity" Data..=) Prelude.<$> maxCapacity,
            ("MaxRetries" Data..=) Prelude.<$> maxRetries,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("ValidationConfigurations" Data..=)
              Prelude.<$> validationConfigurations,
            Prelude.Just ("DatasetName" Data..= datasetName),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("OutputLocation" Data..= outputLocation),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateProfileJob where
  toPath = Prelude.const "/profileJobs"

instance Data.ToQuery CreateProfileJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProfileJobResponse' smart constructor.
data CreateProfileJobResponse = CreateProfileJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job that was created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfileJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createProfileJobResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createProfileJobResponse_name' - The name of the job that was created.
newCreateProfileJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  CreateProfileJobResponse
newCreateProfileJobResponse pHttpStatus_ pName_ =
  CreateProfileJobResponse'
    { httpStatus =
        pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
createProfileJobResponse_httpStatus :: Lens.Lens' CreateProfileJobResponse Prelude.Int
createProfileJobResponse_httpStatus = Lens.lens (\CreateProfileJobResponse' {httpStatus} -> httpStatus) (\s@CreateProfileJobResponse' {} a -> s {httpStatus = a} :: CreateProfileJobResponse)

-- | The name of the job that was created.
createProfileJobResponse_name :: Lens.Lens' CreateProfileJobResponse Prelude.Text
createProfileJobResponse_name = Lens.lens (\CreateProfileJobResponse' {name} -> name) (\s@CreateProfileJobResponse' {} a -> s {name = a} :: CreateProfileJobResponse)

instance Prelude.NFData CreateProfileJobResponse where
  rnf CreateProfileJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
