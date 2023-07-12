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
-- Module      : Amazonka.DataBrew.DescribeJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the definition of a specific DataBrew job.
module Amazonka.DataBrew.DescribeJob
  ( -- * Creating a Request
    DescribeJob (..),
    newDescribeJob,

    -- * Request Lenses
    describeJob_name,

    -- * Destructuring the Response
    DescribeJobResponse (..),
    newDescribeJobResponse,

    -- * Response Lenses
    describeJobResponse_createDate,
    describeJobResponse_createdBy,
    describeJobResponse_dataCatalogOutputs,
    describeJobResponse_databaseOutputs,
    describeJobResponse_datasetName,
    describeJobResponse_encryptionKeyArn,
    describeJobResponse_encryptionMode,
    describeJobResponse_jobSample,
    describeJobResponse_lastModifiedBy,
    describeJobResponse_lastModifiedDate,
    describeJobResponse_logSubscription,
    describeJobResponse_maxCapacity,
    describeJobResponse_maxRetries,
    describeJobResponse_outputs,
    describeJobResponse_profileConfiguration,
    describeJobResponse_projectName,
    describeJobResponse_recipeReference,
    describeJobResponse_resourceArn,
    describeJobResponse_roleArn,
    describeJobResponse_tags,
    describeJobResponse_timeout,
    describeJobResponse_type,
    describeJobResponse_validationConfigurations,
    describeJobResponse_httpStatus,
    describeJobResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The name of the job to be described.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeJob_name' - The name of the job to be described.
newDescribeJob ::
  -- | 'name'
  Prelude.Text ->
  DescribeJob
newDescribeJob pName_ = DescribeJob' {name = pName_}

-- | The name of the job to be described.
describeJob_name :: Lens.Lens' DescribeJob Prelude.Text
describeJob_name = Lens.lens (\DescribeJob' {name} -> name) (\s@DescribeJob' {} a -> s {name = a} :: DescribeJob)

instance Core.AWSRequest DescribeJob where
  type AWSResponse DescribeJob = DescribeJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Prelude.<$> (x Data..?> "CreateDate")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "DataCatalogOutputs")
            Prelude.<*> (x Data..?> "DatabaseOutputs")
            Prelude.<*> (x Data..?> "DatasetName")
            Prelude.<*> (x Data..?> "EncryptionKeyArn")
            Prelude.<*> (x Data..?> "EncryptionMode")
            Prelude.<*> (x Data..?> "JobSample")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedDate")
            Prelude.<*> (x Data..?> "LogSubscription")
            Prelude.<*> (x Data..?> "MaxCapacity")
            Prelude.<*> (x Data..?> "MaxRetries")
            Prelude.<*> (x Data..?> "Outputs")
            Prelude.<*> (x Data..?> "ProfileConfiguration")
            Prelude.<*> (x Data..?> "ProjectName")
            Prelude.<*> (x Data..?> "RecipeReference")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Timeout")
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (x Data..?> "ValidationConfigurations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable DescribeJob where
  hashWithSalt _salt DescribeJob' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeJob where
  rnf DescribeJob' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeJob where
  toPath DescribeJob' {..} =
    Prelude.mconcat ["/jobs/", Data.toBS name]

instance Data.ToQuery DescribeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { -- | The date and time that the job was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier (user name) of the user associated with the creation of
    -- the job.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write into.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | The dataset that the job acts upon.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | Sample configuration for profile jobs only. Determines the number of
    -- rows on which the profile job will be executed.
    jobSample :: Prelude.Maybe JobSample,
    -- | The identifier (user name) of the user who last modified the job.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the job was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether Amazon CloudWatch logging is enabled for this job.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The maximum number of compute nodes that DataBrew can consume when the
    -- job processes data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    -- | One or more artifacts that represent the output from running the job.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | Configuration for profile jobs. Used to select columns, do evaluations,
    -- and override default parameters of evaluations. When configuration is
    -- null, the profile job will run with default settings.
    profileConfiguration :: Prelude.Maybe ProfileConfiguration,
    -- | The DataBrew project associated with this job.
    projectName :: Prelude.Maybe Prelude.Text,
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | The Amazon Resource Name (ARN) of the job.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Identity and Access Management (IAM) role to be assumed
    -- when DataBrew runs the job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Metadata tags associated with this job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The job type, which must be one of the following:
    --
    -- -   @PROFILE@ - The job analyzes the dataset to determine its size, data
    --     types, data distribution, and more.
    --
    -- -   @RECIPE@ - The job applies one or more transformations to a dataset.
    type' :: Prelude.Maybe JobType,
    -- | List of validation configurations that are applied to the profile job.
    validationConfigurations :: Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'describeJobResponse_createDate' - The date and time that the job was created.
--
-- 'createdBy', 'describeJobResponse_createdBy' - The identifier (user name) of the user associated with the creation of
-- the job.
--
-- 'dataCatalogOutputs', 'describeJobResponse_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'databaseOutputs', 'describeJobResponse_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
--
-- 'datasetName', 'describeJobResponse_datasetName' - The dataset that the job acts upon.
--
-- 'encryptionKeyArn', 'describeJobResponse_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
--
-- 'encryptionMode', 'describeJobResponse_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
--
-- 'jobSample', 'describeJobResponse_jobSample' - Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed.
--
-- 'lastModifiedBy', 'describeJobResponse_lastModifiedBy' - The identifier (user name) of the user who last modified the job.
--
-- 'lastModifiedDate', 'describeJobResponse_lastModifiedDate' - The date and time that the job was last modified.
--
-- 'logSubscription', 'describeJobResponse_logSubscription' - Indicates whether Amazon CloudWatch logging is enabled for this job.
--
-- 'maxCapacity', 'describeJobResponse_maxCapacity' - The maximum number of compute nodes that DataBrew can consume when the
-- job processes data.
--
-- 'maxRetries', 'describeJobResponse_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'outputs', 'describeJobResponse_outputs' - One or more artifacts that represent the output from running the job.
--
-- 'profileConfiguration', 'describeJobResponse_profileConfiguration' - Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
--
-- 'projectName', 'describeJobResponse_projectName' - The DataBrew project associated with this job.
--
-- 'recipeReference', 'describeJobResponse_recipeReference' - Undocumented member.
--
-- 'resourceArn', 'describeJobResponse_resourceArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'roleArn', 'describeJobResponse_roleArn' - The ARN of the Identity and Access Management (IAM) role to be assumed
-- when DataBrew runs the job.
--
-- 'tags', 'describeJobResponse_tags' - Metadata tags associated with this job.
--
-- 'timeout', 'describeJobResponse_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'type'', 'describeJobResponse_type' - The job type, which must be one of the following:
--
-- -   @PROFILE@ - The job analyzes the dataset to determine its size, data
--     types, data distribution, and more.
--
-- -   @RECIPE@ - The job applies one or more transformations to a dataset.
--
-- 'validationConfigurations', 'describeJobResponse_validationConfigurations' - List of validation configurations that are applied to the profile job.
--
-- 'httpStatus', 'describeJobResponse_httpStatus' - The response's http status code.
--
-- 'name', 'describeJobResponse_name' - The name of the job.
newDescribeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  DescribeJobResponse
newDescribeJobResponse pHttpStatus_ pName_ =
  DescribeJobResponse'
    { createDate = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      dataCatalogOutputs = Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      jobSample = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      outputs = Prelude.Nothing,
      profileConfiguration = Prelude.Nothing,
      projectName = Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      type' = Prelude.Nothing,
      validationConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The date and time that the job was created.
describeJobResponse_createDate :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.UTCTime)
describeJobResponse_createDate = Lens.lens (\DescribeJobResponse' {createDate} -> createDate) (\s@DescribeJobResponse' {} a -> s {createDate = a} :: DescribeJobResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier (user name) of the user associated with the creation of
-- the job.
describeJobResponse_createdBy :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_createdBy = Lens.lens (\DescribeJobResponse' {createdBy} -> createdBy) (\s@DescribeJobResponse' {} a -> s {createdBy = a} :: DescribeJobResponse)

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
describeJobResponse_dataCatalogOutputs :: Lens.Lens' DescribeJobResponse (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
describeJobResponse_dataCatalogOutputs = Lens.lens (\DescribeJobResponse' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@DescribeJobResponse' {} a -> s {dataCatalogOutputs = a} :: DescribeJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
describeJobResponse_databaseOutputs :: Lens.Lens' DescribeJobResponse (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
describeJobResponse_databaseOutputs = Lens.lens (\DescribeJobResponse' {databaseOutputs} -> databaseOutputs) (\s@DescribeJobResponse' {} a -> s {databaseOutputs = a} :: DescribeJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The dataset that the job acts upon.
describeJobResponse_datasetName :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_datasetName = Lens.lens (\DescribeJobResponse' {datasetName} -> datasetName) (\s@DescribeJobResponse' {} a -> s {datasetName = a} :: DescribeJobResponse)

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
describeJobResponse_encryptionKeyArn :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_encryptionKeyArn = Lens.lens (\DescribeJobResponse' {encryptionKeyArn} -> encryptionKeyArn) (\s@DescribeJobResponse' {} a -> s {encryptionKeyArn = a} :: DescribeJobResponse)

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
describeJobResponse_encryptionMode :: Lens.Lens' DescribeJobResponse (Prelude.Maybe EncryptionMode)
describeJobResponse_encryptionMode = Lens.lens (\DescribeJobResponse' {encryptionMode} -> encryptionMode) (\s@DescribeJobResponse' {} a -> s {encryptionMode = a} :: DescribeJobResponse)

-- | Sample configuration for profile jobs only. Determines the number of
-- rows on which the profile job will be executed.
describeJobResponse_jobSample :: Lens.Lens' DescribeJobResponse (Prelude.Maybe JobSample)
describeJobResponse_jobSample = Lens.lens (\DescribeJobResponse' {jobSample} -> jobSample) (\s@DescribeJobResponse' {} a -> s {jobSample = a} :: DescribeJobResponse)

-- | The identifier (user name) of the user who last modified the job.
describeJobResponse_lastModifiedBy :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_lastModifiedBy = Lens.lens (\DescribeJobResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeJobResponse' {} a -> s {lastModifiedBy = a} :: DescribeJobResponse)

-- | The date and time that the job was last modified.
describeJobResponse_lastModifiedDate :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.UTCTime)
describeJobResponse_lastModifiedDate = Lens.lens (\DescribeJobResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeJobResponse' {} a -> s {lastModifiedDate = a} :: DescribeJobResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates whether Amazon CloudWatch logging is enabled for this job.
describeJobResponse_logSubscription :: Lens.Lens' DescribeJobResponse (Prelude.Maybe LogSubscription)
describeJobResponse_logSubscription = Lens.lens (\DescribeJobResponse' {logSubscription} -> logSubscription) (\s@DescribeJobResponse' {} a -> s {logSubscription = a} :: DescribeJobResponse)

-- | The maximum number of compute nodes that DataBrew can consume when the
-- job processes data.
describeJobResponse_maxCapacity :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Int)
describeJobResponse_maxCapacity = Lens.lens (\DescribeJobResponse' {maxCapacity} -> maxCapacity) (\s@DescribeJobResponse' {} a -> s {maxCapacity = a} :: DescribeJobResponse)

-- | The maximum number of times to retry the job after a job run fails.
describeJobResponse_maxRetries :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Natural)
describeJobResponse_maxRetries = Lens.lens (\DescribeJobResponse' {maxRetries} -> maxRetries) (\s@DescribeJobResponse' {} a -> s {maxRetries = a} :: DescribeJobResponse)

-- | One or more artifacts that represent the output from running the job.
describeJobResponse_outputs :: Lens.Lens' DescribeJobResponse (Prelude.Maybe (Prelude.NonEmpty Output))
describeJobResponse_outputs = Lens.lens (\DescribeJobResponse' {outputs} -> outputs) (\s@DescribeJobResponse' {} a -> s {outputs = a} :: DescribeJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for profile jobs. Used to select columns, do evaluations,
-- and override default parameters of evaluations. When configuration is
-- null, the profile job will run with default settings.
describeJobResponse_profileConfiguration :: Lens.Lens' DescribeJobResponse (Prelude.Maybe ProfileConfiguration)
describeJobResponse_profileConfiguration = Lens.lens (\DescribeJobResponse' {profileConfiguration} -> profileConfiguration) (\s@DescribeJobResponse' {} a -> s {profileConfiguration = a} :: DescribeJobResponse)

-- | The DataBrew project associated with this job.
describeJobResponse_projectName :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_projectName = Lens.lens (\DescribeJobResponse' {projectName} -> projectName) (\s@DescribeJobResponse' {} a -> s {projectName = a} :: DescribeJobResponse)

-- | Undocumented member.
describeJobResponse_recipeReference :: Lens.Lens' DescribeJobResponse (Prelude.Maybe RecipeReference)
describeJobResponse_recipeReference = Lens.lens (\DescribeJobResponse' {recipeReference} -> recipeReference) (\s@DescribeJobResponse' {} a -> s {recipeReference = a} :: DescribeJobResponse)

-- | The Amazon Resource Name (ARN) of the job.
describeJobResponse_resourceArn :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_resourceArn = Lens.lens (\DescribeJobResponse' {resourceArn} -> resourceArn) (\s@DescribeJobResponse' {} a -> s {resourceArn = a} :: DescribeJobResponse)

-- | The ARN of the Identity and Access Management (IAM) role to be assumed
-- when DataBrew runs the job.
describeJobResponse_roleArn :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Text)
describeJobResponse_roleArn = Lens.lens (\DescribeJobResponse' {roleArn} -> roleArn) (\s@DescribeJobResponse' {} a -> s {roleArn = a} :: DescribeJobResponse)

-- | Metadata tags associated with this job.
describeJobResponse_tags :: Lens.Lens' DescribeJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeJobResponse_tags = Lens.lens (\DescribeJobResponse' {tags} -> tags) (\s@DescribeJobResponse' {} a -> s {tags = a} :: DescribeJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
describeJobResponse_timeout :: Lens.Lens' DescribeJobResponse (Prelude.Maybe Prelude.Natural)
describeJobResponse_timeout = Lens.lens (\DescribeJobResponse' {timeout} -> timeout) (\s@DescribeJobResponse' {} a -> s {timeout = a} :: DescribeJobResponse)

-- | The job type, which must be one of the following:
--
-- -   @PROFILE@ - The job analyzes the dataset to determine its size, data
--     types, data distribution, and more.
--
-- -   @RECIPE@ - The job applies one or more transformations to a dataset.
describeJobResponse_type :: Lens.Lens' DescribeJobResponse (Prelude.Maybe JobType)
describeJobResponse_type = Lens.lens (\DescribeJobResponse' {type'} -> type') (\s@DescribeJobResponse' {} a -> s {type' = a} :: DescribeJobResponse)

-- | List of validation configurations that are applied to the profile job.
describeJobResponse_validationConfigurations :: Lens.Lens' DescribeJobResponse (Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration))
describeJobResponse_validationConfigurations = Lens.lens (\DescribeJobResponse' {validationConfigurations} -> validationConfigurations) (\s@DescribeJobResponse' {} a -> s {validationConfigurations = a} :: DescribeJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeJobResponse_httpStatus :: Lens.Lens' DescribeJobResponse Prelude.Int
describeJobResponse_httpStatus = Lens.lens (\DescribeJobResponse' {httpStatus} -> httpStatus) (\s@DescribeJobResponse' {} a -> s {httpStatus = a} :: DescribeJobResponse)

-- | The name of the job.
describeJobResponse_name :: Lens.Lens' DescribeJobResponse Prelude.Text
describeJobResponse_name = Lens.lens (\DescribeJobResponse' {name} -> name) (\s@DescribeJobResponse' {} a -> s {name = a} :: DescribeJobResponse)

instance Prelude.NFData DescribeJobResponse where
  rnf DescribeJobResponse' {..} =
    Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf dataCatalogOutputs
      `Prelude.seq` Prelude.rnf databaseOutputs
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf jobSample
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf logSubscription
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf profileConfiguration
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf recipeReference
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf
        validationConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
