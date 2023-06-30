{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataBrew.Types.Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.DataCatalogOutput
import Amazonka.DataBrew.Types.DatabaseOutput
import Amazonka.DataBrew.Types.EncryptionMode
import Amazonka.DataBrew.Types.JobSample
import Amazonka.DataBrew.Types.JobType
import Amazonka.DataBrew.Types.LogSubscription
import Amazonka.DataBrew.Types.Output
import Amazonka.DataBrew.Types.RecipeReference
import Amazonka.DataBrew.Types.ValidationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Represents all of the attributes of a DataBrew job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The ID of the Amazon Web Services account that owns the job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the job was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the user who created the job.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write into.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | A dataset that the job is to process.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job output. For more information, see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/encryption-security-configuration.html Encrypting data written by DataBrew jobs>
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | A sample configuration for profile jobs only, which determines the
    -- number of rows on which the profile job is run. If a @JobSample@ value
    -- isn\'t provided, the default value is used. The default value is
    -- CUSTOM_ROWS for the mode parameter and 20,000 for the size parameter.
    jobSample :: Prelude.Maybe JobSample,
    -- | The Amazon Resource Name (ARN) of the user who last modified the job.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The modification date and time of the job.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The current status of Amazon CloudWatch logging for the job.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The maximum number of nodes that can be consumed when the job processes
    -- data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    -- | One or more artifacts that represent output from running the job.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | The name of the project that the job is associated with.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | A set of steps that the job runs.
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | The unique Amazon Resource Name (ARN) for the job.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role to be assumed for this job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Metadata tags that have been applied to the job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The job type of the job, which must be one of the following:
    --
    -- -   @PROFILE@ - A job to analyze a dataset, to determine its size, data
    --     types, data distribution, and more.
    --
    -- -   @RECIPE@ - A job to apply one or more transformations to a dataset.
    type' :: Prelude.Maybe JobType,
    -- | List of validation configurations that are applied to the profile job.
    validationConfigurations :: Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration),
    -- | The unique name of the job.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'job_accountId' - The ID of the Amazon Web Services account that owns the job.
--
-- 'createDate', 'job_createDate' - The date and time that the job was created.
--
-- 'createdBy', 'job_createdBy' - The Amazon Resource Name (ARN) of the user who created the job.
--
-- 'dataCatalogOutputs', 'job_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'databaseOutputs', 'job_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
--
-- 'datasetName', 'job_datasetName' - A dataset that the job is to process.
--
-- 'encryptionKeyArn', 'job_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job output. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/encryption-security-configuration.html Encrypting data written by DataBrew jobs>
--
-- 'encryptionMode', 'job_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
--
-- 'jobSample', 'job_jobSample' - A sample configuration for profile jobs only, which determines the
-- number of rows on which the profile job is run. If a @JobSample@ value
-- isn\'t provided, the default value is used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20,000 for the size parameter.
--
-- 'lastModifiedBy', 'job_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last modified the job.
--
-- 'lastModifiedDate', 'job_lastModifiedDate' - The modification date and time of the job.
--
-- 'logSubscription', 'job_logSubscription' - The current status of Amazon CloudWatch logging for the job.
--
-- 'maxCapacity', 'job_maxCapacity' - The maximum number of nodes that can be consumed when the job processes
-- data.
--
-- 'maxRetries', 'job_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'outputs', 'job_outputs' - One or more artifacts that represent output from running the job.
--
-- 'projectName', 'job_projectName' - The name of the project that the job is associated with.
--
-- 'recipeReference', 'job_recipeReference' - A set of steps that the job runs.
--
-- 'resourceArn', 'job_resourceArn' - The unique Amazon Resource Name (ARN) for the job.
--
-- 'roleArn', 'job_roleArn' - The Amazon Resource Name (ARN) of the role to be assumed for this job.
--
-- 'tags', 'job_tags' - Metadata tags that have been applied to the job.
--
-- 'timeout', 'job_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'type'', 'job_type' - The job type of the job, which must be one of the following:
--
-- -   @PROFILE@ - A job to analyze a dataset, to determine its size, data
--     types, data distribution, and more.
--
-- -   @RECIPE@ - A job to apply one or more transformations to a dataset.
--
-- 'validationConfigurations', 'job_validationConfigurations' - List of validation configurations that are applied to the profile job.
--
-- 'name', 'job_name' - The unique name of the job.
newJob ::
  -- | 'name'
  Prelude.Text ->
  Job
newJob pName_ =
  Job'
    { accountId = Prelude.Nothing,
      createDate = Prelude.Nothing,
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
      projectName = Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      type' = Prelude.Nothing,
      validationConfigurations = Prelude.Nothing,
      name = pName_
    }

-- | The ID of the Amazon Web Services account that owns the job.
job_accountId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_accountId = Lens.lens (\Job' {accountId} -> accountId) (\s@Job' {} a -> s {accountId = a} :: Job)

-- | The date and time that the job was created.
job_createDate :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createDate = Lens.lens (\Job' {createDate} -> createDate) (\s@Job' {} a -> s {createDate = a} :: Job) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the user who created the job.
job_createdBy :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_createdBy = Lens.lens (\Job' {createdBy} -> createdBy) (\s@Job' {} a -> s {createdBy = a} :: Job)

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
job_dataCatalogOutputs :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
job_dataCatalogOutputs = Lens.lens (\Job' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@Job' {} a -> s {dataCatalogOutputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
job_databaseOutputs :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
job_databaseOutputs = Lens.lens (\Job' {databaseOutputs} -> databaseOutputs) (\s@Job' {} a -> s {databaseOutputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | A dataset that the job is to process.
job_datasetName :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_datasetName = Lens.lens (\Job' {datasetName} -> datasetName) (\s@Job' {} a -> s {datasetName = a} :: Job)

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job output. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/encryption-security-configuration.html Encrypting data written by DataBrew jobs>
job_encryptionKeyArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_encryptionKeyArn = Lens.lens (\Job' {encryptionKeyArn} -> encryptionKeyArn) (\s@Job' {} a -> s {encryptionKeyArn = a} :: Job)

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
job_encryptionMode :: Lens.Lens' Job (Prelude.Maybe EncryptionMode)
job_encryptionMode = Lens.lens (\Job' {encryptionMode} -> encryptionMode) (\s@Job' {} a -> s {encryptionMode = a} :: Job)

-- | A sample configuration for profile jobs only, which determines the
-- number of rows on which the profile job is run. If a @JobSample@ value
-- isn\'t provided, the default value is used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20,000 for the size parameter.
job_jobSample :: Lens.Lens' Job (Prelude.Maybe JobSample)
job_jobSample = Lens.lens (\Job' {jobSample} -> jobSample) (\s@Job' {} a -> s {jobSample = a} :: Job)

-- | The Amazon Resource Name (ARN) of the user who last modified the job.
job_lastModifiedBy :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_lastModifiedBy = Lens.lens (\Job' {lastModifiedBy} -> lastModifiedBy) (\s@Job' {} a -> s {lastModifiedBy = a} :: Job)

-- | The modification date and time of the job.
job_lastModifiedDate :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastModifiedDate = Lens.lens (\Job' {lastModifiedDate} -> lastModifiedDate) (\s@Job' {} a -> s {lastModifiedDate = a} :: Job) Prelude.. Lens.mapping Data._Time

-- | The current status of Amazon CloudWatch logging for the job.
job_logSubscription :: Lens.Lens' Job (Prelude.Maybe LogSubscription)
job_logSubscription = Lens.lens (\Job' {logSubscription} -> logSubscription) (\s@Job' {} a -> s {logSubscription = a} :: Job)

-- | The maximum number of nodes that can be consumed when the job processes
-- data.
job_maxCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_maxCapacity = Lens.lens (\Job' {maxCapacity} -> maxCapacity) (\s@Job' {} a -> s {maxCapacity = a} :: Job)

-- | The maximum number of times to retry the job after a job run fails.
job_maxRetries :: Lens.Lens' Job (Prelude.Maybe Prelude.Natural)
job_maxRetries = Lens.lens (\Job' {maxRetries} -> maxRetries) (\s@Job' {} a -> s {maxRetries = a} :: Job)

-- | One or more artifacts that represent output from running the job.
job_outputs :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty Output))
job_outputs = Lens.lens (\Job' {outputs} -> outputs) (\s@Job' {} a -> s {outputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The name of the project that the job is associated with.
job_projectName :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_projectName = Lens.lens (\Job' {projectName} -> projectName) (\s@Job' {} a -> s {projectName = a} :: Job)

-- | A set of steps that the job runs.
job_recipeReference :: Lens.Lens' Job (Prelude.Maybe RecipeReference)
job_recipeReference = Lens.lens (\Job' {recipeReference} -> recipeReference) (\s@Job' {} a -> s {recipeReference = a} :: Job)

-- | The unique Amazon Resource Name (ARN) for the job.
job_resourceArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_resourceArn = Lens.lens (\Job' {resourceArn} -> resourceArn) (\s@Job' {} a -> s {resourceArn = a} :: Job)

-- | The Amazon Resource Name (ARN) of the role to be assumed for this job.
job_roleArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_roleArn = Lens.lens (\Job' {roleArn} -> roleArn) (\s@Job' {} a -> s {roleArn = a} :: Job)

-- | Metadata tags that have been applied to the job.
job_tags :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_tags = Lens.lens (\Job' {tags} -> tags) (\s@Job' {} a -> s {tags = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
job_timeout :: Lens.Lens' Job (Prelude.Maybe Prelude.Natural)
job_timeout = Lens.lens (\Job' {timeout} -> timeout) (\s@Job' {} a -> s {timeout = a} :: Job)

-- | The job type of the job, which must be one of the following:
--
-- -   @PROFILE@ - A job to analyze a dataset, to determine its size, data
--     types, data distribution, and more.
--
-- -   @RECIPE@ - A job to apply one or more transformations to a dataset.
job_type :: Lens.Lens' Job (Prelude.Maybe JobType)
job_type = Lens.lens (\Job' {type'} -> type') (\s@Job' {} a -> s {type' = a} :: Job)

-- | List of validation configurations that are applied to the profile job.
job_validationConfigurations :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty ValidationConfiguration))
job_validationConfigurations = Lens.lens (\Job' {validationConfigurations} -> validationConfigurations) (\s@Job' {} a -> s {validationConfigurations = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the job.
job_name :: Lens.Lens' Job Prelude.Text
job_name = Lens.lens (\Job' {name} -> name) (\s@Job' {} a -> s {name = a} :: Job)

instance Data.FromJSON Job where
  parseJSON =
    Data.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "DataCatalogOutputs")
            Prelude.<*> (x Data..:? "DatabaseOutputs")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "EncryptionKeyArn")
            Prelude.<*> (x Data..:? "EncryptionMode")
            Prelude.<*> (x Data..:? "JobSample")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "LogSubscription")
            Prelude.<*> (x Data..:? "MaxCapacity")
            Prelude.<*> (x Data..:? "MaxRetries")
            Prelude.<*> (x Data..:? "Outputs")
            Prelude.<*> (x Data..:? "ProjectName")
            Prelude.<*> (x Data..:? "RecipeReference")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Timeout")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "ValidationConfigurations")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` dataCatalogOutputs
      `Prelude.hashWithSalt` databaseOutputs
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` jobSample
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` logSubscription
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` recipeReference
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` validationConfigurations
      `Prelude.hashWithSalt` name

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf createDate
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
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf recipeReference
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf
        validationConfigurations
      `Prelude.seq` Prelude.rnf name
