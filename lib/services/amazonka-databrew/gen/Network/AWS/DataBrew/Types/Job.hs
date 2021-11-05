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
-- Module      : Network.AWS.DataBrew.Types.Job
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataBrew.Types.Job where

import qualified Network.AWS.Core as Core
import Network.AWS.DataBrew.Types.DataCatalogOutput
import Network.AWS.DataBrew.Types.DatabaseOutput
import Network.AWS.DataBrew.Types.EncryptionMode
import Network.AWS.DataBrew.Types.JobSample
import Network.AWS.DataBrew.Types.JobType
import Network.AWS.DataBrew.Types.LogSubscription
import Network.AWS.DataBrew.Types.Output
import Network.AWS.DataBrew.Types.RecipeReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents all of the attributes of a DataBrew job.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    -- | The modification date and time of the job.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the job was created.
    createDate :: Prelude.Maybe Core.POSIX,
    -- | A set of steps that the job runs.
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | The Amazon Resource Name (ARN) of the user who created the job.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write into.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | The ID of the Amazon Web Services account that owns the job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | One or more artifacts that represent output from running the job.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | The unique Amazon Resource Name (ARN) for the job.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of Amazon CloudWatch logging for the job.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The name of the project that the job is associated with.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    -- | A dataset that the job is to process.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job output. For more information, see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/encryption-security-configuration.html Encrypting data written by DataBrew jobs>
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of nodes that can be consumed when the job processes
    -- data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the user who last modified the job.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The job type of the job, which must be one of the following:
    --
    -- -   @PROFILE@ - A job to analyze a dataset, to determine its size, data
    --     types, data distribution, and more.
    --
    -- -   @RECIPE@ - A job to apply one or more transformations to a dataset.
    type' :: Prelude.Maybe JobType,
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Metadata tags that have been applied to the job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the role to be assumed for this job.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A sample configuration for profile jobs only, which determines the
    -- number of rows on which the profile job is run. If a @JobSample@ value
    -- isn\'t provided, the default value is used. The default value is
    -- CUSTOM_ROWS for the mode parameter and 20,000 for the size parameter.
    jobSample :: Prelude.Maybe JobSample,
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
-- 'dataCatalogOutputs', 'job_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'lastModifiedDate', 'job_lastModifiedDate' - The modification date and time of the job.
--
-- 'createDate', 'job_createDate' - The date and time that the job was created.
--
-- 'recipeReference', 'job_recipeReference' - A set of steps that the job runs.
--
-- 'createdBy', 'job_createdBy' - The Amazon Resource Name (ARN) of the user who created the job.
--
-- 'databaseOutputs', 'job_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
--
-- 'accountId', 'job_accountId' - The ID of the Amazon Web Services account that owns the job.
--
-- 'encryptionMode', 'job_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
--
-- 'outputs', 'job_outputs' - One or more artifacts that represent output from running the job.
--
-- 'resourceArn', 'job_resourceArn' - The unique Amazon Resource Name (ARN) for the job.
--
-- 'logSubscription', 'job_logSubscription' - The current status of Amazon CloudWatch logging for the job.
--
-- 'projectName', 'job_projectName' - The name of the project that the job is associated with.
--
-- 'maxRetries', 'job_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'datasetName', 'job_datasetName' - A dataset that the job is to process.
--
-- 'encryptionKeyArn', 'job_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job output. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/encryption-security-configuration.html Encrypting data written by DataBrew jobs>
--
-- 'maxCapacity', 'job_maxCapacity' - The maximum number of nodes that can be consumed when the job processes
-- data.
--
-- 'lastModifiedBy', 'job_lastModifiedBy' - The Amazon Resource Name (ARN) of the user who last modified the job.
--
-- 'type'', 'job_type' - The job type of the job, which must be one of the following:
--
-- -   @PROFILE@ - A job to analyze a dataset, to determine its size, data
--     types, data distribution, and more.
--
-- -   @RECIPE@ - A job to apply one or more transformations to a dataset.
--
-- 'timeout', 'job_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'tags', 'job_tags' - Metadata tags that have been applied to the job.
--
-- 'roleArn', 'job_roleArn' - The Amazon Resource Name (ARN) of the role to be assumed for this job.
--
-- 'jobSample', 'job_jobSample' - A sample configuration for profile jobs only, which determines the
-- number of rows on which the profile job is run. If a @JobSample@ value
-- isn\'t provided, the default value is used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20,000 for the size parameter.
--
-- 'name', 'job_name' - The unique name of the job.
newJob ::
  -- | 'name'
  Prelude.Text ->
  Job
newJob pName_ =
  Job'
    { dataCatalogOutputs = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      createDate = Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      accountId = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      outputs = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      projectName = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      type' = Prelude.Nothing,
      timeout = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      jobSample = Prelude.Nothing,
      name = pName_
    }

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
job_dataCatalogOutputs :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
job_dataCatalogOutputs = Lens.lens (\Job' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@Job' {} a -> s {dataCatalogOutputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The modification date and time of the job.
job_lastModifiedDate :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_lastModifiedDate = Lens.lens (\Job' {lastModifiedDate} -> lastModifiedDate) (\s@Job' {} a -> s {lastModifiedDate = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | The date and time that the job was created.
job_createDate :: Lens.Lens' Job (Prelude.Maybe Prelude.UTCTime)
job_createDate = Lens.lens (\Job' {createDate} -> createDate) (\s@Job' {} a -> s {createDate = a} :: Job) Prelude.. Lens.mapping Core._Time

-- | A set of steps that the job runs.
job_recipeReference :: Lens.Lens' Job (Prelude.Maybe RecipeReference)
job_recipeReference = Lens.lens (\Job' {recipeReference} -> recipeReference) (\s@Job' {} a -> s {recipeReference = a} :: Job)

-- | The Amazon Resource Name (ARN) of the user who created the job.
job_createdBy :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_createdBy = Lens.lens (\Job' {createdBy} -> createdBy) (\s@Job' {} a -> s {createdBy = a} :: Job)

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
job_databaseOutputs :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
job_databaseOutputs = Lens.lens (\Job' {databaseOutputs} -> databaseOutputs) (\s@Job' {} a -> s {databaseOutputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the job.
job_accountId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_accountId = Lens.lens (\Job' {accountId} -> accountId) (\s@Job' {} a -> s {accountId = a} :: Job)

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
job_encryptionMode :: Lens.Lens' Job (Prelude.Maybe EncryptionMode)
job_encryptionMode = Lens.lens (\Job' {encryptionMode} -> encryptionMode) (\s@Job' {} a -> s {encryptionMode = a} :: Job)

-- | One or more artifacts that represent output from running the job.
job_outputs :: Lens.Lens' Job (Prelude.Maybe (Prelude.NonEmpty Output))
job_outputs = Lens.lens (\Job' {outputs} -> outputs) (\s@Job' {} a -> s {outputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The unique Amazon Resource Name (ARN) for the job.
job_resourceArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_resourceArn = Lens.lens (\Job' {resourceArn} -> resourceArn) (\s@Job' {} a -> s {resourceArn = a} :: Job)

-- | The current status of Amazon CloudWatch logging for the job.
job_logSubscription :: Lens.Lens' Job (Prelude.Maybe LogSubscription)
job_logSubscription = Lens.lens (\Job' {logSubscription} -> logSubscription) (\s@Job' {} a -> s {logSubscription = a} :: Job)

-- | The name of the project that the job is associated with.
job_projectName :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_projectName = Lens.lens (\Job' {projectName} -> projectName) (\s@Job' {} a -> s {projectName = a} :: Job)

-- | The maximum number of times to retry the job after a job run fails.
job_maxRetries :: Lens.Lens' Job (Prelude.Maybe Prelude.Natural)
job_maxRetries = Lens.lens (\Job' {maxRetries} -> maxRetries) (\s@Job' {} a -> s {maxRetries = a} :: Job)

-- | A dataset that the job is to process.
job_datasetName :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_datasetName = Lens.lens (\Job' {datasetName} -> datasetName) (\s@Job' {} a -> s {datasetName = a} :: Job)

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job output. For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/encryption-security-configuration.html Encrypting data written by DataBrew jobs>
job_encryptionKeyArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_encryptionKeyArn = Lens.lens (\Job' {encryptionKeyArn} -> encryptionKeyArn) (\s@Job' {} a -> s {encryptionKeyArn = a} :: Job)

-- | The maximum number of nodes that can be consumed when the job processes
-- data.
job_maxCapacity :: Lens.Lens' Job (Prelude.Maybe Prelude.Int)
job_maxCapacity = Lens.lens (\Job' {maxCapacity} -> maxCapacity) (\s@Job' {} a -> s {maxCapacity = a} :: Job)

-- | The Amazon Resource Name (ARN) of the user who last modified the job.
job_lastModifiedBy :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_lastModifiedBy = Lens.lens (\Job' {lastModifiedBy} -> lastModifiedBy) (\s@Job' {} a -> s {lastModifiedBy = a} :: Job)

-- | The job type of the job, which must be one of the following:
--
-- -   @PROFILE@ - A job to analyze a dataset, to determine its size, data
--     types, data distribution, and more.
--
-- -   @RECIPE@ - A job to apply one or more transformations to a dataset.
job_type :: Lens.Lens' Job (Prelude.Maybe JobType)
job_type = Lens.lens (\Job' {type'} -> type') (\s@Job' {} a -> s {type' = a} :: Job)

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
job_timeout :: Lens.Lens' Job (Prelude.Maybe Prelude.Natural)
job_timeout = Lens.lens (\Job' {timeout} -> timeout) (\s@Job' {} a -> s {timeout = a} :: Job)

-- | Metadata tags that have been applied to the job.
job_tags :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_tags = Lens.lens (\Job' {tags} -> tags) (\s@Job' {} a -> s {tags = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role to be assumed for this job.
job_roleArn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_roleArn = Lens.lens (\Job' {roleArn} -> roleArn) (\s@Job' {} a -> s {roleArn = a} :: Job)

-- | A sample configuration for profile jobs only, which determines the
-- number of rows on which the profile job is run. If a @JobSample@ value
-- isn\'t provided, the default value is used. The default value is
-- CUSTOM_ROWS for the mode parameter and 20,000 for the size parameter.
job_jobSample :: Lens.Lens' Job (Prelude.Maybe JobSample)
job_jobSample = Lens.lens (\Job' {jobSample} -> jobSample) (\s@Job' {} a -> s {jobSample = a} :: Job)

-- | The unique name of the job.
job_name :: Lens.Lens' Job Prelude.Text
job_name = Lens.lens (\Job' {name} -> name) (\s@Job' {} a -> s {name = a} :: Job)

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Core..:? "DataCatalogOutputs")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "RecipeReference")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "DatabaseOutputs")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "EncryptionMode")
            Prelude.<*> (x Core..:? "Outputs")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "LogSubscription")
            Prelude.<*> (x Core..:? "ProjectName")
            Prelude.<*> (x Core..:? "MaxRetries")
            Prelude.<*> (x Core..:? "DatasetName")
            Prelude.<*> (x Core..:? "EncryptionKeyArn")
            Prelude.<*> (x Core..:? "MaxCapacity")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "JobSample")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable Job

instance Prelude.NFData Job
