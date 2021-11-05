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
-- Module      : Amazonka.DataBrew.CreateRecipeJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new job to transform input data, using steps defined in an
-- existing Glue DataBrew recipe
module Amazonka.DataBrew.CreateRecipeJob
  ( -- * Creating a Request
    CreateRecipeJob (..),
    newCreateRecipeJob,

    -- * Request Lenses
    createRecipeJob_dataCatalogOutputs,
    createRecipeJob_recipeReference,
    createRecipeJob_databaseOutputs,
    createRecipeJob_encryptionMode,
    createRecipeJob_outputs,
    createRecipeJob_logSubscription,
    createRecipeJob_projectName,
    createRecipeJob_maxRetries,
    createRecipeJob_datasetName,
    createRecipeJob_encryptionKeyArn,
    createRecipeJob_maxCapacity,
    createRecipeJob_timeout,
    createRecipeJob_tags,
    createRecipeJob_name,
    createRecipeJob_roleArn,

    -- * Destructuring the Response
    CreateRecipeJobResponse (..),
    newCreateRecipeJobResponse,

    -- * Response Lenses
    createRecipeJobResponse_httpStatus,
    createRecipeJobResponse_name,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRecipeJob' smart constructor.
data CreateRecipeJob = CreateRecipeJob'
  { -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write to.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | One or more artifacts that represent the output from running the job.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | Enables or disables Amazon CloudWatch logging for the job. If logging is
    -- enabled, CloudWatch writes one log stream for each job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | Either the name of an existing project, or a combination of a recipe and
    -- a dataset to associate with the recipe.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    -- | The name of the dataset that this job processes.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of nodes that DataBrew can consume when the job
    -- processes data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Metadata tags to apply to this job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique name for the job. Valid characters are alphanumeric (A-Z, a-z,
    -- 0-9), hyphen (-), period (.), and space.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role to be assumed when DataBrew runs the job.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRecipeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCatalogOutputs', 'createRecipeJob_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'recipeReference', 'createRecipeJob_recipeReference' - Undocumented member.
--
-- 'databaseOutputs', 'createRecipeJob_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write to.
--
-- 'encryptionMode', 'createRecipeJob_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
--
-- 'outputs', 'createRecipeJob_outputs' - One or more artifacts that represent the output from running the job.
--
-- 'logSubscription', 'createRecipeJob_logSubscription' - Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
--
-- 'projectName', 'createRecipeJob_projectName' - Either the name of an existing project, or a combination of a recipe and
-- a dataset to associate with the recipe.
--
-- 'maxRetries', 'createRecipeJob_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'datasetName', 'createRecipeJob_datasetName' - The name of the dataset that this job processes.
--
-- 'encryptionKeyArn', 'createRecipeJob_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
--
-- 'maxCapacity', 'createRecipeJob_maxCapacity' - The maximum number of nodes that DataBrew can consume when the job
-- processes data.
--
-- 'timeout', 'createRecipeJob_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'tags', 'createRecipeJob_tags' - Metadata tags to apply to this job.
--
-- 'name', 'createRecipeJob_name' - A unique name for the job. Valid characters are alphanumeric (A-Z, a-z,
-- 0-9), hyphen (-), period (.), and space.
--
-- 'roleArn', 'createRecipeJob_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
newCreateRecipeJob ::
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateRecipeJob
newCreateRecipeJob pName_ pRoleArn_ =
  CreateRecipeJob'
    { dataCatalogOutputs =
        Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      outputs = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      projectName = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      timeout = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      roleArn = pRoleArn_
    }

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
createRecipeJob_dataCatalogOutputs :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
createRecipeJob_dataCatalogOutputs = Lens.lens (\CreateRecipeJob' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@CreateRecipeJob' {} a -> s {dataCatalogOutputs = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createRecipeJob_recipeReference :: Lens.Lens' CreateRecipeJob (Prelude.Maybe RecipeReference)
createRecipeJob_recipeReference = Lens.lens (\CreateRecipeJob' {recipeReference} -> recipeReference) (\s@CreateRecipeJob' {} a -> s {recipeReference = a} :: CreateRecipeJob)

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write to.
createRecipeJob_databaseOutputs :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
createRecipeJob_databaseOutputs = Lens.lens (\CreateRecipeJob' {databaseOutputs} -> databaseOutputs) (\s@CreateRecipeJob' {} a -> s {databaseOutputs = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
createRecipeJob_encryptionMode :: Lens.Lens' CreateRecipeJob (Prelude.Maybe EncryptionMode)
createRecipeJob_encryptionMode = Lens.lens (\CreateRecipeJob' {encryptionMode} -> encryptionMode) (\s@CreateRecipeJob' {} a -> s {encryptionMode = a} :: CreateRecipeJob)

-- | One or more artifacts that represent the output from running the job.
createRecipeJob_outputs :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.NonEmpty Output))
createRecipeJob_outputs = Lens.lens (\CreateRecipeJob' {outputs} -> outputs) (\s@CreateRecipeJob' {} a -> s {outputs = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
createRecipeJob_logSubscription :: Lens.Lens' CreateRecipeJob (Prelude.Maybe LogSubscription)
createRecipeJob_logSubscription = Lens.lens (\CreateRecipeJob' {logSubscription} -> logSubscription) (\s@CreateRecipeJob' {} a -> s {logSubscription = a} :: CreateRecipeJob)

-- | Either the name of an existing project, or a combination of a recipe and
-- a dataset to associate with the recipe.
createRecipeJob_projectName :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Text)
createRecipeJob_projectName = Lens.lens (\CreateRecipeJob' {projectName} -> projectName) (\s@CreateRecipeJob' {} a -> s {projectName = a} :: CreateRecipeJob)

-- | The maximum number of times to retry the job after a job run fails.
createRecipeJob_maxRetries :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Natural)
createRecipeJob_maxRetries = Lens.lens (\CreateRecipeJob' {maxRetries} -> maxRetries) (\s@CreateRecipeJob' {} a -> s {maxRetries = a} :: CreateRecipeJob)

-- | The name of the dataset that this job processes.
createRecipeJob_datasetName :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Text)
createRecipeJob_datasetName = Lens.lens (\CreateRecipeJob' {datasetName} -> datasetName) (\s@CreateRecipeJob' {} a -> s {datasetName = a} :: CreateRecipeJob)

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
createRecipeJob_encryptionKeyArn :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Text)
createRecipeJob_encryptionKeyArn = Lens.lens (\CreateRecipeJob' {encryptionKeyArn} -> encryptionKeyArn) (\s@CreateRecipeJob' {} a -> s {encryptionKeyArn = a} :: CreateRecipeJob)

-- | The maximum number of nodes that DataBrew can consume when the job
-- processes data.
createRecipeJob_maxCapacity :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Int)
createRecipeJob_maxCapacity = Lens.lens (\CreateRecipeJob' {maxCapacity} -> maxCapacity) (\s@CreateRecipeJob' {} a -> s {maxCapacity = a} :: CreateRecipeJob)

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
createRecipeJob_timeout :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Natural)
createRecipeJob_timeout = Lens.lens (\CreateRecipeJob' {timeout} -> timeout) (\s@CreateRecipeJob' {} a -> s {timeout = a} :: CreateRecipeJob)

-- | Metadata tags to apply to this job.
createRecipeJob_tags :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRecipeJob_tags = Lens.lens (\CreateRecipeJob' {tags} -> tags) (\s@CreateRecipeJob' {} a -> s {tags = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | A unique name for the job. Valid characters are alphanumeric (A-Z, a-z,
-- 0-9), hyphen (-), period (.), and space.
createRecipeJob_name :: Lens.Lens' CreateRecipeJob Prelude.Text
createRecipeJob_name = Lens.lens (\CreateRecipeJob' {name} -> name) (\s@CreateRecipeJob' {} a -> s {name = a} :: CreateRecipeJob)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
createRecipeJob_roleArn :: Lens.Lens' CreateRecipeJob Prelude.Text
createRecipeJob_roleArn = Lens.lens (\CreateRecipeJob' {roleArn} -> roleArn) (\s@CreateRecipeJob' {} a -> s {roleArn = a} :: CreateRecipeJob)

instance Core.AWSRequest CreateRecipeJob where
  type
    AWSResponse CreateRecipeJob =
      CreateRecipeJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRecipeJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Name")
      )

instance Prelude.Hashable CreateRecipeJob

instance Prelude.NFData CreateRecipeJob

instance Core.ToHeaders CreateRecipeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRecipeJob where
  toJSON CreateRecipeJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataCatalogOutputs" Core..=)
              Prelude.<$> dataCatalogOutputs,
            ("RecipeReference" Core..=)
              Prelude.<$> recipeReference,
            ("DatabaseOutputs" Core..=)
              Prelude.<$> databaseOutputs,
            ("EncryptionMode" Core..=)
              Prelude.<$> encryptionMode,
            ("Outputs" Core..=) Prelude.<$> outputs,
            ("LogSubscription" Core..=)
              Prelude.<$> logSubscription,
            ("ProjectName" Core..=) Prelude.<$> projectName,
            ("MaxRetries" Core..=) Prelude.<$> maxRetries,
            ("DatasetName" Core..=) Prelude.<$> datasetName,
            ("EncryptionKeyArn" Core..=)
              Prelude.<$> encryptionKeyArn,
            ("MaxCapacity" Core..=) Prelude.<$> maxCapacity,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateRecipeJob where
  toPath = Prelude.const "/recipeJobs"

instance Core.ToQuery CreateRecipeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRecipeJobResponse' smart constructor.
data CreateRecipeJobResponse = CreateRecipeJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job that you created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRecipeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRecipeJobResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createRecipeJobResponse_name' - The name of the job that you created.
newCreateRecipeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  CreateRecipeJobResponse
newCreateRecipeJobResponse pHttpStatus_ pName_ =
  CreateRecipeJobResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
createRecipeJobResponse_httpStatus :: Lens.Lens' CreateRecipeJobResponse Prelude.Int
createRecipeJobResponse_httpStatus = Lens.lens (\CreateRecipeJobResponse' {httpStatus} -> httpStatus) (\s@CreateRecipeJobResponse' {} a -> s {httpStatus = a} :: CreateRecipeJobResponse)

-- | The name of the job that you created.
createRecipeJobResponse_name :: Lens.Lens' CreateRecipeJobResponse Prelude.Text
createRecipeJobResponse_name = Lens.lens (\CreateRecipeJobResponse' {name} -> name) (\s@CreateRecipeJobResponse' {} a -> s {name = a} :: CreateRecipeJobResponse)

instance Prelude.NFData CreateRecipeJobResponse
