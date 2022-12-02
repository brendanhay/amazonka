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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createRecipeJob_tags,
    createRecipeJob_encryptionKeyArn,
    createRecipeJob_timeout,
    createRecipeJob_databaseOutputs,
    createRecipeJob_dataCatalogOutputs,
    createRecipeJob_datasetName,
    createRecipeJob_logSubscription,
    createRecipeJob_maxRetries,
    createRecipeJob_recipeReference,
    createRecipeJob_outputs,
    createRecipeJob_projectName,
    createRecipeJob_maxCapacity,
    createRecipeJob_encryptionMode,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRecipeJob' smart constructor.
data CreateRecipeJob = CreateRecipeJob'
  { -- | Metadata tags to apply to this job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write to.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    -- | The name of the dataset that this job processes.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables Amazon CloudWatch logging for the job. If logging is
    -- enabled, CloudWatch writes one log stream for each job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    recipeReference :: Prelude.Maybe RecipeReference,
    -- | One or more artifacts that represent the output from running the job.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | Either the name of an existing project, or a combination of a recipe and
    -- a dataset to associate with the recipe.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of nodes that DataBrew can consume when the job
    -- processes data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
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
-- 'tags', 'createRecipeJob_tags' - Metadata tags to apply to this job.
--
-- 'encryptionKeyArn', 'createRecipeJob_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
--
-- 'timeout', 'createRecipeJob_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'databaseOutputs', 'createRecipeJob_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write to.
--
-- 'dataCatalogOutputs', 'createRecipeJob_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'datasetName', 'createRecipeJob_datasetName' - The name of the dataset that this job processes.
--
-- 'logSubscription', 'createRecipeJob_logSubscription' - Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
--
-- 'maxRetries', 'createRecipeJob_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'recipeReference', 'createRecipeJob_recipeReference' - Undocumented member.
--
-- 'outputs', 'createRecipeJob_outputs' - One or more artifacts that represent the output from running the job.
--
-- 'projectName', 'createRecipeJob_projectName' - Either the name of an existing project, or a combination of a recipe and
-- a dataset to associate with the recipe.
--
-- 'maxCapacity', 'createRecipeJob_maxCapacity' - The maximum number of nodes that DataBrew can consume when the job
-- processes data.
--
-- 'encryptionMode', 'createRecipeJob_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
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
    { tags = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      timeout = Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      dataCatalogOutputs = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      recipeReference = Prelude.Nothing,
      outputs = Prelude.Nothing,
      projectName = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      name = pName_,
      roleArn = pRoleArn_
    }

-- | Metadata tags to apply to this job.
createRecipeJob_tags :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRecipeJob_tags = Lens.lens (\CreateRecipeJob' {tags} -> tags) (\s@CreateRecipeJob' {} a -> s {tags = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
createRecipeJob_encryptionKeyArn :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Text)
createRecipeJob_encryptionKeyArn = Lens.lens (\CreateRecipeJob' {encryptionKeyArn} -> encryptionKeyArn) (\s@CreateRecipeJob' {} a -> s {encryptionKeyArn = a} :: CreateRecipeJob)

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
createRecipeJob_timeout :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Natural)
createRecipeJob_timeout = Lens.lens (\CreateRecipeJob' {timeout} -> timeout) (\s@CreateRecipeJob' {} a -> s {timeout = a} :: CreateRecipeJob)

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write to.
createRecipeJob_databaseOutputs :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
createRecipeJob_databaseOutputs = Lens.lens (\CreateRecipeJob' {databaseOutputs} -> databaseOutputs) (\s@CreateRecipeJob' {} a -> s {databaseOutputs = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
createRecipeJob_dataCatalogOutputs :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
createRecipeJob_dataCatalogOutputs = Lens.lens (\CreateRecipeJob' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@CreateRecipeJob' {} a -> s {dataCatalogOutputs = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset that this job processes.
createRecipeJob_datasetName :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Text)
createRecipeJob_datasetName = Lens.lens (\CreateRecipeJob' {datasetName} -> datasetName) (\s@CreateRecipeJob' {} a -> s {datasetName = a} :: CreateRecipeJob)

-- | Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
createRecipeJob_logSubscription :: Lens.Lens' CreateRecipeJob (Prelude.Maybe LogSubscription)
createRecipeJob_logSubscription = Lens.lens (\CreateRecipeJob' {logSubscription} -> logSubscription) (\s@CreateRecipeJob' {} a -> s {logSubscription = a} :: CreateRecipeJob)

-- | The maximum number of times to retry the job after a job run fails.
createRecipeJob_maxRetries :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Natural)
createRecipeJob_maxRetries = Lens.lens (\CreateRecipeJob' {maxRetries} -> maxRetries) (\s@CreateRecipeJob' {} a -> s {maxRetries = a} :: CreateRecipeJob)

-- | Undocumented member.
createRecipeJob_recipeReference :: Lens.Lens' CreateRecipeJob (Prelude.Maybe RecipeReference)
createRecipeJob_recipeReference = Lens.lens (\CreateRecipeJob' {recipeReference} -> recipeReference) (\s@CreateRecipeJob' {} a -> s {recipeReference = a} :: CreateRecipeJob)

-- | One or more artifacts that represent the output from running the job.
createRecipeJob_outputs :: Lens.Lens' CreateRecipeJob (Prelude.Maybe (Prelude.NonEmpty Output))
createRecipeJob_outputs = Lens.lens (\CreateRecipeJob' {outputs} -> outputs) (\s@CreateRecipeJob' {} a -> s {outputs = a} :: CreateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | Either the name of an existing project, or a combination of a recipe and
-- a dataset to associate with the recipe.
createRecipeJob_projectName :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Text)
createRecipeJob_projectName = Lens.lens (\CreateRecipeJob' {projectName} -> projectName) (\s@CreateRecipeJob' {} a -> s {projectName = a} :: CreateRecipeJob)

-- | The maximum number of nodes that DataBrew can consume when the job
-- processes data.
createRecipeJob_maxCapacity :: Lens.Lens' CreateRecipeJob (Prelude.Maybe Prelude.Int)
createRecipeJob_maxCapacity = Lens.lens (\CreateRecipeJob' {maxCapacity} -> maxCapacity) (\s@CreateRecipeJob' {} a -> s {maxCapacity = a} :: CreateRecipeJob)

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
createRecipeJob_encryptionMode :: Lens.Lens' CreateRecipeJob (Prelude.Maybe EncryptionMode)
createRecipeJob_encryptionMode = Lens.lens (\CreateRecipeJob' {encryptionMode} -> encryptionMode) (\s@CreateRecipeJob' {} a -> s {encryptionMode = a} :: CreateRecipeJob)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRecipeJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable CreateRecipeJob where
  hashWithSalt _salt CreateRecipeJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` databaseOutputs
      `Prelude.hashWithSalt` dataCatalogOutputs
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` logSubscription
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` recipeReference
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateRecipeJob where
  rnf CreateRecipeJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf databaseOutputs
      `Prelude.seq` Prelude.rnf dataCatalogOutputs
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf logSubscription
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf recipeReference
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateRecipeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRecipeJob where
  toJSON CreateRecipeJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("EncryptionKeyArn" Data..=)
              Prelude.<$> encryptionKeyArn,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("DatabaseOutputs" Data..=)
              Prelude.<$> databaseOutputs,
            ("DataCatalogOutputs" Data..=)
              Prelude.<$> dataCatalogOutputs,
            ("DatasetName" Data..=) Prelude.<$> datasetName,
            ("LogSubscription" Data..=)
              Prelude.<$> logSubscription,
            ("MaxRetries" Data..=) Prelude.<$> maxRetries,
            ("RecipeReference" Data..=)
              Prelude.<$> recipeReference,
            ("Outputs" Data..=) Prelude.<$> outputs,
            ("ProjectName" Data..=) Prelude.<$> projectName,
            ("MaxCapacity" Data..=) Prelude.<$> maxCapacity,
            ("EncryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateRecipeJob where
  toPath = Prelude.const "/recipeJobs"

instance Data.ToQuery CreateRecipeJob where
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

instance Prelude.NFData CreateRecipeJobResponse where
  rnf CreateRecipeJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
