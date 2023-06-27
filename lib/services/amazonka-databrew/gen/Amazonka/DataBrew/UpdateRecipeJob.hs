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
-- Module      : Amazonka.DataBrew.UpdateRecipeJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the definition of an existing DataBrew recipe job.
module Amazonka.DataBrew.UpdateRecipeJob
  ( -- * Creating a Request
    UpdateRecipeJob (..),
    newUpdateRecipeJob,

    -- * Request Lenses
    updateRecipeJob_dataCatalogOutputs,
    updateRecipeJob_databaseOutputs,
    updateRecipeJob_encryptionKeyArn,
    updateRecipeJob_encryptionMode,
    updateRecipeJob_logSubscription,
    updateRecipeJob_maxCapacity,
    updateRecipeJob_maxRetries,
    updateRecipeJob_outputs,
    updateRecipeJob_timeout,
    updateRecipeJob_name,
    updateRecipeJob_roleArn,

    -- * Destructuring the Response
    UpdateRecipeJobResponse (..),
    newUpdateRecipeJobResponse,

    -- * Response Lenses
    updateRecipeJobResponse_httpStatus,
    updateRecipeJobResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRecipeJob' smart constructor.
data UpdateRecipeJob = UpdateRecipeJob'
  { -- | One or more artifacts that represent the Glue Data Catalog output from
    -- running the job.
    dataCatalogOutputs :: Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput),
    -- | Represents a list of JDBC database output objects which defines the
    -- output destination for a DataBrew recipe job to write into.
    databaseOutputs :: Prelude.Maybe (Prelude.NonEmpty DatabaseOutput),
    -- | The Amazon Resource Name (ARN) of an encryption key that is used to
    -- protect the job.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The encryption mode for the job, which can be one of the following:
    --
    -- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
    --
    -- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
    encryptionMode :: Prelude.Maybe EncryptionMode,
    -- | Enables or disables Amazon CloudWatch logging for the job. If logging is
    -- enabled, CloudWatch writes one log stream for each job run.
    logSubscription :: Prelude.Maybe LogSubscription,
    -- | The maximum number of nodes that DataBrew can consume when the job
    -- processes data.
    maxCapacity :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of times to retry the job after a job run fails.
    maxRetries :: Prelude.Maybe Prelude.Natural,
    -- | One or more artifacts that represent the output from running the job.
    outputs :: Prelude.Maybe (Prelude.NonEmpty Output),
    -- | The job\'s timeout in minutes. A job that attempts to run longer than
    -- this timeout period ends with a status of @TIMEOUT@.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The name of the job to update.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role to be assumed when DataBrew runs the job.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecipeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCatalogOutputs', 'updateRecipeJob_dataCatalogOutputs' - One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
--
-- 'databaseOutputs', 'updateRecipeJob_databaseOutputs' - Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
--
-- 'encryptionKeyArn', 'updateRecipeJob_encryptionKeyArn' - The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
--
-- 'encryptionMode', 'updateRecipeJob_encryptionMode' - The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
--
-- 'logSubscription', 'updateRecipeJob_logSubscription' - Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
--
-- 'maxCapacity', 'updateRecipeJob_maxCapacity' - The maximum number of nodes that DataBrew can consume when the job
-- processes data.
--
-- 'maxRetries', 'updateRecipeJob_maxRetries' - The maximum number of times to retry the job after a job run fails.
--
-- 'outputs', 'updateRecipeJob_outputs' - One or more artifacts that represent the output from running the job.
--
-- 'timeout', 'updateRecipeJob_timeout' - The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
--
-- 'name', 'updateRecipeJob_name' - The name of the job to update.
--
-- 'roleArn', 'updateRecipeJob_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
newUpdateRecipeJob ::
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  UpdateRecipeJob
newUpdateRecipeJob pName_ pRoleArn_ =
  UpdateRecipeJob'
    { dataCatalogOutputs =
        Prelude.Nothing,
      databaseOutputs = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      logSubscription = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      outputs = Prelude.Nothing,
      timeout = Prelude.Nothing,
      name = pName_,
      roleArn = pRoleArn_
    }

-- | One or more artifacts that represent the Glue Data Catalog output from
-- running the job.
updateRecipeJob_dataCatalogOutputs :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe (Prelude.NonEmpty DataCatalogOutput))
updateRecipeJob_dataCatalogOutputs = Lens.lens (\UpdateRecipeJob' {dataCatalogOutputs} -> dataCatalogOutputs) (\s@UpdateRecipeJob' {} a -> s {dataCatalogOutputs = a} :: UpdateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | Represents a list of JDBC database output objects which defines the
-- output destination for a DataBrew recipe job to write into.
updateRecipeJob_databaseOutputs :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe (Prelude.NonEmpty DatabaseOutput))
updateRecipeJob_databaseOutputs = Lens.lens (\UpdateRecipeJob' {databaseOutputs} -> databaseOutputs) (\s@UpdateRecipeJob' {} a -> s {databaseOutputs = a} :: UpdateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an encryption key that is used to
-- protect the job.
updateRecipeJob_encryptionKeyArn :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe Prelude.Text)
updateRecipeJob_encryptionKeyArn = Lens.lens (\UpdateRecipeJob' {encryptionKeyArn} -> encryptionKeyArn) (\s@UpdateRecipeJob' {} a -> s {encryptionKeyArn = a} :: UpdateRecipeJob)

-- | The encryption mode for the job, which can be one of the following:
--
-- -   @SSE-KMS@ - Server-side encryption with keys managed by KMS.
--
-- -   @SSE-S3@ - Server-side encryption with keys managed by Amazon S3.
updateRecipeJob_encryptionMode :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe EncryptionMode)
updateRecipeJob_encryptionMode = Lens.lens (\UpdateRecipeJob' {encryptionMode} -> encryptionMode) (\s@UpdateRecipeJob' {} a -> s {encryptionMode = a} :: UpdateRecipeJob)

-- | Enables or disables Amazon CloudWatch logging for the job. If logging is
-- enabled, CloudWatch writes one log stream for each job run.
updateRecipeJob_logSubscription :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe LogSubscription)
updateRecipeJob_logSubscription = Lens.lens (\UpdateRecipeJob' {logSubscription} -> logSubscription) (\s@UpdateRecipeJob' {} a -> s {logSubscription = a} :: UpdateRecipeJob)

-- | The maximum number of nodes that DataBrew can consume when the job
-- processes data.
updateRecipeJob_maxCapacity :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe Prelude.Int)
updateRecipeJob_maxCapacity = Lens.lens (\UpdateRecipeJob' {maxCapacity} -> maxCapacity) (\s@UpdateRecipeJob' {} a -> s {maxCapacity = a} :: UpdateRecipeJob)

-- | The maximum number of times to retry the job after a job run fails.
updateRecipeJob_maxRetries :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe Prelude.Natural)
updateRecipeJob_maxRetries = Lens.lens (\UpdateRecipeJob' {maxRetries} -> maxRetries) (\s@UpdateRecipeJob' {} a -> s {maxRetries = a} :: UpdateRecipeJob)

-- | One or more artifacts that represent the output from running the job.
updateRecipeJob_outputs :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe (Prelude.NonEmpty Output))
updateRecipeJob_outputs = Lens.lens (\UpdateRecipeJob' {outputs} -> outputs) (\s@UpdateRecipeJob' {} a -> s {outputs = a} :: UpdateRecipeJob) Prelude.. Lens.mapping Lens.coerced

-- | The job\'s timeout in minutes. A job that attempts to run longer than
-- this timeout period ends with a status of @TIMEOUT@.
updateRecipeJob_timeout :: Lens.Lens' UpdateRecipeJob (Prelude.Maybe Prelude.Natural)
updateRecipeJob_timeout = Lens.lens (\UpdateRecipeJob' {timeout} -> timeout) (\s@UpdateRecipeJob' {} a -> s {timeout = a} :: UpdateRecipeJob)

-- | The name of the job to update.
updateRecipeJob_name :: Lens.Lens' UpdateRecipeJob Prelude.Text
updateRecipeJob_name = Lens.lens (\UpdateRecipeJob' {name} -> name) (\s@UpdateRecipeJob' {} a -> s {name = a} :: UpdateRecipeJob)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be assumed when DataBrew runs the job.
updateRecipeJob_roleArn :: Lens.Lens' UpdateRecipeJob Prelude.Text
updateRecipeJob_roleArn = Lens.lens (\UpdateRecipeJob' {roleArn} -> roleArn) (\s@UpdateRecipeJob' {} a -> s {roleArn = a} :: UpdateRecipeJob)

instance Core.AWSRequest UpdateRecipeJob where
  type
    AWSResponse UpdateRecipeJob =
      UpdateRecipeJobResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecipeJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable UpdateRecipeJob where
  hashWithSalt _salt UpdateRecipeJob' {..} =
    _salt
      `Prelude.hashWithSalt` dataCatalogOutputs
      `Prelude.hashWithSalt` databaseOutputs
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` logSubscription
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData UpdateRecipeJob where
  rnf UpdateRecipeJob' {..} =
    Prelude.rnf dataCatalogOutputs
      `Prelude.seq` Prelude.rnf databaseOutputs
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf logSubscription
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders UpdateRecipeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRecipeJob where
  toJSON UpdateRecipeJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataCatalogOutputs" Data..=)
              Prelude.<$> dataCatalogOutputs,
            ("DatabaseOutputs" Data..=)
              Prelude.<$> databaseOutputs,
            ("EncryptionKeyArn" Data..=)
              Prelude.<$> encryptionKeyArn,
            ("EncryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            ("LogSubscription" Data..=)
              Prelude.<$> logSubscription,
            ("MaxCapacity" Data..=) Prelude.<$> maxCapacity,
            ("MaxRetries" Data..=) Prelude.<$> maxRetries,
            ("Outputs" Data..=) Prelude.<$> outputs,
            ("Timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath UpdateRecipeJob where
  toPath UpdateRecipeJob' {..} =
    Prelude.mconcat ["/recipeJobs/", Data.toBS name]

instance Data.ToQuery UpdateRecipeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRecipeJobResponse' smart constructor.
data UpdateRecipeJobResponse = UpdateRecipeJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the job that you updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecipeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRecipeJobResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateRecipeJobResponse_name' - The name of the job that you updated.
newUpdateRecipeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateRecipeJobResponse
newUpdateRecipeJobResponse pHttpStatus_ pName_ =
  UpdateRecipeJobResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateRecipeJobResponse_httpStatus :: Lens.Lens' UpdateRecipeJobResponse Prelude.Int
updateRecipeJobResponse_httpStatus = Lens.lens (\UpdateRecipeJobResponse' {httpStatus} -> httpStatus) (\s@UpdateRecipeJobResponse' {} a -> s {httpStatus = a} :: UpdateRecipeJobResponse)

-- | The name of the job that you updated.
updateRecipeJobResponse_name :: Lens.Lens' UpdateRecipeJobResponse Prelude.Text
updateRecipeJobResponse_name = Lens.lens (\UpdateRecipeJobResponse' {name} -> name) (\s@UpdateRecipeJobResponse' {} a -> s {name = a} :: UpdateRecipeJobResponse)

instance Prelude.NFData UpdateRecipeJobResponse where
  rnf UpdateRecipeJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
