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
-- Module      : Amazonka.Glue.UpdateMLTransform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing machine learning transform. Call this operation to
-- tune the algorithm parameters to achieve better results.
--
-- After calling this operation, you can call the
-- @StartMLEvaluationTaskRun@ operation to assess how well your new
-- parameters achieved your goals (such as improving the quality of your
-- machine learning transform, or making it more cost-effective).
module Amazonka.Glue.UpdateMLTransform
  ( -- * Creating a Request
    UpdateMLTransform (..),
    newUpdateMLTransform,

    -- * Request Lenses
    updateMLTransform_timeout,
    updateMLTransform_name,
    updateMLTransform_numberOfWorkers,
    updateMLTransform_glueVersion,
    updateMLTransform_workerType,
    updateMLTransform_description,
    updateMLTransform_maxRetries,
    updateMLTransform_role,
    updateMLTransform_maxCapacity,
    updateMLTransform_parameters,
    updateMLTransform_transformId,

    -- * Destructuring the Response
    UpdateMLTransformResponse (..),
    newUpdateMLTransformResponse,

    -- * Response Lenses
    updateMLTransformResponse_transformId,
    updateMLTransformResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMLTransform' smart constructor.
data UpdateMLTransform = UpdateMLTransform'
  { -- | The timeout for a task run for this transform in minutes. This is the
    -- maximum time that a task run for this transform can consume resources
    -- before it is terminated and enters @TIMEOUT@ status. The default is
    -- 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The unique name that you gave the transform when you created it.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- this task runs.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | This value determines which version of Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of predefined worker that is allocated when this task runs.
    -- Accepts a value of Standard, G.1X, or G.2X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
    --     memory and a 64GB disk, and 1 executor per worker.
    --
    -- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
    --     memory and a 128GB disk, and 1 executor per worker.
    workerType :: Prelude.Maybe WorkerType,
    -- | A description of the transform. The default is an empty string.
    description :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of times to retry a task for this transform after a
    -- task run fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required
    -- permissions.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The number of Glue data processing units (DPUs) that are allocated to
    -- task runs for this transform. You can allocate from 2 to 100 DPUs; the
    -- default is 10. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@, the
    -- @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The configuration parameters that are specific to the transform type
    -- (algorithm) used. Conditionally dependent on the transform type.
    parameters :: Prelude.Maybe TransformParameters,
    -- | A unique identifier that was generated when the transform was created.
    transformId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMLTransform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeout', 'updateMLTransform_timeout' - The timeout for a task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
--
-- 'name', 'updateMLTransform_name' - The unique name that you gave the transform when you created it.
--
-- 'numberOfWorkers', 'updateMLTransform_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
--
-- 'glueVersion', 'updateMLTransform_glueVersion' - This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
--
-- 'workerType', 'updateMLTransform_workerType' - The type of predefined worker that is allocated when this task runs.
-- Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
--     memory and a 64GB disk, and 1 executor per worker.
--
-- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
--     memory and a 128GB disk, and 1 executor per worker.
--
-- 'description', 'updateMLTransform_description' - A description of the transform. The default is an empty string.
--
-- 'maxRetries', 'updateMLTransform_maxRetries' - The maximum number of times to retry a task for this transform after a
-- task run fails.
--
-- 'role'', 'updateMLTransform_role' - The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions.
--
-- 'maxCapacity', 'updateMLTransform_maxCapacity' - The number of Glue data processing units (DPUs) that are allocated to
-- task runs for this transform. You can allocate from 2 to 100 DPUs; the
-- default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
--
-- 'parameters', 'updateMLTransform_parameters' - The configuration parameters that are specific to the transform type
-- (algorithm) used. Conditionally dependent on the transform type.
--
-- 'transformId', 'updateMLTransform_transformId' - A unique identifier that was generated when the transform was created.
newUpdateMLTransform ::
  -- | 'transformId'
  Prelude.Text ->
  UpdateMLTransform
newUpdateMLTransform pTransformId_ =
  UpdateMLTransform'
    { timeout = Prelude.Nothing,
      name = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      workerType = Prelude.Nothing,
      description = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      role' = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      parameters = Prelude.Nothing,
      transformId = pTransformId_
    }

-- | The timeout for a task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
updateMLTransform_timeout :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Natural)
updateMLTransform_timeout = Lens.lens (\UpdateMLTransform' {timeout} -> timeout) (\s@UpdateMLTransform' {} a -> s {timeout = a} :: UpdateMLTransform)

-- | The unique name that you gave the transform when you created it.
updateMLTransform_name :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Text)
updateMLTransform_name = Lens.lens (\UpdateMLTransform' {name} -> name) (\s@UpdateMLTransform' {} a -> s {name = a} :: UpdateMLTransform)

-- | The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
updateMLTransform_numberOfWorkers :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Int)
updateMLTransform_numberOfWorkers = Lens.lens (\UpdateMLTransform' {numberOfWorkers} -> numberOfWorkers) (\s@UpdateMLTransform' {} a -> s {numberOfWorkers = a} :: UpdateMLTransform)

-- | This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
updateMLTransform_glueVersion :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Text)
updateMLTransform_glueVersion = Lens.lens (\UpdateMLTransform' {glueVersion} -> glueVersion) (\s@UpdateMLTransform' {} a -> s {glueVersion = a} :: UpdateMLTransform)

-- | The type of predefined worker that is allocated when this task runs.
-- Accepts a value of Standard, G.1X, or G.2X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of
--     memory and a 64GB disk, and 1 executor per worker.
--
-- -   For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of
--     memory and a 128GB disk, and 1 executor per worker.
updateMLTransform_workerType :: Lens.Lens' UpdateMLTransform (Prelude.Maybe WorkerType)
updateMLTransform_workerType = Lens.lens (\UpdateMLTransform' {workerType} -> workerType) (\s@UpdateMLTransform' {} a -> s {workerType = a} :: UpdateMLTransform)

-- | A description of the transform. The default is an empty string.
updateMLTransform_description :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Text)
updateMLTransform_description = Lens.lens (\UpdateMLTransform' {description} -> description) (\s@UpdateMLTransform' {} a -> s {description = a} :: UpdateMLTransform)

-- | The maximum number of times to retry a task for this transform after a
-- task run fails.
updateMLTransform_maxRetries :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Int)
updateMLTransform_maxRetries = Lens.lens (\UpdateMLTransform' {maxRetries} -> maxRetries) (\s@UpdateMLTransform' {} a -> s {maxRetries = a} :: UpdateMLTransform)

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions.
updateMLTransform_role :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Text)
updateMLTransform_role = Lens.lens (\UpdateMLTransform' {role'} -> role') (\s@UpdateMLTransform' {} a -> s {role' = a} :: UpdateMLTransform)

-- | The number of Glue data processing units (DPUs) that are allocated to
-- task runs for this transform. You can allocate from 2 to 100 DPUs; the
-- default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
updateMLTransform_maxCapacity :: Lens.Lens' UpdateMLTransform (Prelude.Maybe Prelude.Double)
updateMLTransform_maxCapacity = Lens.lens (\UpdateMLTransform' {maxCapacity} -> maxCapacity) (\s@UpdateMLTransform' {} a -> s {maxCapacity = a} :: UpdateMLTransform)

-- | The configuration parameters that are specific to the transform type
-- (algorithm) used. Conditionally dependent on the transform type.
updateMLTransform_parameters :: Lens.Lens' UpdateMLTransform (Prelude.Maybe TransformParameters)
updateMLTransform_parameters = Lens.lens (\UpdateMLTransform' {parameters} -> parameters) (\s@UpdateMLTransform' {} a -> s {parameters = a} :: UpdateMLTransform)

-- | A unique identifier that was generated when the transform was created.
updateMLTransform_transformId :: Lens.Lens' UpdateMLTransform Prelude.Text
updateMLTransform_transformId = Lens.lens (\UpdateMLTransform' {transformId} -> transformId) (\s@UpdateMLTransform' {} a -> s {transformId = a} :: UpdateMLTransform)

instance Core.AWSRequest UpdateMLTransform where
  type
    AWSResponse UpdateMLTransform =
      UpdateMLTransformResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMLTransformResponse'
            Prelude.<$> (x Data..?> "TransformId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMLTransform where
  hashWithSalt _salt UpdateMLTransform' {..} =
    _salt `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` transformId

instance Prelude.NFData UpdateMLTransform where
  rnf UpdateMLTransform' {..} =
    Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf transformId

instance Data.ToHeaders UpdateMLTransform where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateMLTransform" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMLTransform where
  toJSON UpdateMLTransform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Timeout" Data..=) Prelude.<$> timeout,
            ("Name" Data..=) Prelude.<$> name,
            ("NumberOfWorkers" Data..=)
              Prelude.<$> numberOfWorkers,
            ("GlueVersion" Data..=) Prelude.<$> glueVersion,
            ("WorkerType" Data..=) Prelude.<$> workerType,
            ("Description" Data..=) Prelude.<$> description,
            ("MaxRetries" Data..=) Prelude.<$> maxRetries,
            ("Role" Data..=) Prelude.<$> role',
            ("MaxCapacity" Data..=) Prelude.<$> maxCapacity,
            ("Parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("TransformId" Data..= transformId)
          ]
      )

instance Data.ToPath UpdateMLTransform where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMLTransform where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMLTransformResponse' smart constructor.
data UpdateMLTransformResponse = UpdateMLTransformResponse'
  { -- | The unique identifier for the transform that was updated.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMLTransformResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'updateMLTransformResponse_transformId' - The unique identifier for the transform that was updated.
--
-- 'httpStatus', 'updateMLTransformResponse_httpStatus' - The response's http status code.
newUpdateMLTransformResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMLTransformResponse
newUpdateMLTransformResponse pHttpStatus_ =
  UpdateMLTransformResponse'
    { transformId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the transform that was updated.
updateMLTransformResponse_transformId :: Lens.Lens' UpdateMLTransformResponse (Prelude.Maybe Prelude.Text)
updateMLTransformResponse_transformId = Lens.lens (\UpdateMLTransformResponse' {transformId} -> transformId) (\s@UpdateMLTransformResponse' {} a -> s {transformId = a} :: UpdateMLTransformResponse)

-- | The response's http status code.
updateMLTransformResponse_httpStatus :: Lens.Lens' UpdateMLTransformResponse Prelude.Int
updateMLTransformResponse_httpStatus = Lens.lens (\UpdateMLTransformResponse' {httpStatus} -> httpStatus) (\s@UpdateMLTransformResponse' {} a -> s {httpStatus = a} :: UpdateMLTransformResponse)

instance Prelude.NFData UpdateMLTransformResponse where
  rnf UpdateMLTransformResponse' {..} =
    Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf httpStatus
