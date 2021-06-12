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
-- Module      : Network.AWS.Glue.UpdateMLTransform
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glue.UpdateMLTransform
  ( -- * Creating a Request
    UpdateMLTransform (..),
    newUpdateMLTransform,

    -- * Request Lenses
    updateMLTransform_timeout,
    updateMLTransform_maxCapacity,
    updateMLTransform_numberOfWorkers,
    updateMLTransform_name,
    updateMLTransform_role,
    updateMLTransform_glueVersion,
    updateMLTransform_workerType,
    updateMLTransform_description,
    updateMLTransform_parameters,
    updateMLTransform_maxRetries,
    updateMLTransform_transformId,

    -- * Destructuring the Response
    UpdateMLTransformResponse (..),
    newUpdateMLTransformResponse,

    -- * Response Lenses
    updateMLTransformResponse_transformId,
    updateMLTransformResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMLTransform' smart constructor.
data UpdateMLTransform = UpdateMLTransform'
  { -- | The timeout for a task run for this transform in minutes. This is the
    -- maximum time that a task run for this transform can consume resources
    -- before it is terminated and enters @TIMEOUT@ status. The default is
    -- 2,880 minutes (48 hours).
    timeout :: Core.Maybe Core.Natural,
    -- | The number of AWS Glue data processing units (DPUs) that are allocated
    -- to task runs for this transform. You can allocate from 2 to 100 DPUs;
    -- the default is 10. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@, the
    -- @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Core.Maybe Core.Double,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- this task runs.
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | The unique name that you gave the transform when you created it.
    name :: Core.Maybe Core.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required
    -- permissions.
    role' :: Core.Maybe Core.Text,
    -- | This value determines which version of AWS Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
    -- in the developer guide.
    glueVersion :: Core.Maybe Core.Text,
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
    workerType :: Core.Maybe WorkerType,
    -- | A description of the transform. The default is an empty string.
    description :: Core.Maybe Core.Text,
    -- | The configuration parameters that are specific to the transform type
    -- (algorithm) used. Conditionally dependent on the transform type.
    parameters :: Core.Maybe TransformParameters,
    -- | The maximum number of times to retry a task for this transform after a
    -- task run fails.
    maxRetries :: Core.Maybe Core.Int,
    -- | A unique identifier that was generated when the transform was created.
    transformId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxCapacity', 'updateMLTransform_maxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
--
-- 'numberOfWorkers', 'updateMLTransform_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
--
-- 'name', 'updateMLTransform_name' - The unique name that you gave the transform when you created it.
--
-- 'role'', 'updateMLTransform_role' - The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions.
--
-- 'glueVersion', 'updateMLTransform_glueVersion' - This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
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
-- 'parameters', 'updateMLTransform_parameters' - The configuration parameters that are specific to the transform type
-- (algorithm) used. Conditionally dependent on the transform type.
--
-- 'maxRetries', 'updateMLTransform_maxRetries' - The maximum number of times to retry a task for this transform after a
-- task run fails.
--
-- 'transformId', 'updateMLTransform_transformId' - A unique identifier that was generated when the transform was created.
newUpdateMLTransform ::
  -- | 'transformId'
  Core.Text ->
  UpdateMLTransform
newUpdateMLTransform pTransformId_ =
  UpdateMLTransform'
    { timeout = Core.Nothing,
      maxCapacity = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      name = Core.Nothing,
      role' = Core.Nothing,
      glueVersion = Core.Nothing,
      workerType = Core.Nothing,
      description = Core.Nothing,
      parameters = Core.Nothing,
      maxRetries = Core.Nothing,
      transformId = pTransformId_
    }

-- | The timeout for a task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
updateMLTransform_timeout :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Natural)
updateMLTransform_timeout = Lens.lens (\UpdateMLTransform' {timeout} -> timeout) (\s@UpdateMLTransform' {} a -> s {timeout = a} :: UpdateMLTransform)

-- | The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
updateMLTransform_maxCapacity :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Double)
updateMLTransform_maxCapacity = Lens.lens (\UpdateMLTransform' {maxCapacity} -> maxCapacity) (\s@UpdateMLTransform' {} a -> s {maxCapacity = a} :: UpdateMLTransform)

-- | The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
updateMLTransform_numberOfWorkers :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Int)
updateMLTransform_numberOfWorkers = Lens.lens (\UpdateMLTransform' {numberOfWorkers} -> numberOfWorkers) (\s@UpdateMLTransform' {} a -> s {numberOfWorkers = a} :: UpdateMLTransform)

-- | The unique name that you gave the transform when you created it.
updateMLTransform_name :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Text)
updateMLTransform_name = Lens.lens (\UpdateMLTransform' {name} -> name) (\s@UpdateMLTransform' {} a -> s {name = a} :: UpdateMLTransform)

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions.
updateMLTransform_role :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Text)
updateMLTransform_role = Lens.lens (\UpdateMLTransform' {role'} -> role') (\s@UpdateMLTransform' {} a -> s {role' = a} :: UpdateMLTransform)

-- | This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
updateMLTransform_glueVersion :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Text)
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
updateMLTransform_workerType :: Lens.Lens' UpdateMLTransform (Core.Maybe WorkerType)
updateMLTransform_workerType = Lens.lens (\UpdateMLTransform' {workerType} -> workerType) (\s@UpdateMLTransform' {} a -> s {workerType = a} :: UpdateMLTransform)

-- | A description of the transform. The default is an empty string.
updateMLTransform_description :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Text)
updateMLTransform_description = Lens.lens (\UpdateMLTransform' {description} -> description) (\s@UpdateMLTransform' {} a -> s {description = a} :: UpdateMLTransform)

-- | The configuration parameters that are specific to the transform type
-- (algorithm) used. Conditionally dependent on the transform type.
updateMLTransform_parameters :: Lens.Lens' UpdateMLTransform (Core.Maybe TransformParameters)
updateMLTransform_parameters = Lens.lens (\UpdateMLTransform' {parameters} -> parameters) (\s@UpdateMLTransform' {} a -> s {parameters = a} :: UpdateMLTransform)

-- | The maximum number of times to retry a task for this transform after a
-- task run fails.
updateMLTransform_maxRetries :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Int)
updateMLTransform_maxRetries = Lens.lens (\UpdateMLTransform' {maxRetries} -> maxRetries) (\s@UpdateMLTransform' {} a -> s {maxRetries = a} :: UpdateMLTransform)

-- | A unique identifier that was generated when the transform was created.
updateMLTransform_transformId :: Lens.Lens' UpdateMLTransform Core.Text
updateMLTransform_transformId = Lens.lens (\UpdateMLTransform' {transformId} -> transformId) (\s@UpdateMLTransform' {} a -> s {transformId = a} :: UpdateMLTransform)

instance Core.AWSRequest UpdateMLTransform where
  type
    AWSResponse UpdateMLTransform =
      UpdateMLTransformResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMLTransformResponse'
            Core.<$> (x Core..?> "TransformId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateMLTransform

instance Core.NFData UpdateMLTransform

instance Core.ToHeaders UpdateMLTransform where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateMLTransform" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateMLTransform where
  toJSON UpdateMLTransform' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Timeout" Core..=) Core.<$> timeout,
            ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("Name" Core..=) Core.<$> name,
            ("Role" Core..=) Core.<$> role',
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("WorkerType" Core..=) Core.<$> workerType,
            ("Description" Core..=) Core.<$> description,
            ("Parameters" Core..=) Core.<$> parameters,
            ("MaxRetries" Core..=) Core.<$> maxRetries,
            Core.Just ("TransformId" Core..= transformId)
          ]
      )

instance Core.ToPath UpdateMLTransform where
  toPath = Core.const "/"

instance Core.ToQuery UpdateMLTransform where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateMLTransformResponse' smart constructor.
data UpdateMLTransformResponse = UpdateMLTransformResponse'
  { -- | The unique identifier for the transform that was updated.
    transformId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateMLTransformResponse
newUpdateMLTransformResponse pHttpStatus_ =
  UpdateMLTransformResponse'
    { transformId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the transform that was updated.
updateMLTransformResponse_transformId :: Lens.Lens' UpdateMLTransformResponse (Core.Maybe Core.Text)
updateMLTransformResponse_transformId = Lens.lens (\UpdateMLTransformResponse' {transformId} -> transformId) (\s@UpdateMLTransformResponse' {} a -> s {transformId = a} :: UpdateMLTransformResponse)

-- | The response's http status code.
updateMLTransformResponse_httpStatus :: Lens.Lens' UpdateMLTransformResponse Core.Int
updateMLTransformResponse_httpStatus = Lens.lens (\UpdateMLTransformResponse' {httpStatus} -> httpStatus) (\s@UpdateMLTransformResponse' {} a -> s {httpStatus = a} :: UpdateMLTransformResponse)

instance Core.NFData UpdateMLTransformResponse
