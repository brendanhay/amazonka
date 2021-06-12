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
-- Module      : Network.AWS.Glue.GetMLTransform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an AWS Glue machine learning transform artifact and all its
-- corresponding metadata. Machine learning transforms are a special type
-- of transform that use machine learning to learn the details of the
-- transformation to be performed by learning from examples provided by
-- humans. These transformations are then saved by AWS Glue. You can
-- retrieve their metadata by calling @GetMLTransform@.
module Network.AWS.Glue.GetMLTransform
  ( -- * Creating a Request
    GetMLTransform (..),
    newGetMLTransform,

    -- * Request Lenses
    getMLTransform_transformId,

    -- * Destructuring the Response
    GetMLTransformResponse (..),
    newGetMLTransformResponse,

    -- * Response Lenses
    getMLTransformResponse_status,
    getMLTransformResponse_transformId,
    getMLTransformResponse_schema,
    getMLTransformResponse_createdOn,
    getMLTransformResponse_inputRecordTables,
    getMLTransformResponse_transformEncryption,
    getMLTransformResponse_timeout,
    getMLTransformResponse_maxCapacity,
    getMLTransformResponse_lastModifiedOn,
    getMLTransformResponse_numberOfWorkers,
    getMLTransformResponse_name,
    getMLTransformResponse_role,
    getMLTransformResponse_glueVersion,
    getMLTransformResponse_evaluationMetrics,
    getMLTransformResponse_workerType,
    getMLTransformResponse_description,
    getMLTransformResponse_labelCount,
    getMLTransformResponse_parameters,
    getMLTransformResponse_maxRetries,
    getMLTransformResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMLTransform' smart constructor.
data GetMLTransform = GetMLTransform'
  { -- | The unique identifier of the transform, generated at the time that the
    -- transform was created.
    transformId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMLTransform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'getMLTransform_transformId' - The unique identifier of the transform, generated at the time that the
-- transform was created.
newGetMLTransform ::
  -- | 'transformId'
  Core.Text ->
  GetMLTransform
newGetMLTransform pTransformId_ =
  GetMLTransform' {transformId = pTransformId_}

-- | The unique identifier of the transform, generated at the time that the
-- transform was created.
getMLTransform_transformId :: Lens.Lens' GetMLTransform Core.Text
getMLTransform_transformId = Lens.lens (\GetMLTransform' {transformId} -> transformId) (\s@GetMLTransform' {} a -> s {transformId = a} :: GetMLTransform)

instance Core.AWSRequest GetMLTransform where
  type
    AWSResponse GetMLTransform =
      GetMLTransformResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLTransformResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "TransformId")
            Core.<*> (x Core..?> "Schema" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "CreatedOn")
            Core.<*> (x Core..?> "InputRecordTables" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TransformEncryption")
            Core.<*> (x Core..?> "Timeout")
            Core.<*> (x Core..?> "MaxCapacity")
            Core.<*> (x Core..?> "LastModifiedOn")
            Core.<*> (x Core..?> "NumberOfWorkers")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "Role")
            Core.<*> (x Core..?> "GlueVersion")
            Core.<*> (x Core..?> "EvaluationMetrics")
            Core.<*> (x Core..?> "WorkerType")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "LabelCount")
            Core.<*> (x Core..?> "Parameters")
            Core.<*> (x Core..?> "MaxRetries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMLTransform

instance Core.NFData GetMLTransform

instance Core.ToHeaders GetMLTransform where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetMLTransform" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMLTransform where
  toJSON GetMLTransform' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TransformId" Core..= transformId)]
      )

instance Core.ToPath GetMLTransform where
  toPath = Core.const "/"

instance Core.ToQuery GetMLTransform where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMLTransformResponse' smart constructor.
data GetMLTransformResponse = GetMLTransformResponse'
  { -- | The last known status of the transform (to indicate whether it can be
    -- used or not). One of \"NOT_READY\", \"READY\", or \"DELETING\".
    status :: Core.Maybe TransformStatusType,
    -- | The unique identifier of the transform, generated at the time that the
    -- transform was created.
    transformId :: Core.Maybe Core.Text,
    -- | The @Map\<Column, Type>@ object that represents the schema that this
    -- transform accepts. Has an upper bound of 100 columns.
    schema :: Core.Maybe [SchemaColumn],
    -- | The date and time when the transform was created.
    createdOn :: Core.Maybe Core.POSIX,
    -- | A list of AWS Glue table definitions used by the transform.
    inputRecordTables :: Core.Maybe [GlueTable],
    -- | The encryption-at-rest settings of the transform that apply to accessing
    -- user data. Machine learning transforms can access user data encrypted in
    -- Amazon S3 using KMS.
    transformEncryption :: Core.Maybe TransformEncryption,
    -- | The timeout for a task run for this transform in minutes. This is the
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
    -- | The date and time when the transform was last modified.
    lastModifiedOn :: Core.Maybe Core.POSIX,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- this task runs.
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | The unique name given to the transform when it was created.
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
    -- | The latest evaluation metrics.
    evaluationMetrics :: Core.Maybe EvaluationMetrics,
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
    -- | A description of the transform.
    description :: Core.Maybe Core.Text,
    -- | The number of labels available for this transform.
    labelCount :: Core.Maybe Core.Int,
    -- | The configuration parameters that are specific to the algorithm used.
    parameters :: Core.Maybe TransformParameters,
    -- | The maximum number of times to retry a task for this transform after a
    -- task run fails.
    maxRetries :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMLTransformResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getMLTransformResponse_status' - The last known status of the transform (to indicate whether it can be
-- used or not). One of \"NOT_READY\", \"READY\", or \"DELETING\".
--
-- 'transformId', 'getMLTransformResponse_transformId' - The unique identifier of the transform, generated at the time that the
-- transform was created.
--
-- 'schema', 'getMLTransformResponse_schema' - The @Map\<Column, Type>@ object that represents the schema that this
-- transform accepts. Has an upper bound of 100 columns.
--
-- 'createdOn', 'getMLTransformResponse_createdOn' - The date and time when the transform was created.
--
-- 'inputRecordTables', 'getMLTransformResponse_inputRecordTables' - A list of AWS Glue table definitions used by the transform.
--
-- 'transformEncryption', 'getMLTransformResponse_transformEncryption' - The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
--
-- 'timeout', 'getMLTransformResponse_timeout' - The timeout for a task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
--
-- 'maxCapacity', 'getMLTransformResponse_maxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
--
-- 'lastModifiedOn', 'getMLTransformResponse_lastModifiedOn' - The date and time when the transform was last modified.
--
-- 'numberOfWorkers', 'getMLTransformResponse_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
--
-- 'name', 'getMLTransformResponse_name' - The unique name given to the transform when it was created.
--
-- 'role'', 'getMLTransformResponse_role' - The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions.
--
-- 'glueVersion', 'getMLTransformResponse_glueVersion' - This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
--
-- 'evaluationMetrics', 'getMLTransformResponse_evaluationMetrics' - The latest evaluation metrics.
--
-- 'workerType', 'getMLTransformResponse_workerType' - The type of predefined worker that is allocated when this task runs.
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
-- 'description', 'getMLTransformResponse_description' - A description of the transform.
--
-- 'labelCount', 'getMLTransformResponse_labelCount' - The number of labels available for this transform.
--
-- 'parameters', 'getMLTransformResponse_parameters' - The configuration parameters that are specific to the algorithm used.
--
-- 'maxRetries', 'getMLTransformResponse_maxRetries' - The maximum number of times to retry a task for this transform after a
-- task run fails.
--
-- 'httpStatus', 'getMLTransformResponse_httpStatus' - The response's http status code.
newGetMLTransformResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMLTransformResponse
newGetMLTransformResponse pHttpStatus_ =
  GetMLTransformResponse'
    { status = Core.Nothing,
      transformId = Core.Nothing,
      schema = Core.Nothing,
      createdOn = Core.Nothing,
      inputRecordTables = Core.Nothing,
      transformEncryption = Core.Nothing,
      timeout = Core.Nothing,
      maxCapacity = Core.Nothing,
      lastModifiedOn = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      name = Core.Nothing,
      role' = Core.Nothing,
      glueVersion = Core.Nothing,
      evaluationMetrics = Core.Nothing,
      workerType = Core.Nothing,
      description = Core.Nothing,
      labelCount = Core.Nothing,
      parameters = Core.Nothing,
      maxRetries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The last known status of the transform (to indicate whether it can be
-- used or not). One of \"NOT_READY\", \"READY\", or \"DELETING\".
getMLTransformResponse_status :: Lens.Lens' GetMLTransformResponse (Core.Maybe TransformStatusType)
getMLTransformResponse_status = Lens.lens (\GetMLTransformResponse' {status} -> status) (\s@GetMLTransformResponse' {} a -> s {status = a} :: GetMLTransformResponse)

-- | The unique identifier of the transform, generated at the time that the
-- transform was created.
getMLTransformResponse_transformId :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Text)
getMLTransformResponse_transformId = Lens.lens (\GetMLTransformResponse' {transformId} -> transformId) (\s@GetMLTransformResponse' {} a -> s {transformId = a} :: GetMLTransformResponse)

-- | The @Map\<Column, Type>@ object that represents the schema that this
-- transform accepts. Has an upper bound of 100 columns.
getMLTransformResponse_schema :: Lens.Lens' GetMLTransformResponse (Core.Maybe [SchemaColumn])
getMLTransformResponse_schema = Lens.lens (\GetMLTransformResponse' {schema} -> schema) (\s@GetMLTransformResponse' {} a -> s {schema = a} :: GetMLTransformResponse) Core.. Lens.mapping Lens._Coerce

-- | The date and time when the transform was created.
getMLTransformResponse_createdOn :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.UTCTime)
getMLTransformResponse_createdOn = Lens.lens (\GetMLTransformResponse' {createdOn} -> createdOn) (\s@GetMLTransformResponse' {} a -> s {createdOn = a} :: GetMLTransformResponse) Core.. Lens.mapping Core._Time

-- | A list of AWS Glue table definitions used by the transform.
getMLTransformResponse_inputRecordTables :: Lens.Lens' GetMLTransformResponse (Core.Maybe [GlueTable])
getMLTransformResponse_inputRecordTables = Lens.lens (\GetMLTransformResponse' {inputRecordTables} -> inputRecordTables) (\s@GetMLTransformResponse' {} a -> s {inputRecordTables = a} :: GetMLTransformResponse) Core.. Lens.mapping Lens._Coerce

-- | The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
getMLTransformResponse_transformEncryption :: Lens.Lens' GetMLTransformResponse (Core.Maybe TransformEncryption)
getMLTransformResponse_transformEncryption = Lens.lens (\GetMLTransformResponse' {transformEncryption} -> transformEncryption) (\s@GetMLTransformResponse' {} a -> s {transformEncryption = a} :: GetMLTransformResponse)

-- | The timeout for a task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
getMLTransformResponse_timeout :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Natural)
getMLTransformResponse_timeout = Lens.lens (\GetMLTransformResponse' {timeout} -> timeout) (\s@GetMLTransformResponse' {} a -> s {timeout = a} :: GetMLTransformResponse)

-- | The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
getMLTransformResponse_maxCapacity :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Double)
getMLTransformResponse_maxCapacity = Lens.lens (\GetMLTransformResponse' {maxCapacity} -> maxCapacity) (\s@GetMLTransformResponse' {} a -> s {maxCapacity = a} :: GetMLTransformResponse)

-- | The date and time when the transform was last modified.
getMLTransformResponse_lastModifiedOn :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.UTCTime)
getMLTransformResponse_lastModifiedOn = Lens.lens (\GetMLTransformResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetMLTransformResponse' {} a -> s {lastModifiedOn = a} :: GetMLTransformResponse) Core.. Lens.mapping Core._Time

-- | The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
getMLTransformResponse_numberOfWorkers :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Int)
getMLTransformResponse_numberOfWorkers = Lens.lens (\GetMLTransformResponse' {numberOfWorkers} -> numberOfWorkers) (\s@GetMLTransformResponse' {} a -> s {numberOfWorkers = a} :: GetMLTransformResponse)

-- | The unique name given to the transform when it was created.
getMLTransformResponse_name :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Text)
getMLTransformResponse_name = Lens.lens (\GetMLTransformResponse' {name} -> name) (\s@GetMLTransformResponse' {} a -> s {name = a} :: GetMLTransformResponse)

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions.
getMLTransformResponse_role :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Text)
getMLTransformResponse_role = Lens.lens (\GetMLTransformResponse' {role'} -> role') (\s@GetMLTransformResponse' {} a -> s {role' = a} :: GetMLTransformResponse)

-- | This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
getMLTransformResponse_glueVersion :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Text)
getMLTransformResponse_glueVersion = Lens.lens (\GetMLTransformResponse' {glueVersion} -> glueVersion) (\s@GetMLTransformResponse' {} a -> s {glueVersion = a} :: GetMLTransformResponse)

-- | The latest evaluation metrics.
getMLTransformResponse_evaluationMetrics :: Lens.Lens' GetMLTransformResponse (Core.Maybe EvaluationMetrics)
getMLTransformResponse_evaluationMetrics = Lens.lens (\GetMLTransformResponse' {evaluationMetrics} -> evaluationMetrics) (\s@GetMLTransformResponse' {} a -> s {evaluationMetrics = a} :: GetMLTransformResponse)

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
getMLTransformResponse_workerType :: Lens.Lens' GetMLTransformResponse (Core.Maybe WorkerType)
getMLTransformResponse_workerType = Lens.lens (\GetMLTransformResponse' {workerType} -> workerType) (\s@GetMLTransformResponse' {} a -> s {workerType = a} :: GetMLTransformResponse)

-- | A description of the transform.
getMLTransformResponse_description :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Text)
getMLTransformResponse_description = Lens.lens (\GetMLTransformResponse' {description} -> description) (\s@GetMLTransformResponse' {} a -> s {description = a} :: GetMLTransformResponse)

-- | The number of labels available for this transform.
getMLTransformResponse_labelCount :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Int)
getMLTransformResponse_labelCount = Lens.lens (\GetMLTransformResponse' {labelCount} -> labelCount) (\s@GetMLTransformResponse' {} a -> s {labelCount = a} :: GetMLTransformResponse)

-- | The configuration parameters that are specific to the algorithm used.
getMLTransformResponse_parameters :: Lens.Lens' GetMLTransformResponse (Core.Maybe TransformParameters)
getMLTransformResponse_parameters = Lens.lens (\GetMLTransformResponse' {parameters} -> parameters) (\s@GetMLTransformResponse' {} a -> s {parameters = a} :: GetMLTransformResponse)

-- | The maximum number of times to retry a task for this transform after a
-- task run fails.
getMLTransformResponse_maxRetries :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Int)
getMLTransformResponse_maxRetries = Lens.lens (\GetMLTransformResponse' {maxRetries} -> maxRetries) (\s@GetMLTransformResponse' {} a -> s {maxRetries = a} :: GetMLTransformResponse)

-- | The response's http status code.
getMLTransformResponse_httpStatus :: Lens.Lens' GetMLTransformResponse Core.Int
getMLTransformResponse_httpStatus = Lens.lens (\GetMLTransformResponse' {httpStatus} -> httpStatus) (\s@GetMLTransformResponse' {} a -> s {httpStatus = a} :: GetMLTransformResponse)

instance Core.NFData GetMLTransformResponse
