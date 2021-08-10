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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMLTransform' smart constructor.
data GetMLTransform = GetMLTransform'
  { -- | The unique identifier of the transform, generated at the time that the
    -- transform was created.
    transformId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetMLTransform
newGetMLTransform pTransformId_ =
  GetMLTransform' {transformId = pTransformId_}

-- | The unique identifier of the transform, generated at the time that the
-- transform was created.
getMLTransform_transformId :: Lens.Lens' GetMLTransform Prelude.Text
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
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "TransformId")
            Prelude.<*> (x Core..?> "Schema" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "CreatedOn")
            Prelude.<*> ( x Core..?> "InputRecordTables"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "TransformEncryption")
            Prelude.<*> (x Core..?> "Timeout")
            Prelude.<*> (x Core..?> "MaxCapacity")
            Prelude.<*> (x Core..?> "LastModifiedOn")
            Prelude.<*> (x Core..?> "NumberOfWorkers")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Role")
            Prelude.<*> (x Core..?> "GlueVersion")
            Prelude.<*> (x Core..?> "EvaluationMetrics")
            Prelude.<*> (x Core..?> "WorkerType")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "LabelCount")
            Prelude.<*> (x Core..?> "Parameters")
            Prelude.<*> (x Core..?> "MaxRetries")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMLTransform

instance Prelude.NFData GetMLTransform

instance Core.ToHeaders GetMLTransform where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetMLTransform" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMLTransform where
  toJSON GetMLTransform' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TransformId" Core..= transformId)]
      )

instance Core.ToPath GetMLTransform where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMLTransform where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMLTransformResponse' smart constructor.
data GetMLTransformResponse = GetMLTransformResponse'
  { -- | The last known status of the transform (to indicate whether it can be
    -- used or not). One of \"NOT_READY\", \"READY\", or \"DELETING\".
    status :: Prelude.Maybe TransformStatusType,
    -- | The unique identifier of the transform, generated at the time that the
    -- transform was created.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The @Map\<Column, Type>@ object that represents the schema that this
    -- transform accepts. Has an upper bound of 100 columns.
    schema :: Prelude.Maybe [SchemaColumn],
    -- | The date and time when the transform was created.
    createdOn :: Prelude.Maybe Core.POSIX,
    -- | A list of AWS Glue table definitions used by the transform.
    inputRecordTables :: Prelude.Maybe [GlueTable],
    -- | The encryption-at-rest settings of the transform that apply to accessing
    -- user data. Machine learning transforms can access user data encrypted in
    -- Amazon S3 using KMS.
    transformEncryption :: Prelude.Maybe TransformEncryption,
    -- | The timeout for a task run for this transform in minutes. This is the
    -- maximum time that a task run for this transform can consume resources
    -- before it is terminated and enters @TIMEOUT@ status. The default is
    -- 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The number of AWS Glue data processing units (DPUs) that are allocated
    -- to task runs for this transform. You can allocate from 2 to 100 DPUs;
    -- the default is 10. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@, the
    -- @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The date and time when the transform was last modified.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- this task runs.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The unique name given to the transform when it was created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required
    -- permissions.
    role' :: Prelude.Maybe Prelude.Text,
    -- | This value determines which version of AWS Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The latest evaluation metrics.
    evaluationMetrics :: Prelude.Maybe EvaluationMetrics,
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
    -- | A description of the transform.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of labels available for this transform.
    labelCount :: Prelude.Maybe Prelude.Int,
    -- | The configuration parameters that are specific to the algorithm used.
    parameters :: Prelude.Maybe TransformParameters,
    -- | The maximum number of times to retry a task for this transform after a
    -- task run fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetMLTransformResponse
newGetMLTransformResponse pHttpStatus_ =
  GetMLTransformResponse'
    { status = Prelude.Nothing,
      transformId = Prelude.Nothing,
      schema = Prelude.Nothing,
      createdOn = Prelude.Nothing,
      inputRecordTables = Prelude.Nothing,
      transformEncryption = Prelude.Nothing,
      timeout = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      name = Prelude.Nothing,
      role' = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      evaluationMetrics = Prelude.Nothing,
      workerType = Prelude.Nothing,
      description = Prelude.Nothing,
      labelCount = Prelude.Nothing,
      parameters = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The last known status of the transform (to indicate whether it can be
-- used or not). One of \"NOT_READY\", \"READY\", or \"DELETING\".
getMLTransformResponse_status :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe TransformStatusType)
getMLTransformResponse_status = Lens.lens (\GetMLTransformResponse' {status} -> status) (\s@GetMLTransformResponse' {} a -> s {status = a} :: GetMLTransformResponse)

-- | The unique identifier of the transform, generated at the time that the
-- transform was created.
getMLTransformResponse_transformId :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Text)
getMLTransformResponse_transformId = Lens.lens (\GetMLTransformResponse' {transformId} -> transformId) (\s@GetMLTransformResponse' {} a -> s {transformId = a} :: GetMLTransformResponse)

-- | The @Map\<Column, Type>@ object that represents the schema that this
-- transform accepts. Has an upper bound of 100 columns.
getMLTransformResponse_schema :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe [SchemaColumn])
getMLTransformResponse_schema = Lens.lens (\GetMLTransformResponse' {schema} -> schema) (\s@GetMLTransformResponse' {} a -> s {schema = a} :: GetMLTransformResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The date and time when the transform was created.
getMLTransformResponse_createdOn :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.UTCTime)
getMLTransformResponse_createdOn = Lens.lens (\GetMLTransformResponse' {createdOn} -> createdOn) (\s@GetMLTransformResponse' {} a -> s {createdOn = a} :: GetMLTransformResponse) Prelude.. Lens.mapping Core._Time

-- | A list of AWS Glue table definitions used by the transform.
getMLTransformResponse_inputRecordTables :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe [GlueTable])
getMLTransformResponse_inputRecordTables = Lens.lens (\GetMLTransformResponse' {inputRecordTables} -> inputRecordTables) (\s@GetMLTransformResponse' {} a -> s {inputRecordTables = a} :: GetMLTransformResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
getMLTransformResponse_transformEncryption :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe TransformEncryption)
getMLTransformResponse_transformEncryption = Lens.lens (\GetMLTransformResponse' {transformEncryption} -> transformEncryption) (\s@GetMLTransformResponse' {} a -> s {transformEncryption = a} :: GetMLTransformResponse)

-- | The timeout for a task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
getMLTransformResponse_timeout :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Natural)
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
getMLTransformResponse_maxCapacity :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Double)
getMLTransformResponse_maxCapacity = Lens.lens (\GetMLTransformResponse' {maxCapacity} -> maxCapacity) (\s@GetMLTransformResponse' {} a -> s {maxCapacity = a} :: GetMLTransformResponse)

-- | The date and time when the transform was last modified.
getMLTransformResponse_lastModifiedOn :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.UTCTime)
getMLTransformResponse_lastModifiedOn = Lens.lens (\GetMLTransformResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetMLTransformResponse' {} a -> s {lastModifiedOn = a} :: GetMLTransformResponse) Prelude.. Lens.mapping Core._Time

-- | The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
getMLTransformResponse_numberOfWorkers :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Int)
getMLTransformResponse_numberOfWorkers = Lens.lens (\GetMLTransformResponse' {numberOfWorkers} -> numberOfWorkers) (\s@GetMLTransformResponse' {} a -> s {numberOfWorkers = a} :: GetMLTransformResponse)

-- | The unique name given to the transform when it was created.
getMLTransformResponse_name :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Text)
getMLTransformResponse_name = Lens.lens (\GetMLTransformResponse' {name} -> name) (\s@GetMLTransformResponse' {} a -> s {name = a} :: GetMLTransformResponse)

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions.
getMLTransformResponse_role :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Text)
getMLTransformResponse_role = Lens.lens (\GetMLTransformResponse' {role'} -> role') (\s@GetMLTransformResponse' {} a -> s {role' = a} :: GetMLTransformResponse)

-- | This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
getMLTransformResponse_glueVersion :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Text)
getMLTransformResponse_glueVersion = Lens.lens (\GetMLTransformResponse' {glueVersion} -> glueVersion) (\s@GetMLTransformResponse' {} a -> s {glueVersion = a} :: GetMLTransformResponse)

-- | The latest evaluation metrics.
getMLTransformResponse_evaluationMetrics :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe EvaluationMetrics)
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
getMLTransformResponse_workerType :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe WorkerType)
getMLTransformResponse_workerType = Lens.lens (\GetMLTransformResponse' {workerType} -> workerType) (\s@GetMLTransformResponse' {} a -> s {workerType = a} :: GetMLTransformResponse)

-- | A description of the transform.
getMLTransformResponse_description :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Text)
getMLTransformResponse_description = Lens.lens (\GetMLTransformResponse' {description} -> description) (\s@GetMLTransformResponse' {} a -> s {description = a} :: GetMLTransformResponse)

-- | The number of labels available for this transform.
getMLTransformResponse_labelCount :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Int)
getMLTransformResponse_labelCount = Lens.lens (\GetMLTransformResponse' {labelCount} -> labelCount) (\s@GetMLTransformResponse' {} a -> s {labelCount = a} :: GetMLTransformResponse)

-- | The configuration parameters that are specific to the algorithm used.
getMLTransformResponse_parameters :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe TransformParameters)
getMLTransformResponse_parameters = Lens.lens (\GetMLTransformResponse' {parameters} -> parameters) (\s@GetMLTransformResponse' {} a -> s {parameters = a} :: GetMLTransformResponse)

-- | The maximum number of times to retry a task for this transform after a
-- task run fails.
getMLTransformResponse_maxRetries :: Lens.Lens' GetMLTransformResponse (Prelude.Maybe Prelude.Int)
getMLTransformResponse_maxRetries = Lens.lens (\GetMLTransformResponse' {maxRetries} -> maxRetries) (\s@GetMLTransformResponse' {} a -> s {maxRetries = a} :: GetMLTransformResponse)

-- | The response's http status code.
getMLTransformResponse_httpStatus :: Lens.Lens' GetMLTransformResponse Prelude.Int
getMLTransformResponse_httpStatus = Lens.lens (\GetMLTransformResponse' {httpStatus} -> httpStatus) (\s@GetMLTransformResponse' {} a -> s {httpStatus = a} :: GetMLTransformResponse)

instance Prelude.NFData GetMLTransformResponse
