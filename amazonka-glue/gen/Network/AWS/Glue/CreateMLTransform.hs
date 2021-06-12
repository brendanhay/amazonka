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
-- Module      : Network.AWS.Glue.CreateMLTransform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Glue machine learning transform. This operation creates
-- the transform and all the necessary parameters to train it.
--
-- Call this operation as the first step in the process of using a machine
-- learning transform (such as the @FindMatches@ transform) for
-- deduplicating data. You can provide an optional @Description@, in
-- addition to the parameters that you want to use for your algorithm.
--
-- You must also specify certain parameters for the tasks that AWS Glue
-- runs on your behalf as part of learning from your data and creating a
-- high-quality machine learning transform. These parameters include
-- @Role@, and optionally, @AllocatedCapacity@, @Timeout@, and
-- @MaxRetries@. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-jobs-job.html Jobs>.
module Network.AWS.Glue.CreateMLTransform
  ( -- * Creating a Request
    CreateMLTransform (..),
    newCreateMLTransform,

    -- * Request Lenses
    createMLTransform_transformEncryption,
    createMLTransform_timeout,
    createMLTransform_maxCapacity,
    createMLTransform_numberOfWorkers,
    createMLTransform_glueVersion,
    createMLTransform_tags,
    createMLTransform_workerType,
    createMLTransform_description,
    createMLTransform_maxRetries,
    createMLTransform_name,
    createMLTransform_inputRecordTables,
    createMLTransform_parameters,
    createMLTransform_role,

    -- * Destructuring the Response
    CreateMLTransformResponse (..),
    newCreateMLTransformResponse,

    -- * Response Lenses
    createMLTransformResponse_transformId,
    createMLTransformResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateMLTransform' smart constructor.
data CreateMLTransform = CreateMLTransform'
  { -- | The encryption-at-rest settings of the transform that apply to accessing
    -- user data. Machine learning transforms can access user data encrypted in
    -- Amazon S3 using KMS.
    transformEncryption :: Core.Maybe TransformEncryption,
    -- | The timeout of the task run for this transform in minutes. This is the
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
    -- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
    -- @WorkerType@.
    --
    -- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
    --     @MaxCapacity@ cannot be set.
    --
    -- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
    --     @WorkerType@ can be set.
    --
    -- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
    --     versa).
    --
    -- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@, the
    -- @MaxCapacity@ field is set automatically and becomes read-only.
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@, the
    -- @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Core.Maybe Core.Double,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- this task runs.
    --
    -- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
    -- versa).
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | This value determines which version of AWS Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
    -- in the developer guide.
    glueVersion :: Core.Maybe Core.Text,
    -- | The tags to use with this machine learning transform. You may use tags
    -- to limit access to the machine learning transform. For more information
    -- about tags in AWS Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
    -- in the developer guide.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
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
    --
    -- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
    -- @WorkerType@.
    --
    -- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
    --     @MaxCapacity@ cannot be set.
    --
    -- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
    --     @WorkerType@ can be set.
    --
    -- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
    --     versa).
    --
    -- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
    workerType :: Core.Maybe WorkerType,
    -- | A description of the machine learning transform that is being defined.
    -- The default is an empty string.
    description :: Core.Maybe Core.Text,
    -- | The maximum number of times to retry a task for this transform after a
    -- task run fails.
    maxRetries :: Core.Maybe Core.Int,
    -- | The unique name that you give the transform when you create it.
    name :: Core.Text,
    -- | A list of AWS Glue table definitions used by the transform.
    inputRecordTables :: [GlueTable],
    -- | The algorithmic parameters that are specific to the transform type used.
    -- Conditionally dependent on the transform type.
    parameters :: TransformParameters,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required
    -- permissions. The required permissions include both AWS Glue service role
    -- permissions to AWS Glue resources, and Amazon S3 permissions required by
    -- the transform.
    --
    -- -   This role needs AWS Glue service role permissions to allow access to
    --     resources in AWS Glue. See
    --     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue>.
    --
    -- -   This role needs permission to your Amazon Simple Storage Service
    --     (Amazon S3) sources, targets, temporary directory, scripts, and any
    --     libraries used by the task run for this transform.
    role' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMLTransform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformEncryption', 'createMLTransform_transformEncryption' - The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
--
-- 'timeout', 'createMLTransform_timeout' - The timeout of the task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
--
-- 'maxCapacity', 'createMLTransform_maxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
--
-- 'numberOfWorkers', 'createMLTransform_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
-- versa).
--
-- 'glueVersion', 'createMLTransform_glueVersion' - This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
--
-- 'tags', 'createMLTransform_tags' - The tags to use with this machine learning transform. You may use tags
-- to limit access to the machine learning transform. For more information
-- about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
--
-- 'workerType', 'createMLTransform_workerType' - The type of predefined worker that is allocated when this task runs.
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
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
-- 'description', 'createMLTransform_description' - A description of the machine learning transform that is being defined.
-- The default is an empty string.
--
-- 'maxRetries', 'createMLTransform_maxRetries' - The maximum number of times to retry a task for this transform after a
-- task run fails.
--
-- 'name', 'createMLTransform_name' - The unique name that you give the transform when you create it.
--
-- 'inputRecordTables', 'createMLTransform_inputRecordTables' - A list of AWS Glue table definitions used by the transform.
--
-- 'parameters', 'createMLTransform_parameters' - The algorithmic parameters that are specific to the transform type used.
-- Conditionally dependent on the transform type.
--
-- 'role'', 'createMLTransform_role' - The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions. The required permissions include both AWS Glue service role
-- permissions to AWS Glue resources, and Amazon S3 permissions required by
-- the transform.
--
-- -   This role needs AWS Glue service role permissions to allow access to
--     resources in AWS Glue. See
--     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue>.
--
-- -   This role needs permission to your Amazon Simple Storage Service
--     (Amazon S3) sources, targets, temporary directory, scripts, and any
--     libraries used by the task run for this transform.
newCreateMLTransform ::
  -- | 'name'
  Core.Text ->
  -- | 'parameters'
  TransformParameters ->
  -- | 'role''
  Core.Text ->
  CreateMLTransform
newCreateMLTransform pName_ pParameters_ pRole_ =
  CreateMLTransform'
    { transformEncryption =
        Core.Nothing,
      timeout = Core.Nothing,
      maxCapacity = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      glueVersion = Core.Nothing,
      tags = Core.Nothing,
      workerType = Core.Nothing,
      description = Core.Nothing,
      maxRetries = Core.Nothing,
      name = pName_,
      inputRecordTables = Core.mempty,
      parameters = pParameters_,
      role' = pRole_
    }

-- | The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
createMLTransform_transformEncryption :: Lens.Lens' CreateMLTransform (Core.Maybe TransformEncryption)
createMLTransform_transformEncryption = Lens.lens (\CreateMLTransform' {transformEncryption} -> transformEncryption) (\s@CreateMLTransform' {} a -> s {transformEncryption = a} :: CreateMLTransform)

-- | The timeout of the task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
createMLTransform_timeout :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Natural)
createMLTransform_timeout = Lens.lens (\CreateMLTransform' {timeout} -> timeout) (\s@CreateMLTransform' {} a -> s {timeout = a} :: CreateMLTransform)

-- | The number of AWS Glue data processing units (DPUs) that are allocated
-- to task runs for this transform. You can allocate from 2 to 100 DPUs;
-- the default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page>.
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
--
-- When the @WorkerType@ field is set to a value other than @Standard@, the
-- @MaxCapacity@ field is set automatically and becomes read-only.
createMLTransform_maxCapacity :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Double)
createMLTransform_maxCapacity = Lens.lens (\CreateMLTransform' {maxCapacity} -> maxCapacity) (\s@CreateMLTransform' {} a -> s {maxCapacity = a} :: CreateMLTransform)

-- | The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
-- versa).
createMLTransform_numberOfWorkers :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Int)
createMLTransform_numberOfWorkers = Lens.lens (\CreateMLTransform' {numberOfWorkers} -> numberOfWorkers) (\s@CreateMLTransform' {} a -> s {numberOfWorkers = a} :: CreateMLTransform)

-- | This value determines which version of AWS Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions>
-- in the developer guide.
createMLTransform_glueVersion :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Text)
createMLTransform_glueVersion = Lens.lens (\CreateMLTransform' {glueVersion} -> glueVersion) (\s@CreateMLTransform' {} a -> s {glueVersion = a} :: CreateMLTransform)

-- | The tags to use with this machine learning transform. You may use tags
-- to limit access to the machine learning transform. For more information
-- about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
createMLTransform_tags :: Lens.Lens' CreateMLTransform (Core.Maybe (Core.HashMap Core.Text Core.Text))
createMLTransform_tags = Lens.lens (\CreateMLTransform' {tags} -> tags) (\s@CreateMLTransform' {} a -> s {tags = a} :: CreateMLTransform) Core.. Lens.mapping Lens._Coerce

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
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and
-- @WorkerType@.
--
-- -   If either @NumberOfWorkers@ or @WorkerType@ is set, then
--     @MaxCapacity@ cannot be set.
--
-- -   If @MaxCapacity@ is set then neither @NumberOfWorkers@ or
--     @WorkerType@ can be set.
--
-- -   If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
--     versa).
--
-- -   @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
createMLTransform_workerType :: Lens.Lens' CreateMLTransform (Core.Maybe WorkerType)
createMLTransform_workerType = Lens.lens (\CreateMLTransform' {workerType} -> workerType) (\s@CreateMLTransform' {} a -> s {workerType = a} :: CreateMLTransform)

-- | A description of the machine learning transform that is being defined.
-- The default is an empty string.
createMLTransform_description :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Text)
createMLTransform_description = Lens.lens (\CreateMLTransform' {description} -> description) (\s@CreateMLTransform' {} a -> s {description = a} :: CreateMLTransform)

-- | The maximum number of times to retry a task for this transform after a
-- task run fails.
createMLTransform_maxRetries :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Int)
createMLTransform_maxRetries = Lens.lens (\CreateMLTransform' {maxRetries} -> maxRetries) (\s@CreateMLTransform' {} a -> s {maxRetries = a} :: CreateMLTransform)

-- | The unique name that you give the transform when you create it.
createMLTransform_name :: Lens.Lens' CreateMLTransform Core.Text
createMLTransform_name = Lens.lens (\CreateMLTransform' {name} -> name) (\s@CreateMLTransform' {} a -> s {name = a} :: CreateMLTransform)

-- | A list of AWS Glue table definitions used by the transform.
createMLTransform_inputRecordTables :: Lens.Lens' CreateMLTransform [GlueTable]
createMLTransform_inputRecordTables = Lens.lens (\CreateMLTransform' {inputRecordTables} -> inputRecordTables) (\s@CreateMLTransform' {} a -> s {inputRecordTables = a} :: CreateMLTransform) Core.. Lens._Coerce

-- | The algorithmic parameters that are specific to the transform type used.
-- Conditionally dependent on the transform type.
createMLTransform_parameters :: Lens.Lens' CreateMLTransform TransformParameters
createMLTransform_parameters = Lens.lens (\CreateMLTransform' {parameters} -> parameters) (\s@CreateMLTransform' {} a -> s {parameters = a} :: CreateMLTransform)

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions. The required permissions include both AWS Glue service role
-- permissions to AWS Glue resources, and Amazon S3 permissions required by
-- the transform.
--
-- -   This role needs AWS Glue service role permissions to allow access to
--     resources in AWS Glue. See
--     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue>.
--
-- -   This role needs permission to your Amazon Simple Storage Service
--     (Amazon S3) sources, targets, temporary directory, scripts, and any
--     libraries used by the task run for this transform.
createMLTransform_role :: Lens.Lens' CreateMLTransform Core.Text
createMLTransform_role = Lens.lens (\CreateMLTransform' {role'} -> role') (\s@CreateMLTransform' {} a -> s {role' = a} :: CreateMLTransform)

instance Core.AWSRequest CreateMLTransform where
  type
    AWSResponse CreateMLTransform =
      CreateMLTransformResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMLTransformResponse'
            Core.<$> (x Core..?> "TransformId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateMLTransform

instance Core.NFData CreateMLTransform

instance Core.ToHeaders CreateMLTransform where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateMLTransform" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateMLTransform where
  toJSON CreateMLTransform' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TransformEncryption" Core..=)
              Core.<$> transformEncryption,
            ("Timeout" Core..=) Core.<$> timeout,
            ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("Tags" Core..=) Core.<$> tags,
            ("WorkerType" Core..=) Core.<$> workerType,
            ("Description" Core..=) Core.<$> description,
            ("MaxRetries" Core..=) Core.<$> maxRetries,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("InputRecordTables" Core..= inputRecordTables),
            Core.Just ("Parameters" Core..= parameters),
            Core.Just ("Role" Core..= role')
          ]
      )

instance Core.ToPath CreateMLTransform where
  toPath = Core.const "/"

instance Core.ToQuery CreateMLTransform where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateMLTransformResponse' smart constructor.
data CreateMLTransformResponse = CreateMLTransformResponse'
  { -- | A unique identifier that is generated for the transform.
    transformId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMLTransformResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'createMLTransformResponse_transformId' - A unique identifier that is generated for the transform.
--
-- 'httpStatus', 'createMLTransformResponse_httpStatus' - The response's http status code.
newCreateMLTransformResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateMLTransformResponse
newCreateMLTransformResponse pHttpStatus_ =
  CreateMLTransformResponse'
    { transformId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier that is generated for the transform.
createMLTransformResponse_transformId :: Lens.Lens' CreateMLTransformResponse (Core.Maybe Core.Text)
createMLTransformResponse_transformId = Lens.lens (\CreateMLTransformResponse' {transformId} -> transformId) (\s@CreateMLTransformResponse' {} a -> s {transformId = a} :: CreateMLTransformResponse)

-- | The response's http status code.
createMLTransformResponse_httpStatus :: Lens.Lens' CreateMLTransformResponse Core.Int
createMLTransformResponse_httpStatus = Lens.lens (\CreateMLTransformResponse' {httpStatus} -> httpStatus) (\s@CreateMLTransformResponse' {} a -> s {httpStatus = a} :: CreateMLTransformResponse)

instance Core.NFData CreateMLTransformResponse
