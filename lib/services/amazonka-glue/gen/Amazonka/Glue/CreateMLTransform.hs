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
-- Module      : Amazonka.Glue.CreateMLTransform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Glue machine learning transform. This operation creates the
-- transform and all the necessary parameters to train it.
--
-- Call this operation as the first step in the process of using a machine
-- learning transform (such as the @FindMatches@ transform) for
-- deduplicating data. You can provide an optional @Description@, in
-- addition to the parameters that you want to use for your algorithm.
--
-- You must also specify certain parameters for the tasks that Glue runs on
-- your behalf as part of learning from your data and creating a
-- high-quality machine learning transform. These parameters include
-- @Role@, and optionally, @AllocatedCapacity@, @Timeout@, and
-- @MaxRetries@. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-jobs-job.html Jobs>.
module Amazonka.Glue.CreateMLTransform
  ( -- * Creating a Request
    CreateMLTransform (..),
    newCreateMLTransform,

    -- * Request Lenses
    createMLTransform_description,
    createMLTransform_glueVersion,
    createMLTransform_maxCapacity,
    createMLTransform_maxRetries,
    createMLTransform_numberOfWorkers,
    createMLTransform_tags,
    createMLTransform_timeout,
    createMLTransform_transformEncryption,
    createMLTransform_workerType,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMLTransform' smart constructor.
data CreateMLTransform = CreateMLTransform'
  { -- | A description of the machine learning transform that is being defined.
    -- The default is an empty string.
    description :: Prelude.Maybe Prelude.Text,
    -- | This value determines which version of Glue this machine learning
    -- transform is compatible with. Glue 1.0 is recommended for most
    -- customers. If the value is not set, the Glue compatibility defaults to
    -- Glue 0.9. For more information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
    -- in the developer guide.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of Glue data processing units (DPUs) that are allocated to
    -- task runs for this transform. You can allocate from 2 to 100 DPUs; the
    -- default is 10. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
    -- information, see the
    -- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
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
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The maximum number of times to retry a task for this transform after a
    -- task run fails.
    maxRetries :: Prelude.Maybe Prelude.Int,
    -- | The number of workers of a defined @workerType@ that are allocated when
    -- this task runs.
    --
    -- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
    -- versa).
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The tags to use with this machine learning transform. You may use tags
    -- to limit access to the machine learning transform. For more information
    -- about tags in Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
    -- in the developer guide.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The timeout of the task run for this transform in minutes. This is the
    -- maximum time that a task run for this transform can consume resources
    -- before it is terminated and enters @TIMEOUT@ status. The default is
    -- 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The encryption-at-rest settings of the transform that apply to accessing
    -- user data. Machine learning transforms can access user data encrypted in
    -- Amazon S3 using KMS.
    transformEncryption :: Prelude.Maybe TransformEncryption,
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
    workerType :: Prelude.Maybe WorkerType,
    -- | The unique name that you give the transform when you create it.
    name :: Prelude.Text,
    -- | A list of Glue table definitions used by the transform.
    inputRecordTables :: [GlueTable],
    -- | The algorithmic parameters that are specific to the transform type used.
    -- Conditionally dependent on the transform type.
    parameters :: TransformParameters,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required
    -- permissions. The required permissions include both Glue service role
    -- permissions to Glue resources, and Amazon S3 permissions required by the
    -- transform.
    --
    -- -   This role needs Glue service role permissions to allow access to
    --     resources in Glue. See
    --     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access Glue>.
    --
    -- -   This role needs permission to your Amazon Simple Storage Service
    --     (Amazon S3) sources, targets, temporary directory, scripts, and any
    --     libraries used by the task run for this transform.
    role' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMLTransform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createMLTransform_description' - A description of the machine learning transform that is being defined.
-- The default is an empty string.
--
-- 'glueVersion', 'createMLTransform_glueVersion' - This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
--
-- 'maxCapacity', 'createMLTransform_maxCapacity' - The number of Glue data processing units (DPUs) that are allocated to
-- task runs for this transform. You can allocate from 2 to 100 DPUs; the
-- default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
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
-- 'maxRetries', 'createMLTransform_maxRetries' - The maximum number of times to retry a task for this transform after a
-- task run fails.
--
-- 'numberOfWorkers', 'createMLTransform_numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
-- versa).
--
-- 'tags', 'createMLTransform_tags' - The tags to use with this machine learning transform. You may use tags
-- to limit access to the machine learning transform. For more information
-- about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
--
-- 'timeout', 'createMLTransform_timeout' - The timeout of the task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
--
-- 'transformEncryption', 'createMLTransform_transformEncryption' - The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
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
-- 'name', 'createMLTransform_name' - The unique name that you give the transform when you create it.
--
-- 'inputRecordTables', 'createMLTransform_inputRecordTables' - A list of Glue table definitions used by the transform.
--
-- 'parameters', 'createMLTransform_parameters' - The algorithmic parameters that are specific to the transform type used.
-- Conditionally dependent on the transform type.
--
-- 'role'', 'createMLTransform_role' - The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions. The required permissions include both Glue service role
-- permissions to Glue resources, and Amazon S3 permissions required by the
-- transform.
--
-- -   This role needs Glue service role permissions to allow access to
--     resources in Glue. See
--     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access Glue>.
--
-- -   This role needs permission to your Amazon Simple Storage Service
--     (Amazon S3) sources, targets, temporary directory, scripts, and any
--     libraries used by the task run for this transform.
newCreateMLTransform ::
  -- | 'name'
  Prelude.Text ->
  -- | 'parameters'
  TransformParameters ->
  -- | 'role''
  Prelude.Text ->
  CreateMLTransform
newCreateMLTransform pName_ pParameters_ pRole_ =
  CreateMLTransform'
    { description = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      maxRetries = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      transformEncryption = Prelude.Nothing,
      workerType = Prelude.Nothing,
      name = pName_,
      inputRecordTables = Prelude.mempty,
      parameters = pParameters_,
      role' = pRole_
    }

-- | A description of the machine learning transform that is being defined.
-- The default is an empty string.
createMLTransform_description :: Lens.Lens' CreateMLTransform (Prelude.Maybe Prelude.Text)
createMLTransform_description = Lens.lens (\CreateMLTransform' {description} -> description) (\s@CreateMLTransform' {} a -> s {description = a} :: CreateMLTransform)

-- | This value determines which version of Glue this machine learning
-- transform is compatible with. Glue 1.0 is recommended for most
-- customers. If the value is not set, the Glue compatibility defaults to
-- Glue 0.9. For more information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions Glue Versions>
-- in the developer guide.
createMLTransform_glueVersion :: Lens.Lens' CreateMLTransform (Prelude.Maybe Prelude.Text)
createMLTransform_glueVersion = Lens.lens (\CreateMLTransform' {glueVersion} -> glueVersion) (\s@CreateMLTransform' {} a -> s {glueVersion = a} :: CreateMLTransform)

-- | The number of Glue data processing units (DPUs) that are allocated to
-- task runs for this transform. You can allocate from 2 to 100 DPUs; the
-- default is 10. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB of memory. For more
-- information, see the
-- <https://aws.amazon.com/glue/pricing/ Glue pricing page>.
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
createMLTransform_maxCapacity :: Lens.Lens' CreateMLTransform (Prelude.Maybe Prelude.Double)
createMLTransform_maxCapacity = Lens.lens (\CreateMLTransform' {maxCapacity} -> maxCapacity) (\s@CreateMLTransform' {} a -> s {maxCapacity = a} :: CreateMLTransform)

-- | The maximum number of times to retry a task for this transform after a
-- task run fails.
createMLTransform_maxRetries :: Lens.Lens' CreateMLTransform (Prelude.Maybe Prelude.Int)
createMLTransform_maxRetries = Lens.lens (\CreateMLTransform' {maxRetries} -> maxRetries) (\s@CreateMLTransform' {} a -> s {maxRetries = a} :: CreateMLTransform)

-- | The number of workers of a defined @workerType@ that are allocated when
-- this task runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice
-- versa).
createMLTransform_numberOfWorkers :: Lens.Lens' CreateMLTransform (Prelude.Maybe Prelude.Int)
createMLTransform_numberOfWorkers = Lens.lens (\CreateMLTransform' {numberOfWorkers} -> numberOfWorkers) (\s@CreateMLTransform' {} a -> s {numberOfWorkers = a} :: CreateMLTransform)

-- | The tags to use with this machine learning transform. You may use tags
-- to limit access to the machine learning transform. For more information
-- about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
createMLTransform_tags :: Lens.Lens' CreateMLTransform (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMLTransform_tags = Lens.lens (\CreateMLTransform' {tags} -> tags) (\s@CreateMLTransform' {} a -> s {tags = a} :: CreateMLTransform) Prelude.. Lens.mapping Lens.coerced

-- | The timeout of the task run for this transform in minutes. This is the
-- maximum time that a task run for this transform can consume resources
-- before it is terminated and enters @TIMEOUT@ status. The default is
-- 2,880 minutes (48 hours).
createMLTransform_timeout :: Lens.Lens' CreateMLTransform (Prelude.Maybe Prelude.Natural)
createMLTransform_timeout = Lens.lens (\CreateMLTransform' {timeout} -> timeout) (\s@CreateMLTransform' {} a -> s {timeout = a} :: CreateMLTransform)

-- | The encryption-at-rest settings of the transform that apply to accessing
-- user data. Machine learning transforms can access user data encrypted in
-- Amazon S3 using KMS.
createMLTransform_transformEncryption :: Lens.Lens' CreateMLTransform (Prelude.Maybe TransformEncryption)
createMLTransform_transformEncryption = Lens.lens (\CreateMLTransform' {transformEncryption} -> transformEncryption) (\s@CreateMLTransform' {} a -> s {transformEncryption = a} :: CreateMLTransform)

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
createMLTransform_workerType :: Lens.Lens' CreateMLTransform (Prelude.Maybe WorkerType)
createMLTransform_workerType = Lens.lens (\CreateMLTransform' {workerType} -> workerType) (\s@CreateMLTransform' {} a -> s {workerType = a} :: CreateMLTransform)

-- | The unique name that you give the transform when you create it.
createMLTransform_name :: Lens.Lens' CreateMLTransform Prelude.Text
createMLTransform_name = Lens.lens (\CreateMLTransform' {name} -> name) (\s@CreateMLTransform' {} a -> s {name = a} :: CreateMLTransform)

-- | A list of Glue table definitions used by the transform.
createMLTransform_inputRecordTables :: Lens.Lens' CreateMLTransform [GlueTable]
createMLTransform_inputRecordTables = Lens.lens (\CreateMLTransform' {inputRecordTables} -> inputRecordTables) (\s@CreateMLTransform' {} a -> s {inputRecordTables = a} :: CreateMLTransform) Prelude.. Lens.coerced

-- | The algorithmic parameters that are specific to the transform type used.
-- Conditionally dependent on the transform type.
createMLTransform_parameters :: Lens.Lens' CreateMLTransform TransformParameters
createMLTransform_parameters = Lens.lens (\CreateMLTransform' {parameters} -> parameters) (\s@CreateMLTransform' {} a -> s {parameters = a} :: CreateMLTransform)

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required
-- permissions. The required permissions include both Glue service role
-- permissions to Glue resources, and Amazon S3 permissions required by the
-- transform.
--
-- -   This role needs Glue service role permissions to allow access to
--     resources in Glue. See
--     <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access Glue>.
--
-- -   This role needs permission to your Amazon Simple Storage Service
--     (Amazon S3) sources, targets, temporary directory, scripts, and any
--     libraries used by the task run for this transform.
createMLTransform_role :: Lens.Lens' CreateMLTransform Prelude.Text
createMLTransform_role = Lens.lens (\CreateMLTransform' {role'} -> role') (\s@CreateMLTransform' {} a -> s {role' = a} :: CreateMLTransform)

instance Core.AWSRequest CreateMLTransform where
  type
    AWSResponse CreateMLTransform =
      CreateMLTransformResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMLTransformResponse'
            Prelude.<$> (x Data..?> "TransformId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMLTransform where
  hashWithSalt _salt CreateMLTransform' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` transformEncryption
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputRecordTables
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` role'

instance Prelude.NFData CreateMLTransform where
  rnf CreateMLTransform' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf transformEncryption
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputRecordTables
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf role'

instance Data.ToHeaders CreateMLTransform where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateMLTransform" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMLTransform where
  toJSON CreateMLTransform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("GlueVersion" Data..=) Prelude.<$> glueVersion,
            ("MaxCapacity" Data..=) Prelude.<$> maxCapacity,
            ("MaxRetries" Data..=) Prelude.<$> maxRetries,
            ("NumberOfWorkers" Data..=)
              Prelude.<$> numberOfWorkers,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("TransformEncryption" Data..=)
              Prelude.<$> transformEncryption,
            ("WorkerType" Data..=) Prelude.<$> workerType,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("InputRecordTables" Data..= inputRecordTables),
            Prelude.Just ("Parameters" Data..= parameters),
            Prelude.Just ("Role" Data..= role')
          ]
      )

instance Data.ToPath CreateMLTransform where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateMLTransform where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMLTransformResponse' smart constructor.
data CreateMLTransformResponse = CreateMLTransformResponse'
  { -- | A unique identifier that is generated for the transform.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateMLTransformResponse
newCreateMLTransformResponse pHttpStatus_ =
  CreateMLTransformResponse'
    { transformId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier that is generated for the transform.
createMLTransformResponse_transformId :: Lens.Lens' CreateMLTransformResponse (Prelude.Maybe Prelude.Text)
createMLTransformResponse_transformId = Lens.lens (\CreateMLTransformResponse' {transformId} -> transformId) (\s@CreateMLTransformResponse' {} a -> s {transformId = a} :: CreateMLTransformResponse)

-- | The response's http status code.
createMLTransformResponse_httpStatus :: Lens.Lens' CreateMLTransformResponse Prelude.Int
createMLTransformResponse_httpStatus = Lens.lens (\CreateMLTransformResponse' {httpStatus} -> httpStatus) (\s@CreateMLTransformResponse' {} a -> s {httpStatus = a} :: CreateMLTransformResponse)

instance Prelude.NFData CreateMLTransformResponse where
  rnf CreateMLTransformResponse' {..} =
    Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf httpStatus
