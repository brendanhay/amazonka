{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Glue machine learning transform. This operation creates the transform and all the necessary parameters to train it.
--
-- Call this operation as the first step in the process of using a machine learning transform (such as the @FindMatches@ transform) for deduplicating data. You can provide an optional @Description@ , in addition to the parameters that you want to use for your algorithm.
-- You must also specify certain parameters for the tasks that AWS Glue runs on your behalf as part of learning from your data and creating a high-quality machine learning transform. These parameters include @Role@ , and optionally, @AllocatedCapacity@ , @Timeout@ , and @MaxRetries@ . For more information, see <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-jobs-job.html Jobs> .
module Network.AWS.Glue.CreateMLTransform
  ( -- * Creating a request
    CreateMLTransform (..),
    mkCreateMLTransform,

    -- ** Request lenses
    cmltNumberOfWorkers,
    cmltWorkerType,
    cmltInputRecordTables,
    cmltGlueVersion,
    cmltRole,
    cmltName,
    cmltParameters,
    cmltMaxRetries,
    cmltMaxCapacity,
    cmltTimeout,
    cmltTransformEncryption,
    cmltDescription,
    cmltTags,

    -- * Destructuring the response
    CreateMLTransformResponse (..),
    mkCreateMLTransformResponse,

    -- ** Response lenses
    cmltrsTransformId,
    cmltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateMLTransform' smart constructor.
data CreateMLTransform = CreateMLTransform'
  { -- | The number of workers of a defined @workerType@ that are allocated when this task runs.
    --
    -- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
    numberOfWorkers :: Lude.Maybe Lude.Int,
    -- | The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.
    --
    --
    --     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
    --
    --
    --     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
    --
    --
    --     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
    --
    --
    -- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and @WorkerType@ .
    --
    --     * If either @NumberOfWorkers@ or @WorkerType@ is set, then @MaxCapacity@ cannot be set.
    --
    --
    --     * If @MaxCapacity@ is set then neither @NumberOfWorkers@ or @WorkerType@ can be set.
    --
    --
    --     * If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
    --
    --
    --     * @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
    workerType :: Lude.Maybe WorkerType,
    -- | A list of AWS Glue table definitions used by the transform.
    inputRecordTables :: [GlueTable],
    -- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
    glueVersion :: Lude.Maybe Lude.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions. The required permissions include both AWS Glue service role permissions to AWS Glue resources, and Amazon S3 permissions required by the transform.
    --
    --
    --     * This role needs AWS Glue service role permissions to allow access to resources in AWS Glue. See <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue> .
    --
    --
    --     * This role needs permission to your Amazon Simple Storage Service (Amazon S3) sources, targets, temporary directory, scripts, and any libraries used by the task run for this transform.
    role' :: Lude.Text,
    -- | The unique name that you give the transform when you create it.
    name :: Lude.Text,
    -- | The algorithmic parameters that are specific to the transform type used. Conditionally dependent on the transform type.
    parameters :: TransformParameters,
    -- | The maximum number of times to retry a task for this transform after a task run fails.
    maxRetries :: Lude.Maybe Lude.Int,
    -- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    --
    -- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and @WorkerType@ .
    --
    --     * If either @NumberOfWorkers@ or @WorkerType@ is set, then @MaxCapacity@ cannot be set.
    --
    --
    --     * If @MaxCapacity@ is set then neither @NumberOfWorkers@ or @WorkerType@ can be set.
    --
    --
    --     * If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
    --
    --
    --     * @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
    --
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
    -- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Lude.Maybe Lude.Double,
    -- | The timeout of the task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
    timeout :: Lude.Maybe Lude.Natural,
    -- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
    transformEncryption :: Lude.Maybe TransformEncryption,
    -- | A description of the machine learning transform that is being defined. The default is an empty string.
    description :: Lude.Maybe Lude.Text,
    -- | The tags to use with this machine learning transform. You may use tags to limit access to the machine learning transform. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMLTransform' with the minimum fields required to make a request.
--
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
-- * 'workerType' - The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
--
--
--     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and @WorkerType@ .
--
--     * If either @NumberOfWorkers@ or @WorkerType@ is set, then @MaxCapacity@ cannot be set.
--
--
--     * If @MaxCapacity@ is set then neither @NumberOfWorkers@ or @WorkerType@ can be set.
--
--
--     * If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
--
--
--     * @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
--
-- * 'inputRecordTables' - A list of AWS Glue table definitions used by the transform.
-- * 'glueVersion' - This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
-- * 'role'' - The name or Amazon Resource Name (ARN) of the IAM role with the required permissions. The required permissions include both AWS Glue service role permissions to AWS Glue resources, and Amazon S3 permissions required by the transform.
--
--
--     * This role needs AWS Glue service role permissions to allow access to resources in AWS Glue. See <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue> .
--
--
--     * This role needs permission to your Amazon Simple Storage Service (Amazon S3) sources, targets, temporary directory, scripts, and any libraries used by the task run for this transform.
--
--
-- * 'name' - The unique name that you give the transform when you create it.
-- * 'parameters' - The algorithmic parameters that are specific to the transform type used. Conditionally dependent on the transform type.
-- * 'maxRetries' - The maximum number of times to retry a task for this transform after a task run fails.
-- * 'maxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and @WorkerType@ .
--
--     * If either @NumberOfWorkers@ or @WorkerType@ is set, then @MaxCapacity@ cannot be set.
--
--
--     * If @MaxCapacity@ is set then neither @NumberOfWorkers@ or @WorkerType@ can be set.
--
--
--     * If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
--
--
--     * @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
-- * 'timeout' - The timeout of the task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
-- * 'transformEncryption' - The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
-- * 'description' - A description of the machine learning transform that is being defined. The default is an empty string.
-- * 'tags' - The tags to use with this machine learning transform. You may use tags to limit access to the machine learning transform. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
mkCreateMLTransform ::
  -- | 'role''
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'parameters'
  TransformParameters ->
  CreateMLTransform
mkCreateMLTransform pRole_ pName_ pParameters_ =
  CreateMLTransform'
    { numberOfWorkers = Lude.Nothing,
      workerType = Lude.Nothing,
      inputRecordTables = Lude.mempty,
      glueVersion = Lude.Nothing,
      role' = pRole_,
      name = pName_,
      parameters = pParameters_,
      maxRetries = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      transformEncryption = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltNumberOfWorkers :: Lens.Lens' CreateMLTransform (Lude.Maybe Lude.Int)
cmltNumberOfWorkers = Lens.lens (numberOfWorkers :: CreateMLTransform -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: CreateMLTransform)
{-# DEPRECATED cmltNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
--
--
--     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and @WorkerType@ .
--
--     * If either @NumberOfWorkers@ or @WorkerType@ is set, then @MaxCapacity@ cannot be set.
--
--
--     * If @MaxCapacity@ is set then neither @NumberOfWorkers@ or @WorkerType@ can be set.
--
--
--     * If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
--
--
--     * @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
--
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltWorkerType :: Lens.Lens' CreateMLTransform (Lude.Maybe WorkerType)
cmltWorkerType = Lens.lens (workerType :: CreateMLTransform -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: CreateMLTransform)
{-# DEPRECATED cmltWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | A list of AWS Glue table definitions used by the transform.
--
-- /Note:/ Consider using 'inputRecordTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltInputRecordTables :: Lens.Lens' CreateMLTransform [GlueTable]
cmltInputRecordTables = Lens.lens (inputRecordTables :: CreateMLTransform -> [GlueTable]) (\s a -> s {inputRecordTables = a} :: CreateMLTransform)
{-# DEPRECATED cmltInputRecordTables "Use generic-lens or generic-optics with 'inputRecordTables' instead." #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltGlueVersion :: Lens.Lens' CreateMLTransform (Lude.Maybe Lude.Text)
cmltGlueVersion = Lens.lens (glueVersion :: CreateMLTransform -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: CreateMLTransform)
{-# DEPRECATED cmltGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions. The required permissions include both AWS Glue service role permissions to AWS Glue resources, and Amazon S3 permissions required by the transform.
--
--
--     * This role needs AWS Glue service role permissions to allow access to resources in AWS Glue. See <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue> .
--
--
--     * This role needs permission to your Amazon Simple Storage Service (Amazon S3) sources, targets, temporary directory, scripts, and any libraries used by the task run for this transform.
--
--
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltRole :: Lens.Lens' CreateMLTransform Lude.Text
cmltRole = Lens.lens (role' :: CreateMLTransform -> Lude.Text) (\s a -> s {role' = a} :: CreateMLTransform)
{-# DEPRECATED cmltRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The unique name that you give the transform when you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltName :: Lens.Lens' CreateMLTransform Lude.Text
cmltName = Lens.lens (name :: CreateMLTransform -> Lude.Text) (\s a -> s {name = a} :: CreateMLTransform)
{-# DEPRECATED cmltName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The algorithmic parameters that are specific to the transform type used. Conditionally dependent on the transform type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltParameters :: Lens.Lens' CreateMLTransform TransformParameters
cmltParameters = Lens.lens (parameters :: CreateMLTransform -> TransformParameters) (\s a -> s {parameters = a} :: CreateMLTransform)
{-# DEPRECATED cmltParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The maximum number of times to retry a task for this transform after a task run fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltMaxRetries :: Lens.Lens' CreateMLTransform (Lude.Maybe Lude.Int)
cmltMaxRetries = Lens.lens (maxRetries :: CreateMLTransform -> Lude.Maybe Lude.Int) (\s a -> s {maxRetries = a} :: CreateMLTransform)
{-# DEPRECATED cmltMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- @MaxCapacity@ is a mutually exclusive option with @NumberOfWorkers@ and @WorkerType@ .
--
--     * If either @NumberOfWorkers@ or @WorkerType@ is set, then @MaxCapacity@ cannot be set.
--
--
--     * If @MaxCapacity@ is set then neither @NumberOfWorkers@ or @WorkerType@ can be set.
--
--
--     * If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
--
--
--     * @MaxCapacity@ and @NumberOfWorkers@ must both be at least 1.
--
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltMaxCapacity :: Lens.Lens' CreateMLTransform (Lude.Maybe Lude.Double)
cmltMaxCapacity = Lens.lens (maxCapacity :: CreateMLTransform -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: CreateMLTransform)
{-# DEPRECATED cmltMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The timeout of the task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltTimeout :: Lens.Lens' CreateMLTransform (Lude.Maybe Lude.Natural)
cmltTimeout = Lens.lens (timeout :: CreateMLTransform -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: CreateMLTransform)
{-# DEPRECATED cmltTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
-- /Note:/ Consider using 'transformEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltTransformEncryption :: Lens.Lens' CreateMLTransform (Lude.Maybe TransformEncryption)
cmltTransformEncryption = Lens.lens (transformEncryption :: CreateMLTransform -> Lude.Maybe TransformEncryption) (\s a -> s {transformEncryption = a} :: CreateMLTransform)
{-# DEPRECATED cmltTransformEncryption "Use generic-lens or generic-optics with 'transformEncryption' instead." #-}

-- | A description of the machine learning transform that is being defined. The default is an empty string.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltDescription :: Lens.Lens' CreateMLTransform (Lude.Maybe Lude.Text)
cmltDescription = Lens.lens (description :: CreateMLTransform -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateMLTransform)
{-# DEPRECATED cmltDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to use with this machine learning transform. You may use tags to limit access to the machine learning transform. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltTags :: Lens.Lens' CreateMLTransform (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cmltTags = Lens.lens (tags :: CreateMLTransform -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateMLTransform)
{-# DEPRECATED cmltTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateMLTransform where
  type Rs CreateMLTransform = CreateMLTransformResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMLTransformResponse'
            Lude.<$> (x Lude..?> "TransformId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMLTransform where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateMLTransform" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMLTransform where
  toJSON CreateMLTransform' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberOfWorkers" Lude..=) Lude.<$> numberOfWorkers,
            ("WorkerType" Lude..=) Lude.<$> workerType,
            Lude.Just ("InputRecordTables" Lude..= inputRecordTables),
            ("GlueVersion" Lude..=) Lude.<$> glueVersion,
            Lude.Just ("Role" Lude..= role'),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Parameters" Lude..= parameters),
            ("MaxRetries" Lude..=) Lude.<$> maxRetries,
            ("MaxCapacity" Lude..=) Lude.<$> maxCapacity,
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("TransformEncryption" Lude..=) Lude.<$> transformEncryption,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateMLTransform where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMLTransform where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateMLTransformResponse' smart constructor.
data CreateMLTransformResponse = CreateMLTransformResponse'
  { -- | A unique identifier that is generated for the transform.
    transformId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMLTransformResponse' with the minimum fields required to make a request.
--
-- * 'transformId' - A unique identifier that is generated for the transform.
-- * 'responseStatus' - The response status code.
mkCreateMLTransformResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMLTransformResponse
mkCreateMLTransformResponse pResponseStatus_ =
  CreateMLTransformResponse'
    { transformId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier that is generated for the transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrsTransformId :: Lens.Lens' CreateMLTransformResponse (Lude.Maybe Lude.Text)
cmltrsTransformId = Lens.lens (transformId :: CreateMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {transformId = a} :: CreateMLTransformResponse)
{-# DEPRECATED cmltrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrsResponseStatus :: Lens.Lens' CreateMLTransformResponse Lude.Int
cmltrsResponseStatus = Lens.lens (responseStatus :: CreateMLTransformResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMLTransformResponse)
{-# DEPRECATED cmltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
