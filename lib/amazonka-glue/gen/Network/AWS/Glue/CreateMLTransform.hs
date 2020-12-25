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
    cmltName,
    cmltInputRecordTables,
    cmltParameters,
    cmltRole,
    cmltDescription,
    cmltGlueVersion,
    cmltMaxCapacity,
    cmltMaxRetries,
    cmltNumberOfWorkers,
    cmltTags,
    cmltTimeout,
    cmltTransformEncryption,
    cmltWorkerType,

    -- * Destructuring the response
    CreateMLTransformResponse (..),
    mkCreateMLTransformResponse,

    -- ** Response lenses
    cmltrrsTransformId,
    cmltrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateMLTransform' smart constructor.
data CreateMLTransform = CreateMLTransform'
  { -- | The unique name that you give the transform when you create it.
    name :: Types.NameString,
    -- | A list of AWS Glue table definitions used by the transform.
    inputRecordTables :: [Types.GlueTable],
    -- | The algorithmic parameters that are specific to the transform type used. Conditionally dependent on the transform type.
    parameters :: Types.TransformParameters,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions. The required permissions include both AWS Glue service role permissions to AWS Glue resources, and Amazon S3 permissions required by the transform.
    --
    --
    --     * This role needs AWS Glue service role permissions to allow access to resources in AWS Glue. See <https://docs.aws.amazon.com/glue/latest/dg/attach-policy-iam-user.html Attach a Policy to IAM Users That Access AWS Glue> .
    --
    --
    --     * This role needs permission to your Amazon Simple Storage Service (Amazon S3) sources, targets, temporary directory, scripts, and any libraries used by the task run for this transform.
    role' :: Types.Role,
    -- | A description of the machine learning transform that is being defined. The default is an empty string.
    description :: Core.Maybe Types.DescriptionString,
    -- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
    glueVersion :: Core.Maybe Types.GlueVersionString,
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
    maxCapacity :: Core.Maybe Core.Double,
    -- | The maximum number of times to retry a task for this transform after a task run fails.
    maxRetries :: Core.Maybe Core.Int,
    -- | The number of workers of a defined @workerType@ that are allocated when this task runs.
    --
    -- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | The tags to use with this machine learning transform. You may use tags to limit access to the machine learning transform. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The timeout of the task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
    timeout :: Core.Maybe Core.Natural,
    -- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
    transformEncryption :: Core.Maybe Types.TransformEncryption,
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
    workerType :: Core.Maybe Types.WorkerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMLTransform' value with any optional fields omitted.
mkCreateMLTransform ::
  -- | 'name'
  Types.NameString ->
  -- | 'parameters'
  Types.TransformParameters ->
  -- | 'role\''
  Types.Role ->
  CreateMLTransform
mkCreateMLTransform name parameters role' =
  CreateMLTransform'
    { name,
      inputRecordTables = Core.mempty,
      parameters,
      role',
      description = Core.Nothing,
      glueVersion = Core.Nothing,
      maxCapacity = Core.Nothing,
      maxRetries = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      tags = Core.Nothing,
      timeout = Core.Nothing,
      transformEncryption = Core.Nothing,
      workerType = Core.Nothing
    }

-- | The unique name that you give the transform when you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltName :: Lens.Lens' CreateMLTransform Types.NameString
cmltName = Lens.field @"name"
{-# DEPRECATED cmltName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of AWS Glue table definitions used by the transform.
--
-- /Note:/ Consider using 'inputRecordTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltInputRecordTables :: Lens.Lens' CreateMLTransform [Types.GlueTable]
cmltInputRecordTables = Lens.field @"inputRecordTables"
{-# DEPRECATED cmltInputRecordTables "Use generic-lens or generic-optics with 'inputRecordTables' instead." #-}

-- | The algorithmic parameters that are specific to the transform type used. Conditionally dependent on the transform type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltParameters :: Lens.Lens' CreateMLTransform Types.TransformParameters
cmltParameters = Lens.field @"parameters"
{-# DEPRECATED cmltParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

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
cmltRole :: Lens.Lens' CreateMLTransform Types.Role
cmltRole = Lens.field @"role'"
{-# DEPRECATED cmltRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | A description of the machine learning transform that is being defined. The default is an empty string.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltDescription :: Lens.Lens' CreateMLTransform (Core.Maybe Types.DescriptionString)
cmltDescription = Lens.field @"description"
{-# DEPRECATED cmltDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltGlueVersion :: Lens.Lens' CreateMLTransform (Core.Maybe Types.GlueVersionString)
cmltGlueVersion = Lens.field @"glueVersion"
{-# DEPRECATED cmltGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

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
cmltMaxCapacity :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Double)
cmltMaxCapacity = Lens.field @"maxCapacity"
{-# DEPRECATED cmltMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The maximum number of times to retry a task for this transform after a task run fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltMaxRetries :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Int)
cmltMaxRetries = Lens.field @"maxRetries"
{-# DEPRECATED cmltMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- If @WorkerType@ is set, then @NumberOfWorkers@ is required (and vice versa).
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltNumberOfWorkers :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Int)
cmltNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# DEPRECATED cmltNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The tags to use with this machine learning transform. You may use tags to limit access to the machine learning transform. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltTags :: Lens.Lens' CreateMLTransform (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cmltTags = Lens.field @"tags"
{-# DEPRECATED cmltTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The timeout of the task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltTimeout :: Lens.Lens' CreateMLTransform (Core.Maybe Core.Natural)
cmltTimeout = Lens.field @"timeout"
{-# DEPRECATED cmltTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
-- /Note:/ Consider using 'transformEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltTransformEncryption :: Lens.Lens' CreateMLTransform (Core.Maybe Types.TransformEncryption)
cmltTransformEncryption = Lens.field @"transformEncryption"
{-# DEPRECATED cmltTransformEncryption "Use generic-lens or generic-optics with 'transformEncryption' instead." #-}

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
cmltWorkerType :: Lens.Lens' CreateMLTransform (Core.Maybe Types.WorkerType)
cmltWorkerType = Lens.field @"workerType"
{-# DEPRECATED cmltWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

instance Core.FromJSON CreateMLTransform where
  toJSON CreateMLTransform {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("InputRecordTables" Core..= inputRecordTables),
            Core.Just ("Parameters" Core..= parameters),
            Core.Just ("Role" Core..= role'),
            ("Description" Core..=) Core.<$> description,
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("MaxRetries" Core..=) Core.<$> maxRetries,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("Tags" Core..=) Core.<$> tags,
            ("Timeout" Core..=) Core.<$> timeout,
            ("TransformEncryption" Core..=) Core.<$> transformEncryption,
            ("WorkerType" Core..=) Core.<$> workerType
          ]
      )

instance Core.AWSRequest CreateMLTransform where
  type Rs CreateMLTransform = CreateMLTransformResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateMLTransform")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMLTransformResponse'
            Core.<$> (x Core..:? "TransformId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateMLTransformResponse' smart constructor.
data CreateMLTransformResponse = CreateMLTransformResponse'
  { -- | A unique identifier that is generated for the transform.
    transformId :: Core.Maybe Types.HashString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMLTransformResponse' value with any optional fields omitted.
mkCreateMLTransformResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateMLTransformResponse
mkCreateMLTransformResponse responseStatus =
  CreateMLTransformResponse'
    { transformId = Core.Nothing,
      responseStatus
    }

-- | A unique identifier that is generated for the transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrsTransformId :: Lens.Lens' CreateMLTransformResponse (Core.Maybe Types.HashString)
cmltrrsTransformId = Lens.field @"transformId"
{-# DEPRECATED cmltrrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmltrrsResponseStatus :: Lens.Lens' CreateMLTransformResponse Core.Int
cmltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
