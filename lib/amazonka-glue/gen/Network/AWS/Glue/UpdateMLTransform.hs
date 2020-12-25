{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing machine learning transform. Call this operation to tune the algorithm parameters to achieve better results.
--
-- After calling this operation, you can call the @StartMLEvaluationTaskRun@ operation to assess how well your new parameters achieved your goals (such as improving the quality of your machine learning transform, or making it more cost-effective).
module Network.AWS.Glue.UpdateMLTransform
  ( -- * Creating a request
    UpdateMLTransform (..),
    mkUpdateMLTransform,

    -- ** Request lenses
    umltTransformId,
    umltDescription,
    umltGlueVersion,
    umltMaxCapacity,
    umltMaxRetries,
    umltName,
    umltNumberOfWorkers,
    umltParameters,
    umltRole,
    umltTimeout,
    umltWorkerType,

    -- * Destructuring the response
    UpdateMLTransformResponse (..),
    mkUpdateMLTransformResponse,

    -- ** Response lenses
    umltrrsTransformId,
    umltrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateMLTransform' smart constructor.
data UpdateMLTransform = UpdateMLTransform'
  { -- | A unique identifier that was generated when the transform was created.
    transformId :: Types.HashString,
    -- | A description of the transform. The default is an empty string.
    description :: Core.Maybe Types.DescriptionString,
    -- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
    glueVersion :: Core.Maybe Types.GlueVersionString,
    -- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Core.Maybe Core.Double,
    -- | The maximum number of times to retry a task for this transform after a task run fails.
    maxRetries :: Core.Maybe Core.Int,
    -- | The unique name that you gave the transform when you created it.
    name :: Core.Maybe Types.NameString,
    -- | The number of workers of a defined @workerType@ that are allocated when this task runs.
    numberOfWorkers :: Core.Maybe Core.Int,
    -- | The configuration parameters that are specific to the transform type (algorithm) used. Conditionally dependent on the transform type.
    parameters :: Core.Maybe Types.TransformParameters,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
    role' :: Core.Maybe Types.Role,
    -- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
    timeout :: Core.Maybe Core.Natural,
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
    workerType :: Core.Maybe Types.WorkerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMLTransform' value with any optional fields omitted.
mkUpdateMLTransform ::
  -- | 'transformId'
  Types.HashString ->
  UpdateMLTransform
mkUpdateMLTransform transformId =
  UpdateMLTransform'
    { transformId,
      description = Core.Nothing,
      glueVersion = Core.Nothing,
      maxCapacity = Core.Nothing,
      maxRetries = Core.Nothing,
      name = Core.Nothing,
      numberOfWorkers = Core.Nothing,
      parameters = Core.Nothing,
      role' = Core.Nothing,
      timeout = Core.Nothing,
      workerType = Core.Nothing
    }

-- | A unique identifier that was generated when the transform was created.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltTransformId :: Lens.Lens' UpdateMLTransform Types.HashString
umltTransformId = Lens.field @"transformId"
{-# DEPRECATED umltTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | A description of the transform. The default is an empty string.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltDescription :: Lens.Lens' UpdateMLTransform (Core.Maybe Types.DescriptionString)
umltDescription = Lens.field @"description"
{-# DEPRECATED umltDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltGlueVersion :: Lens.Lens' UpdateMLTransform (Core.Maybe Types.GlueVersionString)
umltGlueVersion = Lens.field @"glueVersion"
{-# DEPRECATED umltGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltMaxCapacity :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Double)
umltMaxCapacity = Lens.field @"maxCapacity"
{-# DEPRECATED umltMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The maximum number of times to retry a task for this transform after a task run fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltMaxRetries :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Int)
umltMaxRetries = Lens.field @"maxRetries"
{-# DEPRECATED umltMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | The unique name that you gave the transform when you created it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltName :: Lens.Lens' UpdateMLTransform (Core.Maybe Types.NameString)
umltName = Lens.field @"name"
{-# DEPRECATED umltName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltNumberOfWorkers :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Int)
umltNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# DEPRECATED umltNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The configuration parameters that are specific to the transform type (algorithm) used. Conditionally dependent on the transform type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltParameters :: Lens.Lens' UpdateMLTransform (Core.Maybe Types.TransformParameters)
umltParameters = Lens.field @"parameters"
{-# DEPRECATED umltParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltRole :: Lens.Lens' UpdateMLTransform (Core.Maybe Types.Role)
umltRole = Lens.field @"role'"
{-# DEPRECATED umltRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltTimeout :: Lens.Lens' UpdateMLTransform (Core.Maybe Core.Natural)
umltTimeout = Lens.field @"timeout"
{-# DEPRECATED umltTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

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
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltWorkerType :: Lens.Lens' UpdateMLTransform (Core.Maybe Types.WorkerType)
umltWorkerType = Lens.field @"workerType"
{-# DEPRECATED umltWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

instance Core.FromJSON UpdateMLTransform where
  toJSON UpdateMLTransform {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            ("Description" Core..=) Core.<$> description,
            ("GlueVersion" Core..=) Core.<$> glueVersion,
            ("MaxCapacity" Core..=) Core.<$> maxCapacity,
            ("MaxRetries" Core..=) Core.<$> maxRetries,
            ("Name" Core..=) Core.<$> name,
            ("NumberOfWorkers" Core..=) Core.<$> numberOfWorkers,
            ("Parameters" Core..=) Core.<$> parameters,
            ("Role" Core..=) Core.<$> role',
            ("Timeout" Core..=) Core.<$> timeout,
            ("WorkerType" Core..=) Core.<$> workerType
          ]
      )

instance Core.AWSRequest UpdateMLTransform where
  type Rs UpdateMLTransform = UpdateMLTransformResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateMLTransform")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMLTransformResponse'
            Core.<$> (x Core..:? "TransformId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateMLTransformResponse' smart constructor.
data UpdateMLTransformResponse = UpdateMLTransformResponse'
  { -- | The unique identifier for the transform that was updated.
    transformId :: Core.Maybe Types.HashString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMLTransformResponse' value with any optional fields omitted.
mkUpdateMLTransformResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMLTransformResponse
mkUpdateMLTransformResponse responseStatus =
  UpdateMLTransformResponse'
    { transformId = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for the transform that was updated.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltrrsTransformId :: Lens.Lens' UpdateMLTransformResponse (Core.Maybe Types.HashString)
umltrrsTransformId = Lens.field @"transformId"
{-# DEPRECATED umltrrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltrrsResponseStatus :: Lens.Lens' UpdateMLTransformResponse Core.Int
umltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
