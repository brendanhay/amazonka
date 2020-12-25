{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the result of a Lambda validation function. The function validates lifecycle hooks during a deployment that uses the AWS Lambda or Amazon ECS compute platform. For AWS Lambda deployments, the available lifecycle hooks are @BeforeAllowTraffic@ and @AfterAllowTraffic@ . For Amazon ECS deployments, the available lifecycle hooks are @BeforeInstall@ , @AfterInstall@ , @AfterAllowTestTraffic@ , @BeforeAllowTraffic@ , and @AfterAllowTraffic@ . Lambda validation functions return @Succeeded@ or @Failed@ . For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-lambda AppSpec 'hooks' Section for an AWS Lambda Deployment > and <https://docs.aws.amazon.com/codedeploy/latest/userguide/reference-appspec-file-structure-hooks.html#appspec-hooks-ecs AppSpec 'hooks' Section for an Amazon ECS Deployment> .
module Network.AWS.CodeDeploy.PutLifecycleEventHookExecutionStatus
  ( -- * Creating a request
    PutLifecycleEventHookExecutionStatus (..),
    mkPutLifecycleEventHookExecutionStatus,

    -- ** Request lenses
    plehesDeploymentId,
    plehesLifecycleEventHookExecutionId,
    plehesStatus,

    -- * Destructuring the response
    PutLifecycleEventHookExecutionStatusResponse (..),
    mkPutLifecycleEventHookExecutionStatusResponse,

    -- ** Response lenses
    plehesrrsLifecycleEventHookExecutionId,
    plehesrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLifecycleEventHookExecutionStatus' smart constructor.
data PutLifecycleEventHookExecutionStatus = PutLifecycleEventHookExecutionStatus'
  { -- | The unique ID of a deployment. Pass this ID to a Lambda function that validates a deployment lifecycle event.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | The execution ID of a deployment's lifecycle hook. A deployment lifecycle hook is specified in the @hooks@ section of the AppSpec file.
    lifecycleEventHookExecutionId :: Core.Maybe Types.LifecycleEventHookExecutionId,
    -- | The result of a Lambda function that validates a deployment lifecycle event (@Succeeded@ or @Failed@ ).
    status :: Core.Maybe Types.LifecycleEventStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecycleEventHookExecutionStatus' value with any optional fields omitted.
mkPutLifecycleEventHookExecutionStatus ::
  PutLifecycleEventHookExecutionStatus
mkPutLifecycleEventHookExecutionStatus =
  PutLifecycleEventHookExecutionStatus'
    { deploymentId =
        Core.Nothing,
      lifecycleEventHookExecutionId = Core.Nothing,
      status = Core.Nothing
    }

-- | The unique ID of a deployment. Pass this ID to a Lambda function that validates a deployment lifecycle event.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesDeploymentId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Core.Maybe Types.DeploymentId)
plehesDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED plehesDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The execution ID of a deployment's lifecycle hook. A deployment lifecycle hook is specified in the @hooks@ section of the AppSpec file.
--
-- /Note:/ Consider using 'lifecycleEventHookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesLifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Core.Maybe Types.LifecycleEventHookExecutionId)
plehesLifecycleEventHookExecutionId = Lens.field @"lifecycleEventHookExecutionId"
{-# DEPRECATED plehesLifecycleEventHookExecutionId "Use generic-lens or generic-optics with 'lifecycleEventHookExecutionId' instead." #-}

-- | The result of a Lambda function that validates a deployment lifecycle event (@Succeeded@ or @Failed@ ).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesStatus :: Lens.Lens' PutLifecycleEventHookExecutionStatus (Core.Maybe Types.LifecycleEventStatus)
plehesStatus = Lens.field @"status"
{-# DEPRECATED plehesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON PutLifecycleEventHookExecutionStatus where
  toJSON PutLifecycleEventHookExecutionStatus {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("lifecycleEventHookExecutionId" Core..=)
              Core.<$> lifecycleEventHookExecutionId,
            ("status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest PutLifecycleEventHookExecutionStatus where
  type
    Rs PutLifecycleEventHookExecutionStatus =
      PutLifecycleEventHookExecutionStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeDeploy_20141006.PutLifecycleEventHookExecutionStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLifecycleEventHookExecutionStatusResponse'
            Core.<$> (x Core..:? "lifecycleEventHookExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutLifecycleEventHookExecutionStatusResponse' smart constructor.
data PutLifecycleEventHookExecutionStatusResponse = PutLifecycleEventHookExecutionStatusResponse'
  { -- | The execution ID of the lifecycle event hook. A hook is specified in the @hooks@ section of the deployment's AppSpec file.
    lifecycleEventHookExecutionId :: Core.Maybe Types.LifecycleEventHookExecutionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLifecycleEventHookExecutionStatusResponse' value with any optional fields omitted.
mkPutLifecycleEventHookExecutionStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutLifecycleEventHookExecutionStatusResponse
mkPutLifecycleEventHookExecutionStatusResponse responseStatus =
  PutLifecycleEventHookExecutionStatusResponse'
    { lifecycleEventHookExecutionId =
        Core.Nothing,
      responseStatus
    }

-- | The execution ID of the lifecycle event hook. A hook is specified in the @hooks@ section of the deployment's AppSpec file.
--
-- /Note:/ Consider using 'lifecycleEventHookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesrrsLifecycleEventHookExecutionId :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse (Core.Maybe Types.LifecycleEventHookExecutionId)
plehesrrsLifecycleEventHookExecutionId = Lens.field @"lifecycleEventHookExecutionId"
{-# DEPRECATED plehesrrsLifecycleEventHookExecutionId "Use generic-lens or generic-optics with 'lifecycleEventHookExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plehesrrsResponseStatus :: Lens.Lens' PutLifecycleEventHookExecutionStatusResponse Core.Int
plehesrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED plehesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
