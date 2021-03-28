{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.UpdateComputeEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an AWS Batch compute environment.
module Network.AWS.Batch.UpdateComputeEnvironment
    (
    -- * Creating a request
      UpdateComputeEnvironment (..)
    , mkUpdateComputeEnvironment
    -- ** Request lenses
    , uceComputeEnvironment
    , uceComputeResources
    , uceServiceRole
    , uceState

    -- * Destructuring the response
    , UpdateComputeEnvironmentResponse (..)
    , mkUpdateComputeEnvironmentResponse
    -- ** Response lenses
    , ucerrsComputeEnvironmentArn
    , ucerrsComputeEnvironmentName
    , ucerrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateComputeEnvironment' smart constructor.
data UpdateComputeEnvironment = UpdateComputeEnvironment'
  { computeEnvironment :: Core.Text
    -- ^ The name or full Amazon Resource Name (ARN) of the compute environment to update.
  , computeResources :: Core.Maybe Types.ComputeResourceUpdate
    -- ^ Details of the compute resources managed by the compute environment. Required for a managed compute environment.
  , serviceRole :: Core.Maybe Core.Text
    -- ^ The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
  , state :: Core.Maybe Types.CEState
    -- ^ The state of the compute environment. Compute environments in the @ENABLED@ state can accept jobs from a queue and scale in or out automatically based on the workload demand of its associated queues.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateComputeEnvironment' value with any optional fields omitted.
mkUpdateComputeEnvironment
    :: Core.Text -- ^ 'computeEnvironment'
    -> UpdateComputeEnvironment
mkUpdateComputeEnvironment computeEnvironment
  = UpdateComputeEnvironment'{computeEnvironment,
                              computeResources = Core.Nothing, serviceRole = Core.Nothing,
                              state = Core.Nothing}

-- | The name or full Amazon Resource Name (ARN) of the compute environment to update.
--
-- /Note:/ Consider using 'computeEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceComputeEnvironment :: Lens.Lens' UpdateComputeEnvironment Core.Text
uceComputeEnvironment = Lens.field @"computeEnvironment"
{-# INLINEABLE uceComputeEnvironment #-}
{-# DEPRECATED computeEnvironment "Use generic-lens or generic-optics with 'computeEnvironment' instead"  #-}

-- | Details of the compute resources managed by the compute environment. Required for a managed compute environment.
--
-- /Note:/ Consider using 'computeResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceComputeResources :: Lens.Lens' UpdateComputeEnvironment (Core.Maybe Types.ComputeResourceUpdate)
uceComputeResources = Lens.field @"computeResources"
{-# INLINEABLE uceComputeResources #-}
{-# DEPRECATED computeResources "Use generic-lens or generic-optics with 'computeResources' instead"  #-}

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceServiceRole :: Lens.Lens' UpdateComputeEnvironment (Core.Maybe Core.Text)
uceServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE uceServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | The state of the compute environment. Compute environments in the @ENABLED@ state can accept jobs from a queue and scale in or out automatically based on the workload demand of its associated queues.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceState :: Lens.Lens' UpdateComputeEnvironment (Core.Maybe Types.CEState)
uceState = Lens.field @"state"
{-# INLINEABLE uceState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.ToQuery UpdateComputeEnvironment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateComputeEnvironment where
        toHeaders UpdateComputeEnvironment{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateComputeEnvironment where
        toJSON UpdateComputeEnvironment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("computeEnvironment" Core..= computeEnvironment),
                  ("computeResources" Core..=) Core.<$> computeResources,
                  ("serviceRole" Core..=) Core.<$> serviceRole,
                  ("state" Core..=) Core.<$> state])

instance Core.AWSRequest UpdateComputeEnvironment where
        type Rs UpdateComputeEnvironment = UpdateComputeEnvironmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/v1/updatecomputeenvironment",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateComputeEnvironmentResponse' Core.<$>
                   (x Core..:? "computeEnvironmentArn") Core.<*>
                     x Core..:? "computeEnvironmentName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateComputeEnvironmentResponse' smart constructor.
data UpdateComputeEnvironmentResponse = UpdateComputeEnvironmentResponse'
  { computeEnvironmentArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the compute environment.
  , computeEnvironmentName :: Core.Maybe Core.Text
    -- ^ The name of the compute environment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateComputeEnvironmentResponse' value with any optional fields omitted.
mkUpdateComputeEnvironmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateComputeEnvironmentResponse
mkUpdateComputeEnvironmentResponse responseStatus
  = UpdateComputeEnvironmentResponse'{computeEnvironmentArn =
                                        Core.Nothing,
                                      computeEnvironmentName = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucerrsComputeEnvironmentArn :: Lens.Lens' UpdateComputeEnvironmentResponse (Core.Maybe Core.Text)
ucerrsComputeEnvironmentArn = Lens.field @"computeEnvironmentArn"
{-# INLINEABLE ucerrsComputeEnvironmentArn #-}
{-# DEPRECATED computeEnvironmentArn "Use generic-lens or generic-optics with 'computeEnvironmentArn' instead"  #-}

-- | The name of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucerrsComputeEnvironmentName :: Lens.Lens' UpdateComputeEnvironmentResponse (Core.Maybe Core.Text)
ucerrsComputeEnvironmentName = Lens.field @"computeEnvironmentName"
{-# INLINEABLE ucerrsComputeEnvironmentName #-}
{-# DEPRECATED computeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucerrsResponseStatus :: Lens.Lens' UpdateComputeEnvironmentResponse Core.Int
ucerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
