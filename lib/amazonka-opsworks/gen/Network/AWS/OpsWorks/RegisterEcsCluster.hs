{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterEcsCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a specified Amazon ECS cluster with a stack. You can register only one cluster with a stack. A cluster can be registered with only one stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterEcsCluster
    (
    -- * Creating a request
      RegisterEcsCluster (..)
    , mkRegisterEcsCluster
    -- ** Request lenses
    , recEcsClusterArn
    , recStackId

    -- * Destructuring the response
    , RegisterEcsClusterResponse (..)
    , mkRegisterEcsClusterResponse
    -- ** Response lenses
    , recrrsEcsClusterArn
    , recrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterEcsCluster' smart constructor.
data RegisterEcsCluster = RegisterEcsCluster'
  { ecsClusterArn :: Core.Text
    -- ^ The cluster's ARN.
  , stackId :: Core.Text
    -- ^ The stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterEcsCluster' value with any optional fields omitted.
mkRegisterEcsCluster
    :: Core.Text -- ^ 'ecsClusterArn'
    -> Core.Text -- ^ 'stackId'
    -> RegisterEcsCluster
mkRegisterEcsCluster ecsClusterArn stackId
  = RegisterEcsCluster'{ecsClusterArn, stackId}

-- | The cluster's ARN.
--
-- /Note:/ Consider using 'ecsClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recEcsClusterArn :: Lens.Lens' RegisterEcsCluster Core.Text
recEcsClusterArn = Lens.field @"ecsClusterArn"
{-# INLINEABLE recEcsClusterArn #-}
{-# DEPRECATED ecsClusterArn "Use generic-lens or generic-optics with 'ecsClusterArn' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recStackId :: Lens.Lens' RegisterEcsCluster Core.Text
recStackId = Lens.field @"stackId"
{-# INLINEABLE recStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery RegisterEcsCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterEcsCluster where
        toHeaders RegisterEcsCluster{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.RegisterEcsCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterEcsCluster where
        toJSON RegisterEcsCluster{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EcsClusterArn" Core..= ecsClusterArn),
                  Core.Just ("StackId" Core..= stackId)])

instance Core.AWSRequest RegisterEcsCluster where
        type Rs RegisterEcsCluster = RegisterEcsClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterEcsClusterResponse' Core.<$>
                   (x Core..:? "EcsClusterArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @RegisterEcsCluster@ request.
--
-- /See:/ 'mkRegisterEcsClusterResponse' smart constructor.
data RegisterEcsClusterResponse = RegisterEcsClusterResponse'
  { ecsClusterArn :: Core.Maybe Core.Text
    -- ^ The cluster's ARN.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterEcsClusterResponse' value with any optional fields omitted.
mkRegisterEcsClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterEcsClusterResponse
mkRegisterEcsClusterResponse responseStatus
  = RegisterEcsClusterResponse'{ecsClusterArn = Core.Nothing,
                                responseStatus}

-- | The cluster's ARN.
--
-- /Note:/ Consider using 'ecsClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recrrsEcsClusterArn :: Lens.Lens' RegisterEcsClusterResponse (Core.Maybe Core.Text)
recrrsEcsClusterArn = Lens.field @"ecsClusterArn"
{-# INLINEABLE recrrsEcsClusterArn #-}
{-# DEPRECATED ecsClusterArn "Use generic-lens or generic-optics with 'ecsClusterArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
recrrsResponseStatus :: Lens.Lens' RegisterEcsClusterResponse Core.Int
recrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE recrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
