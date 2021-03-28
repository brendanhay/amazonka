{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterEcsCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Amazon ECS cluster from a stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html#workinglayers-ecscluster-delete Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html> .
module Network.AWS.OpsWorks.DeregisterEcsCluster
    (
    -- * Creating a request
      DeregisterEcsCluster (..)
    , mkDeregisterEcsCluster
    -- ** Request lenses
    , decEcsClusterArn

    -- * Destructuring the response
    , DeregisterEcsClusterResponse (..)
    , mkDeregisterEcsClusterResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterEcsCluster' smart constructor.
newtype DeregisterEcsCluster = DeregisterEcsCluster'
  { ecsClusterArn :: Core.Text
    -- ^ The cluster's Amazon Resource Number (ARN).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterEcsCluster' value with any optional fields omitted.
mkDeregisterEcsCluster
    :: Core.Text -- ^ 'ecsClusterArn'
    -> DeregisterEcsCluster
mkDeregisterEcsCluster ecsClusterArn
  = DeregisterEcsCluster'{ecsClusterArn}

-- | The cluster's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'ecsClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEcsClusterArn :: Lens.Lens' DeregisterEcsCluster Core.Text
decEcsClusterArn = Lens.field @"ecsClusterArn"
{-# INLINEABLE decEcsClusterArn #-}
{-# DEPRECATED ecsClusterArn "Use generic-lens or generic-optics with 'ecsClusterArn' instead"  #-}

instance Core.ToQuery DeregisterEcsCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterEcsCluster where
        toHeaders DeregisterEcsCluster{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DeregisterEcsCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterEcsCluster where
        toJSON DeregisterEcsCluster{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EcsClusterArn" Core..= ecsClusterArn)])

instance Core.AWSRequest DeregisterEcsCluster where
        type Rs DeregisterEcsCluster = DeregisterEcsClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeregisterEcsClusterResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterEcsClusterResponse' smart constructor.
data DeregisterEcsClusterResponse = DeregisterEcsClusterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterEcsClusterResponse' value with any optional fields omitted.
mkDeregisterEcsClusterResponse
    :: DeregisterEcsClusterResponse
mkDeregisterEcsClusterResponse = DeregisterEcsClusterResponse'
