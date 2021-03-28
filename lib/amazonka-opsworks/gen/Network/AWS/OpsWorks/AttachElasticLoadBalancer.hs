{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Elastic Load Balancing load balancer to a specified layer. AWS OpsWorks Stacks does not support Application Load Balancer. You can only use Classic Load Balancer with AWS OpsWorks Stacks. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/layers-elb.html Elastic Load Balancing> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AttachElasticLoadBalancer
    (
    -- * Creating a request
      AttachElasticLoadBalancer (..)
    , mkAttachElasticLoadBalancer
    -- ** Request lenses
    , aelbElasticLoadBalancerName
    , aelbLayerId

    -- * Destructuring the response
    , AttachElasticLoadBalancerResponse (..)
    , mkAttachElasticLoadBalancerResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachElasticLoadBalancer' smart constructor.
data AttachElasticLoadBalancer = AttachElasticLoadBalancer'
  { elasticLoadBalancerName :: Core.Text
    -- ^ The Elastic Load Balancing instance's name.
  , layerId :: Core.Text
    -- ^ The ID of the layer to which the Elastic Load Balancing instance is to be attached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachElasticLoadBalancer' value with any optional fields omitted.
mkAttachElasticLoadBalancer
    :: Core.Text -- ^ 'elasticLoadBalancerName'
    -> Core.Text -- ^ 'layerId'
    -> AttachElasticLoadBalancer
mkAttachElasticLoadBalancer elasticLoadBalancerName layerId
  = AttachElasticLoadBalancer'{elasticLoadBalancerName, layerId}

-- | The Elastic Load Balancing instance's name.
--
-- /Note:/ Consider using 'elasticLoadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aelbElasticLoadBalancerName :: Lens.Lens' AttachElasticLoadBalancer Core.Text
aelbElasticLoadBalancerName = Lens.field @"elasticLoadBalancerName"
{-# INLINEABLE aelbElasticLoadBalancerName #-}
{-# DEPRECATED elasticLoadBalancerName "Use generic-lens or generic-optics with 'elasticLoadBalancerName' instead"  #-}

-- | The ID of the layer to which the Elastic Load Balancing instance is to be attached.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aelbLayerId :: Lens.Lens' AttachElasticLoadBalancer Core.Text
aelbLayerId = Lens.field @"layerId"
{-# INLINEABLE aelbLayerId #-}
{-# DEPRECATED layerId "Use generic-lens or generic-optics with 'layerId' instead"  #-}

instance Core.ToQuery AttachElasticLoadBalancer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AttachElasticLoadBalancer where
        toHeaders AttachElasticLoadBalancer{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.AttachElasticLoadBalancer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AttachElasticLoadBalancer where
        toJSON AttachElasticLoadBalancer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ElasticLoadBalancerName" Core..= elasticLoadBalancerName),
                  Core.Just ("LayerId" Core..= layerId)])

instance Core.AWSRequest AttachElasticLoadBalancer where
        type Rs AttachElasticLoadBalancer =
             AttachElasticLoadBalancerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AttachElasticLoadBalancerResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachElasticLoadBalancerResponse' smart constructor.
data AttachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachElasticLoadBalancerResponse' value with any optional fields omitted.
mkAttachElasticLoadBalancerResponse
    :: AttachElasticLoadBalancerResponse
mkAttachElasticLoadBalancerResponse
  = AttachElasticLoadBalancerResponse'
