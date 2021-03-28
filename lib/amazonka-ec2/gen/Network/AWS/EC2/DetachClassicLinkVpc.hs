{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachClassicLinkVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the instance has been unlinked, the VPC security groups are no longer associated with it. An instance is automatically unlinked from a VPC when it's stopped.
module Network.AWS.EC2.DetachClassicLinkVpc
    (
    -- * Creating a request
      DetachClassicLinkVpc (..)
    , mkDetachClassicLinkVpc
    -- ** Request lenses
    , dclvInstanceId
    , dclvVpcId
    , dclvDryRun

    -- * Destructuring the response
    , DetachClassicLinkVpcResponse (..)
    , mkDetachClassicLinkVpcResponse
    -- ** Response lenses
    , dclvrrsReturn
    , dclvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachClassicLinkVpc' smart constructor.
data DetachClassicLinkVpc = DetachClassicLinkVpc'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance to unlink from the VPC.
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC to which the instance is linked.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachClassicLinkVpc' value with any optional fields omitted.
mkDetachClassicLinkVpc
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.VpcId -- ^ 'vpcId'
    -> DetachClassicLinkVpc
mkDetachClassicLinkVpc instanceId vpcId
  = DetachClassicLinkVpc'{instanceId, vpcId, dryRun = Core.Nothing}

-- | The ID of the instance to unlink from the VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvInstanceId :: Lens.Lens' DetachClassicLinkVpc Types.InstanceId
dclvInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dclvInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The ID of the VPC to which the instance is linked.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvVpcId :: Lens.Lens' DetachClassicLinkVpc Types.VpcId
dclvVpcId = Lens.field @"vpcId"
{-# INLINEABLE dclvVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvDryRun :: Lens.Lens' DetachClassicLinkVpc (Core.Maybe Core.Bool)
dclvDryRun = Lens.field @"dryRun"
{-# INLINEABLE dclvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DetachClassicLinkVpc where
        toQuery DetachClassicLinkVpc{..}
          = Core.toQueryPair "Action" ("DetachClassicLinkVpc" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DetachClassicLinkVpc where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DetachClassicLinkVpc where
        type Rs DetachClassicLinkVpc = DetachClassicLinkVpcResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DetachClassicLinkVpcResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachClassicLinkVpcResponse' smart constructor.
data DetachClassicLinkVpcResponse = DetachClassicLinkVpcResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachClassicLinkVpcResponse' value with any optional fields omitted.
mkDetachClassicLinkVpcResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachClassicLinkVpcResponse
mkDetachClassicLinkVpcResponse responseStatus
  = DetachClassicLinkVpcResponse'{return = Core.Nothing,
                                  responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvrrsReturn :: Lens.Lens' DetachClassicLinkVpcResponse (Core.Maybe Core.Bool)
dclvrrsReturn = Lens.field @"return"
{-# INLINEABLE dclvrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvrrsResponseStatus :: Lens.Lens' DetachClassicLinkVpcResponse Core.Int
dclvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dclvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
