{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachClassicLinkVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links an EC2-Classic instance to a ClassicLink-enabled VPC through one or more of the VPC's security groups. You cannot link an EC2-Classic instance to more than one VPC at a time. You can only link an instance that's in the @running@ state. An instance is automatically unlinked from a VPC when it's stopped - you can link it to the VPC again when you restart it.
--
-- After you've linked an instance, you cannot change the VPC security groups that are associated with it. To change the security groups, you must first unlink the instance, and then link it again.
-- Linking your instance to a VPC is sometimes referred to as /attaching/ your instance.
module Network.AWS.EC2.AttachClassicLinkVpc
    (
    -- * Creating a request
      AttachClassicLinkVpc (..)
    , mkAttachClassicLinkVpc
    -- ** Request lenses
    , aclvGroups
    , aclvInstanceId
    , aclvVpcId
    , aclvDryRun

    -- * Destructuring the response
    , AttachClassicLinkVpcResponse (..)
    , mkAttachClassicLinkVpcResponse
    -- ** Response lenses
    , aclvrrsReturn
    , aclvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachClassicLinkVpc' smart constructor.
data AttachClassicLinkVpc = AttachClassicLinkVpc'
  { groups :: [Core.Text]
    -- ^ The ID of one or more of the VPC's security groups. You cannot specify security groups from a different VPC.
  , instanceId :: Types.InstanceId
    -- ^ The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
  , vpcId :: Types.VpcId
    -- ^ The ID of a ClassicLink-enabled VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachClassicLinkVpc' value with any optional fields omitted.
mkAttachClassicLinkVpc
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.VpcId -- ^ 'vpcId'
    -> AttachClassicLinkVpc
mkAttachClassicLinkVpc instanceId vpcId
  = AttachClassicLinkVpc'{groups = Core.mempty, instanceId, vpcId,
                          dryRun = Core.Nothing}

-- | The ID of one or more of the VPC's security groups. You cannot specify security groups from a different VPC.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvGroups :: Lens.Lens' AttachClassicLinkVpc [Core.Text]
aclvGroups = Lens.field @"groups"
{-# INLINEABLE aclvGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvInstanceId :: Lens.Lens' AttachClassicLinkVpc Types.InstanceId
aclvInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aclvInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The ID of a ClassicLink-enabled VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvVpcId :: Lens.Lens' AttachClassicLinkVpc Types.VpcId
aclvVpcId = Lens.field @"vpcId"
{-# INLINEABLE aclvVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvDryRun :: Lens.Lens' AttachClassicLinkVpc (Core.Maybe Core.Bool)
aclvDryRun = Lens.field @"dryRun"
{-# INLINEABLE aclvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AttachClassicLinkVpc where
        toQuery AttachClassicLinkVpc{..}
          = Core.toQueryPair "Action" ("AttachClassicLinkVpc" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "SecurityGroupId" groups
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AttachClassicLinkVpc where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AttachClassicLinkVpc where
        type Rs AttachClassicLinkVpc = AttachClassicLinkVpcResponse
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
                 AttachClassicLinkVpcResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachClassicLinkVpcResponse' smart constructor.
data AttachClassicLinkVpcResponse = AttachClassicLinkVpcResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachClassicLinkVpcResponse' value with any optional fields omitted.
mkAttachClassicLinkVpcResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachClassicLinkVpcResponse
mkAttachClassicLinkVpcResponse responseStatus
  = AttachClassicLinkVpcResponse'{return = Core.Nothing,
                                  responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvrrsReturn :: Lens.Lens' AttachClassicLinkVpcResponse (Core.Maybe Core.Bool)
aclvrrsReturn = Lens.field @"return"
{-# INLINEABLE aclvrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvrrsResponseStatus :: Lens.Lens' AttachClassicLinkVpcResponse Core.Int
aclvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aclvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
