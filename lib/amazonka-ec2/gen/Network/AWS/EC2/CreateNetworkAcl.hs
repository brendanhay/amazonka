{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network ACL in a VPC. Network ACLs provide an optional layer of security (in addition to security groups) for the instances in your VPC.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNetworkAcl
    (
    -- * Creating a request
      CreateNetworkAcl (..)
    , mkCreateNetworkAcl
    -- ** Request lenses
    , cnaVpcId
    , cnaDryRun
    , cnaTagSpecifications

    -- * Destructuring the response
    , CreateNetworkAclResponse (..)
    , mkCreateNetworkAclResponse
    -- ** Response lenses
    , cnarrsNetworkAcl
    , cnarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNetworkAcl' smart constructor.
data CreateNetworkAcl = CreateNetworkAcl'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the network ACL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkAcl' value with any optional fields omitted.
mkCreateNetworkAcl
    :: Types.VpcId -- ^ 'vpcId'
    -> CreateNetworkAcl
mkCreateNetworkAcl vpcId
  = CreateNetworkAcl'{vpcId, dryRun = Core.Nothing,
                      tagSpecifications = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaVpcId :: Lens.Lens' CreateNetworkAcl Types.VpcId
cnaVpcId = Lens.field @"vpcId"
{-# INLINEABLE cnaVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaDryRun :: Lens.Lens' CreateNetworkAcl (Core.Maybe Core.Bool)
cnaDryRun = Lens.field @"dryRun"
{-# INLINEABLE cnaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to assign to the network ACL.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaTagSpecifications :: Lens.Lens' CreateNetworkAcl (Core.Maybe [Types.TagSpecification])
cnaTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cnaTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateNetworkAcl where
        toQuery CreateNetworkAcl{..}
          = Core.toQueryPair "Action" ("CreateNetworkAcl" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateNetworkAcl where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateNetworkAcl where
        type Rs CreateNetworkAcl = CreateNetworkAclResponse
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
                 CreateNetworkAclResponse' Core.<$>
                   (x Core..@? "networkAcl") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateNetworkAclResponse' smart constructor.
data CreateNetworkAclResponse = CreateNetworkAclResponse'
  { networkAcl :: Core.Maybe Types.NetworkAcl
    -- ^ Information about the network ACL.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNetworkAclResponse' value with any optional fields omitted.
mkCreateNetworkAclResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNetworkAclResponse
mkCreateNetworkAclResponse responseStatus
  = CreateNetworkAclResponse'{networkAcl = Core.Nothing,
                              responseStatus}

-- | Information about the network ACL.
--
-- /Note:/ Consider using 'networkAcl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnarrsNetworkAcl :: Lens.Lens' CreateNetworkAclResponse (Core.Maybe Types.NetworkAcl)
cnarrsNetworkAcl = Lens.field @"networkAcl"
{-# INLINEABLE cnarrsNetworkAcl #-}
{-# DEPRECATED networkAcl "Use generic-lens or generic-optics with 'networkAcl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnarrsResponseStatus :: Lens.Lens' CreateNetworkAclResponse Core.Int
cnarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cnarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
