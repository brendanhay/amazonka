{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVpcClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC for ClassicLink. You can then link EC2-Classic instances to your ClassicLink-enabled VPC to allow communication over private IP addresses. You cannot enable your VPC for ClassicLink if any of your VPC route tables have existing routes for address ranges within the @10.0.0.0/8@ IP address range, excluding local routes for VPCs in the @10.0.0.0/16@ and @10.1.0.0/16@ IP address ranges. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.EnableVpcClassicLink
    (
    -- * Creating a request
      EnableVpcClassicLink (..)
    , mkEnableVpcClassicLink
    -- ** Request lenses
    , evclVpcId
    , evclDryRun

    -- * Destructuring the response
    , EnableVpcClassicLinkResponse (..)
    , mkEnableVpcClassicLinkResponse
    -- ** Response lenses
    , evclrrsReturn
    , evclrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableVpcClassicLink' smart constructor.
data EnableVpcClassicLink = EnableVpcClassicLink'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVpcClassicLink' value with any optional fields omitted.
mkEnableVpcClassicLink
    :: Types.VpcId -- ^ 'vpcId'
    -> EnableVpcClassicLink
mkEnableVpcClassicLink vpcId
  = EnableVpcClassicLink'{vpcId, dryRun = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclVpcId :: Lens.Lens' EnableVpcClassicLink Types.VpcId
evclVpcId = Lens.field @"vpcId"
{-# INLINEABLE evclVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclDryRun :: Lens.Lens' EnableVpcClassicLink (Core.Maybe Core.Bool)
evclDryRun = Lens.field @"dryRun"
{-# INLINEABLE evclDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery EnableVpcClassicLink where
        toQuery EnableVpcClassicLink{..}
          = Core.toQueryPair "Action" ("EnableVpcClassicLink" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders EnableVpcClassicLink where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableVpcClassicLink where
        type Rs EnableVpcClassicLink = EnableVpcClassicLinkResponse
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
                 EnableVpcClassicLinkResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableVpcClassicLinkResponse' smart constructor.
data EnableVpcClassicLinkResponse = EnableVpcClassicLinkResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVpcClassicLinkResponse' value with any optional fields omitted.
mkEnableVpcClassicLinkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableVpcClassicLinkResponse
mkEnableVpcClassicLinkResponse responseStatus
  = EnableVpcClassicLinkResponse'{return = Core.Nothing,
                                  responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclrrsReturn :: Lens.Lens' EnableVpcClassicLinkResponse (Core.Maybe Core.Bool)
evclrrsReturn = Lens.field @"return"
{-# INLINEABLE evclrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evclrrsResponseStatus :: Lens.Lens' EnableVpcClassicLinkResponse Core.Int
evclrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE evclrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
