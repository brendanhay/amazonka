{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableVpcClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink for a VPC. You cannot disable ClassicLink for a VPC that has EC2-Classic instances linked to it.
module Network.AWS.EC2.DisableVpcClassicLink
    (
    -- * Creating a request
      DisableVpcClassicLink (..)
    , mkDisableVpcClassicLink
    -- ** Request lenses
    , dvclfVpcId
    , dvclfDryRun

    -- * Destructuring the response
    , DisableVpcClassicLinkResponse (..)
    , mkDisableVpcClassicLinkResponse
    -- ** Response lenses
    , drsReturn
    , drsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableVpcClassicLink' smart constructor.
data DisableVpcClassicLink = DisableVpcClassicLink'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableVpcClassicLink' value with any optional fields omitted.
mkDisableVpcClassicLink
    :: Types.VpcId -- ^ 'vpcId'
    -> DisableVpcClassicLink
mkDisableVpcClassicLink vpcId
  = DisableVpcClassicLink'{vpcId, dryRun = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclfVpcId :: Lens.Lens' DisableVpcClassicLink Types.VpcId
dvclfVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvclfVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclfDryRun :: Lens.Lens' DisableVpcClassicLink (Core.Maybe Core.Bool)
dvclfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvclfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisableVpcClassicLink where
        toQuery DisableVpcClassicLink{..}
          = Core.toQueryPair "Action" ("DisableVpcClassicLink" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisableVpcClassicLink where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableVpcClassicLink where
        type Rs DisableVpcClassicLink = DisableVpcClassicLinkResponse
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
                 DisableVpcClassicLinkResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableVpcClassicLinkResponse' smart constructor.
data DisableVpcClassicLinkResponse = DisableVpcClassicLinkResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableVpcClassicLinkResponse' value with any optional fields omitted.
mkDisableVpcClassicLinkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableVpcClassicLinkResponse
mkDisableVpcClassicLinkResponse responseStatus
  = DisableVpcClassicLinkResponse'{return = Core.Nothing,
                                   responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsReturn :: Lens.Lens' DisableVpcClassicLinkResponse (Core.Maybe Core.Bool)
drsReturn = Lens.field @"return"
{-# INLINEABLE drsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DisableVpcClassicLinkResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
