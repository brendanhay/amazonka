{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink DNS support for a VPC. If disabled, DNS hostnames resolve to public IP addresses when addressed between a linked EC2-Classic instance and instances in the VPC to which it's linked. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You must specify a VPC ID in the request.
module Network.AWS.EC2.DisableVpcClassicLinkDnsSupport
    (
    -- * Creating a request
      DisableVpcClassicLinkDnsSupport (..)
    , mkDisableVpcClassicLinkDnsSupport
    -- ** Request lenses
    , dvcldsVpcId

    -- * Destructuring the response
    , DisableVpcClassicLinkDnsSupportResponse (..)
    , mkDisableVpcClassicLinkDnsSupportResponse
    -- ** Response lenses
    , dvcldsrfrsReturn
    , dvcldsrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableVpcClassicLinkDnsSupport' smart constructor.
newtype DisableVpcClassicLinkDnsSupport = DisableVpcClassicLinkDnsSupport'
  { vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableVpcClassicLinkDnsSupport' value with any optional fields omitted.
mkDisableVpcClassicLinkDnsSupport
    :: DisableVpcClassicLinkDnsSupport
mkDisableVpcClassicLinkDnsSupport
  = DisableVpcClassicLinkDnsSupport'{vpcId = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsVpcId :: Lens.Lens' DisableVpcClassicLinkDnsSupport (Core.Maybe Types.VpcId)
dvcldsVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvcldsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.ToQuery DisableVpcClassicLinkDnsSupport where
        toQuery DisableVpcClassicLinkDnsSupport{..}
          = Core.toQueryPair "Action"
              ("DisableVpcClassicLinkDnsSupport" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpcId") vpcId

instance Core.ToHeaders DisableVpcClassicLinkDnsSupport where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableVpcClassicLinkDnsSupport where
        type Rs DisableVpcClassicLinkDnsSupport =
             DisableVpcClassicLinkDnsSupportResponse
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
                 DisableVpcClassicLinkDnsSupportResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableVpcClassicLinkDnsSupportResponse' smart constructor.
data DisableVpcClassicLinkDnsSupportResponse = DisableVpcClassicLinkDnsSupportResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableVpcClassicLinkDnsSupportResponse' value with any optional fields omitted.
mkDisableVpcClassicLinkDnsSupportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisableVpcClassicLinkDnsSupportResponse
mkDisableVpcClassicLinkDnsSupportResponse responseStatus
  = DisableVpcClassicLinkDnsSupportResponse'{return = Core.Nothing,
                                             responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsrfrsReturn :: Lens.Lens' DisableVpcClassicLinkDnsSupportResponse (Core.Maybe Core.Bool)
dvcldsrfrsReturn = Lens.field @"return"
{-# INLINEABLE dvcldsrfrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcldsrfrsResponseStatus :: Lens.Lens' DisableVpcClassicLinkDnsSupportResponse Core.Int
dvcldsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvcldsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
