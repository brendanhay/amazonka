{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC to support DNS hostname resolution for ClassicLink. If enabled, the DNS hostname of a linked EC2-Classic instance resolves to its private IP address when addressed from an instance in the VPC to which it's linked. Similarly, the DNS hostname of an instance in a VPC resolves to its private IP address when addressed from a linked EC2-Classic instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You must specify a VPC ID in the request.
module Network.AWS.EC2.EnableVpcClassicLinkDnsSupport
    (
    -- * Creating a request
      EnableVpcClassicLinkDnsSupport (..)
    , mkEnableVpcClassicLinkDnsSupport
    -- ** Request lenses
    , evcldsVpcId

    -- * Destructuring the response
    , EnableVpcClassicLinkDnsSupportResponse (..)
    , mkEnableVpcClassicLinkDnsSupportResponse
    -- ** Response lenses
    , evcldsrrsReturn
    , evcldsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableVpcClassicLinkDnsSupport' smart constructor.
newtype EnableVpcClassicLinkDnsSupport = EnableVpcClassicLinkDnsSupport'
  { vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVpcClassicLinkDnsSupport' value with any optional fields omitted.
mkEnableVpcClassicLinkDnsSupport
    :: EnableVpcClassicLinkDnsSupport
mkEnableVpcClassicLinkDnsSupport
  = EnableVpcClassicLinkDnsSupport'{vpcId = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evcldsVpcId :: Lens.Lens' EnableVpcClassicLinkDnsSupport (Core.Maybe Types.VpcId)
evcldsVpcId = Lens.field @"vpcId"
{-# INLINEABLE evcldsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.ToQuery EnableVpcClassicLinkDnsSupport where
        toQuery EnableVpcClassicLinkDnsSupport{..}
          = Core.toQueryPair "Action"
              ("EnableVpcClassicLinkDnsSupport" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpcId") vpcId

instance Core.ToHeaders EnableVpcClassicLinkDnsSupport where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableVpcClassicLinkDnsSupport where
        type Rs EnableVpcClassicLinkDnsSupport =
             EnableVpcClassicLinkDnsSupportResponse
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
                 EnableVpcClassicLinkDnsSupportResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableVpcClassicLinkDnsSupportResponse' smart constructor.
data EnableVpcClassicLinkDnsSupportResponse = EnableVpcClassicLinkDnsSupportResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVpcClassicLinkDnsSupportResponse' value with any optional fields omitted.
mkEnableVpcClassicLinkDnsSupportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableVpcClassicLinkDnsSupportResponse
mkEnableVpcClassicLinkDnsSupportResponse responseStatus
  = EnableVpcClassicLinkDnsSupportResponse'{return = Core.Nothing,
                                            responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evcldsrrsReturn :: Lens.Lens' EnableVpcClassicLinkDnsSupportResponse (Core.Maybe Core.Bool)
evcldsrrsReturn = Lens.field @"return"
{-# INLINEABLE evcldsrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evcldsrrsResponseStatus :: Lens.Lens' EnableVpcClassicLinkDnsSupportResponse Core.Int
evcldsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE evcldsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
