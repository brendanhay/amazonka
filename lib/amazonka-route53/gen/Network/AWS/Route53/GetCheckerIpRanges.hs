{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetCheckerIpRanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /Important:/ @GetCheckerIpRanges@ still works, but we recommend that you download ip-ranges.json, which includes IP address ranges for all AWS services. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/route-53-ip-addresses.html IP Address Ranges of Amazon Route 53 Servers> in the /Amazon Route 53 Developer Guide/ .
module Network.AWS.Route53.GetCheckerIpRanges
    (
    -- * Creating a request
      GetCheckerIpRanges (..)
    , mkGetCheckerIpRanges

    -- * Destructuring the response
    , GetCheckerIpRangesResponse (..)
    , mkGetCheckerIpRangesResponse
    -- ** Response lenses
    , gcirrrsCheckerIpRanges
    , gcirrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Empty request.
--
-- /See:/ 'mkGetCheckerIpRanges' smart constructor.
data GetCheckerIpRanges = GetCheckerIpRanges'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCheckerIpRanges' value with any optional fields omitted.
mkGetCheckerIpRanges
    :: GetCheckerIpRanges
mkGetCheckerIpRanges = GetCheckerIpRanges'

instance Core.ToQuery GetCheckerIpRanges where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCheckerIpRanges where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCheckerIpRanges where
        type Rs GetCheckerIpRanges = GetCheckerIpRangesResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2013-04-01/checkeripranges",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetCheckerIpRangesResponse' Core.<$>
                   (x Core..@ "CheckerIpRanges" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the @CheckerIpRanges@ element.
--
-- /See:/ 'mkGetCheckerIpRangesResponse' smart constructor.
data GetCheckerIpRangesResponse = GetCheckerIpRangesResponse'
  { checkerIpRanges :: [Types.IPAddressCidr]
    -- ^ A complex type that contains sorted list of IP ranges in CIDR format for Amazon Route 53 health checkers.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCheckerIpRangesResponse' value with any optional fields omitted.
mkGetCheckerIpRangesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCheckerIpRangesResponse
mkGetCheckerIpRangesResponse responseStatus
  = GetCheckerIpRangesResponse'{checkerIpRanges = Core.mempty,
                                responseStatus}

-- | A complex type that contains sorted list of IP ranges in CIDR format for Amazon Route 53 health checkers.
--
-- /Note:/ Consider using 'checkerIpRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrrsCheckerIpRanges :: Lens.Lens' GetCheckerIpRangesResponse [Types.IPAddressCidr]
gcirrrsCheckerIpRanges = Lens.field @"checkerIpRanges"
{-# INLINEABLE gcirrrsCheckerIpRanges #-}
{-# DEPRECATED checkerIpRanges "Use generic-lens or generic-optics with 'checkerIpRanges' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrrsResponseStatus :: Lens.Lens' GetCheckerIpRangesResponse Core.Int
gcirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
