{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.WithdrawByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops advertising an address range that is provisioned as an address pool.
--
-- You can perform this operation at most once every 10 seconds, even if you specify different address ranges each time.
-- It can take a few minutes before traffic to the specified addresses stops routing to AWS because of BGP propagation delays.
module Network.AWS.EC2.WithdrawByoipCidr
    (
    -- * Creating a request
      WithdrawByoipCidr (..)
    , mkWithdrawByoipCidr
    -- ** Request lenses
    , wbcCidr
    , wbcDryRun

    -- * Destructuring the response
    , WithdrawByoipCidrResponse (..)
    , mkWithdrawByoipCidrResponse
    -- ** Response lenses
    , wbcrrsByoipCidr
    , wbcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkWithdrawByoipCidr' smart constructor.
data WithdrawByoipCidr = WithdrawByoipCidr'
  { cidr :: Core.Text
    -- ^ The address range, in CIDR notation.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WithdrawByoipCidr' value with any optional fields omitted.
mkWithdrawByoipCidr
    :: Core.Text -- ^ 'cidr'
    -> WithdrawByoipCidr
mkWithdrawByoipCidr cidr
  = WithdrawByoipCidr'{cidr, dryRun = Core.Nothing}

-- | The address range, in CIDR notation.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcCidr :: Lens.Lens' WithdrawByoipCidr Core.Text
wbcCidr = Lens.field @"cidr"
{-# INLINEABLE wbcCidr #-}
{-# DEPRECATED cidr "Use generic-lens or generic-optics with 'cidr' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcDryRun :: Lens.Lens' WithdrawByoipCidr (Core.Maybe Core.Bool)
wbcDryRun = Lens.field @"dryRun"
{-# INLINEABLE wbcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery WithdrawByoipCidr where
        toQuery WithdrawByoipCidr{..}
          = Core.toQueryPair "Action" ("WithdrawByoipCidr" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Cidr" cidr
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders WithdrawByoipCidr where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest WithdrawByoipCidr where
        type Rs WithdrawByoipCidr = WithdrawByoipCidrResponse
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
                 WithdrawByoipCidrResponse' Core.<$>
                   (x Core..@? "byoipCidr") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkWithdrawByoipCidrResponse' smart constructor.
data WithdrawByoipCidrResponse = WithdrawByoipCidrResponse'
  { byoipCidr :: Core.Maybe Types.ByoipCidr
    -- ^ Information about the address pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WithdrawByoipCidrResponse' value with any optional fields omitted.
mkWithdrawByoipCidrResponse
    :: Core.Int -- ^ 'responseStatus'
    -> WithdrawByoipCidrResponse
mkWithdrawByoipCidrResponse responseStatus
  = WithdrawByoipCidrResponse'{byoipCidr = Core.Nothing,
                               responseStatus}

-- | Information about the address pool.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcrrsByoipCidr :: Lens.Lens' WithdrawByoipCidrResponse (Core.Maybe Types.ByoipCidr)
wbcrrsByoipCidr = Lens.field @"byoipCidr"
{-# INLINEABLE wbcrrsByoipCidr #-}
{-# DEPRECATED byoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcrrsResponseStatus :: Lens.Lens' WithdrawByoipCidrResponse Core.Int
wbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE wbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
