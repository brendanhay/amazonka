{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeprovisionByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases the specified address range that you provisioned for use with your AWS resources through bring your own IP addresses (BYOIP) and deletes the corresponding address pool.
--
-- Before you can release an address range, you must stop advertising it using 'WithdrawByoipCidr' and you must not have any IP addresses allocated from its address range.
module Network.AWS.EC2.DeprovisionByoipCidr
    (
    -- * Creating a request
      DeprovisionByoipCidr (..)
    , mkDeprovisionByoipCidr
    -- ** Request lenses
    , dbcfCidr
    , dbcfDryRun

    -- * Destructuring the response
    , DeprovisionByoipCidrResponse (..)
    , mkDeprovisionByoipCidrResponse
    -- ** Response lenses
    , dbcrfrsByoipCidr
    , dbcrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeprovisionByoipCidr' smart constructor.
data DeprovisionByoipCidr = DeprovisionByoipCidr'
  { cidr :: Core.Text
    -- ^ The address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprovisionByoipCidr' value with any optional fields omitted.
mkDeprovisionByoipCidr
    :: Core.Text -- ^ 'cidr'
    -> DeprovisionByoipCidr
mkDeprovisionByoipCidr cidr
  = DeprovisionByoipCidr'{cidr, dryRun = Core.Nothing}

-- | The address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcfCidr :: Lens.Lens' DeprovisionByoipCidr Core.Text
dbcfCidr = Lens.field @"cidr"
{-# INLINEABLE dbcfCidr #-}
{-# DEPRECATED cidr "Use generic-lens or generic-optics with 'cidr' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcfDryRun :: Lens.Lens' DeprovisionByoipCidr (Core.Maybe Core.Bool)
dbcfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dbcfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeprovisionByoipCidr where
        toQuery DeprovisionByoipCidr{..}
          = Core.toQueryPair "Action" ("DeprovisionByoipCidr" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Cidr" cidr
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeprovisionByoipCidr where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeprovisionByoipCidr where
        type Rs DeprovisionByoipCidr = DeprovisionByoipCidrResponse
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
                 DeprovisionByoipCidrResponse' Core.<$>
                   (x Core..@? "byoipCidr") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeprovisionByoipCidrResponse' smart constructor.
data DeprovisionByoipCidrResponse = DeprovisionByoipCidrResponse'
  { byoipCidr :: Core.Maybe Types.ByoipCidr
    -- ^ Information about the address range.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprovisionByoipCidrResponse' value with any optional fields omitted.
mkDeprovisionByoipCidrResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeprovisionByoipCidrResponse
mkDeprovisionByoipCidrResponse responseStatus
  = DeprovisionByoipCidrResponse'{byoipCidr = Core.Nothing,
                                  responseStatus}

-- | Information about the address range.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrfrsByoipCidr :: Lens.Lens' DeprovisionByoipCidrResponse (Core.Maybe Types.ByoipCidr)
dbcrfrsByoipCidr = Lens.field @"byoipCidr"
{-# INLINEABLE dbcrfrsByoipCidr #-}
{-# DEPRECATED byoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrfrsResponseStatus :: Lens.Lens' DeprovisionByoipCidrResponse Core.Int
dbcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
