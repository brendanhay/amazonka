{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    WithdrawByoipCidr (..),
    mkWithdrawByoipCidr,

    -- ** Request lenses
    wbcCidr,
    wbcDryRun,

    -- * Destructuring the response
    WithdrawByoipCidrResponse (..),
    mkWithdrawByoipCidrResponse,

    -- ** Response lenses
    wbcrrsByoipCidr,
    wbcrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkWithdrawByoipCidr' smart constructor.
data WithdrawByoipCidr = WithdrawByoipCidr'
  { -- | The address range, in CIDR notation.
    cidr :: Types.Cidr,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WithdrawByoipCidr' value with any optional fields omitted.
mkWithdrawByoipCidr ::
  -- | 'cidr'
  Types.Cidr ->
  WithdrawByoipCidr
mkWithdrawByoipCidr cidr =
  WithdrawByoipCidr' {cidr, dryRun = Core.Nothing}

-- | The address range, in CIDR notation.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcCidr :: Lens.Lens' WithdrawByoipCidr Types.Cidr
wbcCidr = Lens.field @"cidr"
{-# DEPRECATED wbcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcDryRun :: Lens.Lens' WithdrawByoipCidr (Core.Maybe Core.Bool)
wbcDryRun = Lens.field @"dryRun"
{-# DEPRECATED wbcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest WithdrawByoipCidr where
  type Rs WithdrawByoipCidr = WithdrawByoipCidrResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "WithdrawByoipCidr")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Cidr" cidr)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          WithdrawByoipCidrResponse'
            Core.<$> (x Core..@? "byoipCidr") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkWithdrawByoipCidrResponse' smart constructor.
data WithdrawByoipCidrResponse = WithdrawByoipCidrResponse'
  { -- | Information about the address pool.
    byoipCidr :: Core.Maybe Types.ByoipCidr,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WithdrawByoipCidrResponse' value with any optional fields omitted.
mkWithdrawByoipCidrResponse ::
  -- | 'responseStatus'
  Core.Int ->
  WithdrawByoipCidrResponse
mkWithdrawByoipCidrResponse responseStatus =
  WithdrawByoipCidrResponse'
    { byoipCidr = Core.Nothing,
      responseStatus
    }

-- | Information about the address pool.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcrrsByoipCidr :: Lens.Lens' WithdrawByoipCidrResponse (Core.Maybe Types.ByoipCidr)
wbcrrsByoipCidr = Lens.field @"byoipCidr"
{-# DEPRECATED wbcrrsByoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wbcrrsResponseStatus :: Lens.Lens' WithdrawByoipCidrResponse Core.Int
wbcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED wbcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
