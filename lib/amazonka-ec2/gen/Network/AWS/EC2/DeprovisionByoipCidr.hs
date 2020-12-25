{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeprovisionByoipCidr (..),
    mkDeprovisionByoipCidr,

    -- ** Request lenses
    dbcfCidr,
    dbcfDryRun,

    -- * Destructuring the response
    DeprovisionByoipCidrResponse (..),
    mkDeprovisionByoipCidrResponse,

    -- ** Response lenses
    dbcrfrsByoipCidr,
    dbcrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeprovisionByoipCidr' smart constructor.
data DeprovisionByoipCidr = DeprovisionByoipCidr'
  { -- | The address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
    cidr :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprovisionByoipCidr' value with any optional fields omitted.
mkDeprovisionByoipCidr ::
  -- | 'cidr'
  Types.String ->
  DeprovisionByoipCidr
mkDeprovisionByoipCidr cidr =
  DeprovisionByoipCidr' {cidr, dryRun = Core.Nothing}

-- | The address range, in CIDR notation. The prefix must be the same prefix that you specified when you provisioned the address range.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcfCidr :: Lens.Lens' DeprovisionByoipCidr Types.String
dbcfCidr = Lens.field @"cidr"
{-# DEPRECATED dbcfCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcfDryRun :: Lens.Lens' DeprovisionByoipCidr (Core.Maybe Core.Bool)
dbcfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dbcfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeprovisionByoipCidr where
  type Rs DeprovisionByoipCidr = DeprovisionByoipCidrResponse
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
            ( Core.pure ("Action", "DeprovisionByoipCidr")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Cidr" cidr)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeprovisionByoipCidrResponse'
            Core.<$> (x Core..@? "byoipCidr") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeprovisionByoipCidrResponse' smart constructor.
data DeprovisionByoipCidrResponse = DeprovisionByoipCidrResponse'
  { -- | Information about the address range.
    byoipCidr :: Core.Maybe Types.ByoipCidr,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprovisionByoipCidrResponse' value with any optional fields omitted.
mkDeprovisionByoipCidrResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeprovisionByoipCidrResponse
mkDeprovisionByoipCidrResponse responseStatus =
  DeprovisionByoipCidrResponse'
    { byoipCidr = Core.Nothing,
      responseStatus
    }

-- | Information about the address range.
--
-- /Note:/ Consider using 'byoipCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrfrsByoipCidr :: Lens.Lens' DeprovisionByoipCidrResponse (Core.Maybe Types.ByoipCidr)
dbcrfrsByoipCidr = Lens.field @"byoipCidr"
{-# DEPRECATED dbcrfrsByoipCidr "Use generic-lens or generic-optics with 'byoipCidr' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcrfrsResponseStatus :: Lens.Lens' DeprovisionByoipCidrResponse Core.Int
dbcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
