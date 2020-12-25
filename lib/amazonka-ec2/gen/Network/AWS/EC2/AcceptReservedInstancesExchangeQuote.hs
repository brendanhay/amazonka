{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the Convertible Reserved Instance exchange quote described in the 'GetReservedInstancesExchangeQuote' call.
module Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
  ( -- * Creating a request
    AcceptReservedInstancesExchangeQuote (..),
    mkAcceptReservedInstancesExchangeQuote,

    -- ** Request lenses
    arieqReservedInstanceIds,
    arieqDryRun,
    arieqTargetConfigurations,

    -- * Destructuring the response
    AcceptReservedInstancesExchangeQuoteResponse (..),
    mkAcceptReservedInstancesExchangeQuoteResponse,

    -- ** Response lenses
    arieqrrsExchangeId,
    arieqrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for accepting the quote.
--
-- /See:/ 'mkAcceptReservedInstancesExchangeQuote' smart constructor.
data AcceptReservedInstancesExchangeQuote = AcceptReservedInstancesExchangeQuote'
  { -- | The IDs of the Convertible Reserved Instances to exchange for another Convertible Reserved Instance of the same or higher value.
    reservedInstanceIds :: [Types.ReservationId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
    targetConfigurations :: Core.Maybe [Types.TargetConfigurationRequest]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptReservedInstancesExchangeQuote' value with any optional fields omitted.
mkAcceptReservedInstancesExchangeQuote ::
  AcceptReservedInstancesExchangeQuote
mkAcceptReservedInstancesExchangeQuote =
  AcceptReservedInstancesExchangeQuote'
    { reservedInstanceIds =
        Core.mempty,
      dryRun = Core.Nothing,
      targetConfigurations = Core.Nothing
    }

-- | The IDs of the Convertible Reserved Instances to exchange for another Convertible Reserved Instance of the same or higher value.
--
-- /Note:/ Consider using 'reservedInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqReservedInstanceIds :: Lens.Lens' AcceptReservedInstancesExchangeQuote [Types.ReservationId]
arieqReservedInstanceIds = Lens.field @"reservedInstanceIds"
{-# DEPRECATED arieqReservedInstanceIds "Use generic-lens or generic-optics with 'reservedInstanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqDryRun :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Core.Maybe Core.Bool)
arieqDryRun = Lens.field @"dryRun"
{-# DEPRECATED arieqDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqTargetConfigurations :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Core.Maybe [Types.TargetConfigurationRequest])
arieqTargetConfigurations = Lens.field @"targetConfigurations"
{-# DEPRECATED arieqTargetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead." #-}

instance Core.AWSRequest AcceptReservedInstancesExchangeQuote where
  type
    Rs AcceptReservedInstancesExchangeQuote =
      AcceptReservedInstancesExchangeQuoteResponse
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
            ( Core.pure ("Action", "AcceptReservedInstancesExchangeQuote")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "ReservedInstanceId" reservedInstanceIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryList "TargetConfiguration"
                            Core.<$> targetConfigurations
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptReservedInstancesExchangeQuoteResponse'
            Core.<$> (x Core..@? "exchangeId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of the exchange and whether it was @successful@ .
--
-- /See:/ 'mkAcceptReservedInstancesExchangeQuoteResponse' smart constructor.
data AcceptReservedInstancesExchangeQuoteResponse = AcceptReservedInstancesExchangeQuoteResponse'
  { -- | The ID of the successful exchange.
    exchangeId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptReservedInstancesExchangeQuoteResponse' value with any optional fields omitted.
mkAcceptReservedInstancesExchangeQuoteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptReservedInstancesExchangeQuoteResponse
mkAcceptReservedInstancesExchangeQuoteResponse responseStatus =
  AcceptReservedInstancesExchangeQuoteResponse'
    { exchangeId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the successful exchange.
--
-- /Note:/ Consider using 'exchangeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqrrsExchangeId :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse (Core.Maybe Types.String)
arieqrrsExchangeId = Lens.field @"exchangeId"
{-# DEPRECATED arieqrrsExchangeId "Use generic-lens or generic-optics with 'exchangeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqrrsResponseStatus :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse Core.Int
arieqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED arieqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
