{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetReservedInstancesExchangeQuote
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a quote and exchange information for exchanging one or more specified Convertible Reserved Instances for a new Convertible Reserved Instance. If the exchange cannot be performed, the reason is returned in the response. Use 'AcceptReservedInstancesExchangeQuote' to perform the exchange.
module Network.AWS.EC2.GetReservedInstancesExchangeQuote
  ( -- * Creating a request
    GetReservedInstancesExchangeQuote (..),
    mkGetReservedInstancesExchangeQuote,

    -- ** Request lenses
    grieqReservedInstanceIds,
    grieqDryRun,
    grieqTargetConfigurations,

    -- * Destructuring the response
    GetReservedInstancesExchangeQuoteResponse (..),
    mkGetReservedInstancesExchangeQuoteResponse,

    -- ** Response lenses
    grieqrrsCurrencyCode,
    grieqrrsIsValidExchange,
    grieqrrsOutputReservedInstancesWillExpireAt,
    grieqrrsPaymentDue,
    grieqrrsReservedInstanceValueRollup,
    grieqrrsReservedInstanceValueSet,
    grieqrrsTargetConfigurationValueRollup,
    grieqrrsTargetConfigurationValueSet,
    grieqrrsValidationFailureReason,
    grieqrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for GetReservedInstanceExchangeQuote.
--
-- /See:/ 'mkGetReservedInstancesExchangeQuote' smart constructor.
data GetReservedInstancesExchangeQuote = GetReservedInstancesExchangeQuote'
  { -- | The IDs of the Convertible Reserved Instances to exchange.
    reservedInstanceIds :: [Types.ReservationId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
    targetConfigurations :: Core.Maybe [Types.TargetConfigurationRequest]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservedInstancesExchangeQuote' value with any optional fields omitted.
mkGetReservedInstancesExchangeQuote ::
  GetReservedInstancesExchangeQuote
mkGetReservedInstancesExchangeQuote =
  GetReservedInstancesExchangeQuote'
    { reservedInstanceIds =
        Core.mempty,
      dryRun = Core.Nothing,
      targetConfigurations = Core.Nothing
    }

-- | The IDs of the Convertible Reserved Instances to exchange.
--
-- /Note:/ Consider using 'reservedInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqReservedInstanceIds :: Lens.Lens' GetReservedInstancesExchangeQuote [Types.ReservationId]
grieqReservedInstanceIds = Lens.field @"reservedInstanceIds"
{-# DEPRECATED grieqReservedInstanceIds "Use generic-lens or generic-optics with 'reservedInstanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqDryRun :: Lens.Lens' GetReservedInstancesExchangeQuote (Core.Maybe Core.Bool)
grieqDryRun = Lens.field @"dryRun"
{-# DEPRECATED grieqDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqTargetConfigurations :: Lens.Lens' GetReservedInstancesExchangeQuote (Core.Maybe [Types.TargetConfigurationRequest])
grieqTargetConfigurations = Lens.field @"targetConfigurations"
{-# DEPRECATED grieqTargetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead." #-}

instance Core.AWSRequest GetReservedInstancesExchangeQuote where
  type
    Rs GetReservedInstancesExchangeQuote =
      GetReservedInstancesExchangeQuoteResponse
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
            ( Core.pure ("Action", "GetReservedInstancesExchangeQuote")
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
          GetReservedInstancesExchangeQuoteResponse'
            Core.<$> (x Core..@? "currencyCode")
            Core.<*> (x Core..@? "isValidExchange")
            Core.<*> (x Core..@? "outputReservedInstancesWillExpireAt")
            Core.<*> (x Core..@? "paymentDue")
            Core.<*> (x Core..@? "reservedInstanceValueRollup")
            Core.<*> ( x Core..@? "reservedInstanceValueSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "targetConfigurationValueRollup")
            Core.<*> ( x Core..@? "targetConfigurationValueSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "validationFailureReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of GetReservedInstancesExchangeQuote.
--
-- /See:/ 'mkGetReservedInstancesExchangeQuoteResponse' smart constructor.
data GetReservedInstancesExchangeQuoteResponse = GetReservedInstancesExchangeQuoteResponse'
  { -- | The currency of the transaction.
    currencyCode :: Core.Maybe Types.String,
    -- | If @true@ , the exchange is valid. If @false@ , the exchange cannot be completed.
    isValidExchange :: Core.Maybe Core.Bool,
    -- | The new end date of the reservation term.
    outputReservedInstancesWillExpireAt :: Core.Maybe Core.UTCTime,
    -- | The total true upfront charge for the exchange.
    paymentDue :: Core.Maybe Types.String,
    -- | The cost associated with the Reserved Instance.
    reservedInstanceValueRollup :: Core.Maybe Types.ReservationValue,
    -- | The configuration of your Convertible Reserved Instances.
    reservedInstanceValueSet :: Core.Maybe [Types.ReservedInstanceReservationValue],
    -- | The cost associated with the Reserved Instance.
    targetConfigurationValueRollup :: Core.Maybe Types.ReservationValue,
    -- | The values of the target Convertible Reserved Instances.
    targetConfigurationValueSet :: Core.Maybe [Types.TargetReservationValue],
    -- | Describes the reason why the exchange cannot be completed.
    validationFailureReason :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetReservedInstancesExchangeQuoteResponse' value with any optional fields omitted.
mkGetReservedInstancesExchangeQuoteResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetReservedInstancesExchangeQuoteResponse
mkGetReservedInstancesExchangeQuoteResponse responseStatus =
  GetReservedInstancesExchangeQuoteResponse'
    { currencyCode =
        Core.Nothing,
      isValidExchange = Core.Nothing,
      outputReservedInstancesWillExpireAt = Core.Nothing,
      paymentDue = Core.Nothing,
      reservedInstanceValueRollup = Core.Nothing,
      reservedInstanceValueSet = Core.Nothing,
      targetConfigurationValueRollup = Core.Nothing,
      targetConfigurationValueSet = Core.Nothing,
      validationFailureReason = Core.Nothing,
      responseStatus
    }

-- | The currency of the transaction.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsCurrencyCode :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Types.String)
grieqrrsCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED grieqrrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | If @true@ , the exchange is valid. If @false@ , the exchange cannot be completed.
--
-- /Note:/ Consider using 'isValidExchange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsIsValidExchange :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Core.Bool)
grieqrrsIsValidExchange = Lens.field @"isValidExchange"
{-# DEPRECATED grieqrrsIsValidExchange "Use generic-lens or generic-optics with 'isValidExchange' instead." #-}

-- | The new end date of the reservation term.
--
-- /Note:/ Consider using 'outputReservedInstancesWillExpireAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsOutputReservedInstancesWillExpireAt :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Core.UTCTime)
grieqrrsOutputReservedInstancesWillExpireAt = Lens.field @"outputReservedInstancesWillExpireAt"
{-# DEPRECATED grieqrrsOutputReservedInstancesWillExpireAt "Use generic-lens or generic-optics with 'outputReservedInstancesWillExpireAt' instead." #-}

-- | The total true upfront charge for the exchange.
--
-- /Note:/ Consider using 'paymentDue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsPaymentDue :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Types.String)
grieqrrsPaymentDue = Lens.field @"paymentDue"
{-# DEPRECATED grieqrrsPaymentDue "Use generic-lens or generic-optics with 'paymentDue' instead." #-}

-- | The cost associated with the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstanceValueRollup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsReservedInstanceValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Types.ReservationValue)
grieqrrsReservedInstanceValueRollup = Lens.field @"reservedInstanceValueRollup"
{-# DEPRECATED grieqrrsReservedInstanceValueRollup "Use generic-lens or generic-optics with 'reservedInstanceValueRollup' instead." #-}

-- | The configuration of your Convertible Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstanceValueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsReservedInstanceValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe [Types.ReservedInstanceReservationValue])
grieqrrsReservedInstanceValueSet = Lens.field @"reservedInstanceValueSet"
{-# DEPRECATED grieqrrsReservedInstanceValueSet "Use generic-lens or generic-optics with 'reservedInstanceValueSet' instead." #-}

-- | The cost associated with the Reserved Instance.
--
-- /Note:/ Consider using 'targetConfigurationValueRollup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsTargetConfigurationValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Types.ReservationValue)
grieqrrsTargetConfigurationValueRollup = Lens.field @"targetConfigurationValueRollup"
{-# DEPRECATED grieqrrsTargetConfigurationValueRollup "Use generic-lens or generic-optics with 'targetConfigurationValueRollup' instead." #-}

-- | The values of the target Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurationValueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsTargetConfigurationValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe [Types.TargetReservationValue])
grieqrrsTargetConfigurationValueSet = Lens.field @"targetConfigurationValueSet"
{-# DEPRECATED grieqrrsTargetConfigurationValueSet "Use generic-lens or generic-optics with 'targetConfigurationValueSet' instead." #-}

-- | Describes the reason why the exchange cannot be completed.
--
-- /Note:/ Consider using 'validationFailureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsValidationFailureReason :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Types.String)
grieqrrsValidationFailureReason = Lens.field @"validationFailureReason"
{-# DEPRECATED grieqrrsValidationFailureReason "Use generic-lens or generic-optics with 'validationFailureReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsResponseStatus :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse Core.Int
grieqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grieqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
