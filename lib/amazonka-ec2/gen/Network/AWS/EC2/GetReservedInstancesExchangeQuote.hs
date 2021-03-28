{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetReservedInstancesExchangeQuote (..)
    , mkGetReservedInstancesExchangeQuote
    -- ** Request lenses
    , grieqReservedInstanceIds
    , grieqDryRun
    , grieqTargetConfigurations

    -- * Destructuring the response
    , GetReservedInstancesExchangeQuoteResponse (..)
    , mkGetReservedInstancesExchangeQuoteResponse
    -- ** Response lenses
    , grieqrrsCurrencyCode
    , grieqrrsIsValidExchange
    , grieqrrsOutputReservedInstancesWillExpireAt
    , grieqrrsPaymentDue
    , grieqrrsReservedInstanceValueRollup
    , grieqrrsReservedInstanceValueSet
    , grieqrrsTargetConfigurationValueRollup
    , grieqrrsTargetConfigurationValueSet
    , grieqrrsValidationFailureReason
    , grieqrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for GetReservedInstanceExchangeQuote.
--
-- /See:/ 'mkGetReservedInstancesExchangeQuote' smart constructor.
data GetReservedInstancesExchangeQuote = GetReservedInstancesExchangeQuote'
  { reservedInstanceIds :: [Types.ReservationId]
    -- ^ The IDs of the Convertible Reserved Instances to exchange.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , targetConfigurations :: Core.Maybe [Types.TargetConfigurationRequest]
    -- ^ The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservedInstancesExchangeQuote' value with any optional fields omitted.
mkGetReservedInstancesExchangeQuote
    :: GetReservedInstancesExchangeQuote
mkGetReservedInstancesExchangeQuote
  = GetReservedInstancesExchangeQuote'{reservedInstanceIds =
                                         Core.mempty,
                                       dryRun = Core.Nothing, targetConfigurations = Core.Nothing}

-- | The IDs of the Convertible Reserved Instances to exchange.
--
-- /Note:/ Consider using 'reservedInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqReservedInstanceIds :: Lens.Lens' GetReservedInstancesExchangeQuote [Types.ReservationId]
grieqReservedInstanceIds = Lens.field @"reservedInstanceIds"
{-# INLINEABLE grieqReservedInstanceIds #-}
{-# DEPRECATED reservedInstanceIds "Use generic-lens or generic-optics with 'reservedInstanceIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqDryRun :: Lens.Lens' GetReservedInstancesExchangeQuote (Core.Maybe Core.Bool)
grieqDryRun = Lens.field @"dryRun"
{-# INLINEABLE grieqDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqTargetConfigurations :: Lens.Lens' GetReservedInstancesExchangeQuote (Core.Maybe [Types.TargetConfigurationRequest])
grieqTargetConfigurations = Lens.field @"targetConfigurations"
{-# INLINEABLE grieqTargetConfigurations #-}
{-# DEPRECATED targetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead"  #-}

instance Core.ToQuery GetReservedInstancesExchangeQuote where
        toQuery GetReservedInstancesExchangeQuote{..}
          = Core.toQueryPair "Action"
              ("GetReservedInstancesExchangeQuote" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "ReservedInstanceId" reservedInstanceIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TargetConfiguration")
                targetConfigurations

instance Core.ToHeaders GetReservedInstancesExchangeQuote where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetReservedInstancesExchangeQuote where
        type Rs GetReservedInstancesExchangeQuote =
             GetReservedInstancesExchangeQuoteResponse
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
                 GetReservedInstancesExchangeQuoteResponse' Core.<$>
                   (x Core..@? "currencyCode") Core.<*> x Core..@? "isValidExchange"
                     Core.<*> x Core..@? "outputReservedInstancesWillExpireAt"
                     Core.<*> x Core..@? "paymentDue"
                     Core.<*> x Core..@? "reservedInstanceValueRollup"
                     Core.<*>
                     x Core..@? "reservedInstanceValueSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> x Core..@? "targetConfigurationValueRollup"
                     Core.<*>
                     x Core..@? "targetConfigurationValueSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> x Core..@? "validationFailureReason"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of GetReservedInstancesExchangeQuote.
--
-- /See:/ 'mkGetReservedInstancesExchangeQuoteResponse' smart constructor.
data GetReservedInstancesExchangeQuoteResponse = GetReservedInstancesExchangeQuoteResponse'
  { currencyCode :: Core.Maybe Core.Text
    -- ^ The currency of the transaction.
  , isValidExchange :: Core.Maybe Core.Bool
    -- ^ If @true@ , the exchange is valid. If @false@ , the exchange cannot be completed.
  , outputReservedInstancesWillExpireAt :: Core.Maybe Core.UTCTime
    -- ^ The new end date of the reservation term.
  , paymentDue :: Core.Maybe Core.Text
    -- ^ The total true upfront charge for the exchange.
  , reservedInstanceValueRollup :: Core.Maybe Types.ReservationValue
    -- ^ The cost associated with the Reserved Instance.
  , reservedInstanceValueSet :: Core.Maybe [Types.ReservedInstanceReservationValue]
    -- ^ The configuration of your Convertible Reserved Instances.
  , targetConfigurationValueRollup :: Core.Maybe Types.ReservationValue
    -- ^ The cost associated with the Reserved Instance.
  , targetConfigurationValueSet :: Core.Maybe [Types.TargetReservationValue]
    -- ^ The values of the target Convertible Reserved Instances.
  , validationFailureReason :: Core.Maybe Core.Text
    -- ^ Describes the reason why the exchange cannot be completed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetReservedInstancesExchangeQuoteResponse' value with any optional fields omitted.
mkGetReservedInstancesExchangeQuoteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetReservedInstancesExchangeQuoteResponse
mkGetReservedInstancesExchangeQuoteResponse responseStatus
  = GetReservedInstancesExchangeQuoteResponse'{currencyCode =
                                                 Core.Nothing,
                                               isValidExchange = Core.Nothing,
                                               outputReservedInstancesWillExpireAt = Core.Nothing,
                                               paymentDue = Core.Nothing,
                                               reservedInstanceValueRollup = Core.Nothing,
                                               reservedInstanceValueSet = Core.Nothing,
                                               targetConfigurationValueRollup = Core.Nothing,
                                               targetConfigurationValueSet = Core.Nothing,
                                               validationFailureReason = Core.Nothing,
                                               responseStatus}

-- | The currency of the transaction.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsCurrencyCode :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Core.Text)
grieqrrsCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE grieqrrsCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | If @true@ , the exchange is valid. If @false@ , the exchange cannot be completed.
--
-- /Note:/ Consider using 'isValidExchange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsIsValidExchange :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Core.Bool)
grieqrrsIsValidExchange = Lens.field @"isValidExchange"
{-# INLINEABLE grieqrrsIsValidExchange #-}
{-# DEPRECATED isValidExchange "Use generic-lens or generic-optics with 'isValidExchange' instead"  #-}

-- | The new end date of the reservation term.
--
-- /Note:/ Consider using 'outputReservedInstancesWillExpireAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsOutputReservedInstancesWillExpireAt :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Core.UTCTime)
grieqrrsOutputReservedInstancesWillExpireAt = Lens.field @"outputReservedInstancesWillExpireAt"
{-# INLINEABLE grieqrrsOutputReservedInstancesWillExpireAt #-}
{-# DEPRECATED outputReservedInstancesWillExpireAt "Use generic-lens or generic-optics with 'outputReservedInstancesWillExpireAt' instead"  #-}

-- | The total true upfront charge for the exchange.
--
-- /Note:/ Consider using 'paymentDue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsPaymentDue :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Core.Text)
grieqrrsPaymentDue = Lens.field @"paymentDue"
{-# INLINEABLE grieqrrsPaymentDue #-}
{-# DEPRECATED paymentDue "Use generic-lens or generic-optics with 'paymentDue' instead"  #-}

-- | The cost associated with the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstanceValueRollup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsReservedInstanceValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Types.ReservationValue)
grieqrrsReservedInstanceValueRollup = Lens.field @"reservedInstanceValueRollup"
{-# INLINEABLE grieqrrsReservedInstanceValueRollup #-}
{-# DEPRECATED reservedInstanceValueRollup "Use generic-lens or generic-optics with 'reservedInstanceValueRollup' instead"  #-}

-- | The configuration of your Convertible Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstanceValueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsReservedInstanceValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe [Types.ReservedInstanceReservationValue])
grieqrrsReservedInstanceValueSet = Lens.field @"reservedInstanceValueSet"
{-# INLINEABLE grieqrrsReservedInstanceValueSet #-}
{-# DEPRECATED reservedInstanceValueSet "Use generic-lens or generic-optics with 'reservedInstanceValueSet' instead"  #-}

-- | The cost associated with the Reserved Instance.
--
-- /Note:/ Consider using 'targetConfigurationValueRollup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsTargetConfigurationValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Types.ReservationValue)
grieqrrsTargetConfigurationValueRollup = Lens.field @"targetConfigurationValueRollup"
{-# INLINEABLE grieqrrsTargetConfigurationValueRollup #-}
{-# DEPRECATED targetConfigurationValueRollup "Use generic-lens or generic-optics with 'targetConfigurationValueRollup' instead"  #-}

-- | The values of the target Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurationValueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsTargetConfigurationValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe [Types.TargetReservationValue])
grieqrrsTargetConfigurationValueSet = Lens.field @"targetConfigurationValueSet"
{-# INLINEABLE grieqrrsTargetConfigurationValueSet #-}
{-# DEPRECATED targetConfigurationValueSet "Use generic-lens or generic-optics with 'targetConfigurationValueSet' instead"  #-}

-- | Describes the reason why the exchange cannot be completed.
--
-- /Note:/ Consider using 'validationFailureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsValidationFailureReason :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Core.Maybe Core.Text)
grieqrrsValidationFailureReason = Lens.field @"validationFailureReason"
{-# INLINEABLE grieqrrsValidationFailureReason #-}
{-# DEPRECATED validationFailureReason "Use generic-lens or generic-optics with 'validationFailureReason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrrsResponseStatus :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse Core.Int
grieqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grieqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
