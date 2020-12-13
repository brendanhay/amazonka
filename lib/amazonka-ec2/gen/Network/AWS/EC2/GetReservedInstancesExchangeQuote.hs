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
    grieqTargetConfigurations,
    grieqDryRun,

    -- * Destructuring the response
    GetReservedInstancesExchangeQuoteResponse (..),
    mkGetReservedInstancesExchangeQuoteResponse,

    -- ** Response lenses
    grieqrsValidationFailureReason,
    grieqrsTargetConfigurationValueRollup,
    grieqrsCurrencyCode,
    grieqrsTargetConfigurationValueSet,
    grieqrsReservedInstanceValueRollup,
    grieqrsOutputReservedInstancesWillExpireAt,
    grieqrsReservedInstanceValueSet,
    grieqrsIsValidExchange,
    grieqrsPaymentDue,
    grieqrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for GetReservedInstanceExchangeQuote.
--
-- /See:/ 'mkGetReservedInstancesExchangeQuote' smart constructor.
data GetReservedInstancesExchangeQuote = GetReservedInstancesExchangeQuote'
  { -- | The IDs of the Convertible Reserved Instances to exchange.
    reservedInstanceIds :: [Lude.Text],
    -- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
    targetConfigurations :: Lude.Maybe [TargetConfigurationRequest],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservedInstancesExchangeQuote' with the minimum fields required to make a request.
--
-- * 'reservedInstanceIds' - The IDs of the Convertible Reserved Instances to exchange.
-- * 'targetConfigurations' - The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetReservedInstancesExchangeQuote ::
  GetReservedInstancesExchangeQuote
mkGetReservedInstancesExchangeQuote =
  GetReservedInstancesExchangeQuote'
    { reservedInstanceIds =
        Lude.mempty,
      targetConfigurations = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IDs of the Convertible Reserved Instances to exchange.
--
-- /Note:/ Consider using 'reservedInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqReservedInstanceIds :: Lens.Lens' GetReservedInstancesExchangeQuote [Lude.Text]
grieqReservedInstanceIds = Lens.lens (reservedInstanceIds :: GetReservedInstancesExchangeQuote -> [Lude.Text]) (\s a -> s {reservedInstanceIds = a} :: GetReservedInstancesExchangeQuote)
{-# DEPRECATED grieqReservedInstanceIds "Use generic-lens or generic-optics with 'reservedInstanceIds' instead." #-}

-- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqTargetConfigurations :: Lens.Lens' GetReservedInstancesExchangeQuote (Lude.Maybe [TargetConfigurationRequest])
grieqTargetConfigurations = Lens.lens (targetConfigurations :: GetReservedInstancesExchangeQuote -> Lude.Maybe [TargetConfigurationRequest]) (\s a -> s {targetConfigurations = a} :: GetReservedInstancesExchangeQuote)
{-# DEPRECATED grieqTargetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqDryRun :: Lens.Lens' GetReservedInstancesExchangeQuote (Lude.Maybe Lude.Bool)
grieqDryRun = Lens.lens (dryRun :: GetReservedInstancesExchangeQuote -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetReservedInstancesExchangeQuote)
{-# DEPRECATED grieqDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetReservedInstancesExchangeQuote where
  type
    Rs GetReservedInstancesExchangeQuote =
      GetReservedInstancesExchangeQuoteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetReservedInstancesExchangeQuoteResponse'
            Lude.<$> (x Lude..@? "validationFailureReason")
            Lude.<*> (x Lude..@? "targetConfigurationValueRollup")
            Lude.<*> (x Lude..@? "currencyCode")
            Lude.<*> ( x Lude..@? "targetConfigurationValueSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "reservedInstanceValueRollup")
            Lude.<*> (x Lude..@? "outputReservedInstancesWillExpireAt")
            Lude.<*> ( x Lude..@? "reservedInstanceValueSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "isValidExchange")
            Lude.<*> (x Lude..@? "paymentDue")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReservedInstancesExchangeQuote where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetReservedInstancesExchangeQuote where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReservedInstancesExchangeQuote where
  toQuery GetReservedInstancesExchangeQuote' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetReservedInstancesExchangeQuote" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "ReservedInstanceId" reservedInstanceIds,
        Lude.toQuery
          ( Lude.toQueryList "TargetConfiguration"
              Lude.<$> targetConfigurations
          ),
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of GetReservedInstancesExchangeQuote.
--
-- /See:/ 'mkGetReservedInstancesExchangeQuoteResponse' smart constructor.
data GetReservedInstancesExchangeQuoteResponse = GetReservedInstancesExchangeQuoteResponse'
  { -- | Describes the reason why the exchange cannot be completed.
    validationFailureReason :: Lude.Maybe Lude.Text,
    -- | The cost associated with the Reserved Instance.
    targetConfigurationValueRollup :: Lude.Maybe ReservationValue,
    -- | The currency of the transaction.
    currencyCode :: Lude.Maybe Lude.Text,
    -- | The values of the target Convertible Reserved Instances.
    targetConfigurationValueSet :: Lude.Maybe [TargetReservationValue],
    -- | The cost associated with the Reserved Instance.
    reservedInstanceValueRollup :: Lude.Maybe ReservationValue,
    -- | The new end date of the reservation term.
    outputReservedInstancesWillExpireAt :: Lude.Maybe Lude.DateTime,
    -- | The configuration of your Convertible Reserved Instances.
    reservedInstanceValueSet :: Lude.Maybe [ReservedInstanceReservationValue],
    -- | If @true@ , the exchange is valid. If @false@ , the exchange cannot be completed.
    isValidExchange :: Lude.Maybe Lude.Bool,
    -- | The total true upfront charge for the exchange.
    paymentDue :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservedInstancesExchangeQuoteResponse' with the minimum fields required to make a request.
--
-- * 'validationFailureReason' - Describes the reason why the exchange cannot be completed.
-- * 'targetConfigurationValueRollup' - The cost associated with the Reserved Instance.
-- * 'currencyCode' - The currency of the transaction.
-- * 'targetConfigurationValueSet' - The values of the target Convertible Reserved Instances.
-- * 'reservedInstanceValueRollup' - The cost associated with the Reserved Instance.
-- * 'outputReservedInstancesWillExpireAt' - The new end date of the reservation term.
-- * 'reservedInstanceValueSet' - The configuration of your Convertible Reserved Instances.
-- * 'isValidExchange' - If @true@ , the exchange is valid. If @false@ , the exchange cannot be completed.
-- * 'paymentDue' - The total true upfront charge for the exchange.
-- * 'responseStatus' - The response status code.
mkGetReservedInstancesExchangeQuoteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReservedInstancesExchangeQuoteResponse
mkGetReservedInstancesExchangeQuoteResponse pResponseStatus_ =
  GetReservedInstancesExchangeQuoteResponse'
    { validationFailureReason =
        Lude.Nothing,
      targetConfigurationValueRollup = Lude.Nothing,
      currencyCode = Lude.Nothing,
      targetConfigurationValueSet = Lude.Nothing,
      reservedInstanceValueRollup = Lude.Nothing,
      outputReservedInstancesWillExpireAt = Lude.Nothing,
      reservedInstanceValueSet = Lude.Nothing,
      isValidExchange = Lude.Nothing,
      paymentDue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Describes the reason why the exchange cannot be completed.
--
-- /Note:/ Consider using 'validationFailureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsValidationFailureReason :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe Lude.Text)
grieqrsValidationFailureReason = Lens.lens (validationFailureReason :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe Lude.Text) (\s a -> s {validationFailureReason = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsValidationFailureReason "Use generic-lens or generic-optics with 'validationFailureReason' instead." #-}

-- | The cost associated with the Reserved Instance.
--
-- /Note:/ Consider using 'targetConfigurationValueRollup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsTargetConfigurationValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe ReservationValue)
grieqrsTargetConfigurationValueRollup = Lens.lens (targetConfigurationValueRollup :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe ReservationValue) (\s a -> s {targetConfigurationValueRollup = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsTargetConfigurationValueRollup "Use generic-lens or generic-optics with 'targetConfigurationValueRollup' instead." #-}

-- | The currency of the transaction.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsCurrencyCode :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe Lude.Text)
grieqrsCurrencyCode = Lens.lens (currencyCode :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The values of the target Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurationValueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsTargetConfigurationValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe [TargetReservationValue])
grieqrsTargetConfigurationValueSet = Lens.lens (targetConfigurationValueSet :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe [TargetReservationValue]) (\s a -> s {targetConfigurationValueSet = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsTargetConfigurationValueSet "Use generic-lens or generic-optics with 'targetConfigurationValueSet' instead." #-}

-- | The cost associated with the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstanceValueRollup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsReservedInstanceValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe ReservationValue)
grieqrsReservedInstanceValueRollup = Lens.lens (reservedInstanceValueRollup :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe ReservationValue) (\s a -> s {reservedInstanceValueRollup = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsReservedInstanceValueRollup "Use generic-lens or generic-optics with 'reservedInstanceValueRollup' instead." #-}

-- | The new end date of the reservation term.
--
-- /Note:/ Consider using 'outputReservedInstancesWillExpireAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsOutputReservedInstancesWillExpireAt :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe Lude.DateTime)
grieqrsOutputReservedInstancesWillExpireAt = Lens.lens (outputReservedInstancesWillExpireAt :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {outputReservedInstancesWillExpireAt = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsOutputReservedInstancesWillExpireAt "Use generic-lens or generic-optics with 'outputReservedInstancesWillExpireAt' instead." #-}

-- | The configuration of your Convertible Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstanceValueSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsReservedInstanceValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe [ReservedInstanceReservationValue])
grieqrsReservedInstanceValueSet = Lens.lens (reservedInstanceValueSet :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe [ReservedInstanceReservationValue]) (\s a -> s {reservedInstanceValueSet = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsReservedInstanceValueSet "Use generic-lens or generic-optics with 'reservedInstanceValueSet' instead." #-}

-- | If @true@ , the exchange is valid. If @false@ , the exchange cannot be completed.
--
-- /Note:/ Consider using 'isValidExchange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsIsValidExchange :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe Lude.Bool)
grieqrsIsValidExchange = Lens.lens (isValidExchange :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isValidExchange = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsIsValidExchange "Use generic-lens or generic-optics with 'isValidExchange' instead." #-}

-- | The total true upfront charge for the exchange.
--
-- /Note:/ Consider using 'paymentDue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsPaymentDue :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Lude.Maybe Lude.Text)
grieqrsPaymentDue = Lens.lens (paymentDue :: GetReservedInstancesExchangeQuoteResponse -> Lude.Maybe Lude.Text) (\s a -> s {paymentDue = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsPaymentDue "Use generic-lens or generic-optics with 'paymentDue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grieqrsResponseStatus :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse Lude.Int
grieqrsResponseStatus = Lens.lens (responseStatus :: GetReservedInstancesExchangeQuoteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED grieqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
