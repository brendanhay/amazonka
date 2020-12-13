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
    arieqTargetConfigurations,
    arieqDryRun,

    -- * Destructuring the response
    AcceptReservedInstancesExchangeQuoteResponse (..),
    mkAcceptReservedInstancesExchangeQuoteResponse,

    -- ** Response lenses
    arieqrsExchangeId,
    arieqrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for accepting the quote.
--
-- /See:/ 'mkAcceptReservedInstancesExchangeQuote' smart constructor.
data AcceptReservedInstancesExchangeQuote = AcceptReservedInstancesExchangeQuote'
  { -- | The IDs of the Convertible Reserved Instances to exchange for another Convertible Reserved Instance of the same or higher value.
    reservedInstanceIds :: [Lude.Text],
    -- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
    targetConfigurations :: Lude.Maybe [TargetConfigurationRequest],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptReservedInstancesExchangeQuote' with the minimum fields required to make a request.
--
-- * 'reservedInstanceIds' - The IDs of the Convertible Reserved Instances to exchange for another Convertible Reserved Instance of the same or higher value.
-- * 'targetConfigurations' - The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAcceptReservedInstancesExchangeQuote ::
  AcceptReservedInstancesExchangeQuote
mkAcceptReservedInstancesExchangeQuote =
  AcceptReservedInstancesExchangeQuote'
    { reservedInstanceIds =
        Lude.mempty,
      targetConfigurations = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IDs of the Convertible Reserved Instances to exchange for another Convertible Reserved Instance of the same or higher value.
--
-- /Note:/ Consider using 'reservedInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqReservedInstanceIds :: Lens.Lens' AcceptReservedInstancesExchangeQuote [Lude.Text]
arieqReservedInstanceIds = Lens.lens (reservedInstanceIds :: AcceptReservedInstancesExchangeQuote -> [Lude.Text]) (\s a -> s {reservedInstanceIds = a} :: AcceptReservedInstancesExchangeQuote)
{-# DEPRECATED arieqReservedInstanceIds "Use generic-lens or generic-optics with 'reservedInstanceIds' instead." #-}

-- | The configuration of the target Convertible Reserved Instance to exchange for your current Convertible Reserved Instances.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqTargetConfigurations :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Lude.Maybe [TargetConfigurationRequest])
arieqTargetConfigurations = Lens.lens (targetConfigurations :: AcceptReservedInstancesExchangeQuote -> Lude.Maybe [TargetConfigurationRequest]) (\s a -> s {targetConfigurations = a} :: AcceptReservedInstancesExchangeQuote)
{-# DEPRECATED arieqTargetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqDryRun :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Lude.Maybe Lude.Bool)
arieqDryRun = Lens.lens (dryRun :: AcceptReservedInstancesExchangeQuote -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AcceptReservedInstancesExchangeQuote)
{-# DEPRECATED arieqDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AcceptReservedInstancesExchangeQuote where
  type
    Rs AcceptReservedInstancesExchangeQuote =
      AcceptReservedInstancesExchangeQuoteResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AcceptReservedInstancesExchangeQuoteResponse'
            Lude.<$> (x Lude..@? "exchangeId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptReservedInstancesExchangeQuote where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AcceptReservedInstancesExchangeQuote where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptReservedInstancesExchangeQuote where
  toQuery AcceptReservedInstancesExchangeQuote' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AcceptReservedInstancesExchangeQuote" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "ReservedInstanceId" reservedInstanceIds,
        Lude.toQuery
          ( Lude.toQueryList "TargetConfiguration"
              Lude.<$> targetConfigurations
          ),
        "DryRun" Lude.=: dryRun
      ]

-- | The result of the exchange and whether it was @successful@ .
--
-- /See:/ 'mkAcceptReservedInstancesExchangeQuoteResponse' smart constructor.
data AcceptReservedInstancesExchangeQuoteResponse = AcceptReservedInstancesExchangeQuoteResponse'
  { -- | The ID of the successful exchange.
    exchangeId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptReservedInstancesExchangeQuoteResponse' with the minimum fields required to make a request.
--
-- * 'exchangeId' - The ID of the successful exchange.
-- * 'responseStatus' - The response status code.
mkAcceptReservedInstancesExchangeQuoteResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptReservedInstancesExchangeQuoteResponse
mkAcceptReservedInstancesExchangeQuoteResponse pResponseStatus_ =
  AcceptReservedInstancesExchangeQuoteResponse'
    { exchangeId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the successful exchange.
--
-- /Note:/ Consider using 'exchangeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqrsExchangeId :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse (Lude.Maybe Lude.Text)
arieqrsExchangeId = Lens.lens (exchangeId :: AcceptReservedInstancesExchangeQuoteResponse -> Lude.Maybe Lude.Text) (\s a -> s {exchangeId = a} :: AcceptReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED arieqrsExchangeId "Use generic-lens or generic-optics with 'exchangeId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arieqrsResponseStatus :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse Lude.Int
arieqrsResponseStatus = Lens.lens (responseStatus :: AcceptReservedInstancesExchangeQuoteResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptReservedInstancesExchangeQuoteResponse)
{-# DEPRECATED arieqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
