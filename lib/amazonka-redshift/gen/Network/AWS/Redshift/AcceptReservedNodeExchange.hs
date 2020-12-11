{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.AcceptReservedNodeExchange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exchanges a DC1 Reserved Node for a DC2 Reserved Node with no changes to the configuration (term, payment type, or number of nodes) and no additional costs.
module Network.AWS.Redshift.AcceptReservedNodeExchange
  ( -- * Creating a request
    AcceptReservedNodeExchange (..),
    mkAcceptReservedNodeExchange,

    -- ** Request lenses
    arneReservedNodeId,
    arneTargetReservedNodeOfferingId,

    -- * Destructuring the response
    AcceptReservedNodeExchangeResponse (..),
    mkAcceptReservedNodeExchangeResponse,

    -- ** Response lenses
    arnersExchangedReservedNode,
    arnersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptReservedNodeExchange' smart constructor.
data AcceptReservedNodeExchange = AcceptReservedNodeExchange'
  { reservedNodeId ::
      Lude.Text,
    targetReservedNodeOfferingId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptReservedNodeExchange' with the minimum fields required to make a request.
--
-- * 'reservedNodeId' - A string representing the node identifier of the DC1 Reserved Node to be exchanged.
-- * 'targetReservedNodeOfferingId' - The unique identifier of the DC2 Reserved Node offering to be used for the exchange. You can obtain the value for the parameter by calling 'GetReservedNodeExchangeOfferings'
mkAcceptReservedNodeExchange ::
  -- | 'reservedNodeId'
  Lude.Text ->
  -- | 'targetReservedNodeOfferingId'
  Lude.Text ->
  AcceptReservedNodeExchange
mkAcceptReservedNodeExchange
  pReservedNodeId_
  pTargetReservedNodeOfferingId_ =
    AcceptReservedNodeExchange'
      { reservedNodeId = pReservedNodeId_,
        targetReservedNodeOfferingId = pTargetReservedNodeOfferingId_
      }

-- | A string representing the node identifier of the DC1 Reserved Node to be exchanged.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arneReservedNodeId :: Lens.Lens' AcceptReservedNodeExchange Lude.Text
arneReservedNodeId = Lens.lens (reservedNodeId :: AcceptReservedNodeExchange -> Lude.Text) (\s a -> s {reservedNodeId = a} :: AcceptReservedNodeExchange)
{-# DEPRECATED arneReservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead." #-}

-- | The unique identifier of the DC2 Reserved Node offering to be used for the exchange. You can obtain the value for the parameter by calling 'GetReservedNodeExchangeOfferings'
--
-- /Note:/ Consider using 'targetReservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arneTargetReservedNodeOfferingId :: Lens.Lens' AcceptReservedNodeExchange Lude.Text
arneTargetReservedNodeOfferingId = Lens.lens (targetReservedNodeOfferingId :: AcceptReservedNodeExchange -> Lude.Text) (\s a -> s {targetReservedNodeOfferingId = a} :: AcceptReservedNodeExchange)
{-# DEPRECATED arneTargetReservedNodeOfferingId "Use generic-lens or generic-optics with 'targetReservedNodeOfferingId' instead." #-}

instance Lude.AWSRequest AcceptReservedNodeExchange where
  type
    Rs AcceptReservedNodeExchange =
      AcceptReservedNodeExchangeResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "AcceptReservedNodeExchangeResult"
      ( \s h x ->
          AcceptReservedNodeExchangeResponse'
            Lude.<$> (x Lude..@? "ExchangedReservedNode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptReservedNodeExchange where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AcceptReservedNodeExchange where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptReservedNodeExchange where
  toQuery AcceptReservedNodeExchange' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AcceptReservedNodeExchange" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ReservedNodeId" Lude.=: reservedNodeId,
        "TargetReservedNodeOfferingId"
          Lude.=: targetReservedNodeOfferingId
      ]

-- | /See:/ 'mkAcceptReservedNodeExchangeResponse' smart constructor.
data AcceptReservedNodeExchangeResponse = AcceptReservedNodeExchangeResponse'
  { exchangedReservedNode ::
      Lude.Maybe
        ReservedNode,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptReservedNodeExchangeResponse' with the minimum fields required to make a request.
--
-- * 'exchangedReservedNode' -
-- * 'responseStatus' - The response status code.
mkAcceptReservedNodeExchangeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptReservedNodeExchangeResponse
mkAcceptReservedNodeExchangeResponse pResponseStatus_ =
  AcceptReservedNodeExchangeResponse'
    { exchangedReservedNode =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- |
--
-- /Note:/ Consider using 'exchangedReservedNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnersExchangedReservedNode :: Lens.Lens' AcceptReservedNodeExchangeResponse (Lude.Maybe ReservedNode)
arnersExchangedReservedNode = Lens.lens (exchangedReservedNode :: AcceptReservedNodeExchangeResponse -> Lude.Maybe ReservedNode) (\s a -> s {exchangedReservedNode = a} :: AcceptReservedNodeExchangeResponse)
{-# DEPRECATED arnersExchangedReservedNode "Use generic-lens or generic-optics with 'exchangedReservedNode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnersResponseStatus :: Lens.Lens' AcceptReservedNodeExchangeResponse Lude.Int
arnersResponseStatus = Lens.lens (responseStatus :: AcceptReservedNodeExchangeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptReservedNodeExchangeResponse)
{-# DEPRECATED arnersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
