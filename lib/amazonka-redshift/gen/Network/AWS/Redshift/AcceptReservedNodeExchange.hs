{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    arnerrsExchangedReservedNode,
    arnerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptReservedNodeExchange' smart constructor.
data AcceptReservedNodeExchange = AcceptReservedNodeExchange'
  { -- | A string representing the node identifier of the DC1 Reserved Node to be exchanged.
    reservedNodeId :: Types.String,
    -- | The unique identifier of the DC2 Reserved Node offering to be used for the exchange. You can obtain the value for the parameter by calling 'GetReservedNodeExchangeOfferings'
    targetReservedNodeOfferingId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptReservedNodeExchange' value with any optional fields omitted.
mkAcceptReservedNodeExchange ::
  -- | 'reservedNodeId'
  Types.String ->
  -- | 'targetReservedNodeOfferingId'
  Types.String ->
  AcceptReservedNodeExchange
mkAcceptReservedNodeExchange
  reservedNodeId
  targetReservedNodeOfferingId =
    AcceptReservedNodeExchange'
      { reservedNodeId,
        targetReservedNodeOfferingId
      }

-- | A string representing the node identifier of the DC1 Reserved Node to be exchanged.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arneReservedNodeId :: Lens.Lens' AcceptReservedNodeExchange Types.String
arneReservedNodeId = Lens.field @"reservedNodeId"
{-# DEPRECATED arneReservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead." #-}

-- | The unique identifier of the DC2 Reserved Node offering to be used for the exchange. You can obtain the value for the parameter by calling 'GetReservedNodeExchangeOfferings'
--
-- /Note:/ Consider using 'targetReservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arneTargetReservedNodeOfferingId :: Lens.Lens' AcceptReservedNodeExchange Types.String
arneTargetReservedNodeOfferingId = Lens.field @"targetReservedNodeOfferingId"
{-# DEPRECATED arneTargetReservedNodeOfferingId "Use generic-lens or generic-optics with 'targetReservedNodeOfferingId' instead." #-}

instance Core.AWSRequest AcceptReservedNodeExchange where
  type
    Rs AcceptReservedNodeExchange =
      AcceptReservedNodeExchangeResponse
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
            ( Core.pure ("Action", "AcceptReservedNodeExchange")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ReservedNodeId" reservedNodeId)
                Core.<> ( Core.toQueryValue
                            "TargetReservedNodeOfferingId"
                            targetReservedNodeOfferingId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "AcceptReservedNodeExchangeResult"
      ( \s h x ->
          AcceptReservedNodeExchangeResponse'
            Core.<$> (x Core..@? "ExchangedReservedNode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAcceptReservedNodeExchangeResponse' smart constructor.
data AcceptReservedNodeExchangeResponse = AcceptReservedNodeExchangeResponse'
  { -- |
    exchangedReservedNode :: Core.Maybe Types.ReservedNode,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AcceptReservedNodeExchangeResponse' value with any optional fields omitted.
mkAcceptReservedNodeExchangeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptReservedNodeExchangeResponse
mkAcceptReservedNodeExchangeResponse responseStatus =
  AcceptReservedNodeExchangeResponse'
    { exchangedReservedNode =
        Core.Nothing,
      responseStatus
    }

-- |
--
-- /Note:/ Consider using 'exchangedReservedNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnerrsExchangedReservedNode :: Lens.Lens' AcceptReservedNodeExchangeResponse (Core.Maybe Types.ReservedNode)
arnerrsExchangedReservedNode = Lens.field @"exchangedReservedNode"
{-# DEPRECATED arnerrsExchangedReservedNode "Use generic-lens or generic-optics with 'exchangedReservedNode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnerrsResponseStatus :: Lens.Lens' AcceptReservedNodeExchangeResponse Core.Int
arnerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED arnerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
