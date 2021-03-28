{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AcceptReservedNodeExchange (..)
    , mkAcceptReservedNodeExchange
    -- ** Request lenses
    , arneReservedNodeId
    , arneTargetReservedNodeOfferingId

    -- * Destructuring the response
    , AcceptReservedNodeExchangeResponse (..)
    , mkAcceptReservedNodeExchangeResponse
    -- ** Response lenses
    , arnerrsExchangedReservedNode
    , arnerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptReservedNodeExchange' smart constructor.
data AcceptReservedNodeExchange = AcceptReservedNodeExchange'
  { reservedNodeId :: Core.Text
    -- ^ A string representing the node identifier of the DC1 Reserved Node to be exchanged.
  , targetReservedNodeOfferingId :: Core.Text
    -- ^ The unique identifier of the DC2 Reserved Node offering to be used for the exchange. You can obtain the value for the parameter by calling 'GetReservedNodeExchangeOfferings' 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptReservedNodeExchange' value with any optional fields omitted.
mkAcceptReservedNodeExchange
    :: Core.Text -- ^ 'reservedNodeId'
    -> Core.Text -- ^ 'targetReservedNodeOfferingId'
    -> AcceptReservedNodeExchange
mkAcceptReservedNodeExchange reservedNodeId
  targetReservedNodeOfferingId
  = AcceptReservedNodeExchange'{reservedNodeId,
                                targetReservedNodeOfferingId}

-- | A string representing the node identifier of the DC1 Reserved Node to be exchanged.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arneReservedNodeId :: Lens.Lens' AcceptReservedNodeExchange Core.Text
arneReservedNodeId = Lens.field @"reservedNodeId"
{-# INLINEABLE arneReservedNodeId #-}
{-# DEPRECATED reservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead"  #-}

-- | The unique identifier of the DC2 Reserved Node offering to be used for the exchange. You can obtain the value for the parameter by calling 'GetReservedNodeExchangeOfferings' 
--
-- /Note:/ Consider using 'targetReservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arneTargetReservedNodeOfferingId :: Lens.Lens' AcceptReservedNodeExchange Core.Text
arneTargetReservedNodeOfferingId = Lens.field @"targetReservedNodeOfferingId"
{-# INLINEABLE arneTargetReservedNodeOfferingId #-}
{-# DEPRECATED targetReservedNodeOfferingId "Use generic-lens or generic-optics with 'targetReservedNodeOfferingId' instead"  #-}

instance Core.ToQuery AcceptReservedNodeExchange where
        toQuery AcceptReservedNodeExchange{..}
          = Core.toQueryPair "Action"
              ("AcceptReservedNodeExchange" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ReservedNodeId" reservedNodeId
              Core.<>
              Core.toQueryPair "TargetReservedNodeOfferingId"
                targetReservedNodeOfferingId

instance Core.ToHeaders AcceptReservedNodeExchange where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AcceptReservedNodeExchange where
        type Rs AcceptReservedNodeExchange =
             AcceptReservedNodeExchangeResponse
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
          = Response.receiveXMLWrapper "AcceptReservedNodeExchangeResult"
              (\ s h x ->
                 AcceptReservedNodeExchangeResponse' Core.<$>
                   (x Core..@? "ExchangedReservedNode") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptReservedNodeExchangeResponse' smart constructor.
data AcceptReservedNodeExchangeResponse = AcceptReservedNodeExchangeResponse'
  { exchangedReservedNode :: Core.Maybe Types.ReservedNode
    -- ^ 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AcceptReservedNodeExchangeResponse' value with any optional fields omitted.
mkAcceptReservedNodeExchangeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptReservedNodeExchangeResponse
mkAcceptReservedNodeExchangeResponse responseStatus
  = AcceptReservedNodeExchangeResponse'{exchangedReservedNode =
                                          Core.Nothing,
                                        responseStatus}

-- | 
--
-- /Note:/ Consider using 'exchangedReservedNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnerrsExchangedReservedNode :: Lens.Lens' AcceptReservedNodeExchangeResponse (Core.Maybe Types.ReservedNode)
arnerrsExchangedReservedNode = Lens.field @"exchangedReservedNode"
{-# INLINEABLE arnerrsExchangedReservedNode #-}
{-# DEPRECATED exchangedReservedNode "Use generic-lens or generic-optics with 'exchangedReservedNode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arnerrsResponseStatus :: Lens.Lens' AcceptReservedNodeExchangeResponse Core.Int
arnerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE arnerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
