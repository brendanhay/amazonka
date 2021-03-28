{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to reject an inbound cross-cluster search connection request.
module Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
    (
    -- * Creating a request
      RejectInboundCrossClusterSearchConnection (..)
    , mkRejectInboundCrossClusterSearchConnection
    -- ** Request lenses
    , riccscCrossClusterSearchConnectionId

    -- * Destructuring the response
    , RejectInboundCrossClusterSearchConnectionResponse (..)
    , mkRejectInboundCrossClusterSearchConnectionResponse
    -- ** Response lenses
    , riccscrrsCrossClusterSearchConnection
    , riccscrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'RejectInboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkRejectInboundCrossClusterSearchConnection' smart constructor.
newtype RejectInboundCrossClusterSearchConnection = RejectInboundCrossClusterSearchConnection'
  { crossClusterSearchConnectionId :: Types.CrossClusterSearchConnectionId
    -- ^ The id of the inbound connection that you want to reject.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectInboundCrossClusterSearchConnection' value with any optional fields omitted.
mkRejectInboundCrossClusterSearchConnection
    :: Types.CrossClusterSearchConnectionId -- ^ 'crossClusterSearchConnectionId'
    -> RejectInboundCrossClusterSearchConnection
mkRejectInboundCrossClusterSearchConnection
  crossClusterSearchConnectionId
  = RejectInboundCrossClusterSearchConnection'{crossClusterSearchConnectionId}

-- | The id of the inbound connection that you want to reject.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riccscCrossClusterSearchConnectionId :: Lens.Lens' RejectInboundCrossClusterSearchConnection Types.CrossClusterSearchConnectionId
riccscCrossClusterSearchConnectionId = Lens.field @"crossClusterSearchConnectionId"
{-# INLINEABLE riccscCrossClusterSearchConnectionId #-}
{-# DEPRECATED crossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead"  #-}

instance Core.ToQuery RejectInboundCrossClusterSearchConnection
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RejectInboundCrossClusterSearchConnection
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RejectInboundCrossClusterSearchConnection
         where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest RejectInboundCrossClusterSearchConnection
         where
        type Rs RejectInboundCrossClusterSearchConnection =
             RejectInboundCrossClusterSearchConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2015-01-01/es/ccs/inboundConnection/" Core.<>
                             Core.toText crossClusterSearchConnectionId
                             Core.<> "/reject",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RejectInboundCrossClusterSearchConnectionResponse' Core.<$>
                   (x Core..:? "CrossClusterSearchConnection") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @'RejectInboundCrossClusterSearchConnection' @ operation. Contains details of rejected inbound connection.
--
-- /See:/ 'mkRejectInboundCrossClusterSearchConnectionResponse' smart constructor.
data RejectInboundCrossClusterSearchConnectionResponse = RejectInboundCrossClusterSearchConnectionResponse'
  { crossClusterSearchConnection :: Core.Maybe Types.InboundCrossClusterSearchConnection
    -- ^ Specifies the @'InboundCrossClusterSearchConnection' @ of rejected inbound connection. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectInboundCrossClusterSearchConnectionResponse' value with any optional fields omitted.
mkRejectInboundCrossClusterSearchConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectInboundCrossClusterSearchConnectionResponse
mkRejectInboundCrossClusterSearchConnectionResponse responseStatus
  = RejectInboundCrossClusterSearchConnectionResponse'{crossClusterSearchConnection
                                                         = Core.Nothing,
                                                       responseStatus}

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of rejected inbound connection. 
--
-- /Note:/ Consider using 'crossClusterSearchConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riccscrrsCrossClusterSearchConnection :: Lens.Lens' RejectInboundCrossClusterSearchConnectionResponse (Core.Maybe Types.InboundCrossClusterSearchConnection)
riccscrrsCrossClusterSearchConnection = Lens.field @"crossClusterSearchConnection"
{-# INLINEABLE riccscrrsCrossClusterSearchConnection #-}
{-# DEPRECATED crossClusterSearchConnection "Use generic-lens or generic-optics with 'crossClusterSearchConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riccscrrsResponseStatus :: Lens.Lens' RejectInboundCrossClusterSearchConnectionResponse Core.Int
riccscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE riccscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
