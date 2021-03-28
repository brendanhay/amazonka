{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to delete an existing inbound cross-cluster search connection.
module Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
    (
    -- * Creating a request
      DeleteInboundCrossClusterSearchConnection (..)
    , mkDeleteInboundCrossClusterSearchConnection
    -- ** Request lenses
    , diccscCrossClusterSearchConnectionId

    -- * Destructuring the response
    , DeleteInboundCrossClusterSearchConnectionResponse (..)
    , mkDeleteInboundCrossClusterSearchConnectionResponse
    -- ** Response lenses
    , diccscrfrsCrossClusterSearchConnection
    , diccscrfrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteInboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkDeleteInboundCrossClusterSearchConnection' smart constructor.
newtype DeleteInboundCrossClusterSearchConnection = DeleteInboundCrossClusterSearchConnection'
  { crossClusterSearchConnectionId :: Types.CrossClusterSearchConnectionId
    -- ^ The id of the inbound connection that you want to permanently delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInboundCrossClusterSearchConnection' value with any optional fields omitted.
mkDeleteInboundCrossClusterSearchConnection
    :: Types.CrossClusterSearchConnectionId -- ^ 'crossClusterSearchConnectionId'
    -> DeleteInboundCrossClusterSearchConnection
mkDeleteInboundCrossClusterSearchConnection
  crossClusterSearchConnectionId
  = DeleteInboundCrossClusterSearchConnection'{crossClusterSearchConnectionId}

-- | The id of the inbound connection that you want to permanently delete.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscCrossClusterSearchConnectionId :: Lens.Lens' DeleteInboundCrossClusterSearchConnection Types.CrossClusterSearchConnectionId
diccscCrossClusterSearchConnectionId = Lens.field @"crossClusterSearchConnectionId"
{-# INLINEABLE diccscCrossClusterSearchConnectionId #-}
{-# DEPRECATED crossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead"  #-}

instance Core.ToQuery DeleteInboundCrossClusterSearchConnection
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInboundCrossClusterSearchConnection
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteInboundCrossClusterSearchConnection
         where
        type Rs DeleteInboundCrossClusterSearchConnection =
             DeleteInboundCrossClusterSearchConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2015-01-01/es/ccs/inboundConnection/" Core.<>
                             Core.toText crossClusterSearchConnectionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteInboundCrossClusterSearchConnectionResponse' Core.<$>
                   (x Core..:? "CrossClusterSearchConnection") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @'DeleteInboundCrossClusterSearchConnection' @ operation. Contains details of deleted inbound connection.
--
-- /See:/ 'mkDeleteInboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteInboundCrossClusterSearchConnectionResponse = DeleteInboundCrossClusterSearchConnectionResponse'
  { crossClusterSearchConnection :: Core.Maybe Types.InboundCrossClusterSearchConnection
    -- ^ Specifies the @'InboundCrossClusterSearchConnection' @ of deleted inbound connection. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInboundCrossClusterSearchConnectionResponse' value with any optional fields omitted.
mkDeleteInboundCrossClusterSearchConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteInboundCrossClusterSearchConnectionResponse
mkDeleteInboundCrossClusterSearchConnectionResponse responseStatus
  = DeleteInboundCrossClusterSearchConnectionResponse'{crossClusterSearchConnection
                                                         = Core.Nothing,
                                                       responseStatus}

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of deleted inbound connection. 
--
-- /Note:/ Consider using 'crossClusterSearchConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrfrsCrossClusterSearchConnection :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse (Core.Maybe Types.InboundCrossClusterSearchConnection)
diccscrfrsCrossClusterSearchConnection = Lens.field @"crossClusterSearchConnection"
{-# INLINEABLE diccscrfrsCrossClusterSearchConnection #-}
{-# DEPRECATED crossClusterSearchConnection "Use generic-lens or generic-optics with 'crossClusterSearchConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrfrsResponseStatus :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse Core.Int
diccscrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diccscrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
