{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cross-cluster search connection from a source domain to a destination domain.
module Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
    (
    -- * Creating a request
      CreateOutboundCrossClusterSearchConnection (..)
    , mkCreateOutboundCrossClusterSearchConnection
    -- ** Request lenses
    , coccscSourceDomainInfo
    , coccscDestinationDomainInfo
    , coccscConnectionAlias

    -- * Destructuring the response
    , CreateOutboundCrossClusterSearchConnectionResponse (..)
    , mkCreateOutboundCrossClusterSearchConnectionResponse
    -- ** Response lenses
    , coccscrrsConnectionAlias
    , coccscrrsConnectionStatus
    , coccscrrsCrossClusterSearchConnectionId
    , coccscrrsDestinationDomainInfo
    , coccscrrsSourceDomainInfo
    , coccscrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'CreateOutboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkCreateOutboundCrossClusterSearchConnection' smart constructor.
data CreateOutboundCrossClusterSearchConnection = CreateOutboundCrossClusterSearchConnection'
  { sourceDomainInfo :: Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
  , destinationDomainInfo :: Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
  , connectionAlias :: Types.ConnectionAlias
    -- ^ Specifies the connection alias that will be used by the customer for this connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOutboundCrossClusterSearchConnection' value with any optional fields omitted.
mkCreateOutboundCrossClusterSearchConnection
    :: Types.DomainInformation -- ^ 'sourceDomainInfo'
    -> Types.DomainInformation -- ^ 'destinationDomainInfo'
    -> Types.ConnectionAlias -- ^ 'connectionAlias'
    -> CreateOutboundCrossClusterSearchConnection
mkCreateOutboundCrossClusterSearchConnection sourceDomainInfo
  destinationDomainInfo connectionAlias
  = CreateOutboundCrossClusterSearchConnection'{sourceDomainInfo,
                                                destinationDomainInfo, connectionAlias}

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscSourceDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnection Types.DomainInformation
coccscSourceDomainInfo = Lens.field @"sourceDomainInfo"
{-# INLINEABLE coccscSourceDomainInfo #-}
{-# DEPRECATED sourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead"  #-}

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscDestinationDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnection Types.DomainInformation
coccscDestinationDomainInfo = Lens.field @"destinationDomainInfo"
{-# INLINEABLE coccscDestinationDomainInfo #-}
{-# DEPRECATED destinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead"  #-}

-- | Specifies the connection alias that will be used by the customer for this connection.
--
-- /Note:/ Consider using 'connectionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscConnectionAlias :: Lens.Lens' CreateOutboundCrossClusterSearchConnection Types.ConnectionAlias
coccscConnectionAlias = Lens.field @"connectionAlias"
{-# INLINEABLE coccscConnectionAlias #-}
{-# DEPRECATED connectionAlias "Use generic-lens or generic-optics with 'connectionAlias' instead"  #-}

instance Core.ToQuery CreateOutboundCrossClusterSearchConnection
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateOutboundCrossClusterSearchConnection
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateOutboundCrossClusterSearchConnection
         where
        toJSON CreateOutboundCrossClusterSearchConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SourceDomainInfo" Core..= sourceDomainInfo),
                  Core.Just ("DestinationDomainInfo" Core..= destinationDomainInfo),
                  Core.Just ("ConnectionAlias" Core..= connectionAlias)])

instance Core.AWSRequest CreateOutboundCrossClusterSearchConnection
         where
        type Rs CreateOutboundCrossClusterSearchConnection =
             CreateOutboundCrossClusterSearchConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-01-01/es/ccs/outboundConnection",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateOutboundCrossClusterSearchConnectionResponse' Core.<$>
                   (x Core..:? "ConnectionAlias") Core.<*>
                     x Core..:? "ConnectionStatus"
                     Core.<*> x Core..:? "CrossClusterSearchConnectionId"
                     Core.<*> x Core..:? "DestinationDomainInfo"
                     Core.<*> x Core..:? "SourceDomainInfo"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @'CreateOutboundCrossClusterSearchConnection' @ request. Contains the details of the newly created cross-cluster search connection.
--
-- /See:/ 'mkCreateOutboundCrossClusterSearchConnectionResponse' smart constructor.
data CreateOutboundCrossClusterSearchConnectionResponse = CreateOutboundCrossClusterSearchConnectionResponse'
  { connectionAlias :: Core.Maybe Types.ConnectionAlias
    -- ^ Specifies the connection alias provided during the create connection request.
  , connectionStatus :: Core.Maybe Types.OutboundCrossClusterSearchConnectionStatus
    -- ^ Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the newly created connection.
  , crossClusterSearchConnectionId :: Core.Maybe Types.CrossClusterSearchConnectionId
    -- ^ Unique id for the created outbound connection, which is used for subsequent operations on connection.
  , destinationDomainInfo :: Core.Maybe Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
  , sourceDomainInfo :: Core.Maybe Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOutboundCrossClusterSearchConnectionResponse' value with any optional fields omitted.
mkCreateOutboundCrossClusterSearchConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateOutboundCrossClusterSearchConnectionResponse
mkCreateOutboundCrossClusterSearchConnectionResponse responseStatus
  = CreateOutboundCrossClusterSearchConnectionResponse'{connectionAlias
                                                          = Core.Nothing,
                                                        connectionStatus = Core.Nothing,
                                                        crossClusterSearchConnectionId =
                                                          Core.Nothing,
                                                        destinationDomainInfo = Core.Nothing,
                                                        sourceDomainInfo = Core.Nothing,
                                                        responseStatus}

-- | Specifies the connection alias provided during the create connection request.
--
-- /Note:/ Consider using 'connectionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrrsConnectionAlias :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Core.Maybe Types.ConnectionAlias)
coccscrrsConnectionAlias = Lens.field @"connectionAlias"
{-# INLINEABLE coccscrrsConnectionAlias #-}
{-# DEPRECATED connectionAlias "Use generic-lens or generic-optics with 'connectionAlias' instead"  #-}

-- | Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the newly created connection.
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrrsConnectionStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Core.Maybe Types.OutboundCrossClusterSearchConnectionStatus)
coccscrrsConnectionStatus = Lens.field @"connectionStatus"
{-# INLINEABLE coccscrrsConnectionStatus #-}
{-# DEPRECATED connectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead"  #-}

-- | Unique id for the created outbound connection, which is used for subsequent operations on connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrrsCrossClusterSearchConnectionId :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Core.Maybe Types.CrossClusterSearchConnectionId)
coccscrrsCrossClusterSearchConnectionId = Lens.field @"crossClusterSearchConnectionId"
{-# INLINEABLE coccscrrsCrossClusterSearchConnectionId #-}
{-# DEPRECATED crossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead"  #-}

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrrsDestinationDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Core.Maybe Types.DomainInformation)
coccscrrsDestinationDomainInfo = Lens.field @"destinationDomainInfo"
{-# INLINEABLE coccscrrsDestinationDomainInfo #-}
{-# DEPRECATED destinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead"  #-}

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrrsSourceDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Core.Maybe Types.DomainInformation)
coccscrrsSourceDomainInfo = Lens.field @"sourceDomainInfo"
{-# INLINEABLE coccscrrsSourceDomainInfo #-}
{-# DEPRECATED sourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrrsResponseStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse Core.Int
coccscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE coccscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
