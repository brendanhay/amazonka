{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
  ( OutboundCrossClusterSearchConnection (..)
  -- * Smart constructor
  , mkOutboundCrossClusterSearchConnection
  -- * Lenses
  , occscConnectionAlias
  , occscConnectionStatus
  , occscCrossClusterSearchConnectionId
  , occscDestinationDomainInfo
  , occscSourceDomainInfo
  ) where

import qualified Network.AWS.ElasticSearch.Types.ConnectionAlias as Types
import qualified Network.AWS.ElasticSearch.Types.CrossClusterSearchConnectionId as Types
import qualified Network.AWS.ElasticSearch.Types.DomainInformation as Types
import qualified Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies details of an outbound connection.
--
-- /See:/ 'mkOutboundCrossClusterSearchConnection' smart constructor.
data OutboundCrossClusterSearchConnection = OutboundCrossClusterSearchConnection'
  { connectionAlias :: Core.Maybe Types.ConnectionAlias
    -- ^ Specifies the connection alias for the outbound cross-cluster search connection.
  , connectionStatus :: Core.Maybe Types.OutboundCrossClusterSearchConnectionStatus
    -- ^ Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
  , crossClusterSearchConnectionId :: Core.Maybe Types.CrossClusterSearchConnectionId
    -- ^ Specifies the connection id for the outbound cross-cluster search connection.
  , destinationDomainInfo :: Core.Maybe Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
  , sourceDomainInfo :: Core.Maybe Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutboundCrossClusterSearchConnection' value with any optional fields omitted.
mkOutboundCrossClusterSearchConnection
    :: OutboundCrossClusterSearchConnection
mkOutboundCrossClusterSearchConnection
  = OutboundCrossClusterSearchConnection'{connectionAlias =
                                            Core.Nothing,
                                          connectionStatus = Core.Nothing,
                                          crossClusterSearchConnectionId = Core.Nothing,
                                          destinationDomainInfo = Core.Nothing,
                                          sourceDomainInfo = Core.Nothing}

-- | Specifies the connection alias for the outbound cross-cluster search connection.
--
-- /Note:/ Consider using 'connectionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscConnectionAlias :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe Types.ConnectionAlias)
occscConnectionAlias = Lens.field @"connectionAlias"
{-# INLINEABLE occscConnectionAlias #-}
{-# DEPRECATED connectionAlias "Use generic-lens or generic-optics with 'connectionAlias' instead"  #-}

-- | Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscConnectionStatus :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe Types.OutboundCrossClusterSearchConnectionStatus)
occscConnectionStatus = Lens.field @"connectionStatus"
{-# INLINEABLE occscConnectionStatus #-}
{-# DEPRECATED connectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead"  #-}

-- | Specifies the connection id for the outbound cross-cluster search connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscCrossClusterSearchConnectionId :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe Types.CrossClusterSearchConnectionId)
occscCrossClusterSearchConnectionId = Lens.field @"crossClusterSearchConnectionId"
{-# INLINEABLE occscCrossClusterSearchConnectionId #-}
{-# DEPRECATED crossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead"  #-}

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscDestinationDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe Types.DomainInformation)
occscDestinationDomainInfo = Lens.field @"destinationDomainInfo"
{-# INLINEABLE occscDestinationDomainInfo #-}
{-# DEPRECATED destinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead"  #-}

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscSourceDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Core.Maybe Types.DomainInformation)
occscSourceDomainInfo = Lens.field @"sourceDomainInfo"
{-# INLINEABLE occscSourceDomainInfo #-}
{-# DEPRECATED sourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead"  #-}

instance Core.FromJSON OutboundCrossClusterSearchConnection where
        parseJSON
          = Core.withObject "OutboundCrossClusterSearchConnection" Core.$
              \ x ->
                OutboundCrossClusterSearchConnection' Core.<$>
                  (x Core..:? "ConnectionAlias") Core.<*>
                    x Core..:? "ConnectionStatus"
                    Core.<*> x Core..:? "CrossClusterSearchConnectionId"
                    Core.<*> x Core..:? "DestinationDomainInfo"
                    Core.<*> x Core..:? "SourceDomainInfo"
