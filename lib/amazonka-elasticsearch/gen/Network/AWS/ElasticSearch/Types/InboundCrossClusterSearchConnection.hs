{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
  ( InboundCrossClusterSearchConnection (..)
  -- * Smart constructor
  , mkInboundCrossClusterSearchConnection
  -- * Lenses
  , iccscConnectionStatus
  , iccscCrossClusterSearchConnectionId
  , iccscDestinationDomainInfo
  , iccscSourceDomainInfo
  ) where

import qualified Network.AWS.ElasticSearch.Types.CrossClusterSearchConnectionId as Types
import qualified Network.AWS.ElasticSearch.Types.DomainInformation as Types
import qualified Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies details of an inbound connection.
--
-- /See:/ 'mkInboundCrossClusterSearchConnection' smart constructor.
data InboundCrossClusterSearchConnection = InboundCrossClusterSearchConnection'
  { connectionStatus :: Core.Maybe Types.InboundCrossClusterSearchConnectionStatus
    -- ^ Specifies the @'InboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
  , crossClusterSearchConnectionId :: Core.Maybe Types.CrossClusterSearchConnectionId
    -- ^ Specifies the connection id for the inbound cross-cluster search connection.
  , destinationDomainInfo :: Core.Maybe Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
  , sourceDomainInfo :: Core.Maybe Types.DomainInformation
    -- ^ Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InboundCrossClusterSearchConnection' value with any optional fields omitted.
mkInboundCrossClusterSearchConnection
    :: InboundCrossClusterSearchConnection
mkInboundCrossClusterSearchConnection
  = InboundCrossClusterSearchConnection'{connectionStatus =
                                           Core.Nothing,
                                         crossClusterSearchConnectionId = Core.Nothing,
                                         destinationDomainInfo = Core.Nothing,
                                         sourceDomainInfo = Core.Nothing}

-- | Specifies the @'InboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscConnectionStatus :: Lens.Lens' InboundCrossClusterSearchConnection (Core.Maybe Types.InboundCrossClusterSearchConnectionStatus)
iccscConnectionStatus = Lens.field @"connectionStatus"
{-# INLINEABLE iccscConnectionStatus #-}
{-# DEPRECATED connectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead"  #-}

-- | Specifies the connection id for the inbound cross-cluster search connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscCrossClusterSearchConnectionId :: Lens.Lens' InboundCrossClusterSearchConnection (Core.Maybe Types.CrossClusterSearchConnectionId)
iccscCrossClusterSearchConnectionId = Lens.field @"crossClusterSearchConnectionId"
{-# INLINEABLE iccscCrossClusterSearchConnectionId #-}
{-# DEPRECATED crossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead"  #-}

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscDestinationDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Core.Maybe Types.DomainInformation)
iccscDestinationDomainInfo = Lens.field @"destinationDomainInfo"
{-# INLINEABLE iccscDestinationDomainInfo #-}
{-# DEPRECATED destinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead"  #-}

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscSourceDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Core.Maybe Types.DomainInformation)
iccscSourceDomainInfo = Lens.field @"sourceDomainInfo"
{-# INLINEABLE iccscSourceDomainInfo #-}
{-# DEPRECATED sourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead"  #-}

instance Core.FromJSON InboundCrossClusterSearchConnection where
        parseJSON
          = Core.withObject "InboundCrossClusterSearchConnection" Core.$
              \ x ->
                InboundCrossClusterSearchConnection' Core.<$>
                  (x Core..:? "ConnectionStatus") Core.<*>
                    x Core..:? "CrossClusterSearchConnectionId"
                    Core.<*> x Core..:? "DestinationDomainInfo"
                    Core.<*> x Core..:? "SourceDomainInfo"
