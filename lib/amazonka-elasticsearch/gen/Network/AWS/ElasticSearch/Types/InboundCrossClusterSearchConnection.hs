{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
  ( InboundCrossClusterSearchConnection (..),

    -- * Smart constructor
    mkInboundCrossClusterSearchConnection,

    -- * Lenses
    iccscDestinationDomainInfo,
    iccscCrossClusterSearchConnectionId,
    iccscConnectionStatus,
    iccscSourceDomainInfo,
  )
where

import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies details of an inbound connection.
--
-- /See:/ 'mkInboundCrossClusterSearchConnection' smart constructor.
data InboundCrossClusterSearchConnection = InboundCrossClusterSearchConnection'
  { -- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
    destinationDomainInfo :: Lude.Maybe DomainInformation,
    -- | Specifies the connection id for the inbound cross-cluster search connection.
    crossClusterSearchConnectionId :: Lude.Maybe Lude.Text,
    -- | Specifies the @'InboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
    connectionStatus :: Lude.Maybe InboundCrossClusterSearchConnectionStatus,
    -- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
    sourceDomainInfo :: Lude.Maybe DomainInformation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- * 'destinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
-- * 'crossClusterSearchConnectionId' - Specifies the connection id for the inbound cross-cluster search connection.
-- * 'connectionStatus' - Specifies the @'InboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
-- * 'sourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
mkInboundCrossClusterSearchConnection ::
  InboundCrossClusterSearchConnection
mkInboundCrossClusterSearchConnection =
  InboundCrossClusterSearchConnection'
    { destinationDomainInfo =
        Lude.Nothing,
      crossClusterSearchConnectionId = Lude.Nothing,
      connectionStatus = Lude.Nothing,
      sourceDomainInfo = Lude.Nothing
    }

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscDestinationDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Lude.Maybe DomainInformation)
iccscDestinationDomainInfo = Lens.lens (destinationDomainInfo :: InboundCrossClusterSearchConnection -> Lude.Maybe DomainInformation) (\s a -> s {destinationDomainInfo = a} :: InboundCrossClusterSearchConnection)
{-# DEPRECATED iccscDestinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead." #-}

-- | Specifies the connection id for the inbound cross-cluster search connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscCrossClusterSearchConnectionId :: Lens.Lens' InboundCrossClusterSearchConnection (Lude.Maybe Lude.Text)
iccscCrossClusterSearchConnectionId = Lens.lens (crossClusterSearchConnectionId :: InboundCrossClusterSearchConnection -> Lude.Maybe Lude.Text) (\s a -> s {crossClusterSearchConnectionId = a} :: InboundCrossClusterSearchConnection)
{-# DEPRECATED iccscCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

-- | Specifies the @'InboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscConnectionStatus :: Lens.Lens' InboundCrossClusterSearchConnection (Lude.Maybe InboundCrossClusterSearchConnectionStatus)
iccscConnectionStatus = Lens.lens (connectionStatus :: InboundCrossClusterSearchConnection -> Lude.Maybe InboundCrossClusterSearchConnectionStatus) (\s a -> s {connectionStatus = a} :: InboundCrossClusterSearchConnection)
{-# DEPRECATED iccscConnectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead." #-}

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscSourceDomainInfo :: Lens.Lens' InboundCrossClusterSearchConnection (Lude.Maybe DomainInformation)
iccscSourceDomainInfo = Lens.lens (sourceDomainInfo :: InboundCrossClusterSearchConnection -> Lude.Maybe DomainInformation) (\s a -> s {sourceDomainInfo = a} :: InboundCrossClusterSearchConnection)
{-# DEPRECATED iccscSourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead." #-}

instance Lude.FromJSON InboundCrossClusterSearchConnection where
  parseJSON =
    Lude.withObject
      "InboundCrossClusterSearchConnection"
      ( \x ->
          InboundCrossClusterSearchConnection'
            Lude.<$> (x Lude..:? "DestinationDomainInfo")
            Lude.<*> (x Lude..:? "CrossClusterSearchConnectionId")
            Lude.<*> (x Lude..:? "ConnectionStatus")
            Lude.<*> (x Lude..:? "SourceDomainInfo")
      )
