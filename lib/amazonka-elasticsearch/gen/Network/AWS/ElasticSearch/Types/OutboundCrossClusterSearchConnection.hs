{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
  ( OutboundCrossClusterSearchConnection (..),

    -- * Smart constructor
    mkOutboundCrossClusterSearchConnection,

    -- * Lenses
    occscDestinationDomainInfo,
    occscConnectionAlias,
    occscCrossClusterSearchConnectionId,
    occscConnectionStatus,
    occscSourceDomainInfo,
  )
where

import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies details of an outbound connection.
--
-- /See:/ 'mkOutboundCrossClusterSearchConnection' smart constructor.
data OutboundCrossClusterSearchConnection = OutboundCrossClusterSearchConnection'
  { destinationDomainInfo ::
      Lude.Maybe
        DomainInformation,
    connectionAlias ::
      Lude.Maybe
        Lude.Text,
    crossClusterSearchConnectionId ::
      Lude.Maybe
        Lude.Text,
    connectionStatus ::
      Lude.Maybe
        OutboundCrossClusterSearchConnectionStatus,
    sourceDomainInfo ::
      Lude.Maybe
        DomainInformation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- * 'connectionAlias' - Specifies the connection alias for the outbound cross-cluster search connection.
-- * 'connectionStatus' - Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
-- * 'crossClusterSearchConnectionId' - Specifies the connection id for the outbound cross-cluster search connection.
-- * 'destinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
-- * 'sourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
mkOutboundCrossClusterSearchConnection ::
  OutboundCrossClusterSearchConnection
mkOutboundCrossClusterSearchConnection =
  OutboundCrossClusterSearchConnection'
    { destinationDomainInfo =
        Lude.Nothing,
      connectionAlias = Lude.Nothing,
      crossClusterSearchConnectionId = Lude.Nothing,
      connectionStatus = Lude.Nothing,
      sourceDomainInfo = Lude.Nothing
    }

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscDestinationDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Lude.Maybe DomainInformation)
occscDestinationDomainInfo = Lens.lens (destinationDomainInfo :: OutboundCrossClusterSearchConnection -> Lude.Maybe DomainInformation) (\s a -> s {destinationDomainInfo = a} :: OutboundCrossClusterSearchConnection)
{-# DEPRECATED occscDestinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead." #-}

-- | Specifies the connection alias for the outbound cross-cluster search connection.
--
-- /Note:/ Consider using 'connectionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscConnectionAlias :: Lens.Lens' OutboundCrossClusterSearchConnection (Lude.Maybe Lude.Text)
occscConnectionAlias = Lens.lens (connectionAlias :: OutboundCrossClusterSearchConnection -> Lude.Maybe Lude.Text) (\s a -> s {connectionAlias = a} :: OutboundCrossClusterSearchConnection)
{-# DEPRECATED occscConnectionAlias "Use generic-lens or generic-optics with 'connectionAlias' instead." #-}

-- | Specifies the connection id for the outbound cross-cluster search connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscCrossClusterSearchConnectionId :: Lens.Lens' OutboundCrossClusterSearchConnection (Lude.Maybe Lude.Text)
occscCrossClusterSearchConnectionId = Lens.lens (crossClusterSearchConnectionId :: OutboundCrossClusterSearchConnection -> Lude.Maybe Lude.Text) (\s a -> s {crossClusterSearchConnectionId = a} :: OutboundCrossClusterSearchConnection)
{-# DEPRECATED occscCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

-- | Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the outbound connection.
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscConnectionStatus :: Lens.Lens' OutboundCrossClusterSearchConnection (Lude.Maybe OutboundCrossClusterSearchConnectionStatus)
occscConnectionStatus = Lens.lens (connectionStatus :: OutboundCrossClusterSearchConnection -> Lude.Maybe OutboundCrossClusterSearchConnectionStatus) (\s a -> s {connectionStatus = a} :: OutboundCrossClusterSearchConnection)
{-# DEPRECATED occscConnectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead." #-}

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscSourceDomainInfo :: Lens.Lens' OutboundCrossClusterSearchConnection (Lude.Maybe DomainInformation)
occscSourceDomainInfo = Lens.lens (sourceDomainInfo :: OutboundCrossClusterSearchConnection -> Lude.Maybe DomainInformation) (\s a -> s {sourceDomainInfo = a} :: OutboundCrossClusterSearchConnection)
{-# DEPRECATED occscSourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead." #-}

instance Lude.FromJSON OutboundCrossClusterSearchConnection where
  parseJSON =
    Lude.withObject
      "OutboundCrossClusterSearchConnection"
      ( \x ->
          OutboundCrossClusterSearchConnection'
            Lude.<$> (x Lude..:? "DestinationDomainInfo")
            Lude.<*> (x Lude..:? "ConnectionAlias")
            Lude.<*> (x Lude..:? "CrossClusterSearchConnectionId")
            Lude.<*> (x Lude..:? "ConnectionStatus")
            Lude.<*> (x Lude..:? "SourceDomainInfo")
      )
