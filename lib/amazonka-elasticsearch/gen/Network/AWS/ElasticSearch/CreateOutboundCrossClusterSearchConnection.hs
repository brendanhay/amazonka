{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateOutboundCrossClusterSearchConnection (..),
    mkCreateOutboundCrossClusterSearchConnection,

    -- ** Request lenses
    coccscSourceDomainInfo,
    coccscDestinationDomainInfo,
    coccscConnectionAlias,

    -- * Destructuring the response
    CreateOutboundCrossClusterSearchConnectionResponse (..),
    mkCreateOutboundCrossClusterSearchConnectionResponse,

    -- ** Response lenses
    coccscrsDestinationDomainInfo,
    coccscrsConnectionAlias,
    coccscrsCrossClusterSearchConnectionId,
    coccscrsConnectionStatus,
    coccscrsSourceDomainInfo,
    coccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'CreateOutboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkCreateOutboundCrossClusterSearchConnection' smart constructor.
data CreateOutboundCrossClusterSearchConnection = CreateOutboundCrossClusterSearchConnection'
  { sourceDomainInfo ::
      DomainInformation,
    destinationDomainInfo ::
      DomainInformation,
    connectionAlias ::
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

-- | Creates a value of 'CreateOutboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- * 'connectionAlias' - Specifies the connection alias that will be used by the customer for this connection.
-- * 'destinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
-- * 'sourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
mkCreateOutboundCrossClusterSearchConnection ::
  -- | 'sourceDomainInfo'
  DomainInformation ->
  -- | 'destinationDomainInfo'
  DomainInformation ->
  -- | 'connectionAlias'
  Lude.Text ->
  CreateOutboundCrossClusterSearchConnection
mkCreateOutboundCrossClusterSearchConnection
  pSourceDomainInfo_
  pDestinationDomainInfo_
  pConnectionAlias_ =
    CreateOutboundCrossClusterSearchConnection'
      { sourceDomainInfo =
          pSourceDomainInfo_,
        destinationDomainInfo = pDestinationDomainInfo_,
        connectionAlias = pConnectionAlias_
      }

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscSourceDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnection DomainInformation
coccscSourceDomainInfo = Lens.lens (sourceDomainInfo :: CreateOutboundCrossClusterSearchConnection -> DomainInformation) (\s a -> s {sourceDomainInfo = a} :: CreateOutboundCrossClusterSearchConnection)
{-# DEPRECATED coccscSourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead." #-}

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscDestinationDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnection DomainInformation
coccscDestinationDomainInfo = Lens.lens (destinationDomainInfo :: CreateOutboundCrossClusterSearchConnection -> DomainInformation) (\s a -> s {destinationDomainInfo = a} :: CreateOutboundCrossClusterSearchConnection)
{-# DEPRECATED coccscDestinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead." #-}

-- | Specifies the connection alias that will be used by the customer for this connection.
--
-- /Note:/ Consider using 'connectionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscConnectionAlias :: Lens.Lens' CreateOutboundCrossClusterSearchConnection Lude.Text
coccscConnectionAlias = Lens.lens (connectionAlias :: CreateOutboundCrossClusterSearchConnection -> Lude.Text) (\s a -> s {connectionAlias = a} :: CreateOutboundCrossClusterSearchConnection)
{-# DEPRECATED coccscConnectionAlias "Use generic-lens or generic-optics with 'connectionAlias' instead." #-}

instance Lude.AWSRequest CreateOutboundCrossClusterSearchConnection where
  type
    Rs CreateOutboundCrossClusterSearchConnection =
      CreateOutboundCrossClusterSearchConnectionResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateOutboundCrossClusterSearchConnectionResponse'
            Lude.<$> (x Lude..?> "DestinationDomainInfo")
            Lude.<*> (x Lude..?> "ConnectionAlias")
            Lude.<*> (x Lude..?> "CrossClusterSearchConnectionId")
            Lude.<*> (x Lude..?> "ConnectionStatus")
            Lude.<*> (x Lude..?> "SourceDomainInfo")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOutboundCrossClusterSearchConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateOutboundCrossClusterSearchConnection where
  toJSON CreateOutboundCrossClusterSearchConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SourceDomainInfo" Lude..= sourceDomainInfo),
            Lude.Just ("DestinationDomainInfo" Lude..= destinationDomainInfo),
            Lude.Just ("ConnectionAlias" Lude..= connectionAlias)
          ]
      )

instance Lude.ToPath CreateOutboundCrossClusterSearchConnection where
  toPath = Lude.const "/2015-01-01/es/ccs/outboundConnection"

instance Lude.ToQuery CreateOutboundCrossClusterSearchConnection where
  toQuery = Lude.const Lude.mempty

-- | The result of a @'CreateOutboundCrossClusterSearchConnection' @ request. Contains the details of the newly created cross-cluster search connection.
--
-- /See:/ 'mkCreateOutboundCrossClusterSearchConnectionResponse' smart constructor.
data CreateOutboundCrossClusterSearchConnectionResponse = CreateOutboundCrossClusterSearchConnectionResponse'
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
        DomainInformation,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CreateOutboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- * 'connectionAlias' - Specifies the connection alias provided during the create connection request.
-- * 'connectionStatus' - Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the newly created connection.
-- * 'crossClusterSearchConnectionId' - Unique id for the created outbound connection, which is used for subsequent operations on connection.
-- * 'destinationDomainInfo' - Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
-- * 'responseStatus' - The response status code.
-- * 'sourceDomainInfo' - Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
mkCreateOutboundCrossClusterSearchConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOutboundCrossClusterSearchConnectionResponse
mkCreateOutboundCrossClusterSearchConnectionResponse
  pResponseStatus_ =
    CreateOutboundCrossClusterSearchConnectionResponse'
      { destinationDomainInfo =
          Lude.Nothing,
        connectionAlias = Lude.Nothing,
        crossClusterSearchConnectionId =
          Lude.Nothing,
        connectionStatus = Lude.Nothing,
        sourceDomainInfo = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Specifies the @'DomainInformation' @ for the destination Elasticsearch domain.
--
-- /Note:/ Consider using 'destinationDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrsDestinationDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Lude.Maybe DomainInformation)
coccscrsDestinationDomainInfo = Lens.lens (destinationDomainInfo :: CreateOutboundCrossClusterSearchConnectionResponse -> Lude.Maybe DomainInformation) (\s a -> s {destinationDomainInfo = a} :: CreateOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED coccscrsDestinationDomainInfo "Use generic-lens or generic-optics with 'destinationDomainInfo' instead." #-}

-- | Specifies the connection alias provided during the create connection request.
--
-- /Note:/ Consider using 'connectionAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrsConnectionAlias :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Lude.Maybe Lude.Text)
coccscrsConnectionAlias = Lens.lens (connectionAlias :: CreateOutboundCrossClusterSearchConnectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {connectionAlias = a} :: CreateOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED coccscrsConnectionAlias "Use generic-lens or generic-optics with 'connectionAlias' instead." #-}

-- | Unique id for the created outbound connection, which is used for subsequent operations on connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrsCrossClusterSearchConnectionId :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Lude.Maybe Lude.Text)
coccscrsCrossClusterSearchConnectionId = Lens.lens (crossClusterSearchConnectionId :: CreateOutboundCrossClusterSearchConnectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {crossClusterSearchConnectionId = a} :: CreateOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED coccscrsCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

-- | Specifies the @'OutboundCrossClusterSearchConnectionStatus' @ for the newly created connection.
--
-- /Note:/ Consider using 'connectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrsConnectionStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Lude.Maybe OutboundCrossClusterSearchConnectionStatus)
coccscrsConnectionStatus = Lens.lens (connectionStatus :: CreateOutboundCrossClusterSearchConnectionResponse -> Lude.Maybe OutboundCrossClusterSearchConnectionStatus) (\s a -> s {connectionStatus = a} :: CreateOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED coccscrsConnectionStatus "Use generic-lens or generic-optics with 'connectionStatus' instead." #-}

-- | Specifies the @'DomainInformation' @ for the source Elasticsearch domain.
--
-- /Note:/ Consider using 'sourceDomainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrsSourceDomainInfo :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse (Lude.Maybe DomainInformation)
coccscrsSourceDomainInfo = Lens.lens (sourceDomainInfo :: CreateOutboundCrossClusterSearchConnectionResponse -> Lude.Maybe DomainInformation) (\s a -> s {sourceDomainInfo = a} :: CreateOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED coccscrsSourceDomainInfo "Use generic-lens or generic-optics with 'sourceDomainInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coccscrsResponseStatus :: Lens.Lens' CreateOutboundCrossClusterSearchConnectionResponse Lude.Int
coccscrsResponseStatus = Lens.lens (responseStatus :: CreateOutboundCrossClusterSearchConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED coccscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
