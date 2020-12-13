{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the destination domain owner to accept an inbound cross-cluster search connection request.
module Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
  ( -- * Creating a request
    AcceptInboundCrossClusterSearchConnection (..),
    mkAcceptInboundCrossClusterSearchConnection,

    -- ** Request lenses
    aiccscCrossClusterSearchConnectionId,

    -- * Destructuring the response
    AcceptInboundCrossClusterSearchConnectionResponse (..),
    mkAcceptInboundCrossClusterSearchConnectionResponse,

    -- ** Response lenses
    aiccscrsCrossClusterSearchConnection,
    aiccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'AcceptInboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkAcceptInboundCrossClusterSearchConnection' smart constructor.
newtype AcceptInboundCrossClusterSearchConnection = AcceptInboundCrossClusterSearchConnection'
  { -- | The id of the inbound connection that you want to accept.
    crossClusterSearchConnectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptInboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnectionId' - The id of the inbound connection that you want to accept.
mkAcceptInboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Lude.Text ->
  AcceptInboundCrossClusterSearchConnection
mkAcceptInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    AcceptInboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to accept.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiccscCrossClusterSearchConnectionId :: Lens.Lens' AcceptInboundCrossClusterSearchConnection Lude.Text
aiccscCrossClusterSearchConnectionId = Lens.lens (crossClusterSearchConnectionId :: AcceptInboundCrossClusterSearchConnection -> Lude.Text) (\s a -> s {crossClusterSearchConnectionId = a} :: AcceptInboundCrossClusterSearchConnection)
{-# DEPRECATED aiccscCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

instance Lude.AWSRequest AcceptInboundCrossClusterSearchConnection where
  type
    Rs AcceptInboundCrossClusterSearchConnection =
      AcceptInboundCrossClusterSearchConnectionResponse
  request = Req.putJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          AcceptInboundCrossClusterSearchConnectionResponse'
            Lude.<$> (x Lude..?> "CrossClusterSearchConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptInboundCrossClusterSearchConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AcceptInboundCrossClusterSearchConnection where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath AcceptInboundCrossClusterSearchConnection where
  toPath AcceptInboundCrossClusterSearchConnection' {..} =
    Lude.mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        Lude.toBS crossClusterSearchConnectionId,
        "/accept"
      ]

instance Lude.ToQuery AcceptInboundCrossClusterSearchConnection where
  toQuery = Lude.const Lude.mempty

-- | The result of a @'AcceptInboundCrossClusterSearchConnection' @ operation. Contains details of accepted inbound connection.
--
-- /See:/ 'mkAcceptInboundCrossClusterSearchConnectionResponse' smart constructor.
data AcceptInboundCrossClusterSearchConnectionResponse = AcceptInboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @'InboundCrossClusterSearchConnection' @ of accepted inbound connection.
    crossClusterSearchConnection :: Lude.Maybe InboundCrossClusterSearchConnection,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptInboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnection' - Specifies the @'InboundCrossClusterSearchConnection' @ of accepted inbound connection.
-- * 'responseStatus' - The response status code.
mkAcceptInboundCrossClusterSearchConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptInboundCrossClusterSearchConnectionResponse
mkAcceptInboundCrossClusterSearchConnectionResponse
  pResponseStatus_ =
    AcceptInboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of accepted inbound connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiccscrsCrossClusterSearchConnection :: Lens.Lens' AcceptInboundCrossClusterSearchConnectionResponse (Lude.Maybe InboundCrossClusterSearchConnection)
aiccscrsCrossClusterSearchConnection = Lens.lens (crossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnectionResponse -> Lude.Maybe InboundCrossClusterSearchConnection) (\s a -> s {crossClusterSearchConnection = a} :: AcceptInboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED aiccscrsCrossClusterSearchConnection "Use generic-lens or generic-optics with 'crossClusterSearchConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiccscrsResponseStatus :: Lens.Lens' AcceptInboundCrossClusterSearchConnectionResponse Lude.Int
aiccscrsResponseStatus = Lens.lens (responseStatus :: AcceptInboundCrossClusterSearchConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptInboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED aiccscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
