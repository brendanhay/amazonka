{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RejectInboundCrossClusterSearchConnection (..),
    mkRejectInboundCrossClusterSearchConnection,

    -- ** Request lenses
    riccscCrossClusterSearchConnectionId,

    -- * Destructuring the response
    RejectInboundCrossClusterSearchConnectionResponse (..),
    mkRejectInboundCrossClusterSearchConnectionResponse,

    -- ** Response lenses
    riccscrsCrossClusterSearchConnection,
    riccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'RejectInboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkRejectInboundCrossClusterSearchConnection' smart constructor.
newtype RejectInboundCrossClusterSearchConnection = RejectInboundCrossClusterSearchConnection'
  { -- | The id of the inbound connection that you want to reject.
    crossClusterSearchConnectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectInboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnectionId' - The id of the inbound connection that you want to reject.
mkRejectInboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Lude.Text ->
  RejectInboundCrossClusterSearchConnection
mkRejectInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    RejectInboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to reject.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riccscCrossClusterSearchConnectionId :: Lens.Lens' RejectInboundCrossClusterSearchConnection Lude.Text
riccscCrossClusterSearchConnectionId = Lens.lens (crossClusterSearchConnectionId :: RejectInboundCrossClusterSearchConnection -> Lude.Text) (\s a -> s {crossClusterSearchConnectionId = a} :: RejectInboundCrossClusterSearchConnection)
{-# DEPRECATED riccscCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

instance Lude.AWSRequest RejectInboundCrossClusterSearchConnection where
  type
    Rs RejectInboundCrossClusterSearchConnection =
      RejectInboundCrossClusterSearchConnectionResponse
  request = Req.putJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          RejectInboundCrossClusterSearchConnectionResponse'
            Lude.<$> (x Lude..?> "CrossClusterSearchConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectInboundCrossClusterSearchConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RejectInboundCrossClusterSearchConnection where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath RejectInboundCrossClusterSearchConnection where
  toPath RejectInboundCrossClusterSearchConnection' {..} =
    Lude.mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        Lude.toBS crossClusterSearchConnectionId,
        "/reject"
      ]

instance Lude.ToQuery RejectInboundCrossClusterSearchConnection where
  toQuery = Lude.const Lude.mempty

-- | The result of a @'RejectInboundCrossClusterSearchConnection' @ operation. Contains details of rejected inbound connection.
--
-- /See:/ 'mkRejectInboundCrossClusterSearchConnectionResponse' smart constructor.
data RejectInboundCrossClusterSearchConnectionResponse = RejectInboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @'InboundCrossClusterSearchConnection' @ of rejected inbound connection.
    crossClusterSearchConnection :: Lude.Maybe InboundCrossClusterSearchConnection,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectInboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnection' - Specifies the @'InboundCrossClusterSearchConnection' @ of rejected inbound connection.
-- * 'responseStatus' - The response status code.
mkRejectInboundCrossClusterSearchConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectInboundCrossClusterSearchConnectionResponse
mkRejectInboundCrossClusterSearchConnectionResponse
  pResponseStatus_ =
    RejectInboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of rejected inbound connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riccscrsCrossClusterSearchConnection :: Lens.Lens' RejectInboundCrossClusterSearchConnectionResponse (Lude.Maybe InboundCrossClusterSearchConnection)
riccscrsCrossClusterSearchConnection = Lens.lens (crossClusterSearchConnection :: RejectInboundCrossClusterSearchConnectionResponse -> Lude.Maybe InboundCrossClusterSearchConnection) (\s a -> s {crossClusterSearchConnection = a} :: RejectInboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED riccscrsCrossClusterSearchConnection "Use generic-lens or generic-optics with 'crossClusterSearchConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riccscrsResponseStatus :: Lens.Lens' RejectInboundCrossClusterSearchConnectionResponse Lude.Int
riccscrsResponseStatus = Lens.lens (responseStatus :: RejectInboundCrossClusterSearchConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectInboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED riccscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
