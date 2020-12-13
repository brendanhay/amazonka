{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the source domain owner to delete an existing outbound cross-cluster search connection.
module Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
  ( -- * Creating a request
    DeleteOutboundCrossClusterSearchConnection (..),
    mkDeleteOutboundCrossClusterSearchConnection,

    -- ** Request lenses
    doccscCrossClusterSearchConnectionId,

    -- * Destructuring the response
    DeleteOutboundCrossClusterSearchConnectionResponse (..),
    mkDeleteOutboundCrossClusterSearchConnectionResponse,

    -- ** Response lenses
    doccscrsCrossClusterSearchConnection,
    doccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteOutboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkDeleteOutboundCrossClusterSearchConnection' smart constructor.
newtype DeleteOutboundCrossClusterSearchConnection = DeleteOutboundCrossClusterSearchConnection'
  { -- | The id of the outbound connection that you want to permanently delete.
    crossClusterSearchConnectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOutboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnectionId' - The id of the outbound connection that you want to permanently delete.
mkDeleteOutboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Lude.Text ->
  DeleteOutboundCrossClusterSearchConnection
mkDeleteOutboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    DeleteOutboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the outbound connection that you want to permanently delete.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscCrossClusterSearchConnectionId :: Lens.Lens' DeleteOutboundCrossClusterSearchConnection Lude.Text
doccscCrossClusterSearchConnectionId = Lens.lens (crossClusterSearchConnectionId :: DeleteOutboundCrossClusterSearchConnection -> Lude.Text) (\s a -> s {crossClusterSearchConnectionId = a} :: DeleteOutboundCrossClusterSearchConnection)
{-# DEPRECATED doccscCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

instance Lude.AWSRequest DeleteOutboundCrossClusterSearchConnection where
  type
    Rs DeleteOutboundCrossClusterSearchConnection =
      DeleteOutboundCrossClusterSearchConnectionResponse
  request = Req.delete elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteOutboundCrossClusterSearchConnectionResponse'
            Lude.<$> (x Lude..?> "CrossClusterSearchConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteOutboundCrossClusterSearchConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteOutboundCrossClusterSearchConnection where
  toPath DeleteOutboundCrossClusterSearchConnection' {..} =
    Lude.mconcat
      [ "/2015-01-01/es/ccs/outboundConnection/",
        Lude.toBS crossClusterSearchConnectionId
      ]

instance Lude.ToQuery DeleteOutboundCrossClusterSearchConnection where
  toQuery = Lude.const Lude.mempty

-- | The result of a @'DeleteOutboundCrossClusterSearchConnection' @ operation. Contains details of deleted outbound connection.
--
-- /See:/ 'mkDeleteOutboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteOutboundCrossClusterSearchConnectionResponse = DeleteOutboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @'OutboundCrossClusterSearchConnection' @ of deleted outbound connection.
    crossClusterSearchConnection :: Lude.Maybe OutboundCrossClusterSearchConnection,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOutboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnection' - Specifies the @'OutboundCrossClusterSearchConnection' @ of deleted outbound connection.
-- * 'responseStatus' - The response status code.
mkDeleteOutboundCrossClusterSearchConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteOutboundCrossClusterSearchConnectionResponse
mkDeleteOutboundCrossClusterSearchConnectionResponse
  pResponseStatus_ =
    DeleteOutboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Specifies the @'OutboundCrossClusterSearchConnection' @ of deleted outbound connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscrsCrossClusterSearchConnection :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse (Lude.Maybe OutboundCrossClusterSearchConnection)
doccscrsCrossClusterSearchConnection = Lens.lens (crossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnectionResponse -> Lude.Maybe OutboundCrossClusterSearchConnection) (\s a -> s {crossClusterSearchConnection = a} :: DeleteOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED doccscrsCrossClusterSearchConnection "Use generic-lens or generic-optics with 'crossClusterSearchConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscrsResponseStatus :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse Lude.Int
doccscrsResponseStatus = Lens.lens (responseStatus :: DeleteOutboundCrossClusterSearchConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteOutboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED doccscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
