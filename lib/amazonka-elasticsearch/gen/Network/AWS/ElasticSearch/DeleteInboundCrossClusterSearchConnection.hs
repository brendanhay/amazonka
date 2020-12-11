{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteInboundCrossClusterSearchConnection (..),
    mkDeleteInboundCrossClusterSearchConnection,

    -- ** Request lenses
    diccscCrossClusterSearchConnectionId,

    -- * Destructuring the response
    DeleteInboundCrossClusterSearchConnectionResponse (..),
    mkDeleteInboundCrossClusterSearchConnectionResponse,

    -- ** Response lenses
    diccscirsCrossClusterSearchConnection,
    diccscirsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteInboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkDeleteInboundCrossClusterSearchConnection' smart constructor.
newtype DeleteInboundCrossClusterSearchConnection = DeleteInboundCrossClusterSearchConnection'
  { crossClusterSearchConnectionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInboundCrossClusterSearchConnection' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnectionId' - The id of the inbound connection that you want to permanently delete.
mkDeleteInboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Lude.Text ->
  DeleteInboundCrossClusterSearchConnection
mkDeleteInboundCrossClusterSearchConnection
  pCrossClusterSearchConnectionId_ =
    DeleteInboundCrossClusterSearchConnection'
      { crossClusterSearchConnectionId =
          pCrossClusterSearchConnectionId_
      }

-- | The id of the inbound connection that you want to permanently delete.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscCrossClusterSearchConnectionId :: Lens.Lens' DeleteInboundCrossClusterSearchConnection Lude.Text
diccscCrossClusterSearchConnectionId = Lens.lens (crossClusterSearchConnectionId :: DeleteInboundCrossClusterSearchConnection -> Lude.Text) (\s a -> s {crossClusterSearchConnectionId = a} :: DeleteInboundCrossClusterSearchConnection)
{-# DEPRECATED diccscCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

instance Lude.AWSRequest DeleteInboundCrossClusterSearchConnection where
  type
    Rs DeleteInboundCrossClusterSearchConnection =
      DeleteInboundCrossClusterSearchConnectionResponse
  request = Req.delete elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteInboundCrossClusterSearchConnectionResponse'
            Lude.<$> (x Lude..?> "CrossClusterSearchConnection")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInboundCrossClusterSearchConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteInboundCrossClusterSearchConnection where
  toPath DeleteInboundCrossClusterSearchConnection' {..} =
    Lude.mconcat
      [ "/2015-01-01/es/ccs/inboundConnection/",
        Lude.toBS crossClusterSearchConnectionId
      ]

instance Lude.ToQuery DeleteInboundCrossClusterSearchConnection where
  toQuery = Lude.const Lude.mempty

-- | The result of a @'DeleteInboundCrossClusterSearchConnection' @ operation. Contains details of deleted inbound connection.
--
-- /See:/ 'mkDeleteInboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteInboundCrossClusterSearchConnectionResponse = DeleteInboundCrossClusterSearchConnectionResponse'
  { crossClusterSearchConnection ::
      Lude.Maybe
        InboundCrossClusterSearchConnection,
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

-- | Creates a value of 'DeleteInboundCrossClusterSearchConnectionResponse' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnection' - Specifies the @'InboundCrossClusterSearchConnection' @ of deleted inbound connection.
-- * 'responseStatus' - The response status code.
mkDeleteInboundCrossClusterSearchConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInboundCrossClusterSearchConnectionResponse
mkDeleteInboundCrossClusterSearchConnectionResponse
  pResponseStatus_ =
    DeleteInboundCrossClusterSearchConnectionResponse'
      { crossClusterSearchConnection =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Specifies the @'InboundCrossClusterSearchConnection' @ of deleted inbound connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscirsCrossClusterSearchConnection :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse (Lude.Maybe InboundCrossClusterSearchConnection)
diccscirsCrossClusterSearchConnection = Lens.lens (crossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnectionResponse -> Lude.Maybe InboundCrossClusterSearchConnection) (\s a -> s {crossClusterSearchConnection = a} :: DeleteInboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED diccscirsCrossClusterSearchConnection "Use generic-lens or generic-optics with 'crossClusterSearchConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscirsResponseStatus :: Lens.Lens' DeleteInboundCrossClusterSearchConnectionResponse Lude.Int
diccscirsResponseStatus = Lens.lens (responseStatus :: DeleteInboundCrossClusterSearchConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInboundCrossClusterSearchConnectionResponse)
{-# DEPRECATED diccscirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
