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
    doccscrrsCrossClusterSearchConnection,
    doccscrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DeleteOutboundCrossClusterSearchConnection' @ operation.
--
-- /See:/ 'mkDeleteOutboundCrossClusterSearchConnection' smart constructor.
newtype DeleteOutboundCrossClusterSearchConnection = DeleteOutboundCrossClusterSearchConnection'
  { -- | The id of the outbound connection that you want to permanently delete.
    crossClusterSearchConnectionId :: Types.CrossClusterSearchConnectionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOutboundCrossClusterSearchConnection' value with any optional fields omitted.
mkDeleteOutboundCrossClusterSearchConnection ::
  -- | 'crossClusterSearchConnectionId'
  Types.CrossClusterSearchConnectionId ->
  DeleteOutboundCrossClusterSearchConnection
mkDeleteOutboundCrossClusterSearchConnection
  crossClusterSearchConnectionId =
    DeleteOutboundCrossClusterSearchConnection' {crossClusterSearchConnectionId}

-- | The id of the outbound connection that you want to permanently delete.
--
-- /Note:/ Consider using 'crossClusterSearchConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscCrossClusterSearchConnectionId :: Lens.Lens' DeleteOutboundCrossClusterSearchConnection Types.CrossClusterSearchConnectionId
doccscCrossClusterSearchConnectionId = Lens.field @"crossClusterSearchConnectionId"
{-# DEPRECATED doccscCrossClusterSearchConnectionId "Use generic-lens or generic-optics with 'crossClusterSearchConnectionId' instead." #-}

instance Core.AWSRequest DeleteOutboundCrossClusterSearchConnection where
  type
    Rs DeleteOutboundCrossClusterSearchConnection =
      DeleteOutboundCrossClusterSearchConnectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/es/ccs/outboundConnection/"
                Core.<> (Core.toText crossClusterSearchConnectionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteOutboundCrossClusterSearchConnectionResponse'
            Core.<$> (x Core..:? "CrossClusterSearchConnection")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @'DeleteOutboundCrossClusterSearchConnection' @ operation. Contains details of deleted outbound connection.
--
-- /See:/ 'mkDeleteOutboundCrossClusterSearchConnectionResponse' smart constructor.
data DeleteOutboundCrossClusterSearchConnectionResponse = DeleteOutboundCrossClusterSearchConnectionResponse'
  { -- | Specifies the @'OutboundCrossClusterSearchConnection' @ of deleted outbound connection.
    crossClusterSearchConnection :: Core.Maybe Types.OutboundCrossClusterSearchConnection,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOutboundCrossClusterSearchConnectionResponse' value with any optional fields omitted.
mkDeleteOutboundCrossClusterSearchConnectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteOutboundCrossClusterSearchConnectionResponse
mkDeleteOutboundCrossClusterSearchConnectionResponse responseStatus =
  DeleteOutboundCrossClusterSearchConnectionResponse'
    { crossClusterSearchConnection =
        Core.Nothing,
      responseStatus
    }

-- | Specifies the @'OutboundCrossClusterSearchConnection' @ of deleted outbound connection.
--
-- /Note:/ Consider using 'crossClusterSearchConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscrrsCrossClusterSearchConnection :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse (Core.Maybe Types.OutboundCrossClusterSearchConnection)
doccscrrsCrossClusterSearchConnection = Lens.field @"crossClusterSearchConnection"
{-# DEPRECATED doccscrrsCrossClusterSearchConnection "Use generic-lens or generic-optics with 'crossClusterSearchConnection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscrrsResponseStatus :: Lens.Lens' DeleteOutboundCrossClusterSearchConnectionResponse Core.Int
doccscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED doccscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
