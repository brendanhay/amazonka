{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DisassociateConnector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified connector from AWS SMS.
--
-- After you disassociate a connector, it is no longer available to support replication jobs.
module Network.AWS.SMS.DisassociateConnector
  ( -- * Creating a request
    DisassociateConnector (..),
    mkDisassociateConnector,

    -- ** Request lenses
    dcConnectorId,

    -- * Destructuring the response
    DisassociateConnectorResponse (..),
    mkDisassociateConnectorResponse,

    -- ** Response lenses
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDisassociateConnector' smart constructor.
newtype DisassociateConnector = DisassociateConnector'
  { -- | The ID of the connector.
    connectorId :: Types.ConnectorId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConnector' value with any optional fields omitted.
mkDisassociateConnector ::
  -- | 'connectorId'
  Types.ConnectorId ->
  DisassociateConnector
mkDisassociateConnector connectorId =
  DisassociateConnector' {connectorId}

-- | The ID of the connector.
--
-- /Note:/ Consider using 'connectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcConnectorId :: Lens.Lens' DisassociateConnector Types.ConnectorId
dcConnectorId = Lens.field @"connectorId"
{-# DEPRECATED dcConnectorId "Use generic-lens or generic-optics with 'connectorId' instead." #-}

instance Core.FromJSON DisassociateConnector where
  toJSON DisassociateConnector {..} =
    Core.object
      (Core.catMaybes [Core.Just ("connectorId" Core..= connectorId)])

instance Core.AWSRequest DisassociateConnector where
  type Rs DisassociateConnector = DisassociateConnectorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.DisassociateConnector"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateConnectorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateConnectorResponse' smart constructor.
newtype DisassociateConnectorResponse = DisassociateConnectorResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateConnectorResponse' value with any optional fields omitted.
mkDisassociateConnectorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateConnectorResponse
mkDisassociateConnectorResponse responseStatus =
  DisassociateConnectorResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DisassociateConnectorResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
