{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of an existing association or disassociation request.
--
-- A @ResourceNotFoundException@ is thrown when no recent association or disassociation request with the specified token is found, or when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
  ( -- * Creating a request
    DescribeNodeAssociationStatus (..),
    mkDescribeNodeAssociationStatus,

    -- ** Request lenses
    dnasNodeAssociationStatusToken,
    dnasServerName,

    -- * Destructuring the response
    DescribeNodeAssociationStatusResponse (..),
    mkDescribeNodeAssociationStatusResponse,

    -- ** Response lenses
    dnasrrsEngineAttributes,
    dnasrrsNodeAssociationStatus,
    dnasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeNodeAssociationStatus' smart constructor.
data DescribeNodeAssociationStatus = DescribeNodeAssociationStatus'
  { -- | The token returned in either the AssociateNodeResponse or the DisassociateNodeResponse.
    nodeAssociationStatusToken :: Types.NodeAssociationStatusToken,
    -- | The name of the server from which to disassociate the node.
    serverName :: Types.ServerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNodeAssociationStatus' value with any optional fields omitted.
mkDescribeNodeAssociationStatus ::
  -- | 'nodeAssociationStatusToken'
  Types.NodeAssociationStatusToken ->
  -- | 'serverName'
  Types.ServerName ->
  DescribeNodeAssociationStatus
mkDescribeNodeAssociationStatus
  nodeAssociationStatusToken
  serverName =
    DescribeNodeAssociationStatus'
      { nodeAssociationStatusToken,
        serverName
      }

-- | The token returned in either the AssociateNodeResponse or the DisassociateNodeResponse.
--
-- /Note:/ Consider using 'nodeAssociationStatusToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasNodeAssociationStatusToken :: Lens.Lens' DescribeNodeAssociationStatus Types.NodeAssociationStatusToken
dnasNodeAssociationStatusToken = Lens.field @"nodeAssociationStatusToken"
{-# DEPRECATED dnasNodeAssociationStatusToken "Use generic-lens or generic-optics with 'nodeAssociationStatusToken' instead." #-}

-- | The name of the server from which to disassociate the node.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasServerName :: Lens.Lens' DescribeNodeAssociationStatus Types.ServerName
dnasServerName = Lens.field @"serverName"
{-# DEPRECATED dnasServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

instance Core.FromJSON DescribeNodeAssociationStatus where
  toJSON DescribeNodeAssociationStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("NodeAssociationStatusToken" Core..= nodeAssociationStatusToken),
            Core.Just ("ServerName" Core..= serverName)
          ]
      )

instance Core.AWSRequest DescribeNodeAssociationStatus where
  type
    Rs DescribeNodeAssociationStatus =
      DescribeNodeAssociationStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "OpsWorksCM_V2016_11_01.DescribeNodeAssociationStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNodeAssociationStatusResponse'
            Core.<$> (x Core..:? "EngineAttributes")
            Core.<*> (x Core..: "NodeAssociationStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeNodeAssociationStatusResponse' smart constructor.
data DescribeNodeAssociationStatusResponse = DescribeNodeAssociationStatusResponse'
  { -- | Attributes specific to the node association. In Puppet, the attibute PUPPET_NODE_CERT contains the signed certificate (the result of the CSR).
    engineAttributes :: Core.Maybe [Types.EngineAttribute],
    -- | The status of the association or disassociation request.
    --
    -- __Possible values:__
    --
    --     * @SUCCESS@ : The association or disassociation succeeded.
    --
    --
    --     * @FAILED@ : The association or disassociation failed.
    --
    --
    --     * @IN_PROGRESS@ : The association or disassociation is still in progress.
    nodeAssociationStatus :: Types.NodeAssociationStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNodeAssociationStatusResponse' value with any optional fields omitted.
mkDescribeNodeAssociationStatusResponse ::
  -- | 'nodeAssociationStatus'
  Types.NodeAssociationStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeNodeAssociationStatusResponse
mkDescribeNodeAssociationStatusResponse
  nodeAssociationStatus
  responseStatus =
    DescribeNodeAssociationStatusResponse'
      { engineAttributes =
          Core.Nothing,
        nodeAssociationStatus,
        responseStatus
      }

-- | Attributes specific to the node association. In Puppet, the attibute PUPPET_NODE_CERT contains the signed certificate (the result of the CSR).
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasrrsEngineAttributes :: Lens.Lens' DescribeNodeAssociationStatusResponse (Core.Maybe [Types.EngineAttribute])
dnasrrsEngineAttributes = Lens.field @"engineAttributes"
{-# DEPRECATED dnasrrsEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

-- | The status of the association or disassociation request.
--
-- __Possible values:__
--
--     * @SUCCESS@ : The association or disassociation succeeded.
--
--
--     * @FAILED@ : The association or disassociation failed.
--
--
--     * @IN_PROGRESS@ : The association or disassociation is still in progress.
--
--
--
-- /Note:/ Consider using 'nodeAssociationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasrrsNodeAssociationStatus :: Lens.Lens' DescribeNodeAssociationStatusResponse Types.NodeAssociationStatus
dnasrrsNodeAssociationStatus = Lens.field @"nodeAssociationStatus"
{-# DEPRECATED dnasrrsNodeAssociationStatus "Use generic-lens or generic-optics with 'nodeAssociationStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasrrsResponseStatus :: Lens.Lens' DescribeNodeAssociationStatusResponse Core.Int
dnasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
