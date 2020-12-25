{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DisassociateNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a node from an AWS OpsWorks CM server, and removes the node from the server's managed nodes. After a node is disassociated, the node key pair is no longer valid for accessing the configuration manager's API. For more information about how to associate a node, see 'AssociateNode' .
--
-- A node can can only be disassociated from a server that is in a @HEALTHY@ state. Otherwise, an @InvalidStateException@ is thrown. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.DisassociateNode
  ( -- * Creating a request
    DisassociateNode (..),
    mkDisassociateNode,

    -- ** Request lenses
    dnServerName,
    dnNodeName,
    dnEngineAttributes,

    -- * Destructuring the response
    DisassociateNodeResponse (..),
    mkDisassociateNodeResponse,

    -- ** Response lenses
    dnrrsNodeAssociationStatusToken,
    dnrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateNode' smart constructor.
data DisassociateNode = DisassociateNode'
  { -- | The name of the server from which to disassociate the node.
    serverName :: Types.ServerName,
    -- | The name of the client node.
    nodeName :: Types.NodeName,
    -- | Engine attributes that are used for disassociating the node. No attributes are required for Puppet.
    --
    -- __Attributes required in a DisassociateNode request for Chef__
    --
    --     * @CHEF_ORGANIZATION@ : The Chef organization with which the node was associated. By default only one organization named @default@ can exist.
    engineAttributes :: Core.Maybe [Types.EngineAttribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateNode' value with any optional fields omitted.
mkDisassociateNode ::
  -- | 'serverName'
  Types.ServerName ->
  -- | 'nodeName'
  Types.NodeName ->
  DisassociateNode
mkDisassociateNode serverName nodeName =
  DisassociateNode'
    { serverName,
      nodeName,
      engineAttributes = Core.Nothing
    }

-- | The name of the server from which to disassociate the node.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnServerName :: Lens.Lens' DisassociateNode Types.ServerName
dnServerName = Lens.field @"serverName"
{-# DEPRECATED dnServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The name of the client node.
--
-- /Note:/ Consider using 'nodeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnNodeName :: Lens.Lens' DisassociateNode Types.NodeName
dnNodeName = Lens.field @"nodeName"
{-# DEPRECATED dnNodeName "Use generic-lens or generic-optics with 'nodeName' instead." #-}

-- | Engine attributes that are used for disassociating the node. No attributes are required for Puppet.
--
-- __Attributes required in a DisassociateNode request for Chef__
--
--     * @CHEF_ORGANIZATION@ : The Chef organization with which the node was associated. By default only one organization named @default@ can exist.
--
--
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnEngineAttributes :: Lens.Lens' DisassociateNode (Core.Maybe [Types.EngineAttribute])
dnEngineAttributes = Lens.field @"engineAttributes"
{-# DEPRECATED dnEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

instance Core.FromJSON DisassociateNode where
  toJSON DisassociateNode {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServerName" Core..= serverName),
            Core.Just ("NodeName" Core..= nodeName),
            ("EngineAttributes" Core..=) Core.<$> engineAttributes
          ]
      )

instance Core.AWSRequest DisassociateNode where
  type Rs DisassociateNode = DisassociateNodeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorksCM_V2016_11_01.DisassociateNode")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateNodeResponse'
            Core.<$> (x Core..:? "NodeAssociationStatusToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateNodeResponse' smart constructor.
data DisassociateNodeResponse = DisassociateNodeResponse'
  { -- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the disassociation request.
    nodeAssociationStatusToken :: Core.Maybe Types.NodeAssociationStatusToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateNodeResponse' value with any optional fields omitted.
mkDisassociateNodeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateNodeResponse
mkDisassociateNodeResponse responseStatus =
  DisassociateNodeResponse'
    { nodeAssociationStatusToken =
        Core.Nothing,
      responseStatus
    }

-- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the disassociation request.
--
-- /Note:/ Consider using 'nodeAssociationStatusToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsNodeAssociationStatusToken :: Lens.Lens' DisassociateNodeResponse (Core.Maybe Types.NodeAssociationStatusToken)
dnrrsNodeAssociationStatusToken = Lens.field @"nodeAssociationStatusToken"
{-# DEPRECATED dnrrsNodeAssociationStatusToken "Use generic-lens or generic-optics with 'nodeAssociationStatusToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsResponseStatus :: Lens.Lens' DisassociateNodeResponse Core.Int
dnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
