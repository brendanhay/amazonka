{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dnEngineAttributes,
    dnServerName,
    dnNodeName,

    -- * Destructuring the response
    DisassociateNodeResponse (..),
    mkDisassociateNodeResponse,

    -- ** Response lenses
    dnrsNodeAssociationStatusToken,
    dnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateNode' smart constructor.
data DisassociateNode = DisassociateNode'
  { engineAttributes ::
      Lude.Maybe [EngineAttribute],
    serverName :: Lude.Text,
    nodeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateNode' with the minimum fields required to make a request.
--
-- * 'engineAttributes' - Engine attributes that are used for disassociating the node. No attributes are required for Puppet.
--
-- __Attributes required in a DisassociateNode request for Chef__
--
--     * @CHEF_ORGANIZATION@ : The Chef organization with which the node was associated. By default only one organization named @default@ can exist.
--
--
-- * 'nodeName' - The name of the client node.
-- * 'serverName' - The name of the server from which to disassociate the node.
mkDisassociateNode ::
  -- | 'serverName'
  Lude.Text ->
  -- | 'nodeName'
  Lude.Text ->
  DisassociateNode
mkDisassociateNode pServerName_ pNodeName_ =
  DisassociateNode'
    { engineAttributes = Lude.Nothing,
      serverName = pServerName_,
      nodeName = pNodeName_
    }

-- | Engine attributes that are used for disassociating the node. No attributes are required for Puppet.
--
-- __Attributes required in a DisassociateNode request for Chef__
--
--     * @CHEF_ORGANIZATION@ : The Chef organization with which the node was associated. By default only one organization named @default@ can exist.
--
--
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnEngineAttributes :: Lens.Lens' DisassociateNode (Lude.Maybe [EngineAttribute])
dnEngineAttributes = Lens.lens (engineAttributes :: DisassociateNode -> Lude.Maybe [EngineAttribute]) (\s a -> s {engineAttributes = a} :: DisassociateNode)
{-# DEPRECATED dnEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

-- | The name of the server from which to disassociate the node.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnServerName :: Lens.Lens' DisassociateNode Lude.Text
dnServerName = Lens.lens (serverName :: DisassociateNode -> Lude.Text) (\s a -> s {serverName = a} :: DisassociateNode)
{-# DEPRECATED dnServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The name of the client node.
--
-- /Note:/ Consider using 'nodeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnNodeName :: Lens.Lens' DisassociateNode Lude.Text
dnNodeName = Lens.lens (nodeName :: DisassociateNode -> Lude.Text) (\s a -> s {nodeName = a} :: DisassociateNode)
{-# DEPRECATED dnNodeName "Use generic-lens or generic-optics with 'nodeName' instead." #-}

instance Lude.AWSRequest DisassociateNode where
  type Rs DisassociateNode = DisassociateNodeResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisassociateNodeResponse'
            Lude.<$> (x Lude..?> "NodeAssociationStatusToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateNode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.DisassociateNode" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateNode where
  toJSON DisassociateNode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EngineAttributes" Lude..=) Lude.<$> engineAttributes,
            Lude.Just ("ServerName" Lude..= serverName),
            Lude.Just ("NodeName" Lude..= nodeName)
          ]
      )

instance Lude.ToPath DisassociateNode where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateNode where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateNodeResponse' smart constructor.
data DisassociateNodeResponse = DisassociateNodeResponse'
  { nodeAssociationStatusToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateNodeResponse' with the minimum fields required to make a request.
--
-- * 'nodeAssociationStatusToken' - Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the disassociation request.
-- * 'responseStatus' - The response status code.
mkDisassociateNodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateNodeResponse
mkDisassociateNodeResponse pResponseStatus_ =
  DisassociateNodeResponse'
    { nodeAssociationStatusToken =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the disassociation request.
--
-- /Note:/ Consider using 'nodeAssociationStatusToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrsNodeAssociationStatusToken :: Lens.Lens' DisassociateNodeResponse (Lude.Maybe Lude.Text)
dnrsNodeAssociationStatusToken = Lens.lens (nodeAssociationStatusToken :: DisassociateNodeResponse -> Lude.Maybe Lude.Text) (\s a -> s {nodeAssociationStatusToken = a} :: DisassociateNodeResponse)
{-# DEPRECATED dnrsNodeAssociationStatusToken "Use generic-lens or generic-optics with 'nodeAssociationStatusToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrsResponseStatus :: Lens.Lens' DisassociateNodeResponse Lude.Int
dnrsResponseStatus = Lens.lens (responseStatus :: DisassociateNodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateNodeResponse)
{-# DEPRECATED dnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
