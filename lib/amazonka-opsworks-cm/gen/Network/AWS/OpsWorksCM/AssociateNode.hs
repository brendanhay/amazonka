{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.AssociateNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a new node with the server. For more information about how to disassociate a node, see 'DisassociateNode' .
--
-- On a Chef server: This command is an alternative to @knife bootstrap@ .
-- Example (Chef): @aws opsworks-cm associate-node --server-name /MyServer/ --node-name /MyManagedNode/ --engine-attributes "Name=/CHEF_ORGANIZATION/ ,Value=default" "Name=/CHEF_NODE_PUBLIC_KEY/ ,Value=/public-key-pem/ "@
-- On a Puppet server, this command is an alternative to the @puppet cert sign@ command that signs a Puppet node CSR.
-- Example (Puppet): @aws opsworks-cm associate-node --server-name /MyServer/ --node-name /MyManagedNode/ --engine-attributes "Name=/PUPPET_NODE_CSR/ ,Value=/csr-pem/ "@
-- A node can can only be associated with servers that are in a @HEALTHY@ state. Otherwise, an @InvalidStateException@ is thrown. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid. The AssociateNode API call can be integrated into Auto Scaling configurations, AWS Cloudformation templates, or the user data of a server's instance.
module Network.AWS.OpsWorksCM.AssociateNode
  ( -- * Creating a request
    AssociateNode (..),
    mkAssociateNode,

    -- ** Request lenses
    anServerName,
    anEngineAttributes,
    anNodeName,

    -- * Destructuring the response
    AssociateNodeResponse (..),
    mkAssociateNodeResponse,

    -- ** Response lenses
    anrsNodeAssociationStatusToken,
    anrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateNode' smart constructor.
data AssociateNode = AssociateNode'
  { -- | The name of the server with which to associate the node.
    serverName :: Lude.Text,
    -- | Engine attributes used for associating the node.
    --
    -- __Attributes accepted in a AssociateNode request for Chef__
    --
    --     * @CHEF_ORGANIZATION@ : The Chef organization with which the node is associated. By default only one organization named @default@ can exist.
    --
    --
    --     * @CHEF_NODE_PUBLIC_KEY@ : A PEM-formatted public key. This key is required for the @chef-client@ agent to access the Chef API.
    --
    --
    -- __Attributes accepted in a AssociateNode request for Puppet__
    --
    --     * @PUPPET_NODE_CSR@ : A PEM-formatted certificate-signing request (CSR) that is created by the node.
    engineAttributes :: [EngineAttribute],
    -- | The name of the node.
    nodeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateNode' with the minimum fields required to make a request.
--
-- * 'serverName' - The name of the server with which to associate the node.
-- * 'engineAttributes' - Engine attributes used for associating the node.
--
-- __Attributes accepted in a AssociateNode request for Chef__
--
--     * @CHEF_ORGANIZATION@ : The Chef organization with which the node is associated. By default only one organization named @default@ can exist.
--
--
--     * @CHEF_NODE_PUBLIC_KEY@ : A PEM-formatted public key. This key is required for the @chef-client@ agent to access the Chef API.
--
--
-- __Attributes accepted in a AssociateNode request for Puppet__
--
--     * @PUPPET_NODE_CSR@ : A PEM-formatted certificate-signing request (CSR) that is created by the node.
--
--
-- * 'nodeName' - The name of the node.
mkAssociateNode ::
  -- | 'serverName'
  Lude.Text ->
  -- | 'nodeName'
  Lude.Text ->
  AssociateNode
mkAssociateNode pServerName_ pNodeName_ =
  AssociateNode'
    { serverName = pServerName_,
      engineAttributes = Lude.mempty,
      nodeName = pNodeName_
    }

-- | The name of the server with which to associate the node.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anServerName :: Lens.Lens' AssociateNode Lude.Text
anServerName = Lens.lens (serverName :: AssociateNode -> Lude.Text) (\s a -> s {serverName = a} :: AssociateNode)
{-# DEPRECATED anServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Engine attributes used for associating the node.
--
-- __Attributes accepted in a AssociateNode request for Chef__
--
--     * @CHEF_ORGANIZATION@ : The Chef organization with which the node is associated. By default only one organization named @default@ can exist.
--
--
--     * @CHEF_NODE_PUBLIC_KEY@ : A PEM-formatted public key. This key is required for the @chef-client@ agent to access the Chef API.
--
--
-- __Attributes accepted in a AssociateNode request for Puppet__
--
--     * @PUPPET_NODE_CSR@ : A PEM-formatted certificate-signing request (CSR) that is created by the node.
--
--
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anEngineAttributes :: Lens.Lens' AssociateNode [EngineAttribute]
anEngineAttributes = Lens.lens (engineAttributes :: AssociateNode -> [EngineAttribute]) (\s a -> s {engineAttributes = a} :: AssociateNode)
{-# DEPRECATED anEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

-- | The name of the node.
--
-- /Note:/ Consider using 'nodeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anNodeName :: Lens.Lens' AssociateNode Lude.Text
anNodeName = Lens.lens (nodeName :: AssociateNode -> Lude.Text) (\s a -> s {nodeName = a} :: AssociateNode)
{-# DEPRECATED anNodeName "Use generic-lens or generic-optics with 'nodeName' instead." #-}

instance Lude.AWSRequest AssociateNode where
  type Rs AssociateNode = AssociateNodeResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateNodeResponse'
            Lude.<$> (x Lude..?> "NodeAssociationStatusToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateNode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.AssociateNode" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateNode where
  toJSON AssociateNode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServerName" Lude..= serverName),
            Lude.Just ("EngineAttributes" Lude..= engineAttributes),
            Lude.Just ("NodeName" Lude..= nodeName)
          ]
      )

instance Lude.ToPath AssociateNode where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateNode where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateNodeResponse' smart constructor.
data AssociateNodeResponse = AssociateNodeResponse'
  { -- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the association request.
    nodeAssociationStatusToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateNodeResponse' with the minimum fields required to make a request.
--
-- * 'nodeAssociationStatusToken' - Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the association request.
-- * 'responseStatus' - The response status code.
mkAssociateNodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateNodeResponse
mkAssociateNodeResponse pResponseStatus_ =
  AssociateNodeResponse'
    { nodeAssociationStatusToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the association request.
--
-- /Note:/ Consider using 'nodeAssociationStatusToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anrsNodeAssociationStatusToken :: Lens.Lens' AssociateNodeResponse (Lude.Maybe Lude.Text)
anrsNodeAssociationStatusToken = Lens.lens (nodeAssociationStatusToken :: AssociateNodeResponse -> Lude.Maybe Lude.Text) (\s a -> s {nodeAssociationStatusToken = a} :: AssociateNodeResponse)
{-# DEPRECATED anrsNodeAssociationStatusToken "Use generic-lens or generic-optics with 'nodeAssociationStatusToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anrsResponseStatus :: Lens.Lens' AssociateNodeResponse Lude.Int
anrsResponseStatus = Lens.lens (responseStatus :: AssociateNodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateNodeResponse)
{-# DEPRECATED anrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
