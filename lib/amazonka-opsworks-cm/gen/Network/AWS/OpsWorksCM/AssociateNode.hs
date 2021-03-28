{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AssociateNode (..)
    , mkAssociateNode
    -- ** Request lenses
    , anServerName
    , anNodeName
    , anEngineAttributes

    -- * Destructuring the response
    , AssociateNodeResponse (..)
    , mkAssociateNodeResponse
    -- ** Response lenses
    , anrrsNodeAssociationStatusToken
    , anrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateNode' smart constructor.
data AssociateNode = AssociateNode'
  { serverName :: Types.ServerName
    -- ^ The name of the server with which to associate the node. 
  , nodeName :: Types.NodeName
    -- ^ The name of the node. 
  , engineAttributes :: [Types.EngineAttribute]
    -- ^ Engine attributes used for associating the node. 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateNode' value with any optional fields omitted.
mkAssociateNode
    :: Types.ServerName -- ^ 'serverName'
    -> Types.NodeName -- ^ 'nodeName'
    -> AssociateNode
mkAssociateNode serverName nodeName
  = AssociateNode'{serverName, nodeName,
                   engineAttributes = Core.mempty}

-- | The name of the server with which to associate the node. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anServerName :: Lens.Lens' AssociateNode Types.ServerName
anServerName = Lens.field @"serverName"
{-# INLINEABLE anServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The name of the node. 
--
-- /Note:/ Consider using 'nodeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anNodeName :: Lens.Lens' AssociateNode Types.NodeName
anNodeName = Lens.field @"nodeName"
{-# INLINEABLE anNodeName #-}
{-# DEPRECATED nodeName "Use generic-lens or generic-optics with 'nodeName' instead"  #-}

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
anEngineAttributes :: Lens.Lens' AssociateNode [Types.EngineAttribute]
anEngineAttributes = Lens.field @"engineAttributes"
{-# INLINEABLE anEngineAttributes #-}
{-# DEPRECATED engineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead"  #-}

instance Core.ToQuery AssociateNode where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateNode where
        toHeaders AssociateNode{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorksCM_V2016_11_01.AssociateNode")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateNode where
        toJSON AssociateNode{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServerName" Core..= serverName),
                  Core.Just ("NodeName" Core..= nodeName),
                  Core.Just ("EngineAttributes" Core..= engineAttributes)])

instance Core.AWSRequest AssociateNode where
        type Rs AssociateNode = AssociateNodeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssociateNodeResponse' Core.<$>
                   (x Core..:? "NodeAssociationStatusToken") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateNodeResponse' smart constructor.
data AssociateNodeResponse = AssociateNodeResponse'
  { nodeAssociationStatusToken :: Core.Maybe Types.NodeAssociationStatusToken
    -- ^ Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the association request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateNodeResponse' value with any optional fields omitted.
mkAssociateNodeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateNodeResponse
mkAssociateNodeResponse responseStatus
  = AssociateNodeResponse'{nodeAssociationStatusToken = Core.Nothing,
                           responseStatus}

-- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the association request. 
--
-- /Note:/ Consider using 'nodeAssociationStatusToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anrrsNodeAssociationStatusToken :: Lens.Lens' AssociateNodeResponse (Core.Maybe Types.NodeAssociationStatusToken)
anrrsNodeAssociationStatusToken = Lens.field @"nodeAssociationStatusToken"
{-# INLINEABLE anrrsNodeAssociationStatusToken #-}
{-# DEPRECATED nodeAssociationStatusToken "Use generic-lens or generic-optics with 'nodeAssociationStatusToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
anrrsResponseStatus :: Lens.Lens' AssociateNodeResponse Core.Int
anrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE anrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
