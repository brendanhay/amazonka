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
    dnasServerName,
    dnasNodeAssociationStatusToken,

    -- * Destructuring the response
    DescribeNodeAssociationStatusResponse (..),
    mkDescribeNodeAssociationStatusResponse,

    -- ** Response lenses
    dnasrsEngineAttributes,
    dnasrsNodeAssociationStatus,
    dnasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeNodeAssociationStatus' smart constructor.
data DescribeNodeAssociationStatus = DescribeNodeAssociationStatus'
  { -- | The name of the server from which to disassociate the node.
    serverName :: Lude.Text,
    -- | The token returned in either the AssociateNodeResponse or the DisassociateNodeResponse.
    nodeAssociationStatusToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNodeAssociationStatus' with the minimum fields required to make a request.
--
-- * 'serverName' - The name of the server from which to disassociate the node.
-- * 'nodeAssociationStatusToken' - The token returned in either the AssociateNodeResponse or the DisassociateNodeResponse.
mkDescribeNodeAssociationStatus ::
  -- | 'serverName'
  Lude.Text ->
  -- | 'nodeAssociationStatusToken'
  Lude.Text ->
  DescribeNodeAssociationStatus
mkDescribeNodeAssociationStatus
  pServerName_
  pNodeAssociationStatusToken_ =
    DescribeNodeAssociationStatus'
      { serverName = pServerName_,
        nodeAssociationStatusToken = pNodeAssociationStatusToken_
      }

-- | The name of the server from which to disassociate the node.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasServerName :: Lens.Lens' DescribeNodeAssociationStatus Lude.Text
dnasServerName = Lens.lens (serverName :: DescribeNodeAssociationStatus -> Lude.Text) (\s a -> s {serverName = a} :: DescribeNodeAssociationStatus)
{-# DEPRECATED dnasServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The token returned in either the AssociateNodeResponse or the DisassociateNodeResponse.
--
-- /Note:/ Consider using 'nodeAssociationStatusToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasNodeAssociationStatusToken :: Lens.Lens' DescribeNodeAssociationStatus Lude.Text
dnasNodeAssociationStatusToken = Lens.lens (nodeAssociationStatusToken :: DescribeNodeAssociationStatus -> Lude.Text) (\s a -> s {nodeAssociationStatusToken = a} :: DescribeNodeAssociationStatus)
{-# DEPRECATED dnasNodeAssociationStatusToken "Use generic-lens or generic-optics with 'nodeAssociationStatusToken' instead." #-}

instance Lude.AWSRequest DescribeNodeAssociationStatus where
  type
    Rs DescribeNodeAssociationStatus =
      DescribeNodeAssociationStatusResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeNodeAssociationStatusResponse'
            Lude.<$> (x Lude..?> "EngineAttributes" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "NodeAssociationStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNodeAssociationStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorksCM_V2016_11_01.DescribeNodeAssociationStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeNodeAssociationStatus where
  toJSON DescribeNodeAssociationStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServerName" Lude..= serverName),
            Lude.Just
              ("NodeAssociationStatusToken" Lude..= nodeAssociationStatusToken)
          ]
      )

instance Lude.ToPath DescribeNodeAssociationStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNodeAssociationStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeNodeAssociationStatusResponse' smart constructor.
data DescribeNodeAssociationStatusResponse = DescribeNodeAssociationStatusResponse'
  { -- | Attributes specific to the node association. In Puppet, the attibute PUPPET_NODE_CERT contains the signed certificate (the result of the CSR).
    engineAttributes :: Lude.Maybe [EngineAttribute],
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
    nodeAssociationStatus :: NodeAssociationStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNodeAssociationStatusResponse' with the minimum fields required to make a request.
--
-- * 'engineAttributes' - Attributes specific to the node association. In Puppet, the attibute PUPPET_NODE_CERT contains the signed certificate (the result of the CSR).
-- * 'nodeAssociationStatus' - The status of the association or disassociation request.
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
-- * 'responseStatus' - The response status code.
mkDescribeNodeAssociationStatusResponse ::
  -- | 'nodeAssociationStatus'
  NodeAssociationStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNodeAssociationStatusResponse
mkDescribeNodeAssociationStatusResponse
  pNodeAssociationStatus_
  pResponseStatus_ =
    DescribeNodeAssociationStatusResponse'
      { engineAttributes =
          Lude.Nothing,
        nodeAssociationStatus = pNodeAssociationStatus_,
        responseStatus = pResponseStatus_
      }

-- | Attributes specific to the node association. In Puppet, the attibute PUPPET_NODE_CERT contains the signed certificate (the result of the CSR).
--
-- /Note:/ Consider using 'engineAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasrsEngineAttributes :: Lens.Lens' DescribeNodeAssociationStatusResponse (Lude.Maybe [EngineAttribute])
dnasrsEngineAttributes = Lens.lens (engineAttributes :: DescribeNodeAssociationStatusResponse -> Lude.Maybe [EngineAttribute]) (\s a -> s {engineAttributes = a} :: DescribeNodeAssociationStatusResponse)
{-# DEPRECATED dnasrsEngineAttributes "Use generic-lens or generic-optics with 'engineAttributes' instead." #-}

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
dnasrsNodeAssociationStatus :: Lens.Lens' DescribeNodeAssociationStatusResponse NodeAssociationStatus
dnasrsNodeAssociationStatus = Lens.lens (nodeAssociationStatus :: DescribeNodeAssociationStatusResponse -> NodeAssociationStatus) (\s a -> s {nodeAssociationStatus = a} :: DescribeNodeAssociationStatusResponse)
{-# DEPRECATED dnasrsNodeAssociationStatus "Use generic-lens or generic-optics with 'nodeAssociationStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasrsResponseStatus :: Lens.Lens' DescribeNodeAssociationStatusResponse Lude.Int
dnasrsResponseStatus = Lens.lens (responseStatus :: DescribeNodeAssociationStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNodeAssociationStatusResponse)
{-# DEPRECATED dnasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
