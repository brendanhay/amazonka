{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPNConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPN connection.
--
-- If you're deleting the VPC and its associated components, we recommend that you detach the virtual private gateway from the VPC and delete the VPC before deleting the VPN connection. If you believe that the tunnel credentials for your VPN connection have been compromised, you can delete the VPN connection and create a new one that has new keys, without needing to delete the VPC or virtual private gateway. If you create a new VPN connection, you must reconfigure the customer gateway device using the new configuration information returned with the new VPN connection ID.
-- For certificate-based authentication, delete all AWS Certificate Manager (ACM) private certificates used for the AWS-side tunnel endpoints for the VPN connection before deleting the VPN connection.
module Network.AWS.EC2.DeleteVPNConnection
  ( -- * Creating a request
    DeleteVPNConnection (..),
    mkDeleteVPNConnection,

    -- ** Request lenses
    dvcVPNConnectionId,
    dvcDryRun,

    -- * Destructuring the response
    DeleteVPNConnectionResponse (..),
    mkDeleteVPNConnectionResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteVpnConnection.
--
-- /See:/ 'mkDeleteVPNConnection' smart constructor.
data DeleteVPNConnection = DeleteVPNConnection'
  { -- | The ID of the VPN connection.
    vpnConnectionId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPNConnection' with the minimum fields required to make a request.
--
-- * 'vpnConnectionId' - The ID of the VPN connection.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteVPNConnection ::
  -- | 'vpnConnectionId'
  Lude.Text ->
  DeleteVPNConnection
mkDeleteVPNConnection pVPNConnectionId_ =
  DeleteVPNConnection'
    { vpnConnectionId = pVPNConnectionId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcVPNConnectionId :: Lens.Lens' DeleteVPNConnection Lude.Text
dvcVPNConnectionId = Lens.lens (vpnConnectionId :: DeleteVPNConnection -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: DeleteVPNConnection)
{-# DEPRECATED dvcVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcDryRun :: Lens.Lens' DeleteVPNConnection (Lude.Maybe Lude.Bool)
dvcDryRun = Lens.lens (dryRun :: DeleteVPNConnection -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPNConnection)
{-# DEPRECATED dvcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteVPNConnection where
  type Rs DeleteVPNConnection = DeleteVPNConnectionResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteVPNConnectionResponse'

instance Lude.ToHeaders DeleteVPNConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPNConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPNConnection where
  toQuery DeleteVPNConnection' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteVpnConnection" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnConnectionId" Lude.=: vpnConnectionId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteVPNConnectionResponse' smart constructor.
data DeleteVPNConnectionResponse = DeleteVPNConnectionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPNConnectionResponse' with the minimum fields required to make a request.
mkDeleteVPNConnectionResponse ::
  DeleteVPNConnectionResponse
mkDeleteVPNConnectionResponse = DeleteVPNConnectionResponse'
