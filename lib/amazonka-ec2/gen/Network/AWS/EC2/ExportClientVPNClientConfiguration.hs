{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ExportClientVPNClientConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the contents of the Client VPN endpoint configuration file for the specified Client VPN endpoint. The Client VPN endpoint configuration file includes the Client VPN endpoint and certificate information clients need to establish a connection with the Client VPN endpoint.
module Network.AWS.EC2.ExportClientVPNClientConfiguration
  ( -- * Creating a request
    ExportClientVPNClientConfiguration (..),
    mkExportClientVPNClientConfiguration,

    -- ** Request lenses
    ecvccClientVPNEndpointId,
    ecvccDryRun,

    -- * Destructuring the response
    ExportClientVPNClientConfigurationResponse (..),
    mkExportClientVPNClientConfigurationResponse,

    -- ** Response lenses
    ecvccrsClientConfiguration,
    ecvccrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportClientVPNClientConfiguration' smart constructor.
data ExportClientVPNClientConfiguration = ExportClientVPNClientConfiguration'
  { -- | The ID of the Client VPN endpoint.
    clientVPNEndpointId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportClientVPNClientConfiguration' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkExportClientVPNClientConfiguration ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  ExportClientVPNClientConfiguration
mkExportClientVPNClientConfiguration pClientVPNEndpointId_ =
  ExportClientVPNClientConfiguration'
    { clientVPNEndpointId =
        pClientVPNEndpointId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccClientVPNEndpointId :: Lens.Lens' ExportClientVPNClientConfiguration Lude.Text
ecvccClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ExportClientVPNClientConfiguration -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ExportClientVPNClientConfiguration)
{-# DEPRECATED ecvccClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccDryRun :: Lens.Lens' ExportClientVPNClientConfiguration (Lude.Maybe Lude.Bool)
ecvccDryRun = Lens.lens (dryRun :: ExportClientVPNClientConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ExportClientVPNClientConfiguration)
{-# DEPRECATED ecvccDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ExportClientVPNClientConfiguration where
  type
    Rs ExportClientVPNClientConfiguration =
      ExportClientVPNClientConfigurationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ExportClientVPNClientConfigurationResponse'
            Lude.<$> (x Lude..@? "clientConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportClientVPNClientConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ExportClientVPNClientConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery ExportClientVPNClientConfiguration where
  toQuery ExportClientVPNClientConfiguration' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ExportClientVpnClientConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkExportClientVPNClientConfigurationResponse' smart constructor.
data ExportClientVPNClientConfigurationResponse = ExportClientVPNClientConfigurationResponse'
  { -- | The contents of the Client VPN endpoint configuration file.
    clientConfiguration :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportClientVPNClientConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'clientConfiguration' - The contents of the Client VPN endpoint configuration file.
-- * 'responseStatus' - The response status code.
mkExportClientVPNClientConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportClientVPNClientConfigurationResponse
mkExportClientVPNClientConfigurationResponse pResponseStatus_ =
  ExportClientVPNClientConfigurationResponse'
    { clientConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The contents of the Client VPN endpoint configuration file.
--
-- /Note:/ Consider using 'clientConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrsClientConfiguration :: Lens.Lens' ExportClientVPNClientConfigurationResponse (Lude.Maybe Lude.Text)
ecvccrsClientConfiguration = Lens.lens (clientConfiguration :: ExportClientVPNClientConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientConfiguration = a} :: ExportClientVPNClientConfigurationResponse)
{-# DEPRECATED ecvccrsClientConfiguration "Use generic-lens or generic-optics with 'clientConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrsResponseStatus :: Lens.Lens' ExportClientVPNClientConfigurationResponse Lude.Int
ecvccrsResponseStatus = Lens.lens (responseStatus :: ExportClientVPNClientConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportClientVPNClientConfigurationResponse)
{-# DEPRECATED ecvccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
