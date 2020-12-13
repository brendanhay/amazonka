{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ExportClientVPNClientCertificateRevocationList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the client certificate revocation list for the specified Client VPN endpoint.
module Network.AWS.EC2.ExportClientVPNClientCertificateRevocationList
  ( -- * Creating a request
    ExportClientVPNClientCertificateRevocationList (..),
    mkExportClientVPNClientCertificateRevocationList,

    -- ** Request lenses
    ecvccrlClientVPNEndpointId,
    ecvccrlDryRun,

    -- * Destructuring the response
    ExportClientVPNClientCertificateRevocationListResponse (..),
    mkExportClientVPNClientCertificateRevocationListResponse,

    -- ** Response lenses
    ecvccrlrsStatus,
    ecvccrlrsCertificateRevocationList,
    ecvccrlrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportClientVPNClientCertificateRevocationList' smart constructor.
data ExportClientVPNClientCertificateRevocationList = ExportClientVPNClientCertificateRevocationList'
  { -- | The ID of the Client VPN endpoint.
    clientVPNEndpointId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportClientVPNClientCertificateRevocationList' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkExportClientVPNClientCertificateRevocationList ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  ExportClientVPNClientCertificateRevocationList
mkExportClientVPNClientCertificateRevocationList
  pClientVPNEndpointId_ =
    ExportClientVPNClientCertificateRevocationList'
      { clientVPNEndpointId =
          pClientVPNEndpointId_,
        dryRun = Lude.Nothing
      }

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlClientVPNEndpointId :: Lens.Lens' ExportClientVPNClientCertificateRevocationList Lude.Text
ecvccrlClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ExportClientVPNClientCertificateRevocationList -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ExportClientVPNClientCertificateRevocationList)
{-# DEPRECATED ecvccrlClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlDryRun :: Lens.Lens' ExportClientVPNClientCertificateRevocationList (Lude.Maybe Lude.Bool)
ecvccrlDryRun = Lens.lens (dryRun :: ExportClientVPNClientCertificateRevocationList -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ExportClientVPNClientCertificateRevocationList)
{-# DEPRECATED ecvccrlDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    ExportClientVPNClientCertificateRevocationList
  where
  type
    Rs ExportClientVPNClientCertificateRevocationList =
      ExportClientVPNClientCertificateRevocationListResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ExportClientVPNClientCertificateRevocationListResponse'
            Lude.<$> (x Lude..@? "status")
            Lude.<*> (x Lude..@? "certificateRevocationList")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    ExportClientVPNClientCertificateRevocationList
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ExportClientVPNClientCertificateRevocationList where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    ExportClientVPNClientCertificateRevocationList
  where
  toQuery ExportClientVPNClientCertificateRevocationList' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "ExportClientVpnClientCertificateRevocationList" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkExportClientVPNClientCertificateRevocationListResponse' smart constructor.
data ExportClientVPNClientCertificateRevocationListResponse = ExportClientVPNClientCertificateRevocationListResponse'
  { -- | The current state of the client certificate revocation list.
    status :: Lude.Maybe ClientCertificateRevocationListStatus,
    -- | Information about the client certificate revocation list.
    certificateRevocationList :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportClientVPNClientCertificateRevocationListResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current state of the client certificate revocation list.
-- * 'certificateRevocationList' - Information about the client certificate revocation list.
-- * 'responseStatus' - The response status code.
mkExportClientVPNClientCertificateRevocationListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportClientVPNClientCertificateRevocationListResponse
mkExportClientVPNClientCertificateRevocationListResponse
  pResponseStatus_ =
    ExportClientVPNClientCertificateRevocationListResponse'
      { status =
          Lude.Nothing,
        certificateRevocationList =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The current state of the client certificate revocation list.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlrsStatus :: Lens.Lens' ExportClientVPNClientCertificateRevocationListResponse (Lude.Maybe ClientCertificateRevocationListStatus)
ecvccrlrsStatus = Lens.lens (status :: ExportClientVPNClientCertificateRevocationListResponse -> Lude.Maybe ClientCertificateRevocationListStatus) (\s a -> s {status = a} :: ExportClientVPNClientCertificateRevocationListResponse)
{-# DEPRECATED ecvccrlrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Information about the client certificate revocation list.
--
-- /Note:/ Consider using 'certificateRevocationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlrsCertificateRevocationList :: Lens.Lens' ExportClientVPNClientCertificateRevocationListResponse (Lude.Maybe Lude.Text)
ecvccrlrsCertificateRevocationList = Lens.lens (certificateRevocationList :: ExportClientVPNClientCertificateRevocationListResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateRevocationList = a} :: ExportClientVPNClientCertificateRevocationListResponse)
{-# DEPRECATED ecvccrlrsCertificateRevocationList "Use generic-lens or generic-optics with 'certificateRevocationList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlrsResponseStatus :: Lens.Lens' ExportClientVPNClientCertificateRevocationListResponse Lude.Int
ecvccrlrsResponseStatus = Lens.lens (responseStatus :: ExportClientVPNClientCertificateRevocationListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportClientVPNClientCertificateRevocationListResponse)
{-# DEPRECATED ecvccrlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
