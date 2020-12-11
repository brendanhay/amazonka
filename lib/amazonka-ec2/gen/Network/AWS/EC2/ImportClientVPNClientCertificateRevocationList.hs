{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportClientVPNClientCertificateRevocationList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a client certificate revocation list to the specified Client VPN endpoint. Uploading a client certificate revocation list overwrites the existing client certificate revocation list.
--
-- Uploading a client certificate revocation list resets existing client connections.
module Network.AWS.EC2.ImportClientVPNClientCertificateRevocationList
  ( -- * Creating a request
    ImportClientVPNClientCertificateRevocationList (..),
    mkImportClientVPNClientCertificateRevocationList,

    -- ** Request lenses
    icvccrlDryRun,
    icvccrlClientVPNEndpointId,
    icvccrlCertificateRevocationList,

    -- * Destructuring the response
    ImportClientVPNClientCertificateRevocationListResponse (..),
    mkImportClientVPNClientCertificateRevocationListResponse,

    -- ** Response lenses
    icvccrlrsReturn,
    icvccrlrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportClientVPNClientCertificateRevocationList' smart constructor.
data ImportClientVPNClientCertificateRevocationList = ImportClientVPNClientCertificateRevocationList'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    clientVPNEndpointId ::
      Lude.Text,
    certificateRevocationList ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ImportClientVPNClientCertificateRevocationList' with the minimum fields required to make a request.
--
-- * 'certificateRevocationList' - The client certificate revocation list file. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List> in the /AWS Client VPN Administrator Guide/ .
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint to which the client certificate revocation list applies.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkImportClientVPNClientCertificateRevocationList ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  -- | 'certificateRevocationList'
  Lude.Text ->
  ImportClientVPNClientCertificateRevocationList
mkImportClientVPNClientCertificateRevocationList
  pClientVPNEndpointId_
  pCertificateRevocationList_ =
    ImportClientVPNClientCertificateRevocationList'
      { dryRun =
          Lude.Nothing,
        clientVPNEndpointId = pClientVPNEndpointId_,
        certificateRevocationList =
          pCertificateRevocationList_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlDryRun :: Lens.Lens' ImportClientVPNClientCertificateRevocationList (Lude.Maybe Lude.Bool)
icvccrlDryRun = Lens.lens (dryRun :: ImportClientVPNClientCertificateRevocationList -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportClientVPNClientCertificateRevocationList)
{-# DEPRECATED icvccrlDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Client VPN endpoint to which the client certificate revocation list applies.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlClientVPNEndpointId :: Lens.Lens' ImportClientVPNClientCertificateRevocationList Lude.Text
icvccrlClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ImportClientVPNClientCertificateRevocationList -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ImportClientVPNClientCertificateRevocationList)
{-# DEPRECATED icvccrlClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The client certificate revocation list file. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'certificateRevocationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlCertificateRevocationList :: Lens.Lens' ImportClientVPNClientCertificateRevocationList Lude.Text
icvccrlCertificateRevocationList = Lens.lens (certificateRevocationList :: ImportClientVPNClientCertificateRevocationList -> Lude.Text) (\s a -> s {certificateRevocationList = a} :: ImportClientVPNClientCertificateRevocationList)
{-# DEPRECATED icvccrlCertificateRevocationList "Use generic-lens or generic-optics with 'certificateRevocationList' instead." #-}

instance
  Lude.AWSRequest
    ImportClientVPNClientCertificateRevocationList
  where
  type
    Rs ImportClientVPNClientCertificateRevocationList =
      ImportClientVPNClientCertificateRevocationListResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ImportClientVPNClientCertificateRevocationListResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    ImportClientVPNClientCertificateRevocationList
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ImportClientVPNClientCertificateRevocationList where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    ImportClientVPNClientCertificateRevocationList
  where
  toQuery ImportClientVPNClientCertificateRevocationList' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "ImportClientVpnClientCertificateRevocationList" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "CertificateRevocationList" Lude.=: certificateRevocationList
      ]

-- | /See:/ 'mkImportClientVPNClientCertificateRevocationListResponse' smart constructor.
data ImportClientVPNClientCertificateRevocationListResponse = ImportClientVPNClientCertificateRevocationListResponse'
  { return ::
      Lude.Maybe
        Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ImportClientVPNClientCertificateRevocationListResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkImportClientVPNClientCertificateRevocationListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportClientVPNClientCertificateRevocationListResponse
mkImportClientVPNClientCertificateRevocationListResponse
  pResponseStatus_ =
    ImportClientVPNClientCertificateRevocationListResponse'
      { return =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlrsReturn :: Lens.Lens' ImportClientVPNClientCertificateRevocationListResponse (Lude.Maybe Lude.Bool)
icvccrlrsReturn = Lens.lens (return :: ImportClientVPNClientCertificateRevocationListResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ImportClientVPNClientCertificateRevocationListResponse)
{-# DEPRECATED icvccrlrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlrsResponseStatus :: Lens.Lens' ImportClientVPNClientCertificateRevocationListResponse Lude.Int
icvccrlrsResponseStatus = Lens.lens (responseStatus :: ImportClientVPNClientCertificateRevocationListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportClientVPNClientCertificateRevocationListResponse)
{-# DEPRECATED icvccrlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
