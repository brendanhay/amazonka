{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status or configuration of a private certificate authority (CA). Your private CA must be in the @ACTIVE@ or @DISABLED@ state before you can update it. You can disable a private CA that is in the @ACTIVE@ state or make a CA that is in the @DISABLED@ state active again.
module Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
  ( -- * Creating a request
    UpdateCertificateAuthority (..),
    mkUpdateCertificateAuthority,

    -- ** Request lenses
    ucaStatus,
    ucaRevocationConfiguration,
    ucaCertificateAuthorityARN,

    -- * Destructuring the response
    UpdateCertificateAuthorityResponse (..),
    mkUpdateCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCertificateAuthority' smart constructor.
data UpdateCertificateAuthority = UpdateCertificateAuthority'
  { status ::
      Lude.Maybe CertificateAuthorityStatus,
    revocationConfiguration ::
      Lude.Maybe RevocationConfiguration,
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
-- * 'revocationConfiguration' - Revocation information for your private CA.
-- * 'status' - Status of your private CA.
mkUpdateCertificateAuthority ::
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  UpdateCertificateAuthority
mkUpdateCertificateAuthority pCertificateAuthorityARN_ =
  UpdateCertificateAuthority'
    { status = Lude.Nothing,
      revocationConfiguration = Lude.Nothing,
      certificateAuthorityARN = pCertificateAuthorityARN_
    }

-- | Status of your private CA.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaStatus :: Lens.Lens' UpdateCertificateAuthority (Lude.Maybe CertificateAuthorityStatus)
ucaStatus = Lens.lens (status :: UpdateCertificateAuthority -> Lude.Maybe CertificateAuthorityStatus) (\s a -> s {status = a} :: UpdateCertificateAuthority)
{-# DEPRECATED ucaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Revocation information for your private CA.
--
-- /Note:/ Consider using 'revocationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaRevocationConfiguration :: Lens.Lens' UpdateCertificateAuthority (Lude.Maybe RevocationConfiguration)
ucaRevocationConfiguration = Lens.lens (revocationConfiguration :: UpdateCertificateAuthority -> Lude.Maybe RevocationConfiguration) (\s a -> s {revocationConfiguration = a} :: UpdateCertificateAuthority)
{-# DEPRECATED ucaRevocationConfiguration "Use generic-lens or generic-optics with 'revocationConfiguration' instead." #-}

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaCertificateAuthorityARN :: Lens.Lens' UpdateCertificateAuthority Lude.Text
ucaCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: UpdateCertificateAuthority -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: UpdateCertificateAuthority)
{-# DEPRECATED ucaCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest UpdateCertificateAuthority where
  type
    Rs UpdateCertificateAuthority =
      UpdateCertificateAuthorityResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull UpdateCertificateAuthorityResponse'

instance Lude.ToHeaders UpdateCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.UpdateCertificateAuthority" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCertificateAuthority where
  toJSON UpdateCertificateAuthority' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("RevocationConfiguration" Lude..=)
              Lude.<$> revocationConfiguration,
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath UpdateCertificateAuthority where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCertificateAuthorityResponse' smart constructor.
data UpdateCertificateAuthorityResponse = UpdateCertificateAuthorityResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCertificateAuthorityResponse' with the minimum fields required to make a request.
mkUpdateCertificateAuthorityResponse ::
  UpdateCertificateAuthorityResponse
mkUpdateCertificateAuthorityResponse =
  UpdateCertificateAuthorityResponse'
