{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterCertificateWithoutCA
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register a certificate that does not have a certificate authority (CA).
module Network.AWS.IoT.RegisterCertificateWithoutCA
  ( -- * Creating a request
    RegisterCertificateWithoutCA (..),
    mkRegisterCertificateWithoutCA,

    -- ** Request lenses
    rcwcaStatus,
    rcwcaCertificatePem,

    -- * Destructuring the response
    RegisterCertificateWithoutCAResponse (..),
    mkRegisterCertificateWithoutCAResponse,

    -- ** Response lenses
    rcwcarsCertificateARN,
    rcwcarsCertificateId,
    rcwcarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterCertificateWithoutCA' smart constructor.
data RegisterCertificateWithoutCA = RegisterCertificateWithoutCA'
  { status ::
      Lude.Maybe CertificateStatus,
    certificatePem :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterCertificateWithoutCA' with the minimum fields required to make a request.
--
-- * 'certificatePem' - The certificate data, in PEM format.
-- * 'status' - The status of the register certificate request.
mkRegisterCertificateWithoutCA ::
  -- | 'certificatePem'
  Lude.Text ->
  RegisterCertificateWithoutCA
mkRegisterCertificateWithoutCA pCertificatePem_ =
  RegisterCertificateWithoutCA'
    { status = Lude.Nothing,
      certificatePem = pCertificatePem_
    }

-- | The status of the register certificate request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcaStatus :: Lens.Lens' RegisterCertificateWithoutCA (Lude.Maybe CertificateStatus)
rcwcaStatus = Lens.lens (status :: RegisterCertificateWithoutCA -> Lude.Maybe CertificateStatus) (\s a -> s {status = a} :: RegisterCertificateWithoutCA)
{-# DEPRECATED rcwcaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcaCertificatePem :: Lens.Lens' RegisterCertificateWithoutCA Lude.Text
rcwcaCertificatePem = Lens.lens (certificatePem :: RegisterCertificateWithoutCA -> Lude.Text) (\s a -> s {certificatePem = a} :: RegisterCertificateWithoutCA)
{-# DEPRECATED rcwcaCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

instance Lude.AWSRequest RegisterCertificateWithoutCA where
  type
    Rs RegisterCertificateWithoutCA =
      RegisterCertificateWithoutCAResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterCertificateWithoutCAResponse'
            Lude.<$> (x Lude..?> "certificateArn")
            Lude.<*> (x Lude..?> "certificateId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterCertificateWithoutCA where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RegisterCertificateWithoutCA where
  toJSON RegisterCertificateWithoutCA' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            Lude.Just ("certificatePem" Lude..= certificatePem)
          ]
      )

instance Lude.ToPath RegisterCertificateWithoutCA where
  toPath = Lude.const "/certificate/register-no-ca"

instance Lude.ToQuery RegisterCertificateWithoutCA where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterCertificateWithoutCAResponse' smart constructor.
data RegisterCertificateWithoutCAResponse = RegisterCertificateWithoutCAResponse'
  { certificateARN ::
      Lude.Maybe
        Lude.Text,
    certificateId ::
      Lude.Maybe
        Lude.Text,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterCertificateWithoutCAResponse' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the registered certificate.
-- * 'certificateId' - The ID of the registered certificate. (The last part of the certificate ARN contains the certificate ID.
-- * 'responseStatus' - The response status code.
mkRegisterCertificateWithoutCAResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterCertificateWithoutCAResponse
mkRegisterCertificateWithoutCAResponse pResponseStatus_ =
  RegisterCertificateWithoutCAResponse'
    { certificateARN =
        Lude.Nothing,
      certificateId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the registered certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcarsCertificateARN :: Lens.Lens' RegisterCertificateWithoutCAResponse (Lude.Maybe Lude.Text)
rcwcarsCertificateARN = Lens.lens (certificateARN :: RegisterCertificateWithoutCAResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: RegisterCertificateWithoutCAResponse)
{-# DEPRECATED rcwcarsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The ID of the registered certificate. (The last part of the certificate ARN contains the certificate ID.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcarsCertificateId :: Lens.Lens' RegisterCertificateWithoutCAResponse (Lude.Maybe Lude.Text)
rcwcarsCertificateId = Lens.lens (certificateId :: RegisterCertificateWithoutCAResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: RegisterCertificateWithoutCAResponse)
{-# DEPRECATED rcwcarsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcarsResponseStatus :: Lens.Lens' RegisterCertificateWithoutCAResponse Lude.Int
rcwcarsResponseStatus = Lens.lens (responseStatus :: RegisterCertificateWithoutCAResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterCertificateWithoutCAResponse)
{-# DEPRECATED rcwcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
