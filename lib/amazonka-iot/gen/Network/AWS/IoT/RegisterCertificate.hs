{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device certificate with AWS IoT. If you have more than one CA certificate that has the same subject field, you must specify the CA certificate that was used to sign the device certificate being registered.
module Network.AWS.IoT.RegisterCertificate
  ( -- * Creating a request
    RegisterCertificate (..),
    mkRegisterCertificate,

    -- ** Request lenses
    rcStatus,
    rcCaCertificatePem,
    rcSetAsActive,
    rcCertificatePem,

    -- * Destructuring the response
    RegisterCertificateResponse (..),
    mkRegisterCertificateResponse,

    -- ** Response lenses
    rcrsCertificateARN,
    rcrsCertificateId,
    rcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input to the RegisterCertificate operation.
--
-- /See:/ 'mkRegisterCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { status ::
      Lude.Maybe CertificateStatus,
    caCertificatePem :: Lude.Maybe Lude.Text,
    setAsActive :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'RegisterCertificate' with the minimum fields required to make a request.
--
-- * 'caCertificatePem' - The CA certificate used to sign the device certificate being registered.
-- * 'certificatePem' - The certificate data, in PEM format.
-- * 'setAsActive' - A boolean value that specifies if the certificate is set to active.
-- * 'status' - The status of the register certificate request.
mkRegisterCertificate ::
  -- | 'certificatePem'
  Lude.Text ->
  RegisterCertificate
mkRegisterCertificate pCertificatePem_ =
  RegisterCertificate'
    { status = Lude.Nothing,
      caCertificatePem = Lude.Nothing,
      setAsActive = Lude.Nothing,
      certificatePem = pCertificatePem_
    }

-- | The status of the register certificate request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcStatus :: Lens.Lens' RegisterCertificate (Lude.Maybe CertificateStatus)
rcStatus = Lens.lens (status :: RegisterCertificate -> Lude.Maybe CertificateStatus) (\s a -> s {status = a} :: RegisterCertificate)
{-# DEPRECATED rcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The CA certificate used to sign the device certificate being registered.
--
-- /Note:/ Consider using 'caCertificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCaCertificatePem :: Lens.Lens' RegisterCertificate (Lude.Maybe Lude.Text)
rcCaCertificatePem = Lens.lens (caCertificatePem :: RegisterCertificate -> Lude.Maybe Lude.Text) (\s a -> s {caCertificatePem = a} :: RegisterCertificate)
{-# DEPRECATED rcCaCertificatePem "Use generic-lens or generic-optics with 'caCertificatePem' instead." #-}

-- | A boolean value that specifies if the certificate is set to active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcSetAsActive :: Lens.Lens' RegisterCertificate (Lude.Maybe Lude.Bool)
rcSetAsActive = Lens.lens (setAsActive :: RegisterCertificate -> Lude.Maybe Lude.Bool) (\s a -> s {setAsActive = a} :: RegisterCertificate)
{-# DEPRECATED rcSetAsActive "Use generic-lens or generic-optics with 'setAsActive' instead." #-}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificatePem :: Lens.Lens' RegisterCertificate Lude.Text
rcCertificatePem = Lens.lens (certificatePem :: RegisterCertificate -> Lude.Text) (\s a -> s {certificatePem = a} :: RegisterCertificate)
{-# DEPRECATED rcCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

instance Lude.AWSRequest RegisterCertificate where
  type Rs RegisterCertificate = RegisterCertificateResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterCertificateResponse'
            Lude.<$> (x Lude..?> "certificateArn")
            Lude.<*> (x Lude..?> "certificateId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RegisterCertificate where
  toJSON RegisterCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("caCertificatePem" Lude..=) Lude.<$> caCertificatePem,
            Lude.Just ("certificatePem" Lude..= certificatePem)
          ]
      )

instance Lude.ToPath RegisterCertificate where
  toPath = Lude.const "/certificate/register"

instance Lude.ToQuery RegisterCertificate where
  toQuery RegisterCertificate' {..} =
    Lude.mconcat ["setAsActive" Lude.=: setAsActive]

-- | The output from the RegisterCertificate operation.
--
-- /See:/ 'mkRegisterCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { certificateARN ::
      Lude.Maybe Lude.Text,
    certificateId ::
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

-- | Creates a value of 'RegisterCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The certificate ARN.
-- * 'certificateId' - The certificate identifier.
-- * 'responseStatus' - The response status code.
mkRegisterCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterCertificateResponse
mkRegisterCertificateResponse pResponseStatus_ =
  RegisterCertificateResponse'
    { certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The certificate ARN.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsCertificateARN :: Lens.Lens' RegisterCertificateResponse (Lude.Maybe Lude.Text)
rcrsCertificateARN = Lens.lens (certificateARN :: RegisterCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: RegisterCertificateResponse)
{-# DEPRECATED rcrsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsCertificateId :: Lens.Lens' RegisterCertificateResponse (Lude.Maybe Lude.Text)
rcrsCertificateId = Lens.lens (certificateId :: RegisterCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: RegisterCertificateResponse)
{-# DEPRECATED rcrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' RegisterCertificateResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: RegisterCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterCertificateResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
