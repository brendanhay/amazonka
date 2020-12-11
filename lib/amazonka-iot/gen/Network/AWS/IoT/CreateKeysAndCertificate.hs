{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateKeysAndCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 2048-bit RSA key pair and issues an X.509 certificate using the issued public key. You can also call @CreateKeysAndCertificate@ over MQTT from a device, for more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/provision-wo-cert.html#provision-mqtt-api Provisioning MQTT API> .
--
-- __Note__ This is the only time AWS IoT issues the private key for this certificate, so it is important to keep it in a secure location.
module Network.AWS.IoT.CreateKeysAndCertificate
  ( -- * Creating a request
    CreateKeysAndCertificate (..),
    mkCreateKeysAndCertificate,

    -- ** Request lenses
    ckacSetAsActive,

    -- * Destructuring the response
    CreateKeysAndCertificateResponse (..),
    mkCreateKeysAndCertificateResponse,

    -- ** Response lenses
    ckacrsKeyPair,
    ckacrsCertificatePem,
    ckacrsCertificateARN,
    ckacrsCertificateId,
    ckacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CreateKeysAndCertificate operation.
--
-- /See:/ 'mkCreateKeysAndCertificate' smart constructor.
newtype CreateKeysAndCertificate = CreateKeysAndCertificate'
  { setAsActive ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateKeysAndCertificate' with the minimum fields required to make a request.
--
-- * 'setAsActive' - Specifies whether the certificate is active.
mkCreateKeysAndCertificate ::
  CreateKeysAndCertificate
mkCreateKeysAndCertificate =
  CreateKeysAndCertificate' {setAsActive = Lude.Nothing}

-- | Specifies whether the certificate is active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacSetAsActive :: Lens.Lens' CreateKeysAndCertificate (Lude.Maybe Lude.Bool)
ckacSetAsActive = Lens.lens (setAsActive :: CreateKeysAndCertificate -> Lude.Maybe Lude.Bool) (\s a -> s {setAsActive = a} :: CreateKeysAndCertificate)
{-# DEPRECATED ckacSetAsActive "Use generic-lens or generic-optics with 'setAsActive' instead." #-}

instance Lude.AWSRequest CreateKeysAndCertificate where
  type Rs CreateKeysAndCertificate = CreateKeysAndCertificateResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateKeysAndCertificateResponse'
            Lude.<$> (x Lude..?> "keyPair")
            Lude.<*> (x Lude..?> "certificatePem")
            Lude.<*> (x Lude..?> "certificateArn")
            Lude.<*> (x Lude..?> "certificateId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateKeysAndCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateKeysAndCertificate where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CreateKeysAndCertificate where
  toPath = Lude.const "/keys-and-certificate"

instance Lude.ToQuery CreateKeysAndCertificate where
  toQuery CreateKeysAndCertificate' {..} =
    Lude.mconcat ["setAsActive" Lude.=: setAsActive]

-- | The output of the CreateKeysAndCertificate operation.
--
-- /See:/ 'mkCreateKeysAndCertificateResponse' smart constructor.
data CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse'
  { keyPair ::
      Lude.Maybe KeyPair,
    certificatePem ::
      Lude.Maybe Lude.Text,
    certificateARN ::
      Lude.Maybe Lude.Text,
    certificateId ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateKeysAndCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the certificate.
-- * 'certificateId' - The ID of the certificate. AWS IoT issues a default subject name for the certificate (for example, AWS IoT Certificate).
-- * 'certificatePem' - The certificate data, in PEM format.
-- * 'keyPair' - The generated key pair.
-- * 'responseStatus' - The response status code.
mkCreateKeysAndCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateKeysAndCertificateResponse
mkCreateKeysAndCertificateResponse pResponseStatus_ =
  CreateKeysAndCertificateResponse'
    { keyPair = Lude.Nothing,
      certificatePem = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The generated key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrsKeyPair :: Lens.Lens' CreateKeysAndCertificateResponse (Lude.Maybe KeyPair)
ckacrsKeyPair = Lens.lens (keyPair :: CreateKeysAndCertificateResponse -> Lude.Maybe KeyPair) (\s a -> s {keyPair = a} :: CreateKeysAndCertificateResponse)
{-# DEPRECATED ckacrsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrsCertificatePem :: Lens.Lens' CreateKeysAndCertificateResponse (Lude.Maybe Lude.Text)
ckacrsCertificatePem = Lens.lens (certificatePem :: CreateKeysAndCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: CreateKeysAndCertificateResponse)
{-# DEPRECATED ckacrsCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrsCertificateARN :: Lens.Lens' CreateKeysAndCertificateResponse (Lude.Maybe Lude.Text)
ckacrsCertificateARN = Lens.lens (certificateARN :: CreateKeysAndCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CreateKeysAndCertificateResponse)
{-# DEPRECATED ckacrsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The ID of the certificate. AWS IoT issues a default subject name for the certificate (for example, AWS IoT Certificate).
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrsCertificateId :: Lens.Lens' CreateKeysAndCertificateResponse (Lude.Maybe Lude.Text)
ckacrsCertificateId = Lens.lens (certificateId :: CreateKeysAndCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: CreateKeysAndCertificateResponse)
{-# DEPRECATED ckacrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrsResponseStatus :: Lens.Lens' CreateKeysAndCertificateResponse Lude.Int
ckacrsResponseStatus = Lens.lens (responseStatus :: CreateKeysAndCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateKeysAndCertificateResponse)
{-# DEPRECATED ckacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
