{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a CA certificate with AWS IoT. This CA certificate can then be used to sign device certificates, which can be then registered with AWS IoT. You can register up to 10 CA certificates per AWS account that have the same subject field. This enables you to have up to 10 certificate authorities sign your device certificates. If you have more than one CA certificate registered, make sure you pass the CA certificate when you register your device certificates with the RegisterCertificate API.
module Network.AWS.IoT.RegisterCACertificate
  ( -- * Creating a request
    RegisterCACertificate (..),
    mkRegisterCACertificate,

    -- ** Request lenses
    rcacSetAsActive,
    rcacAllowAutoRegistration,
    rcacRegistrationConfig,
    rcacTags,
    rcacCaCertificate,
    rcacVerificationCertificate,

    -- * Destructuring the response
    RegisterCACertificateResponse (..),
    mkRegisterCACertificateResponse,

    -- ** Response lenses
    rcacrsCertificateARN,
    rcacrsCertificateId,
    rcacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input to the RegisterCACertificate operation.
--
-- /See:/ 'mkRegisterCACertificate' smart constructor.
data RegisterCACertificate = RegisterCACertificate'
  { setAsActive ::
      Lude.Maybe Lude.Bool,
    allowAutoRegistration :: Lude.Maybe Lude.Bool,
    registrationConfig ::
      Lude.Maybe RegistrationConfig,
    tags :: Lude.Maybe [Tag],
    caCertificate :: Lude.Text,
    verificationCertificate :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterCACertificate' with the minimum fields required to make a request.
--
-- * 'allowAutoRegistration' - Allows this CA certificate to be used for auto registration of device certificates.
-- * 'caCertificate' - The CA certificate.
-- * 'registrationConfig' - Information about the registration configuration.
-- * 'setAsActive' - A boolean value that specifies if the CA certificate is set to active.
-- * 'tags' - Metadata which can be used to manage the CA certificate.
-- * 'verificationCertificate' - The private key verification certificate.
mkRegisterCACertificate ::
  -- | 'caCertificate'
  Lude.Text ->
  -- | 'verificationCertificate'
  Lude.Text ->
  RegisterCACertificate
mkRegisterCACertificate pCaCertificate_ pVerificationCertificate_ =
  RegisterCACertificate'
    { setAsActive = Lude.Nothing,
      allowAutoRegistration = Lude.Nothing,
      registrationConfig = Lude.Nothing,
      tags = Lude.Nothing,
      caCertificate = pCaCertificate_,
      verificationCertificate = pVerificationCertificate_
    }

-- | A boolean value that specifies if the CA certificate is set to active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacSetAsActive :: Lens.Lens' RegisterCACertificate (Lude.Maybe Lude.Bool)
rcacSetAsActive = Lens.lens (setAsActive :: RegisterCACertificate -> Lude.Maybe Lude.Bool) (\s a -> s {setAsActive = a} :: RegisterCACertificate)
{-# DEPRECATED rcacSetAsActive "Use generic-lens or generic-optics with 'setAsActive' instead." #-}

-- | Allows this CA certificate to be used for auto registration of device certificates.
--
-- /Note:/ Consider using 'allowAutoRegistration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacAllowAutoRegistration :: Lens.Lens' RegisterCACertificate (Lude.Maybe Lude.Bool)
rcacAllowAutoRegistration = Lens.lens (allowAutoRegistration :: RegisterCACertificate -> Lude.Maybe Lude.Bool) (\s a -> s {allowAutoRegistration = a} :: RegisterCACertificate)
{-# DEPRECATED rcacAllowAutoRegistration "Use generic-lens or generic-optics with 'allowAutoRegistration' instead." #-}

-- | Information about the registration configuration.
--
-- /Note:/ Consider using 'registrationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacRegistrationConfig :: Lens.Lens' RegisterCACertificate (Lude.Maybe RegistrationConfig)
rcacRegistrationConfig = Lens.lens (registrationConfig :: RegisterCACertificate -> Lude.Maybe RegistrationConfig) (\s a -> s {registrationConfig = a} :: RegisterCACertificate)
{-# DEPRECATED rcacRegistrationConfig "Use generic-lens or generic-optics with 'registrationConfig' instead." #-}

-- | Metadata which can be used to manage the CA certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacTags :: Lens.Lens' RegisterCACertificate (Lude.Maybe [Tag])
rcacTags = Lens.lens (tags :: RegisterCACertificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RegisterCACertificate)
{-# DEPRECATED rcacTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The CA certificate.
--
-- /Note:/ Consider using 'caCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacCaCertificate :: Lens.Lens' RegisterCACertificate Lude.Text
rcacCaCertificate = Lens.lens (caCertificate :: RegisterCACertificate -> Lude.Text) (\s a -> s {caCertificate = a} :: RegisterCACertificate)
{-# DEPRECATED rcacCaCertificate "Use generic-lens or generic-optics with 'caCertificate' instead." #-}

-- | The private key verification certificate.
--
-- /Note:/ Consider using 'verificationCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacVerificationCertificate :: Lens.Lens' RegisterCACertificate Lude.Text
rcacVerificationCertificate = Lens.lens (verificationCertificate :: RegisterCACertificate -> Lude.Text) (\s a -> s {verificationCertificate = a} :: RegisterCACertificate)
{-# DEPRECATED rcacVerificationCertificate "Use generic-lens or generic-optics with 'verificationCertificate' instead." #-}

instance Lude.AWSRequest RegisterCACertificate where
  type Rs RegisterCACertificate = RegisterCACertificateResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterCACertificateResponse'
            Lude.<$> (x Lude..?> "certificateArn")
            Lude.<*> (x Lude..?> "certificateId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterCACertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RegisterCACertificate where
  toJSON RegisterCACertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registrationConfig" Lude..=) Lude.<$> registrationConfig,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("caCertificate" Lude..= caCertificate),
            Lude.Just
              ("verificationCertificate" Lude..= verificationCertificate)
          ]
      )

instance Lude.ToPath RegisterCACertificate where
  toPath = Lude.const "/cacertificate"

instance Lude.ToQuery RegisterCACertificate where
  toQuery RegisterCACertificate' {..} =
    Lude.mconcat
      [ "setAsActive" Lude.=: setAsActive,
        "allowAutoRegistration" Lude.=: allowAutoRegistration
      ]

-- | The output from the RegisterCACertificateResponse operation.
--
-- /See:/ 'mkRegisterCACertificateResponse' smart constructor.
data RegisterCACertificateResponse = RegisterCACertificateResponse'
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

-- | Creates a value of 'RegisterCACertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The CA certificate ARN.
-- * 'certificateId' - The CA certificate identifier.
-- * 'responseStatus' - The response status code.
mkRegisterCACertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterCACertificateResponse
mkRegisterCACertificateResponse pResponseStatus_ =
  RegisterCACertificateResponse'
    { certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The CA certificate ARN.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacrsCertificateARN :: Lens.Lens' RegisterCACertificateResponse (Lude.Maybe Lude.Text)
rcacrsCertificateARN = Lens.lens (certificateARN :: RegisterCACertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: RegisterCACertificateResponse)
{-# DEPRECATED rcacrsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The CA certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacrsCertificateId :: Lens.Lens' RegisterCACertificateResponse (Lude.Maybe Lude.Text)
rcacrsCertificateId = Lens.lens (certificateId :: RegisterCACertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: RegisterCACertificateResponse)
{-# DEPRECATED rcacrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacrsResponseStatus :: Lens.Lens' RegisterCACertificateResponse Lude.Int
rcacrsResponseStatus = Lens.lens (responseStatus :: RegisterCACertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterCACertificateResponse)
{-# DEPRECATED rcacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
