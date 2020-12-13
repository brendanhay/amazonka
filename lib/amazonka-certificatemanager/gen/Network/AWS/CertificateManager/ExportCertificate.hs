{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ExportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a private certificate issued by a private certificate authority (CA) for use anywhere. The exported file contains the certificate, the certificate chain, and the encrypted private 2048-bit RSA key associated with the public key that is embedded in the certificate. For security, you must assign a passphrase for the private key when exporting it.
--
-- For information about exporting and formatting a certificate using the ACM console or CLI, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-export-private.html Export a Private Certificate> .
module Network.AWS.CertificateManager.ExportCertificate
  ( -- * Creating a request
    ExportCertificate (..),
    mkExportCertificate,

    -- ** Request lenses
    ecPassphrase,
    ecCertificateARN,

    -- * Destructuring the response
    ExportCertificateResponse (..),
    mkExportCertificateResponse,

    -- ** Response lenses
    ecrsPrivateKey,
    ecrsCertificate,
    ecrsCertificateChain,
    ecrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportCertificate' smart constructor.
data ExportCertificate = ExportCertificate'
  { -- | Passphrase to associate with the encrypted exported private key. If you want to later decrypt the private key, you must have the passphrase. You can use the following OpenSSL command to decrypt a private key:
    --
    -- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@
    passphrase :: Lude.Sensitive Lude.Base64,
    -- | An Amazon Resource Name (ARN) of the issued certificate. This must be of the form:
    --
    -- @arn:aws:acm:region:account:certificate/12345678-1234-1234-1234-123456789012@
    certificateARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportCertificate' with the minimum fields required to make a request.
--
-- * 'passphrase' - Passphrase to associate with the encrypted exported private key. If you want to later decrypt the private key, you must have the passphrase. You can use the following OpenSSL command to decrypt a private key:
--
-- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@
-- * 'certificateARN' - An Amazon Resource Name (ARN) of the issued certificate. This must be of the form:
--
-- @arn:aws:acm:region:account:certificate/12345678-1234-1234-1234-123456789012@
mkExportCertificate ::
  -- | 'passphrase'
  Lude.Sensitive Lude.Base64 ->
  -- | 'certificateARN'
  Lude.Text ->
  ExportCertificate
mkExportCertificate pPassphrase_ pCertificateARN_ =
  ExportCertificate'
    { passphrase = pPassphrase_,
      certificateARN = pCertificateARN_
    }

-- | Passphrase to associate with the encrypted exported private key. If you want to later decrypt the private key, you must have the passphrase. You can use the following OpenSSL command to decrypt a private key:
--
-- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@ --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'passphrase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecPassphrase :: Lens.Lens' ExportCertificate (Lude.Sensitive Lude.Base64)
ecPassphrase = Lens.lens (passphrase :: ExportCertificate -> Lude.Sensitive Lude.Base64) (\s a -> s {passphrase = a} :: ExportCertificate)
{-# DEPRECATED ecPassphrase "Use generic-lens or generic-optics with 'passphrase' instead." #-}

-- | An Amazon Resource Name (ARN) of the issued certificate. This must be of the form:
--
-- @arn:aws:acm:region:account:certificate/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecCertificateARN :: Lens.Lens' ExportCertificate Lude.Text
ecCertificateARN = Lens.lens (certificateARN :: ExportCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: ExportCertificate)
{-# DEPRECATED ecCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.AWSRequest ExportCertificate where
  type Rs ExportCertificate = ExportCertificateResponse
  request = Req.postJSON certificateManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExportCertificateResponse'
            Lude.<$> (x Lude..?> "PrivateKey")
            Lude.<*> (x Lude..?> "Certificate")
            Lude.<*> (x Lude..?> "CertificateChain")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.ExportCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExportCertificate where
  toJSON ExportCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Passphrase" Lude..= passphrase),
            Lude.Just ("CertificateArn" Lude..= certificateARN)
          ]
      )

instance Lude.ToPath ExportCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery ExportCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExportCertificateResponse' smart constructor.
data ExportCertificateResponse = ExportCertificateResponse'
  { -- | The encrypted private key associated with the public key in the certificate. The key is output in PKCS #8 format and is base64 PEM-encoded.
    privateKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The base64 PEM-encoded certificate.
    certificate :: Lude.Maybe Lude.Text,
    -- | The base64 PEM-encoded certificate chain. This does not include the certificate that you are exporting.
    certificateChain :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportCertificateResponse' with the minimum fields required to make a request.
--
-- * 'privateKey' - The encrypted private key associated with the public key in the certificate. The key is output in PKCS #8 format and is base64 PEM-encoded.
-- * 'certificate' - The base64 PEM-encoded certificate.
-- * 'certificateChain' - The base64 PEM-encoded certificate chain. This does not include the certificate that you are exporting.
-- * 'responseStatus' - The response status code.
mkExportCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportCertificateResponse
mkExportCertificateResponse pResponseStatus_ =
  ExportCertificateResponse'
    { privateKey = Lude.Nothing,
      certificate = Lude.Nothing,
      certificateChain = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The encrypted private key associated with the public key in the certificate. The key is output in PKCS #8 format and is base64 PEM-encoded.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrsPrivateKey :: Lens.Lens' ExportCertificateResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ecrsPrivateKey = Lens.lens (privateKey :: ExportCertificateResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {privateKey = a} :: ExportCertificateResponse)
{-# DEPRECATED ecrsPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | The base64 PEM-encoded certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrsCertificate :: Lens.Lens' ExportCertificateResponse (Lude.Maybe Lude.Text)
ecrsCertificate = Lens.lens (certificate :: ExportCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: ExportCertificateResponse)
{-# DEPRECATED ecrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The base64 PEM-encoded certificate chain. This does not include the certificate that you are exporting.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrsCertificateChain :: Lens.Lens' ExportCertificateResponse (Lude.Maybe Lude.Text)
ecrsCertificateChain = Lens.lens (certificateChain :: ExportCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateChain = a} :: ExportCertificateResponse)
{-# DEPRECATED ecrsCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrsResponseStatus :: Lens.Lens' ExportCertificateResponse Lude.Int
ecrsResponseStatus = Lens.lens (responseStatus :: ExportCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportCertificateResponse)
{-# DEPRECATED ecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
