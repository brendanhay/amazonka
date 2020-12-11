{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ImportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the specified certificate.
module Network.AWS.DMS.ImportCertificate
  ( -- * Creating a request
    ImportCertificate (..),
    mkImportCertificate,

    -- ** Request lenses
    icCertificatePem,
    icCertificateWallet,
    icTags,
    icCertificateIdentifier,

    -- * Destructuring the response
    ImportCertificateResponse (..),
    mkImportCertificateResponse,

    -- ** Response lenses
    icrsCertificate,
    icrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { certificatePem ::
      Lude.Maybe Lude.Text,
    certificateWallet :: Lude.Maybe Lude.Base64,
    tags :: Lude.Maybe [Tag],
    certificateIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportCertificate' with the minimum fields required to make a request.
--
-- * 'certificateIdentifier' - A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
-- * 'certificatePem' - The contents of a @.pem@ file, which contains an X.509 certificate.
-- * 'certificateWallet' - The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'tags' - The tags associated with the certificate.
mkImportCertificate ::
  -- | 'certificateIdentifier'
  Lude.Text ->
  ImportCertificate
mkImportCertificate pCertificateIdentifier_ =
  ImportCertificate'
    { certificatePem = Lude.Nothing,
      certificateWallet = Lude.Nothing,
      tags = Lude.Nothing,
      certificateIdentifier = pCertificateIdentifier_
    }

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificatePem :: Lens.Lens' ImportCertificate (Lude.Maybe Lude.Text)
icCertificatePem = Lens.lens (certificatePem :: ImportCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: ImportCertificate)
{-# DEPRECATED icCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateWallet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateWallet :: Lens.Lens' ImportCertificate (Lude.Maybe Lude.Base64)
icCertificateWallet = Lens.lens (certificateWallet :: ImportCertificate -> Lude.Maybe Lude.Base64) (\s a -> s {certificateWallet = a} :: ImportCertificate)
{-# DEPRECATED icCertificateWallet "Use generic-lens or generic-optics with 'certificateWallet' instead." #-}

-- | The tags associated with the certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTags :: Lens.Lens' ImportCertificate (Lude.Maybe [Tag])
icTags = Lens.lens (tags :: ImportCertificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ImportCertificate)
{-# DEPRECATED icTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateIdentifier :: Lens.Lens' ImportCertificate Lude.Text
icCertificateIdentifier = Lens.lens (certificateIdentifier :: ImportCertificate -> Lude.Text) (\s a -> s {certificateIdentifier = a} :: ImportCertificate)
{-# DEPRECATED icCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

instance Lude.AWSRequest ImportCertificate where
  type Rs ImportCertificate = ImportCertificateResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportCertificateResponse'
            Lude.<$> (x Lude..?> "Certificate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.ImportCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportCertificate where
  toJSON ImportCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CertificatePem" Lude..=) Lude.<$> certificatePem,
            ("CertificateWallet" Lude..=) Lude.<$> certificateWallet,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("CertificateIdentifier" Lude..= certificateIdentifier)
          ]
      )

instance Lude.ToPath ImportCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { certificate ::
      Lude.Maybe Certificate,
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

-- | Creates a value of 'ImportCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - The certificate to be uploaded.
-- * 'responseStatus' - The response status code.
mkImportCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportCertificateResponse
mkImportCertificateResponse pResponseStatus_ =
  ImportCertificateResponse'
    { certificate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The certificate to be uploaded.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsCertificate :: Lens.Lens' ImportCertificateResponse (Lude.Maybe Certificate)
icrsCertificate = Lens.lens (certificate :: ImportCertificateResponse -> Lude.Maybe Certificate) (\s a -> s {certificate = a} :: ImportCertificateResponse)
{-# DEPRECATED icrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsResponseStatus :: Lens.Lens' ImportCertificateResponse Lude.Int
icrsResponseStatus = Lens.lens (responseStatus :: ImportCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportCertificateResponse)
{-# DEPRECATED icrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
