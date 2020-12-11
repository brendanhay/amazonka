{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ImportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a certificate into AWS Certificate Manager (ACM) to use with services that are integrated with ACM. Note that <https://docs.aws.amazon.com/acm/latest/userguide/acm-services.html integrated services> allow only certificate types and keys they support to be associated with their resources. Further, their support differs depending on whether the certificate is imported into IAM or into ACM. For more information, see the documentation for each service. For more information about importing certificates into ACM, see <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
--
-- Note the following guidelines when importing third party certificates:
--
--     * You must enter the private key that matches the certificate you are importing.
--
--
--     * The private key must be unencrypted. You cannot import a private key that is protected by a password or a passphrase.
--
--
--     * If the certificate you are importing is not self-signed, you must enter its certificate chain.
--
--
--     * If a certificate chain is included, the issuer must be the subject of one of the certificates in the chain.
--
--
--     * The certificate, private key, and certificate chain must be PEM-encoded.
--
--
--     * The current time must be between the @Not Before@ and @Not After@ certificate fields.
--
--
--     * The @Issuer@ field must not be empty.
--
--
--     * The OCSP authority URL, if present, must not exceed 1000 characters.
--
--
--     * To import a new certificate, omit the @CertificateArn@ argument. Include this argument only when you want to replace a previously imported certifica
--
--
--     * When you import a certificate by using the CLI, you must specify the certificate, the certificate chain, and the private key by their file names preceded by @file://@ . For example, you can specify a certificate saved in the @C:\temp@ folder as @file://C:\temp\certificate_to_import.pem@ . If you are making an HTTP or HTTPS Query request, include these arguments as BLOBs.
--
--
--     * When you import a certificate by using an SDK, you must specify the certificate, the certificate chain, and the private key files in the manner required by the programming language you're using.
--
--
--     * The cryptographic algorithm of an imported certificate must match the algorithm of the signing CA. For example, if the signing CA key type is RSA, then the certificate key type must also be RSA.
--
--
-- This operation returns the <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
module Network.AWS.CertificateManager.ImportCertificate
  ( -- * Creating a request
    ImportCertificate (..),
    mkImportCertificate,

    -- ** Request lenses
    icCertificateARN,
    icCertificateChain,
    icTags,
    icCertificate,
    icPrivateKey,

    -- * Destructuring the response
    ImportCertificateResponse (..),
    mkImportCertificateResponse,

    -- ** Response lenses
    icrsCertificateARN,
    icrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { certificateARN ::
      Lude.Maybe Lude.Text,
    certificateChain :: Lude.Maybe Lude.Base64,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    certificate :: Lude.Base64,
    privateKey :: Lude.Sensitive Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportCertificate' with the minimum fields required to make a request.
--
-- * 'certificate' - The certificate to import.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'certificateARN' - The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an imported certificate to replace. To import a new certificate, omit this field.
-- * 'certificateChain' - The PEM encoded certificate chain.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'privateKey' - The private key that matches the public key in the certificate.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'tags' - One or more resource tags to associate with the imported certificate.
--
-- Note: You cannot apply tags when reimporting a certificate.
mkImportCertificate ::
  -- | 'certificate'
  Lude.Base64 ->
  -- | 'privateKey'
  Lude.Sensitive Lude.Base64 ->
  ImportCertificate
mkImportCertificate pCertificate_ pPrivateKey_ =
  ImportCertificate'
    { certificateARN = Lude.Nothing,
      certificateChain = Lude.Nothing,
      tags = Lude.Nothing,
      certificate = pCertificate_,
      privateKey = pPrivateKey_
    }

-- | The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an imported certificate to replace. To import a new certificate, omit this field.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateARN :: Lens.Lens' ImportCertificate (Lude.Maybe Lude.Text)
icCertificateARN = Lens.lens (certificateARN :: ImportCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: ImportCertificate)
{-# DEPRECATED icCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The PEM encoded certificate chain.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateChain :: Lens.Lens' ImportCertificate (Lude.Maybe Lude.Base64)
icCertificateChain = Lens.lens (certificateChain :: ImportCertificate -> Lude.Maybe Lude.Base64) (\s a -> s {certificateChain = a} :: ImportCertificate)
{-# DEPRECATED icCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | One or more resource tags to associate with the imported certificate.
--
-- Note: You cannot apply tags when reimporting a certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTags :: Lens.Lens' ImportCertificate (Lude.Maybe (Lude.NonEmpty Tag))
icTags = Lens.lens (tags :: ImportCertificate -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: ImportCertificate)
{-# DEPRECATED icTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The certificate to import.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificate :: Lens.Lens' ImportCertificate Lude.Base64
icCertificate = Lens.lens (certificate :: ImportCertificate -> Lude.Base64) (\s a -> s {certificate = a} :: ImportCertificate)
{-# DEPRECATED icCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The private key that matches the public key in the certificate.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icPrivateKey :: Lens.Lens' ImportCertificate (Lude.Sensitive Lude.Base64)
icPrivateKey = Lens.lens (privateKey :: ImportCertificate -> Lude.Sensitive Lude.Base64) (\s a -> s {privateKey = a} :: ImportCertificate)
{-# DEPRECATED icPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

instance Lude.AWSRequest ImportCertificate where
  type Rs ImportCertificate = ImportCertificateResponse
  request = Req.postJSON certificateManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportCertificateResponse'
            Lude.<$> (x Lude..?> "CertificateArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CertificateManager.ImportCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportCertificate where
  toJSON ImportCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CertificateArn" Lude..=) Lude.<$> certificateARN,
            ("CertificateChain" Lude..=) Lude.<$> certificateChain,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Certificate" Lude..= certificate),
            Lude.Just ("PrivateKey" Lude..= privateKey)
          ]
      )

instance Lude.ToPath ImportCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { certificateARN ::
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

-- | Creates a value of 'ImportCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
-- * 'responseStatus' - The response status code.
mkImportCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportCertificateResponse
mkImportCertificateResponse pResponseStatus_ =
  ImportCertificateResponse'
    { certificateARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsCertificateARN :: Lens.Lens' ImportCertificateResponse (Lude.Maybe Lude.Text)
icrsCertificateARN = Lens.lens (certificateARN :: ImportCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: ImportCertificateResponse)
{-# DEPRECATED icrsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsResponseStatus :: Lens.Lens' ImportCertificateResponse Lude.Int
icrsResponseStatus = Lens.lens (responseStatus :: ImportCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportCertificateResponse)
{-# DEPRECATED icrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
