{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a root or subordinate private certificate authority (CA). You must specify the CA configuration, the certificate revocation list (CRL) configuration, the CA type, and an optional idempotency token to avoid accidental creation of multiple CAs. The CA configuration specifies the name of the algorithm and key size to be used to create the CA private key, the type of signing algorithm that the CA uses, and X.500 subject information. The CRL configuration specifies the CRL expiration period in days (the validity period of the CRL), the Amazon S3 bucket that will contain the CRL, and a CNAME alias for the S3 bucket that is included in certificates issued by the CA. If successful, this action returns the Amazon Resource Name (ARN) of the CA.
--
-- ACM Private CAA assets that are stored in Amazon S3 can be protected with encryption. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaCreateCa.html#crl-encryption Encrypting Your CRLs> .
module Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
  ( -- * Creating a request
    CreateCertificateAuthority (..),
    mkCreateCertificateAuthority,

    -- ** Request lenses
    ccaIdempotencyToken,
    ccaCertificateAuthorityConfiguration,
    ccaCertificateAuthorityType,
    ccaRevocationConfiguration,
    ccaTags,

    -- * Destructuring the response
    CreateCertificateAuthorityResponse (..),
    mkCreateCertificateAuthorityResponse,

    -- ** Response lenses
    ccarsCertificateAuthorityARN,
    ccarsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCertificateAuthority' smart constructor.
data CreateCertificateAuthority = CreateCertificateAuthority'
  { -- | Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . For a given token, ACM Private CA creates exactly one CA. If you issue a subsequent call using the same token, ACM Private CA returns the ARN of the existing CA and takes no further action. If you change the idempotency token across multiple calls, ACM Private CA creates a unique CA for each unique token.
    idempotencyToken :: Lude.Maybe Lude.Text,
    -- | Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
    certificateAuthorityConfiguration :: CertificateAuthorityConfiguration,
    -- | The type of the certificate authority.
    certificateAuthorityType :: CertificateAuthorityType,
    -- | Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM Private CA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration> structure.
    revocationConfiguration :: Lude.Maybe RevocationConfiguration,
    -- | Key-value pairs that will be attached to the new private CA. You can associate up to 50 tags with a private CA. For information using tags with IAM to manage permissions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . For a given token, ACM Private CA creates exactly one CA. If you issue a subsequent call using the same token, ACM Private CA returns the ARN of the existing CA and takes no further action. If you change the idempotency token across multiple calls, ACM Private CA creates a unique CA for each unique token.
-- * 'certificateAuthorityConfiguration' - Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
-- * 'certificateAuthorityType' - The type of the certificate authority.
-- * 'revocationConfiguration' - Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM Private CA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration> structure.
-- * 'tags' - Key-value pairs that will be attached to the new private CA. You can associate up to 50 tags with a private CA. For information using tags with IAM to manage permissions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
mkCreateCertificateAuthority ::
  -- | 'certificateAuthorityConfiguration'
  CertificateAuthorityConfiguration ->
  -- | 'certificateAuthorityType'
  CertificateAuthorityType ->
  CreateCertificateAuthority
mkCreateCertificateAuthority
  pCertificateAuthorityConfiguration_
  pCertificateAuthorityType_ =
    CreateCertificateAuthority'
      { idempotencyToken = Lude.Nothing,
        certificateAuthorityConfiguration =
          pCertificateAuthorityConfiguration_,
        certificateAuthorityType = pCertificateAuthorityType_,
        revocationConfiguration = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . For a given token, ACM Private CA creates exactly one CA. If you issue a subsequent call using the same token, ACM Private CA returns the ARN of the existing CA and takes no further action. If you change the idempotency token across multiple calls, ACM Private CA creates a unique CA for each unique token.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaIdempotencyToken :: Lens.Lens' CreateCertificateAuthority (Lude.Maybe Lude.Text)
ccaIdempotencyToken = Lens.lens (idempotencyToken :: CreateCertificateAuthority -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: CreateCertificateAuthority)
{-# DEPRECATED ccaIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
--
-- /Note:/ Consider using 'certificateAuthorityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaCertificateAuthorityConfiguration :: Lens.Lens' CreateCertificateAuthority CertificateAuthorityConfiguration
ccaCertificateAuthorityConfiguration = Lens.lens (certificateAuthorityConfiguration :: CreateCertificateAuthority -> CertificateAuthorityConfiguration) (\s a -> s {certificateAuthorityConfiguration = a} :: CreateCertificateAuthority)
{-# DEPRECATED ccaCertificateAuthorityConfiguration "Use generic-lens or generic-optics with 'certificateAuthorityConfiguration' instead." #-}

-- | The type of the certificate authority.
--
-- /Note:/ Consider using 'certificateAuthorityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaCertificateAuthorityType :: Lens.Lens' CreateCertificateAuthority CertificateAuthorityType
ccaCertificateAuthorityType = Lens.lens (certificateAuthorityType :: CreateCertificateAuthority -> CertificateAuthorityType) (\s a -> s {certificateAuthorityType = a} :: CreateCertificateAuthority)
{-# DEPRECATED ccaCertificateAuthorityType "Use generic-lens or generic-optics with 'certificateAuthorityType' instead." #-}

-- | Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM Private CA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration> structure.
--
-- /Note:/ Consider using 'revocationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaRevocationConfiguration :: Lens.Lens' CreateCertificateAuthority (Lude.Maybe RevocationConfiguration)
ccaRevocationConfiguration = Lens.lens (revocationConfiguration :: CreateCertificateAuthority -> Lude.Maybe RevocationConfiguration) (\s a -> s {revocationConfiguration = a} :: CreateCertificateAuthority)
{-# DEPRECATED ccaRevocationConfiguration "Use generic-lens or generic-optics with 'revocationConfiguration' instead." #-}

-- | Key-value pairs that will be attached to the new private CA. You can associate up to 50 tags with a private CA. For information using tags with IAM to manage permissions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaTags :: Lens.Lens' CreateCertificateAuthority (Lude.Maybe (Lude.NonEmpty Tag))
ccaTags = Lens.lens (tags :: CreateCertificateAuthority -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateCertificateAuthority)
{-# DEPRECATED ccaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateCertificateAuthority where
  type
    Rs CreateCertificateAuthority =
      CreateCertificateAuthorityResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityResponse'
            Lude.<$> (x Lude..?> "CertificateAuthorityArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.CreateCertificateAuthority" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCertificateAuthority where
  toJSON CreateCertificateAuthority' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            Lude.Just
              ( "CertificateAuthorityConfiguration"
                  Lude..= certificateAuthorityConfiguration
              ),
            Lude.Just
              ("CertificateAuthorityType" Lude..= certificateAuthorityType),
            ("RevocationConfiguration" Lude..=)
              Lude.<$> revocationConfiguration,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateCertificateAuthority where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCertificateAuthorityResponse' smart constructor.
data CreateCertificateAuthorityResponse = CreateCertificateAuthorityResponse'
  { -- | If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityARN' - If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
-- * 'responseStatus' - The response status code.
mkCreateCertificateAuthorityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCertificateAuthorityResponse
mkCreateCertificateAuthorityResponse pResponseStatus_ =
  CreateCertificateAuthorityResponse'
    { certificateAuthorityARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarsCertificateAuthorityARN :: Lens.Lens' CreateCertificateAuthorityResponse (Lude.Maybe Lude.Text)
ccarsCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: CreateCertificateAuthorityResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: CreateCertificateAuthorityResponse)
{-# DEPRECATED ccarsCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarsResponseStatus :: Lens.Lens' CreateCertificateAuthorityResponse Lude.Int
ccarsResponseStatus = Lens.lens (responseStatus :: CreateCertificateAuthorityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCertificateAuthorityResponse)
{-# DEPRECATED ccarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
