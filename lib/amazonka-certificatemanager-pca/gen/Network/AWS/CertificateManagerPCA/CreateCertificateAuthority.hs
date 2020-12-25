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
    ccaCertificateAuthorityConfiguration,
    ccaCertificateAuthorityType,
    ccaIdempotencyToken,
    ccaRevocationConfiguration,
    ccaTags,

    -- * Destructuring the response
    CreateCertificateAuthorityResponse (..),
    mkCreateCertificateAuthorityResponse,

    -- ** Response lenses
    ccarrsCertificateAuthorityArn,
    ccarrsResponseStatus,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCertificateAuthority' smart constructor.
data CreateCertificateAuthority = CreateCertificateAuthority'
  { -- | Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
    certificateAuthorityConfiguration :: Types.CertificateAuthorityConfiguration,
    -- | The type of the certificate authority.
    certificateAuthorityType :: Types.CertificateAuthorityType,
    -- | Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . For a given token, ACM Private CA creates exactly one CA. If you issue a subsequent call using the same token, ACM Private CA returns the ARN of the existing CA and takes no further action. If you change the idempotency token across multiple calls, ACM Private CA creates a unique CA for each unique token.
    idempotencyToken :: Core.Maybe Types.IdempotencyToken,
    -- | Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM Private CA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration> structure.
    revocationConfiguration :: Core.Maybe Types.RevocationConfiguration,
    -- | Key-value pairs that will be attached to the new private CA. You can associate up to 50 tags with a private CA. For information using tags with IAM to manage permissions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCertificateAuthority' value with any optional fields omitted.
mkCreateCertificateAuthority ::
  -- | 'certificateAuthorityConfiguration'
  Types.CertificateAuthorityConfiguration ->
  -- | 'certificateAuthorityType'
  Types.CertificateAuthorityType ->
  CreateCertificateAuthority
mkCreateCertificateAuthority
  certificateAuthorityConfiguration
  certificateAuthorityType =
    CreateCertificateAuthority'
      { certificateAuthorityConfiguration,
        certificateAuthorityType,
        idempotencyToken = Core.Nothing,
        revocationConfiguration = Core.Nothing,
        tags = Core.Nothing
      }

-- | Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
--
-- /Note:/ Consider using 'certificateAuthorityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaCertificateAuthorityConfiguration :: Lens.Lens' CreateCertificateAuthority Types.CertificateAuthorityConfiguration
ccaCertificateAuthorityConfiguration = Lens.field @"certificateAuthorityConfiguration"
{-# DEPRECATED ccaCertificateAuthorityConfiguration "Use generic-lens or generic-optics with 'certificateAuthorityConfiguration' instead." #-}

-- | The type of the certificate authority.
--
-- /Note:/ Consider using 'certificateAuthorityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaCertificateAuthorityType :: Lens.Lens' CreateCertificateAuthority Types.CertificateAuthorityType
ccaCertificateAuthorityType = Lens.field @"certificateAuthorityType"
{-# DEPRECATED ccaCertificateAuthorityType "Use generic-lens or generic-optics with 'certificateAuthorityType' instead." #-}

-- | Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . For a given token, ACM Private CA creates exactly one CA. If you issue a subsequent call using the same token, ACM Private CA returns the ARN of the existing CA and takes no further action. If you change the idempotency token across multiple calls, ACM Private CA creates a unique CA for each unique token.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaIdempotencyToken :: Lens.Lens' CreateCertificateAuthority (Core.Maybe Types.IdempotencyToken)
ccaIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED ccaIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM Private CA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration> structure.
--
-- /Note:/ Consider using 'revocationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaRevocationConfiguration :: Lens.Lens' CreateCertificateAuthority (Core.Maybe Types.RevocationConfiguration)
ccaRevocationConfiguration = Lens.field @"revocationConfiguration"
{-# DEPRECATED ccaRevocationConfiguration "Use generic-lens or generic-optics with 'revocationConfiguration' instead." #-}

-- | Key-value pairs that will be attached to the new private CA. You can associate up to 50 tags with a private CA. For information using tags with IAM to manage permissions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccaTags :: Lens.Lens' CreateCertificateAuthority (Core.Maybe (Core.NonEmpty Types.Tag))
ccaTags = Lens.field @"tags"
{-# DEPRECATED ccaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateCertificateAuthority where
  toJSON CreateCertificateAuthority {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "CertificateAuthorityConfiguration"
                  Core..= certificateAuthorityConfiguration
              ),
            Core.Just
              ("CertificateAuthorityType" Core..= certificateAuthorityType),
            ("IdempotencyToken" Core..=) Core.<$> idempotencyToken,
            ("RevocationConfiguration" Core..=)
              Core.<$> revocationConfiguration,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateCertificateAuthority where
  type
    Rs CreateCertificateAuthority =
      CreateCertificateAuthorityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ACMPrivateCA.CreateCertificateAuthority")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityResponse'
            Core.<$> (x Core..:? "CertificateAuthorityArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCertificateAuthorityResponse' smart constructor.
data CreateCertificateAuthorityResponse = CreateCertificateAuthorityResponse'
  { -- | If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCertificateAuthorityResponse' value with any optional fields omitted.
mkCreateCertificateAuthorityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCertificateAuthorityResponse
mkCreateCertificateAuthorityResponse responseStatus =
  CreateCertificateAuthorityResponse'
    { certificateAuthorityArn =
        Core.Nothing,
      responseStatus
    }

-- | If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarrsCertificateAuthorityArn :: Lens.Lens' CreateCertificateAuthorityResponse (Core.Maybe Types.Arn)
ccarrsCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED ccarrsCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccarrsResponseStatus :: Lens.Lens' CreateCertificateAuthorityResponse Core.Int
ccarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
