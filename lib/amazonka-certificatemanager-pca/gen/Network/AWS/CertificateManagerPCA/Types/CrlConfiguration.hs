{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
  ( CrlConfiguration (..),

    -- * Smart constructor
    mkCrlConfiguration,

    -- * Lenses
    ccEnabled,
    ccCustomCname,
    ccExpirationInDays,
    ccS3BucketName,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types.CustomCname as Types
import qualified Network.AWS.CertificateManagerPCA.Types.String3To255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains configuration information for a certificate revocation list (CRL). Your private certificate authority (CA) creates base CRLs. Delta CRLs are not supported. You can enable CRLs for your new or an existing private CA by setting the __Enabled__ parameter to @true@ . Your private CA writes CRLs to an S3 bucket that you specify in the __S3BucketName__ parameter. You can hide the name of your bucket by specifying a value for the __CustomCname__ parameter. Your private CA copies the CNAME or the S3 bucket name to the __CRL Distribution Points__ extension of each certificate it issues. Your S3 bucket policy must give write permission to ACM Private CA.
--
-- ACM Private CAA assets that are stored in Amazon S3 can be protected with encryption. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaCreateCa.html#crl-encryption Encrypting Your CRLs> .
-- Your private CA uses the value in the __ExpirationInDays__ parameter to calculate the __nextUpdate__ field in the CRL. The CRL is refreshed at 1/2 the age of next update or when a certificate is revoked. When a certificate is revoked, it is recorded in the next CRL that is generated and in the next audit report. Only time valid certificates are listed in the CRL. Expired certificates are not included.
-- CRLs contain the following fields:
--
--     * __Version__ : The current version number defined in RFC 5280 is V2. The integer value is 0x1.
--
--
--     * __Signature Algorithm__ : The name of the algorithm used to sign the CRL.
--
--
--     * __Issuer__ : The X.500 distinguished name of your private CA that issued the CRL.
--
--
--     * __Last Update__ : The issue date and time of this CRL.
--
--
--     * __Next Update__ : The day and time by which the next CRL will be issued.
--
--
--     * __Revoked Certificates__ : List of revoked certificates. Each list item contains the following information.
--
--     * __Serial Number__ : The serial number, in hexadecimal format, of the revoked certificate.
--
--
--     * __Revocation Date__ : Date and time the certificate was revoked.
--
--
--     * __CRL Entry Extensions__ : Optional extensions for the CRL entry.
--
--     * __X509v3 CRL Reason Code__ : Reason the certificate was revoked.
--
--
--
--
--
--
--     * __CRL Extensions__ : Optional extensions for the CRL.
--
--     * __X509v3 Authority Key Identifier__ : Identifies the public key associated with the private key used to sign the certificate.
--
--
--     * __X509v3 CRL Number:__ : Decimal sequence number for the CRL.
--
--
--
--
--     * __Signature Algorithm__ : Algorithm used by your private CA to sign the CRL.
--
--
--     * __Signature Value__ : Signature computed over the CRL.
--
--
-- Certificate revocation lists created by ACM Private CA are DER-encoded. You can use the following OpenSSL command to list a CRL.
-- @openssl crl -inform DER -text -in /crl_path/ -noout@
--
-- /See:/ 'mkCrlConfiguration' smart constructor.
data CrlConfiguration = CrlConfiguration'
  { -- | Boolean value that specifies whether certificate revocation lists (CRLs) are enabled. You can use this value to enable certificate revocation for a new CA when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action or for an existing CA when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action.
    enabled :: Core.Bool,
    -- | Name inserted into the certificate __CRL Distribution Points__ extension that enables the use of an alias for the CRL distribution point. Use this value if you don't want the name of your S3 bucket to be public.
    customCname :: Core.Maybe Types.CustomCname,
    -- | Number of days until a certificate expires.
    expirationInDays :: Core.Maybe Core.Natural,
    -- | Name of the S3 bucket that contains the CRL. If you do not provide a value for the __CustomCname__ argument, the name of your S3 bucket is placed into the __CRL Distribution Points__ extension of the issued certificate. You can change the name of your bucket by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action. You must specify a bucket policy that allows ACM Private CA to write the CRL to your bucket.
    s3BucketName :: Core.Maybe Types.String3To255
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CrlConfiguration' value with any optional fields omitted.
mkCrlConfiguration ::
  -- | 'enabled'
  Core.Bool ->
  CrlConfiguration
mkCrlConfiguration enabled =
  CrlConfiguration'
    { enabled,
      customCname = Core.Nothing,
      expirationInDays = Core.Nothing,
      s3BucketName = Core.Nothing
    }

-- | Boolean value that specifies whether certificate revocation lists (CRLs) are enabled. You can use this value to enable certificate revocation for a new CA when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action or for an existing CA when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEnabled :: Lens.Lens' CrlConfiguration Core.Bool
ccEnabled = Lens.field @"enabled"
{-# DEPRECATED ccEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Name inserted into the certificate __CRL Distribution Points__ extension that enables the use of an alias for the CRL distribution point. Use this value if you don't want the name of your S3 bucket to be public.
--
-- /Note:/ Consider using 'customCname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCustomCname :: Lens.Lens' CrlConfiguration (Core.Maybe Types.CustomCname)
ccCustomCname = Lens.field @"customCname"
{-# DEPRECATED ccCustomCname "Use generic-lens or generic-optics with 'customCname' instead." #-}

-- | Number of days until a certificate expires.
--
-- /Note:/ Consider using 'expirationInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccExpirationInDays :: Lens.Lens' CrlConfiguration (Core.Maybe Core.Natural)
ccExpirationInDays = Lens.field @"expirationInDays"
{-# DEPRECATED ccExpirationInDays "Use generic-lens or generic-optics with 'expirationInDays' instead." #-}

-- | Name of the S3 bucket that contains the CRL. If you do not provide a value for the __CustomCname__ argument, the name of your S3 bucket is placed into the __CRL Distribution Points__ extension of the issued certificate. You can change the name of your bucket by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UpdateCertificateAuthority.html UpdateCertificateAuthority> action. You must specify a bucket policy that allows ACM Private CA to write the CRL to your bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccS3BucketName :: Lens.Lens' CrlConfiguration (Core.Maybe Types.String3To255)
ccS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED ccS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Core.FromJSON CrlConfiguration where
  toJSON CrlConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Enabled" Core..= enabled),
            ("CustomCname" Core..=) Core.<$> customCname,
            ("ExpirationInDays" Core..=) Core.<$> expirationInDays,
            ("S3BucketName" Core..=) Core.<$> s3BucketName
          ]
      )

instance Core.FromJSON CrlConfiguration where
  parseJSON =
    Core.withObject "CrlConfiguration" Core.$
      \x ->
        CrlConfiguration'
          Core.<$> (x Core..: "Enabled")
          Core.<*> (x Core..:? "CustomCname")
          Core.<*> (x Core..:? "ExpirationInDays")
          Core.<*> (x Core..:? "S3BucketName")
