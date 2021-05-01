{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ViewerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ViewerCertificate where

import Network.AWS.CloudFront.Types.CertificateSource
import Network.AWS.CloudFront.Types.MinimumProtocolVersion
import Network.AWS.CloudFront.Types.SSLSupportMethod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that determines the distribution’s SSL\/TLS configuration
-- for communicating with viewers.
--
-- If the distribution doesn’t use @Aliases@ (also known as alternate
-- domain names or CNAMEs)—that is, if the distribution uses the CloudFront
-- domain name such as @d111111abcdef8.cloudfront.net@—set
-- @CloudFrontDefaultCertificate@ to @true@ and leave all other fields
-- empty.
--
-- If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
-- use the fields in this type to specify the following settings:
--
-- -   Which viewers the distribution accepts HTTPS connections from: only
--     viewers that support
--     <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)>
--     (recommended), or all viewers including those that don’t support
--     SNI.
--
--     -   To accept HTTPS connections from only viewers that support SNI,
--         set @SSLSupportMethod@ to @sni-only@. This is recommended. Most
--         browsers and clients support SNI.
--
--     -   To accept HTTPS connections from all viewers, including those
--         that don’t support SNI, set @SSLSupportMethod@ to @vip@. This is
--         not recommended, and results in additional monthly charges from
--         CloudFront.
--
-- -   The minimum SSL\/TLS protocol version that the distribution can use
--     to communicate with viewers. To specify a minimum version, choose a
--     value for @MinimumProtocolVersion@. For more information, see
--     <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy>
--     in the /Amazon CloudFront Developer Guide/.
--
-- -   The location of the SSL\/TLS certificate,
--     <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)>
--     (recommended) or
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)>.
--     You specify the location by setting a value in one of the following
--     fields (not both):
--
--     -   @ACMCertificateArn@
--
--     -   @IAMCertificateId@
--
-- All distributions support HTTPS connections from viewers. To require
-- viewers to use HTTPS only, or to redirect them from HTTP to HTTPS, use
-- @ViewerProtocolPolicy@ in the @CacheBehavior@ or @DefaultCacheBehavior@.
-- To specify how CloudFront should use SSL\/TLS to communicate with your
-- custom origin, use @CustomOriginConfig@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https.html Using HTTPS with CloudFront>
-- and
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-alternate-domain-names.html Using Alternate Domain Names and HTTPS>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newViewerCertificate' smart constructor.
data ViewerCertificate = ViewerCertificate'
  { -- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
    -- specify which viewers the distribution accepts HTTPS connections from.
    --
    -- -   @sni-only@ – The distribution accepts HTTPS connections from only
    --     viewers that support
    --     <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)>.
    --     This is recommended. Most browsers and clients support SNI.
    --
    -- -   @vip@ – The distribution accepts HTTPS connections from all viewers
    --     including those that don’t support SNI. This is not recommended, and
    --     results in additional monthly charges from CloudFront.
    --
    -- -   @static-ip@ - Do not specify this value unless your distribution has
    --     been enabled for this feature by the CloudFront team. If you have a
    --     use case that requires static IP addresses for a distribution,
    --     contact CloudFront through the
    --     <https://console.aws.amazon.com/support/home AWS Support Center>.
    --
    -- If the distribution uses the CloudFront domain name such as
    -- @d111111abcdef8.cloudfront.net@, don’t set a value for this field.
    sSLSupportMethod :: Prelude.Maybe SSLSupportMethod,
    -- | If the distribution uses the CloudFront domain name such as
    -- @d111111abcdef8.cloudfront.net@, set this field to @true@.
    --
    -- If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
    -- set this field to @false@ and specify values for the following fields:
    --
    -- -   @ACMCertificateArn@ or @IAMCertificateId@ (specify a value for one,
    --     not both)
    --
    -- -   @MinimumProtocolVersion@
    --
    -- -   @SSLSupportMethod@
    cloudFrontDefaultCertificate :: Prelude.Maybe Prelude.Bool,
    -- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs)
    -- and the SSL\/TLS certificate is stored in
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)>,
    -- provide the ID of the IAM certificate.
    --
    -- If you specify an IAM certificate ID, you must also specify values for
    -- @MinimumProtocolVersion@ and @SSLSupportMethod@.
    iAMCertificateId :: Prelude.Maybe Prelude.Text,
    -- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs)
    -- and the SSL\/TLS certificate is stored in
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)>,
    -- provide the Amazon Resource Name (ARN) of the ACM certificate.
    -- CloudFront only supports ACM certificates in the US East (N. Virginia)
    -- Region (@us-east-1@).
    --
    -- If you specify an ACM certificate ARN, you must also specify values for
    -- @MinimumProtocolVersion@ and @SSLSupportMethod@.
    aCMCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | This field is deprecated. Use one of the following fields instead:
    --
    -- -   @ACMCertificateArn@
    --
    -- -   @IAMCertificateId@
    --
    -- -   @CloudFrontDefaultCertificate@
    certificateSource :: Prelude.Maybe CertificateSource,
    -- | This field is deprecated. Use one of the following fields instead:
    --
    -- -   @ACMCertificateArn@
    --
    -- -   @IAMCertificateId@
    --
    -- -   @CloudFrontDefaultCertificate@
    certificate :: Prelude.Maybe Prelude.Text,
    -- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
    -- specify the security policy that you want CloudFront to use for HTTPS
    -- connections with viewers. The security policy determines two settings:
    --
    -- -   The minimum SSL\/TLS protocol that CloudFront can use to communicate
    --     with viewers.
    --
    -- -   The ciphers that CloudFront can use to encrypt the content that it
    --     returns to viewers.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy>
    -- and
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported Protocols and Ciphers Between Viewers and CloudFront>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- On the CloudFront console, this setting is called __Security Policy__.
    --
    -- When you’re using SNI only (you set @SSLSupportMethod@ to @sni-only@),
    -- you must specify @TLSv1@ or higher.
    --
    -- If the distribution uses the CloudFront domain name such as
    -- @d111111abcdef8.cloudfront.net@ (you set @CloudFrontDefaultCertificate@
    -- to @true@), CloudFront automatically sets the security policy to @TLSv1@
    -- regardless of the value that you set here.
    minimumProtocolVersion :: Prelude.Maybe MinimumProtocolVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ViewerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sSLSupportMethod', 'viewerCertificate_sSLSupportMethod' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
-- specify which viewers the distribution accepts HTTPS connections from.
--
-- -   @sni-only@ – The distribution accepts HTTPS connections from only
--     viewers that support
--     <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)>.
--     This is recommended. Most browsers and clients support SNI.
--
-- -   @vip@ – The distribution accepts HTTPS connections from all viewers
--     including those that don’t support SNI. This is not recommended, and
--     results in additional monthly charges from CloudFront.
--
-- -   @static-ip@ - Do not specify this value unless your distribution has
--     been enabled for this feature by the CloudFront team. If you have a
--     use case that requires static IP addresses for a distribution,
--     contact CloudFront through the
--     <https://console.aws.amazon.com/support/home AWS Support Center>.
--
-- If the distribution uses the CloudFront domain name such as
-- @d111111abcdef8.cloudfront.net@, don’t set a value for this field.
--
-- 'cloudFrontDefaultCertificate', 'viewerCertificate_cloudFrontDefaultCertificate' - If the distribution uses the CloudFront domain name such as
-- @d111111abcdef8.cloudfront.net@, set this field to @true@.
--
-- If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
-- set this field to @false@ and specify values for the following fields:
--
-- -   @ACMCertificateArn@ or @IAMCertificateId@ (specify a value for one,
--     not both)
--
-- -   @MinimumProtocolVersion@
--
-- -   @SSLSupportMethod@
--
-- 'iAMCertificateId', 'viewerCertificate_iAMCertificateId' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs)
-- and the SSL\/TLS certificate is stored in
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)>,
-- provide the ID of the IAM certificate.
--
-- If you specify an IAM certificate ID, you must also specify values for
-- @MinimumProtocolVersion@ and @SSLSupportMethod@.
--
-- 'aCMCertificateArn', 'viewerCertificate_aCMCertificateArn' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs)
-- and the SSL\/TLS certificate is stored in
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)>,
-- provide the Amazon Resource Name (ARN) of the ACM certificate.
-- CloudFront only supports ACM certificates in the US East (N. Virginia)
-- Region (@us-east-1@).
--
-- If you specify an ACM certificate ARN, you must also specify values for
-- @MinimumProtocolVersion@ and @SSLSupportMethod@.
--
-- 'certificateSource', 'viewerCertificate_certificateSource' - This field is deprecated. Use one of the following fields instead:
--
-- -   @ACMCertificateArn@
--
-- -   @IAMCertificateId@
--
-- -   @CloudFrontDefaultCertificate@
--
-- 'certificate', 'viewerCertificate_certificate' - This field is deprecated. Use one of the following fields instead:
--
-- -   @ACMCertificateArn@
--
-- -   @IAMCertificateId@
--
-- -   @CloudFrontDefaultCertificate@
--
-- 'minimumProtocolVersion', 'viewerCertificate_minimumProtocolVersion' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
-- specify the security policy that you want CloudFront to use for HTTPS
-- connections with viewers. The security policy determines two settings:
--
-- -   The minimum SSL\/TLS protocol that CloudFront can use to communicate
--     with viewers.
--
-- -   The ciphers that CloudFront can use to encrypt the content that it
--     returns to viewers.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy>
-- and
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported Protocols and Ciphers Between Viewers and CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
--
-- On the CloudFront console, this setting is called __Security Policy__.
--
-- When you’re using SNI only (you set @SSLSupportMethod@ to @sni-only@),
-- you must specify @TLSv1@ or higher.
--
-- If the distribution uses the CloudFront domain name such as
-- @d111111abcdef8.cloudfront.net@ (you set @CloudFrontDefaultCertificate@
-- to @true@), CloudFront automatically sets the security policy to @TLSv1@
-- regardless of the value that you set here.
newViewerCertificate ::
  ViewerCertificate
newViewerCertificate =
  ViewerCertificate'
    { sSLSupportMethod =
        Prelude.Nothing,
      cloudFrontDefaultCertificate = Prelude.Nothing,
      iAMCertificateId = Prelude.Nothing,
      aCMCertificateArn = Prelude.Nothing,
      certificateSource = Prelude.Nothing,
      certificate = Prelude.Nothing,
      minimumProtocolVersion = Prelude.Nothing
    }

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
-- specify which viewers the distribution accepts HTTPS connections from.
--
-- -   @sni-only@ – The distribution accepts HTTPS connections from only
--     viewers that support
--     <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)>.
--     This is recommended. Most browsers and clients support SNI.
--
-- -   @vip@ – The distribution accepts HTTPS connections from all viewers
--     including those that don’t support SNI. This is not recommended, and
--     results in additional monthly charges from CloudFront.
--
-- -   @static-ip@ - Do not specify this value unless your distribution has
--     been enabled for this feature by the CloudFront team. If you have a
--     use case that requires static IP addresses for a distribution,
--     contact CloudFront through the
--     <https://console.aws.amazon.com/support/home AWS Support Center>.
--
-- If the distribution uses the CloudFront domain name such as
-- @d111111abcdef8.cloudfront.net@, don’t set a value for this field.
viewerCertificate_sSLSupportMethod :: Lens.Lens' ViewerCertificate (Prelude.Maybe SSLSupportMethod)
viewerCertificate_sSLSupportMethod = Lens.lens (\ViewerCertificate' {sSLSupportMethod} -> sSLSupportMethod) (\s@ViewerCertificate' {} a -> s {sSLSupportMethod = a} :: ViewerCertificate)

-- | If the distribution uses the CloudFront domain name such as
-- @d111111abcdef8.cloudfront.net@, set this field to @true@.
--
-- If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
-- set this field to @false@ and specify values for the following fields:
--
-- -   @ACMCertificateArn@ or @IAMCertificateId@ (specify a value for one,
--     not both)
--
-- -   @MinimumProtocolVersion@
--
-- -   @SSLSupportMethod@
viewerCertificate_cloudFrontDefaultCertificate :: Lens.Lens' ViewerCertificate (Prelude.Maybe Prelude.Bool)
viewerCertificate_cloudFrontDefaultCertificate = Lens.lens (\ViewerCertificate' {cloudFrontDefaultCertificate} -> cloudFrontDefaultCertificate) (\s@ViewerCertificate' {} a -> s {cloudFrontDefaultCertificate = a} :: ViewerCertificate)

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs)
-- and the SSL\/TLS certificate is stored in
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)>,
-- provide the ID of the IAM certificate.
--
-- If you specify an IAM certificate ID, you must also specify values for
-- @MinimumProtocolVersion@ and @SSLSupportMethod@.
viewerCertificate_iAMCertificateId :: Lens.Lens' ViewerCertificate (Prelude.Maybe Prelude.Text)
viewerCertificate_iAMCertificateId = Lens.lens (\ViewerCertificate' {iAMCertificateId} -> iAMCertificateId) (\s@ViewerCertificate' {} a -> s {iAMCertificateId = a} :: ViewerCertificate)

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs)
-- and the SSL\/TLS certificate is stored in
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)>,
-- provide the Amazon Resource Name (ARN) of the ACM certificate.
-- CloudFront only supports ACM certificates in the US East (N. Virginia)
-- Region (@us-east-1@).
--
-- If you specify an ACM certificate ARN, you must also specify values for
-- @MinimumProtocolVersion@ and @SSLSupportMethod@.
viewerCertificate_aCMCertificateArn :: Lens.Lens' ViewerCertificate (Prelude.Maybe Prelude.Text)
viewerCertificate_aCMCertificateArn = Lens.lens (\ViewerCertificate' {aCMCertificateArn} -> aCMCertificateArn) (\s@ViewerCertificate' {} a -> s {aCMCertificateArn = a} :: ViewerCertificate)

-- | This field is deprecated. Use one of the following fields instead:
--
-- -   @ACMCertificateArn@
--
-- -   @IAMCertificateId@
--
-- -   @CloudFrontDefaultCertificate@
viewerCertificate_certificateSource :: Lens.Lens' ViewerCertificate (Prelude.Maybe CertificateSource)
viewerCertificate_certificateSource = Lens.lens (\ViewerCertificate' {certificateSource} -> certificateSource) (\s@ViewerCertificate' {} a -> s {certificateSource = a} :: ViewerCertificate)

-- | This field is deprecated. Use one of the following fields instead:
--
-- -   @ACMCertificateArn@
--
-- -   @IAMCertificateId@
--
-- -   @CloudFrontDefaultCertificate@
viewerCertificate_certificate :: Lens.Lens' ViewerCertificate (Prelude.Maybe Prelude.Text)
viewerCertificate_certificate = Lens.lens (\ViewerCertificate' {certificate} -> certificate) (\s@ViewerCertificate' {} a -> s {certificate = a} :: ViewerCertificate)

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs),
-- specify the security policy that you want CloudFront to use for HTTPS
-- connections with viewers. The security policy determines two settings:
--
-- -   The minimum SSL\/TLS protocol that CloudFront can use to communicate
--     with viewers.
--
-- -   The ciphers that CloudFront can use to encrypt the content that it
--     returns to viewers.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy>
-- and
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported Protocols and Ciphers Between Viewers and CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
--
-- On the CloudFront console, this setting is called __Security Policy__.
--
-- When you’re using SNI only (you set @SSLSupportMethod@ to @sni-only@),
-- you must specify @TLSv1@ or higher.
--
-- If the distribution uses the CloudFront domain name such as
-- @d111111abcdef8.cloudfront.net@ (you set @CloudFrontDefaultCertificate@
-- to @true@), CloudFront automatically sets the security policy to @TLSv1@
-- regardless of the value that you set here.
viewerCertificate_minimumProtocolVersion :: Lens.Lens' ViewerCertificate (Prelude.Maybe MinimumProtocolVersion)
viewerCertificate_minimumProtocolVersion = Lens.lens (\ViewerCertificate' {minimumProtocolVersion} -> minimumProtocolVersion) (\s@ViewerCertificate' {} a -> s {minimumProtocolVersion = a} :: ViewerCertificate)

instance Prelude.FromXML ViewerCertificate where
  parseXML x =
    ViewerCertificate'
      Prelude.<$> (x Prelude..@? "SSLSupportMethod")
      Prelude.<*> (x Prelude..@? "CloudFrontDefaultCertificate")
      Prelude.<*> (x Prelude..@? "IAMCertificateId")
      Prelude.<*> (x Prelude..@? "ACMCertificateArn")
      Prelude.<*> (x Prelude..@? "CertificateSource")
      Prelude.<*> (x Prelude..@? "Certificate")
      Prelude.<*> (x Prelude..@? "MinimumProtocolVersion")

instance Prelude.Hashable ViewerCertificate

instance Prelude.NFData ViewerCertificate

instance Prelude.ToXML ViewerCertificate where
  toXML ViewerCertificate' {..} =
    Prelude.mconcat
      [ "SSLSupportMethod" Prelude.@= sSLSupportMethod,
        "CloudFrontDefaultCertificate"
          Prelude.@= cloudFrontDefaultCertificate,
        "IAMCertificateId" Prelude.@= iAMCertificateId,
        "ACMCertificateArn" Prelude.@= aCMCertificateArn,
        "CertificateSource" Prelude.@= certificateSource,
        "Certificate" Prelude.@= certificate,
        "MinimumProtocolVersion"
          Prelude.@= minimumProtocolVersion
      ]
