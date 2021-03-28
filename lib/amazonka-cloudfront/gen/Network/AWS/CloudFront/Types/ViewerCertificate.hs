{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ViewerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ViewerCertificate
  ( ViewerCertificate (..)
  -- * Smart constructor
  , mkViewerCertificate
  -- * Lenses
  , vcACMCertificateArn
  , vcCertificate
  , vcCertificateSource
  , vcCloudFrontDefaultCertificate
  , vcIAMCertificateId
  , vcMinimumProtocolVersion
  , vcSSLSupportMethod
  ) where

import qualified Network.AWS.CloudFront.Types.CertificateSource as Types
import qualified Network.AWS.CloudFront.Types.MinimumProtocolVersion as Types
import qualified Network.AWS.CloudFront.Types.SSLSupportMethod as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- If the distribution doesn’t use @Aliases@ (also known as alternate domain names or CNAMEs)—that is, if the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ —set @CloudFrontDefaultCertificate@ to @true@ and leave all other fields empty.
-- If the distribution uses @Aliases@ (alternate domain names or CNAMEs), use the fields in this type to specify the following settings:
--
--     * Which viewers the distribution accepts HTTPS connections from: only viewers that support <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)> (recommended), or all viewers including those that don’t support SNI.
--
--     * To accept HTTPS connections from only viewers that support SNI, set @SSLSupportMethod@ to @sni-only@ . This is recommended. Most browsers and clients support SNI. 
--
--
--     * To accept HTTPS connections from all viewers, including those that don’t support SNI, set @SSLSupportMethod@ to @vip@ . This is not recommended, and results in additional monthly charges from CloudFront. 
--
--
--
--
--     * The minimum SSL/TLS protocol version that the distribution can use to communicate with viewers. To specify a minimum version, choose a value for @MinimumProtocolVersion@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy> in the /Amazon CloudFront Developer Guide/ .
--
--
--     * The location of the SSL/TLS certificate, <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> (recommended) or <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> . You specify the location by setting a value in one of the following fields (not both):
--
--     * @ACMCertificateArn@ 
--
--
--     * @IAMCertificateId@ 
--
--
--
--
-- All distributions support HTTPS connections from viewers. To require viewers to use HTTPS only, or to redirect them from HTTP to HTTPS, use @ViewerProtocolPolicy@ in the @CacheBehavior@ or @DefaultCacheBehavior@ . To specify how CloudFront should use SSL/TLS to communicate with your custom origin, use @CustomOriginConfig@ .
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https.html Using HTTPS with CloudFront> and <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-alternate-domain-names.html Using Alternate Domain Names and HTTPS> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkViewerCertificate' smart constructor.
data ViewerCertificate = ViewerCertificate'
  { aCMCertificateArn :: Core.Maybe Core.Text
    -- ^ If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> , provide the Amazon Resource Name (ARN) of the ACM certificate. CloudFront only supports ACM certificates in the US East (N. Virginia) Region (@us-east-1@ ).
--
-- If you specify an ACM certificate ARN, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ . 
  , certificate :: Core.Maybe Core.Text
    -- ^ This field is deprecated. Use one of the following fields instead:
--
--
--     * @ACMCertificateArn@ 
--
--
--     * @IAMCertificateId@ 
--
--
--     * @CloudFrontDefaultCertificate@ 
--
--
  , certificateSource :: Core.Maybe Types.CertificateSource
    -- ^ This field is deprecated. Use one of the following fields instead:
--
--
--     * @ACMCertificateArn@ 
--
--
--     * @IAMCertificateId@ 
--
--
--     * @CloudFrontDefaultCertificate@ 
--
--
  , cloudFrontDefaultCertificate :: Core.Maybe Core.Bool
    -- ^ If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , set this field to @true@ .
--
-- If the distribution uses @Aliases@ (alternate domain names or CNAMEs), set this field to @false@ and specify values for the following fields:
--
--     * @ACMCertificateArn@ or @IAMCertificateId@ (specify a value for one, not both)
--
--
--     * @MinimumProtocolVersion@ 
--
--
--     * @SSLSupportMethod@ 
--
--
  , iAMCertificateId :: Core.Maybe Core.Text
    -- ^ If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> , provide the ID of the IAM certificate.
--
-- If you specify an IAM certificate ID, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ . 
  , minimumProtocolVersion :: Core.Maybe Types.MinimumProtocolVersion
    -- ^ If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify the security policy that you want CloudFront to use for HTTPS connections with viewers. The security policy determines two settings:
--
--
--     * The minimum SSL/TLS protocol that CloudFront can use to communicate with viewers.
--
--
--     * The ciphers that CloudFront can use to encrypt the content that it returns to viewers.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy> and <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported Protocols and Ciphers Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
-- When you’re using SNI only (you set @SSLSupportMethod@ to @sni-only@ ), you must specify @TLSv1@ or higher. 
-- If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ (you set @CloudFrontDefaultCertificate@ to @true@ ), CloudFront automatically sets the security policy to @TLSv1@ regardless of the value that you set here.
  , sSLSupportMethod :: Core.Maybe Types.SSLSupportMethod
    -- ^ If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify which viewers the distribution accepts HTTPS connections from.
--
--
--     * @sni-only@ – The distribution accepts HTTPS connections from only viewers that support <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)> . This is recommended. Most browsers and clients support SNI.
--
--
--     * @vip@ – The distribution accepts HTTPS connections from all viewers including those that don’t support SNI. This is not recommended, and results in additional monthly charges from CloudFront.
--
--
--     * @static-ip@ - Do not specify this value unless your distribution has been enabled for this feature by the CloudFront team. If you have a use case that requires static IP addresses for a distribution, contact CloudFront through the <https://console.aws.amazon.com/support/home AWS Support Center> .
--
--
-- If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , don’t set a value for this field.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ViewerCertificate' value with any optional fields omitted.
mkViewerCertificate
    :: ViewerCertificate
mkViewerCertificate
  = ViewerCertificate'{aCMCertificateArn = Core.Nothing,
                       certificate = Core.Nothing, certificateSource = Core.Nothing,
                       cloudFrontDefaultCertificate = Core.Nothing,
                       iAMCertificateId = Core.Nothing,
                       minimumProtocolVersion = Core.Nothing,
                       sSLSupportMethod = Core.Nothing}

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> , provide the Amazon Resource Name (ARN) of the ACM certificate. CloudFront only supports ACM certificates in the US East (N. Virginia) Region (@us-east-1@ ).
--
-- If you specify an ACM certificate ARN, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ . 
--
-- /Note:/ Consider using 'aCMCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcACMCertificateArn :: Lens.Lens' ViewerCertificate (Core.Maybe Core.Text)
vcACMCertificateArn = Lens.field @"aCMCertificateArn"
{-# INLINEABLE vcACMCertificateArn #-}
{-# DEPRECATED aCMCertificateArn "Use generic-lens or generic-optics with 'aCMCertificateArn' instead"  #-}

-- | This field is deprecated. Use one of the following fields instead:
--
--
--     * @ACMCertificateArn@ 
--
--
--     * @IAMCertificateId@ 
--
--
--     * @CloudFrontDefaultCertificate@ 
--
--
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCertificate :: Lens.Lens' ViewerCertificate (Core.Maybe Core.Text)
vcCertificate = Lens.field @"certificate"
{-# INLINEABLE vcCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | This field is deprecated. Use one of the following fields instead:
--
--
--     * @ACMCertificateArn@ 
--
--
--     * @IAMCertificateId@ 
--
--
--     * @CloudFrontDefaultCertificate@ 
--
--
--
-- /Note:/ Consider using 'certificateSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCertificateSource :: Lens.Lens' ViewerCertificate (Core.Maybe Types.CertificateSource)
vcCertificateSource = Lens.field @"certificateSource"
{-# INLINEABLE vcCertificateSource #-}
{-# DEPRECATED certificateSource "Use generic-lens or generic-optics with 'certificateSource' instead"  #-}

-- | If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , set this field to @true@ .
--
-- If the distribution uses @Aliases@ (alternate domain names or CNAMEs), set this field to @false@ and specify values for the following fields:
--
--     * @ACMCertificateArn@ or @IAMCertificateId@ (specify a value for one, not both)
--
--
--     * @MinimumProtocolVersion@ 
--
--
--     * @SSLSupportMethod@ 
--
--
--
-- /Note:/ Consider using 'cloudFrontDefaultCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCloudFrontDefaultCertificate :: Lens.Lens' ViewerCertificate (Core.Maybe Core.Bool)
vcCloudFrontDefaultCertificate = Lens.field @"cloudFrontDefaultCertificate"
{-# INLINEABLE vcCloudFrontDefaultCertificate #-}
{-# DEPRECATED cloudFrontDefaultCertificate "Use generic-lens or generic-optics with 'cloudFrontDefaultCertificate' instead"  #-}

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> , provide the ID of the IAM certificate.
--
-- If you specify an IAM certificate ID, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ . 
--
-- /Note:/ Consider using 'iAMCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcIAMCertificateId :: Lens.Lens' ViewerCertificate (Core.Maybe Core.Text)
vcIAMCertificateId = Lens.field @"iAMCertificateId"
{-# INLINEABLE vcIAMCertificateId #-}
{-# DEPRECATED iAMCertificateId "Use generic-lens or generic-optics with 'iAMCertificateId' instead"  #-}

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify the security policy that you want CloudFront to use for HTTPS connections with viewers. The security policy determines two settings:
--
--
--     * The minimum SSL/TLS protocol that CloudFront can use to communicate with viewers.
--
--
--     * The ciphers that CloudFront can use to encrypt the content that it returns to viewers.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy> and <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported Protocols and Ciphers Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
-- When you’re using SNI only (you set @SSLSupportMethod@ to @sni-only@ ), you must specify @TLSv1@ or higher. 
-- If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ (you set @CloudFrontDefaultCertificate@ to @true@ ), CloudFront automatically sets the security policy to @TLSv1@ regardless of the value that you set here.
--
-- /Note:/ Consider using 'minimumProtocolVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcMinimumProtocolVersion :: Lens.Lens' ViewerCertificate (Core.Maybe Types.MinimumProtocolVersion)
vcMinimumProtocolVersion = Lens.field @"minimumProtocolVersion"
{-# INLINEABLE vcMinimumProtocolVersion #-}
{-# DEPRECATED minimumProtocolVersion "Use generic-lens or generic-optics with 'minimumProtocolVersion' instead"  #-}

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify which viewers the distribution accepts HTTPS connections from.
--
--
--     * @sni-only@ – The distribution accepts HTTPS connections from only viewers that support <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)> . This is recommended. Most browsers and clients support SNI.
--
--
--     * @vip@ – The distribution accepts HTTPS connections from all viewers including those that don’t support SNI. This is not recommended, and results in additional monthly charges from CloudFront.
--
--
--     * @static-ip@ - Do not specify this value unless your distribution has been enabled for this feature by the CloudFront team. If you have a use case that requires static IP addresses for a distribution, contact CloudFront through the <https://console.aws.amazon.com/support/home AWS Support Center> .
--
--
-- If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , don’t set a value for this field.
--
-- /Note:/ Consider using 'sSLSupportMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSSLSupportMethod :: Lens.Lens' ViewerCertificate (Core.Maybe Types.SSLSupportMethod)
vcSSLSupportMethod = Lens.field @"sSLSupportMethod"
{-# INLINEABLE vcSSLSupportMethod #-}
{-# DEPRECATED sSLSupportMethod "Use generic-lens or generic-optics with 'sSLSupportMethod' instead"  #-}

instance Core.ToXML ViewerCertificate where
        toXML ViewerCertificate{..}
          = Core.maybe Core.mempty (Core.toXMLElement "ACMCertificateArn")
              aCMCertificateArn
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Certificate")
                certificate
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "CertificateSource")
                certificateSource
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "CloudFrontDefaultCertificate")
                cloudFrontDefaultCertificate
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "IAMCertificateId")
                iAMCertificateId
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "MinimumProtocolVersion")
                minimumProtocolVersion
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "SSLSupportMethod")
                sSLSupportMethod

instance Core.FromXML ViewerCertificate where
        parseXML x
          = ViewerCertificate' Core.<$>
              (x Core..@? "ACMCertificateArn") Core.<*> x Core..@? "Certificate"
                Core.<*> x Core..@? "CertificateSource"
                Core.<*> x Core..@? "CloudFrontDefaultCertificate"
                Core.<*> x Core..@? "IAMCertificateId"
                Core.<*> x Core..@? "MinimumProtocolVersion"
                Core.<*> x Core..@? "SSLSupportMethod"
