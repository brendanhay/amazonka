-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ViewerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ViewerCertificate
  ( ViewerCertificate (..),

    -- * Smart constructor
    mkViewerCertificate,

    -- * Lenses
    vcSSLSupportMethod,
    vcACMCertificateARN,
    vcCertificateSource,
    vcMinimumProtocolVersion,
    vcCertificate,
    vcIAMCertificateId,
    vcCloudFrontDefaultCertificate,
  )
where

import Network.AWS.CloudFront.Types.CertificateSource
import Network.AWS.CloudFront.Types.MinimumProtocolVersion
import Network.AWS.CloudFront.Types.SSLSupportMethod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { sslSupportMethod ::
      Lude.Maybe SSLSupportMethod,
    aCMCertificateARN :: Lude.Maybe Lude.Text,
    certificateSource :: Lude.Maybe CertificateSource,
    minimumProtocolVersion ::
      Lude.Maybe MinimumProtocolVersion,
    certificate :: Lude.Maybe Lude.Text,
    iamCertificateId :: Lude.Maybe Lude.Text,
    cloudFrontDefaultCertificate :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ViewerCertificate' with the minimum fields required to make a request.
--
-- * 'aCMCertificateARN' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> , provide the Amazon Resource Name (ARN) of the ACM certificate. CloudFront only supports ACM certificates in the US East (N. Virginia) Region (@us-east-1@ ).
--
-- If you specify an ACM certificate ARN, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
-- * 'certificate' - This field is deprecated. Use one of the following fields instead:
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
-- * 'certificateSource' - This field is deprecated. Use one of the following fields instead:
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
-- * 'cloudFrontDefaultCertificate' - If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , set this field to @true@ .
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
-- * 'iamCertificateId' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> , provide the ID of the IAM certificate.
--
-- If you specify an IAM certificate ID, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
-- * 'minimumProtocolVersion' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify the security policy that you want CloudFront to use for HTTPS connections with viewers. The security policy determines two settings:
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
-- * 'sslSupportMethod' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify which viewers the distribution accepts HTTPS connections from.
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
mkViewerCertificate ::
  ViewerCertificate
mkViewerCertificate =
  ViewerCertificate'
    { sslSupportMethod = Lude.Nothing,
      aCMCertificateARN = Lude.Nothing,
      certificateSource = Lude.Nothing,
      minimumProtocolVersion = Lude.Nothing,
      certificate = Lude.Nothing,
      iamCertificateId = Lude.Nothing,
      cloudFrontDefaultCertificate = Lude.Nothing
    }

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
-- /Note:/ Consider using 'sslSupportMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSSLSupportMethod :: Lens.Lens' ViewerCertificate (Lude.Maybe SSLSupportMethod)
vcSSLSupportMethod = Lens.lens (sslSupportMethod :: ViewerCertificate -> Lude.Maybe SSLSupportMethod) (\s a -> s {sslSupportMethod = a} :: ViewerCertificate)
{-# DEPRECATED vcSSLSupportMethod "Use generic-lens or generic-optics with 'sslSupportMethod' instead." #-}

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> , provide the Amazon Resource Name (ARN) of the ACM certificate. CloudFront only supports ACM certificates in the US East (N. Virginia) Region (@us-east-1@ ).
--
-- If you specify an ACM certificate ARN, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
--
-- /Note:/ Consider using 'aCMCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcACMCertificateARN :: Lens.Lens' ViewerCertificate (Lude.Maybe Lude.Text)
vcACMCertificateARN = Lens.lens (aCMCertificateARN :: ViewerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {aCMCertificateARN = a} :: ViewerCertificate)
{-# DEPRECATED vcACMCertificateARN "Use generic-lens or generic-optics with 'aCMCertificateARN' instead." #-}

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
vcCertificateSource :: Lens.Lens' ViewerCertificate (Lude.Maybe CertificateSource)
vcCertificateSource = Lens.lens (certificateSource :: ViewerCertificate -> Lude.Maybe CertificateSource) (\s a -> s {certificateSource = a} :: ViewerCertificate)
{-# DEPRECATED vcCertificateSource "Use generic-lens or generic-optics with 'certificateSource' instead." #-}

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
vcMinimumProtocolVersion :: Lens.Lens' ViewerCertificate (Lude.Maybe MinimumProtocolVersion)
vcMinimumProtocolVersion = Lens.lens (minimumProtocolVersion :: ViewerCertificate -> Lude.Maybe MinimumProtocolVersion) (\s a -> s {minimumProtocolVersion = a} :: ViewerCertificate)
{-# DEPRECATED vcMinimumProtocolVersion "Use generic-lens or generic-optics with 'minimumProtocolVersion' instead." #-}

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
vcCertificate :: Lens.Lens' ViewerCertificate (Lude.Maybe Lude.Text)
vcCertificate = Lens.lens (certificate :: ViewerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: ViewerCertificate)
{-# DEPRECATED vcCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> , provide the ID of the IAM certificate.
--
-- If you specify an IAM certificate ID, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
--
-- /Note:/ Consider using 'iamCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcIAMCertificateId :: Lens.Lens' ViewerCertificate (Lude.Maybe Lude.Text)
vcIAMCertificateId = Lens.lens (iamCertificateId :: ViewerCertificate -> Lude.Maybe Lude.Text) (\s a -> s {iamCertificateId = a} :: ViewerCertificate)
{-# DEPRECATED vcIAMCertificateId "Use generic-lens or generic-optics with 'iamCertificateId' instead." #-}

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
vcCloudFrontDefaultCertificate :: Lens.Lens' ViewerCertificate (Lude.Maybe Lude.Bool)
vcCloudFrontDefaultCertificate = Lens.lens (cloudFrontDefaultCertificate :: ViewerCertificate -> Lude.Maybe Lude.Bool) (\s a -> s {cloudFrontDefaultCertificate = a} :: ViewerCertificate)
{-# DEPRECATED vcCloudFrontDefaultCertificate "Use generic-lens or generic-optics with 'cloudFrontDefaultCertificate' instead." #-}

instance Lude.FromXML ViewerCertificate where
  parseXML x =
    ViewerCertificate'
      Lude.<$> (x Lude..@? "SSLSupportMethod")
      Lude.<*> (x Lude..@? "ACMCertificateArn")
      Lude.<*> (x Lude..@? "CertificateSource")
      Lude.<*> (x Lude..@? "MinimumProtocolVersion")
      Lude.<*> (x Lude..@? "Certificate")
      Lude.<*> (x Lude..@? "IAMCertificateId")
      Lude.<*> (x Lude..@? "CloudFrontDefaultCertificate")

instance Lude.ToXML ViewerCertificate where
  toXML ViewerCertificate' {..} =
    Lude.mconcat
      [ "SSLSupportMethod" Lude.@= sslSupportMethod,
        "ACMCertificateArn" Lude.@= aCMCertificateARN,
        "CertificateSource" Lude.@= certificateSource,
        "MinimumProtocolVersion" Lude.@= minimumProtocolVersion,
        "Certificate" Lude.@= certificate,
        "IAMCertificateId" Lude.@= iamCertificateId,
        "CloudFrontDefaultCertificate"
          Lude.@= cloudFrontDefaultCertificate
      ]
