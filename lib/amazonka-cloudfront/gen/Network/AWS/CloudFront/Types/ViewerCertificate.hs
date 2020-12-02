{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ViewerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ViewerCertificate where

import Network.AWS.CloudFront.Types.CertificateSource
import Network.AWS.CloudFront.Types.MinimumProtocolVersion
import Network.AWS.CloudFront.Types.SSLSupportMethod
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
--
-- If the distribution doesn’t use @Aliases@ (also known as alternate domain names or CNAMEs)—that is, if the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ —set @CloudFrontDefaultCertificate@ to @true@ and leave all other fields empty.
--
-- If the distribution uses @Aliases@ (alternate domain names or CNAMEs), use the fields in this type to specify the following settings:
--
--     * Which viewers the distribution accepts HTTPS connections from: only viewers that support <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)> (recommended), or all viewers including those that don’t support SNI.
--
--     * To accept HTTPS connections from only viewers that support SNI, set @SSLSupportMethod@ to @sni-only@ . This is recommended. Most browsers and clients support SNI.
--
--     * To accept HTTPS connections from all viewers, including those that don’t support SNI, set @SSLSupportMethod@ to @vip@ . This is not recommended, and results in additional monthly charges from CloudFront.
--
--
--
--     * The minimum SSL/TLS protocol version that the distribution can use to communicate with viewers. To specify a minimum version, choose a value for @MinimumProtocolVersion@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy> in the /Amazon CloudFront Developer Guide/ .
--
--     * The location of the SSL/TLS certificate, <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> (recommended) or <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> . You specify the location by setting a value in one of the following fields (not both):
--
--     * @ACMCertificateArn@
--
--     * @IAMCertificateId@
--
--
--
--
--
-- All distributions support HTTPS connections from viewers. To require viewers to use HTTPS only, or to redirect them from HTTP to HTTPS, use @ViewerProtocolPolicy@ in the @CacheBehavior@ or @DefaultCacheBehavior@ . To specify how CloudFront should use SSL/TLS to communicate with your custom origin, use @CustomOriginConfig@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https.html Using HTTPS with CloudFront> and <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-alternate-domain-names.html Using Alternate Domain Names and HTTPS> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'viewerCertificate' smart constructor.
data ViewerCertificate = ViewerCertificate'
  { _vcSSLSupportMethod ::
      !(Maybe SSLSupportMethod),
    _vcACMCertificateARN :: !(Maybe Text),
    _vcCertificateSource :: !(Maybe CertificateSource),
    _vcMinimumProtocolVersion ::
      !(Maybe MinimumProtocolVersion),
    _vcCertificate :: !(Maybe Text),
    _vcIAMCertificateId :: !(Maybe Text),
    _vcCloudFrontDefaultCertificate :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ViewerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSSLSupportMethod' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify which viewers the distribution accepts HTTPS connections from.     * @sni-only@ – The distribution accepts HTTPS connections from only viewers that support <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)> . This is recommended. Most browsers and clients support SNI.     * @vip@ – The distribution accepts HTTPS connections from all viewers including those that don’t support SNI. This is not recommended, and results in additional monthly charges from CloudFront.     * @static-ip@ - Do not specify this value unless your distribution has been enabled for this feature by the CloudFront team. If you have a use case that requires static IP addresses for a distribution, contact CloudFront through the <https://console.aws.amazon.com/support/home AWS Support Center> . If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , don’t set a value for this field.
--
-- * 'vcACMCertificateARN' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> , provide the Amazon Resource Name (ARN) of the ACM certificate. CloudFront only supports ACM certificates in the US East (N. Virginia) Region (@us-east-1@ ). If you specify an ACM certificate ARN, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
--
-- * 'vcCertificateSource' - This field is deprecated. Use one of the following fields instead:     * @ACMCertificateArn@      * @IAMCertificateId@      * @CloudFrontDefaultCertificate@
--
-- * 'vcMinimumProtocolVersion' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify the security policy that you want CloudFront to use for HTTPS connections with viewers. The security policy determines two settings:     * The minimum SSL/TLS protocol that CloudFront can use to communicate with viewers.     * The ciphers that CloudFront can use to encrypt the content that it returns to viewers. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy> and <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported Protocols and Ciphers Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ . When you’re using SNI only (you set @SSLSupportMethod@ to @sni-only@ ), you must specify @TLSv1@ or higher.  If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ (you set @CloudFrontDefaultCertificate@ to @true@ ), CloudFront automatically sets the security policy to @TLSv1@ regardless of the value that you set here.
--
-- * 'vcCertificate' - This field is deprecated. Use one of the following fields instead:     * @ACMCertificateArn@      * @IAMCertificateId@      * @CloudFrontDefaultCertificate@
--
-- * 'vcIAMCertificateId' - If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> , provide the ID of the IAM certificate. If you specify an IAM certificate ID, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
--
-- * 'vcCloudFrontDefaultCertificate' - If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , set this field to @true@ . If the distribution uses @Aliases@ (alternate domain names or CNAMEs), set this field to @false@ and specify values for the following fields:     * @ACMCertificateArn@ or @IAMCertificateId@ (specify a value for one, not both)     * @MinimumProtocolVersion@      * @SSLSupportMethod@
viewerCertificate ::
  ViewerCertificate
viewerCertificate =
  ViewerCertificate'
    { _vcSSLSupportMethod = Nothing,
      _vcACMCertificateARN = Nothing,
      _vcCertificateSource = Nothing,
      _vcMinimumProtocolVersion = Nothing,
      _vcCertificate = Nothing,
      _vcIAMCertificateId = Nothing,
      _vcCloudFrontDefaultCertificate = Nothing
    }

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify which viewers the distribution accepts HTTPS connections from.     * @sni-only@ – The distribution accepts HTTPS connections from only viewers that support <https://en.wikipedia.org/wiki/Server_Name_Indication server name indication (SNI)> . This is recommended. Most browsers and clients support SNI.     * @vip@ – The distribution accepts HTTPS connections from all viewers including those that don’t support SNI. This is not recommended, and results in additional monthly charges from CloudFront.     * @static-ip@ - Do not specify this value unless your distribution has been enabled for this feature by the CloudFront team. If you have a use case that requires static IP addresses for a distribution, contact CloudFront through the <https://console.aws.amazon.com/support/home AWS Support Center> . If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , don’t set a value for this field.
vcSSLSupportMethod :: Lens' ViewerCertificate (Maybe SSLSupportMethod)
vcSSLSupportMethod = lens _vcSSLSupportMethod (\s a -> s {_vcSSLSupportMethod = a})

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/acm/latest/userguide/acm-overview.html AWS Certificate Manager (ACM)> , provide the Amazon Resource Name (ARN) of the ACM certificate. CloudFront only supports ACM certificates in the US East (N. Virginia) Region (@us-east-1@ ). If you specify an ACM certificate ARN, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
vcACMCertificateARN :: Lens' ViewerCertificate (Maybe Text)
vcACMCertificateARN = lens _vcACMCertificateARN (\s a -> s {_vcACMCertificateARN = a})

-- | This field is deprecated. Use one of the following fields instead:     * @ACMCertificateArn@      * @IAMCertificateId@      * @CloudFrontDefaultCertificate@
vcCertificateSource :: Lens' ViewerCertificate (Maybe CertificateSource)
vcCertificateSource = lens _vcCertificateSource (\s a -> s {_vcCertificateSource = a})

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs), specify the security policy that you want CloudFront to use for HTTPS connections with viewers. The security policy determines two settings:     * The minimum SSL/TLS protocol that CloudFront can use to communicate with viewers.     * The ciphers that CloudFront can use to encrypt the content that it returns to viewers. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValues-security-policy Security Policy> and <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported Protocols and Ciphers Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ . When you’re using SNI only (you set @SSLSupportMethod@ to @sni-only@ ), you must specify @TLSv1@ or higher.  If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ (you set @CloudFrontDefaultCertificate@ to @true@ ), CloudFront automatically sets the security policy to @TLSv1@ regardless of the value that you set here.
vcMinimumProtocolVersion :: Lens' ViewerCertificate (Maybe MinimumProtocolVersion)
vcMinimumProtocolVersion = lens _vcMinimumProtocolVersion (\s a -> s {_vcMinimumProtocolVersion = a})

-- | This field is deprecated. Use one of the following fields instead:     * @ACMCertificateArn@      * @IAMCertificateId@      * @CloudFrontDefaultCertificate@
vcCertificate :: Lens' ViewerCertificate (Maybe Text)
vcCertificate = lens _vcCertificate (\s a -> s {_vcCertificate = a})

-- | If the distribution uses @Aliases@ (alternate domain names or CNAMEs) and the SSL/TLS certificate is stored in <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html AWS Identity and Access Management (AWS IAM)> , provide the ID of the IAM certificate. If you specify an IAM certificate ID, you must also specify values for @MinimumProtocolVerison@ and @SSLSupportMethod@ .
vcIAMCertificateId :: Lens' ViewerCertificate (Maybe Text)
vcIAMCertificateId = lens _vcIAMCertificateId (\s a -> s {_vcIAMCertificateId = a})

-- | If the distribution uses the CloudFront domain name such as @d111111abcdef8.cloudfront.net@ , set this field to @true@ . If the distribution uses @Aliases@ (alternate domain names or CNAMEs), set this field to @false@ and specify values for the following fields:     * @ACMCertificateArn@ or @IAMCertificateId@ (specify a value for one, not both)     * @MinimumProtocolVersion@      * @SSLSupportMethod@
vcCloudFrontDefaultCertificate :: Lens' ViewerCertificate (Maybe Bool)
vcCloudFrontDefaultCertificate = lens _vcCloudFrontDefaultCertificate (\s a -> s {_vcCloudFrontDefaultCertificate = a})

instance FromXML ViewerCertificate where
  parseXML x =
    ViewerCertificate'
      <$> (x .@? "SSLSupportMethod")
      <*> (x .@? "ACMCertificateArn")
      <*> (x .@? "CertificateSource")
      <*> (x .@? "MinimumProtocolVersion")
      <*> (x .@? "Certificate")
      <*> (x .@? "IAMCertificateId")
      <*> (x .@? "CloudFrontDefaultCertificate")

instance Hashable ViewerCertificate

instance NFData ViewerCertificate

instance ToXML ViewerCertificate where
  toXML ViewerCertificate' {..} =
    mconcat
      [ "SSLSupportMethod" @= _vcSSLSupportMethod,
        "ACMCertificateArn" @= _vcACMCertificateARN,
        "CertificateSource" @= _vcCertificateSource,
        "MinimumProtocolVersion" @= _vcMinimumProtocolVersion,
        "Certificate" @= _vcCertificate,
        "IAMCertificateId" @= _vcIAMCertificateId,
        "CloudFrontDefaultCertificate" @= _vcCloudFrontDefaultCertificate
      ]
