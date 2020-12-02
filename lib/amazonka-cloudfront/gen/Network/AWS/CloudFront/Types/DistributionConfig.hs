{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionConfig where

import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.CacheBehaviors
import Network.AWS.CloudFront.Types.CustomErrorResponses
import Network.AWS.CloudFront.Types.DefaultCacheBehavior
import Network.AWS.CloudFront.Types.HTTPVersion
import Network.AWS.CloudFront.Types.LoggingConfig
import Network.AWS.CloudFront.Types.OriginGroups
import Network.AWS.CloudFront.Types.Origins
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.Restrictions
import Network.AWS.CloudFront.Types.ViewerCertificate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A distribution configuration.
--
--
--
-- /See:/ 'distributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
  { _dcHTTPVersion ::
      !(Maybe HTTPVersion),
    _dcOriginGroups :: !(Maybe OriginGroups),
    _dcAliases :: !(Maybe Aliases),
    _dcDefaultRootObject :: !(Maybe Text),
    _dcPriceClass :: !(Maybe PriceClass),
    _dcCustomErrorResponses ::
      !(Maybe CustomErrorResponses),
    _dcWebACLId :: !(Maybe Text),
    _dcViewerCertificate :: !(Maybe ViewerCertificate),
    _dcRestrictions :: !(Maybe Restrictions),
    _dcLogging :: !(Maybe LoggingConfig),
    _dcCacheBehaviors :: !(Maybe CacheBehaviors),
    _dcIsIPV6Enabled :: !(Maybe Bool),
    _dcCallerReference :: !Text,
    _dcOrigins :: !Origins,
    _dcDefaultCacheBehavior :: !DefaultCacheBehavior,
    _dcComment :: !(Sensitive Text),
    _dcEnabled :: !Bool
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DistributionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcHTTPVersion' - (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version. For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI). In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
--
-- * 'dcOriginGroups' - A complex type that contains information about origin groups for this distribution.
--
-- * 'dcAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- * 'dcDefaultRootObject' - The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution. Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name. If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element. To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element. To replace the default root object, update the distribution configuration and specify the new object. For more information about the default root object, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcPriceClass' - The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations. If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance. For more information about price classes, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes (such as Price Class 100) map to CloudFront regions, see <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
--
-- * 'dcCustomErrorResponses' - A complex type that controls the following:     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range. For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcWebACLId' - A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. To specify a web ACL created using the latest version of AWS WAF, use the ACL ARN, for example @arn:aws:wafv2:us-east-1:123456789012:global/webacl/ExampleWebACL/473e64fd-f30b-4765-81a0-62ad96dd167a@ . To specify a web ACL created using AWS WAF Classic, use the ACL ID, for example @473e64fd-f30b-4765-81a0-62ad96dd167a@ . AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
--
-- * 'dcViewerCertificate' - A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- * 'dcRestrictions' - A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- * 'dcLogging' - A complex type that controls whether access logs are written for the distribution. For more information about logging, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcCacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- * 'dcIsIPV6Enabled' - If you want CloudFront to respond to IPv6 DNS requests with an IPv6 address for your distribution, specify @true@ . If you specify @false@ , CloudFront responds to IPv6 DNS requests with the DNS response code @NOERROR@ and with no IP addresses. This allows viewers to submit a second request, for an IPv4 address for your distribution.  In general, you should enable IPv6 if you have users on IPv6 networks who want to access your content. However, if you're using signed URLs or signed cookies to restrict access to your content, and if you're using a custom policy that includes the @IpAddress@ parameter to restrict the IP addresses that can access your content, don't enable IPv6. If you want to restrict access to some content by IP address and not restrict access to other content (or restrict access but not by IP address), you can create two distributions. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy> in the /Amazon CloudFront Developer Guide/ . If you're using an Amazon Route 53 alias resource record set to route traffic to your CloudFront distribution, you need to create a second alias resource record set when both of the following are true:     * You enable IPv6 for the distribution     * You're using alternate domain names in the URLs for your objects For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name> in the /Amazon Route 53 Developer Guide/ . If you created a CNAME resource record set, either with Amazon Route 53 or with another DNS service, you don't need to make any changes. A CNAME record will route traffic to your distribution regardless of the IP address format of the viewer request.
--
-- * 'dcCallerReference' - A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution. If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- * 'dcOrigins' - A complex type that contains information about origins for this distribution.
--
-- * 'dcDefaultCacheBehavior' - A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- * 'dcComment' - Any comments you want to include about the distribution. If you don't want to specify a comment, include an empty @Comment@ element. To delete an existing comment, update the distribution configuration and include an empty @Comment@ element. To add or change a comment, update the distribution configuration and specify the new comment.
--
-- * 'dcEnabled' - From this field, you can enable or disable the selected distribution.
distributionConfig ::
  -- | 'dcCallerReference'
  Text ->
  -- | 'dcOrigins'
  Origins ->
  -- | 'dcDefaultCacheBehavior'
  DefaultCacheBehavior ->
  -- | 'dcComment'
  Text ->
  -- | 'dcEnabled'
  Bool ->
  DistributionConfig
distributionConfig
  pCallerReference_
  pOrigins_
  pDefaultCacheBehavior_
  pComment_
  pEnabled_ =
    DistributionConfig'
      { _dcHTTPVersion = Nothing,
        _dcOriginGroups = Nothing,
        _dcAliases = Nothing,
        _dcDefaultRootObject = Nothing,
        _dcPriceClass = Nothing,
        _dcCustomErrorResponses = Nothing,
        _dcWebACLId = Nothing,
        _dcViewerCertificate = Nothing,
        _dcRestrictions = Nothing,
        _dcLogging = Nothing,
        _dcCacheBehaviors = Nothing,
        _dcIsIPV6Enabled = Nothing,
        _dcCallerReference = pCallerReference_,
        _dcOrigins = pOrigins_,
        _dcDefaultCacheBehavior = pDefaultCacheBehavior_,
        _dcComment = _Sensitive # pComment_,
        _dcEnabled = pEnabled_
      }

-- | (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version. For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI). In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
dcHTTPVersion :: Lens' DistributionConfig (Maybe HTTPVersion)
dcHTTPVersion = lens _dcHTTPVersion (\s a -> s {_dcHTTPVersion = a})

-- | A complex type that contains information about origin groups for this distribution.
dcOriginGroups :: Lens' DistributionConfig (Maybe OriginGroups)
dcOriginGroups = lens _dcOriginGroups (\s a -> s {_dcOriginGroups = a})

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
dcAliases :: Lens' DistributionConfig (Maybe Aliases)
dcAliases = lens _dcAliases (\s a -> s {_dcAliases = a})

-- | The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution. Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name. If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element. To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element. To replace the default root object, update the distribution configuration and specify the new object. For more information about the default root object, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
dcDefaultRootObject :: Lens' DistributionConfig (Maybe Text)
dcDefaultRootObject = lens _dcDefaultRootObject (\s a -> s {_dcDefaultRootObject = a})

-- | The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations. If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance. For more information about price classes, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes (such as Price Class 100) map to CloudFront regions, see <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
dcPriceClass :: Lens' DistributionConfig (Maybe PriceClass)
dcPriceClass = lens _dcPriceClass (\s a -> s {_dcPriceClass = a})

-- | A complex type that controls the following:     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range. For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
dcCustomErrorResponses :: Lens' DistributionConfig (Maybe CustomErrorResponses)
dcCustomErrorResponses = lens _dcCustomErrorResponses (\s a -> s {_dcCustomErrorResponses = a})

-- | A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. To specify a web ACL created using the latest version of AWS WAF, use the ACL ARN, for example @arn:aws:wafv2:us-east-1:123456789012:global/webacl/ExampleWebACL/473e64fd-f30b-4765-81a0-62ad96dd167a@ . To specify a web ACL created using AWS WAF Classic, use the ACL ID, for example @473e64fd-f30b-4765-81a0-62ad96dd167a@ . AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
dcWebACLId :: Lens' DistributionConfig (Maybe Text)
dcWebACLId = lens _dcWebACLId (\s a -> s {_dcWebACLId = a})

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
dcViewerCertificate :: Lens' DistributionConfig (Maybe ViewerCertificate)
dcViewerCertificate = lens _dcViewerCertificate (\s a -> s {_dcViewerCertificate = a})

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
dcRestrictions :: Lens' DistributionConfig (Maybe Restrictions)
dcRestrictions = lens _dcRestrictions (\s a -> s {_dcRestrictions = a})

-- | A complex type that controls whether access logs are written for the distribution. For more information about logging, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
dcLogging :: Lens' DistributionConfig (Maybe LoggingConfig)
dcLogging = lens _dcLogging (\s a -> s {_dcLogging = a})

-- | A complex type that contains zero or more @CacheBehavior@ elements.
dcCacheBehaviors :: Lens' DistributionConfig (Maybe CacheBehaviors)
dcCacheBehaviors = lens _dcCacheBehaviors (\s a -> s {_dcCacheBehaviors = a})

-- | If you want CloudFront to respond to IPv6 DNS requests with an IPv6 address for your distribution, specify @true@ . If you specify @false@ , CloudFront responds to IPv6 DNS requests with the DNS response code @NOERROR@ and with no IP addresses. This allows viewers to submit a second request, for an IPv4 address for your distribution.  In general, you should enable IPv6 if you have users on IPv6 networks who want to access your content. However, if you're using signed URLs or signed cookies to restrict access to your content, and if you're using a custom policy that includes the @IpAddress@ parameter to restrict the IP addresses that can access your content, don't enable IPv6. If you want to restrict access to some content by IP address and not restrict access to other content (or restrict access but not by IP address), you can create two distributions. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy> in the /Amazon CloudFront Developer Guide/ . If you're using an Amazon Route 53 alias resource record set to route traffic to your CloudFront distribution, you need to create a second alias resource record set when both of the following are true:     * You enable IPv6 for the distribution     * You're using alternate domain names in the URLs for your objects For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name> in the /Amazon Route 53 Developer Guide/ . If you created a CNAME resource record set, either with Amazon Route 53 or with another DNS service, you don't need to make any changes. A CNAME record will route traffic to your distribution regardless of the IP address format of the viewer request.
dcIsIPV6Enabled :: Lens' DistributionConfig (Maybe Bool)
dcIsIPV6Enabled = lens _dcIsIPV6Enabled (\s a -> s {_dcIsIPV6Enabled = a})

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution. If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
dcCallerReference :: Lens' DistributionConfig Text
dcCallerReference = lens _dcCallerReference (\s a -> s {_dcCallerReference = a})

-- | A complex type that contains information about origins for this distribution.
dcOrigins :: Lens' DistributionConfig Origins
dcOrigins = lens _dcOrigins (\s a -> s {_dcOrigins = a})

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
dcDefaultCacheBehavior :: Lens' DistributionConfig DefaultCacheBehavior
dcDefaultCacheBehavior = lens _dcDefaultCacheBehavior (\s a -> s {_dcDefaultCacheBehavior = a})

-- | Any comments you want to include about the distribution. If you don't want to specify a comment, include an empty @Comment@ element. To delete an existing comment, update the distribution configuration and include an empty @Comment@ element. To add or change a comment, update the distribution configuration and specify the new comment.
dcComment :: Lens' DistributionConfig Text
dcComment = lens _dcComment (\s a -> s {_dcComment = a}) . _Sensitive

-- | From this field, you can enable or disable the selected distribution.
dcEnabled :: Lens' DistributionConfig Bool
dcEnabled = lens _dcEnabled (\s a -> s {_dcEnabled = a})

instance FromXML DistributionConfig where
  parseXML x =
    DistributionConfig'
      <$> (x .@? "HttpVersion")
      <*> (x .@? "OriginGroups")
      <*> (x .@? "Aliases")
      <*> (x .@? "DefaultRootObject")
      <*> (x .@? "PriceClass")
      <*> (x .@? "CustomErrorResponses")
      <*> (x .@? "WebACLId")
      <*> (x .@? "ViewerCertificate")
      <*> (x .@? "Restrictions")
      <*> (x .@? "Logging")
      <*> (x .@? "CacheBehaviors")
      <*> (x .@? "IsIPV6Enabled")
      <*> (x .@ "CallerReference")
      <*> (x .@ "Origins")
      <*> (x .@ "DefaultCacheBehavior")
      <*> (x .@ "Comment")
      <*> (x .@ "Enabled")

instance Hashable DistributionConfig

instance NFData DistributionConfig

instance ToXML DistributionConfig where
  toXML DistributionConfig' {..} =
    mconcat
      [ "HttpVersion" @= _dcHTTPVersion,
        "OriginGroups" @= _dcOriginGroups,
        "Aliases" @= _dcAliases,
        "DefaultRootObject" @= _dcDefaultRootObject,
        "PriceClass" @= _dcPriceClass,
        "CustomErrorResponses" @= _dcCustomErrorResponses,
        "WebACLId" @= _dcWebACLId,
        "ViewerCertificate" @= _dcViewerCertificate,
        "Restrictions" @= _dcRestrictions,
        "Logging" @= _dcLogging,
        "CacheBehaviors" @= _dcCacheBehaviors,
        "IsIPV6Enabled" @= _dcIsIPV6Enabled,
        "CallerReference" @= _dcCallerReference,
        "Origins" @= _dcOrigins,
        "DefaultCacheBehavior" @= _dcDefaultCacheBehavior,
        "Comment" @= _dcComment,
        "Enabled" @= _dcEnabled
      ]
