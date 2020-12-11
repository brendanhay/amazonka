-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionConfig
  ( DistributionConfig (..),

    -- * Smart constructor
    mkDistributionConfig,

    -- * Lenses
    dcHTTPVersion,
    dcOriginGroups,
    dcAliases,
    dcDefaultRootObject,
    dcPriceClass,
    dcCustomErrorResponses,
    dcWebACLId,
    dcViewerCertificate,
    dcRestrictions,
    dcLogging,
    dcCacheBehaviors,
    dcIsIPV6Enabled,
    dcCallerReference,
    dcOrigins,
    dcDefaultCacheBehavior,
    dcComment,
    dcEnabled,
  )
where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A distribution configuration.
--
-- /See:/ 'mkDistributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
  { hTTPVersion ::
      Lude.Maybe HTTPVersion,
    originGroups :: Lude.Maybe OriginGroups,
    aliases :: Lude.Maybe Aliases,
    defaultRootObject :: Lude.Maybe Lude.Text,
    priceClass :: Lude.Maybe PriceClass,
    customErrorResponses ::
      Lude.Maybe CustomErrorResponses,
    webACLId :: Lude.Maybe Lude.Text,
    viewerCertificate :: Lude.Maybe ViewerCertificate,
    restrictions :: Lude.Maybe Restrictions,
    logging :: Lude.Maybe LoggingConfig,
    cacheBehaviors :: Lude.Maybe CacheBehaviors,
    isIPV6Enabled :: Lude.Maybe Lude.Bool,
    callerReference :: Lude.Text,
    origins :: Origins,
    defaultCacheBehavior :: DefaultCacheBehavior,
    comment :: Lude.Sensitive Lude.Text,
    enabled :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DistributionConfig' with the minimum fields required to make a request.
--
-- * 'aliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
-- * 'cacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
-- * 'callerReference' - A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution.
-- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
-- * 'comment' - Any comments you want to include about the distribution.
--
-- If you don't want to specify a comment, include an empty @Comment@ element.
-- To delete an existing comment, update the distribution configuration and include an empty @Comment@ element.
-- To add or change a comment, update the distribution configuration and specify the new comment.
-- * 'customErrorResponses' - A complex type that controls the following:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
-- For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
-- * 'defaultCacheBehavior' - A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
-- * 'defaultRootObject' - The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution.
--
-- Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name.
-- If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element.
-- To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element.
-- To replace the default root object, update the distribution configuration and specify the new object.
-- For more information about the default root object, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
-- * 'enabled' - From this field, you can enable or disable the selected distribution.
-- * 'hTTPVersion' - (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI).
-- In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
-- * 'isIPV6Enabled' - If you want CloudFront to respond to IPv6 DNS requests with an IPv6 address for your distribution, specify @true@ . If you specify @false@ , CloudFront responds to IPv6 DNS requests with the DNS response code @NOERROR@ and with no IP addresses. This allows viewers to submit a second request, for an IPv4 address for your distribution.
--
-- In general, you should enable IPv6 if you have users on IPv6 networks who want to access your content. However, if you're using signed URLs or signed cookies to restrict access to your content, and if you're using a custom policy that includes the @IpAddress@ parameter to restrict the IP addresses that can access your content, don't enable IPv6. If you want to restrict access to some content by IP address and not restrict access to other content (or restrict access but not by IP address), you can create two distributions. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy> in the /Amazon CloudFront Developer Guide/ .
-- If you're using an Amazon Route 53 alias resource record set to route traffic to your CloudFront distribution, you need to create a second alias resource record set when both of the following are true:
--
--     * You enable IPv6 for the distribution
--
--
--     * You're using alternate domain names in the URLs for your objects
--
--
-- For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name> in the /Amazon Route 53 Developer Guide/ .
-- If you created a CNAME resource record set, either with Amazon Route 53 or with another DNS service, you don't need to make any changes. A CNAME record will route traffic to your distribution regardless of the IP address format of the viewer request.
-- * 'logging' - A complex type that controls whether access logs are written for the distribution.
--
-- For more information about logging, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
-- * 'originGroups' - A complex type that contains information about origin groups for this distribution.
-- * 'origins' - A complex type that contains information about origins for this distribution.
-- * 'priceClass' - The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations.
--
-- If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance.
-- For more information about price classes, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes (such as Price Class 100) map to CloudFront regions, see <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
-- * 'restrictions' - A complex type that identifies ways in which you want to restrict distribution of your content.
-- * 'viewerCertificate' - A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
-- * 'webACLId' - A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. To specify a web ACL created using the latest version of AWS WAF, use the ACL ARN, for example @arn:aws:wafv2:us-east-1:123456789012:global/webacl/ExampleWebACL/473e64fd-f30b-4765-81a0-62ad96dd167a@ . To specify a web ACL created using AWS WAF Classic, use the ACL ID, for example @473e64fd-f30b-4765-81a0-62ad96dd167a@ .
--
-- AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
mkDistributionConfig ::
  -- | 'callerReference'
  Lude.Text ->
  -- | 'origins'
  Origins ->
  -- | 'defaultCacheBehavior'
  DefaultCacheBehavior ->
  -- | 'comment'
  Lude.Sensitive Lude.Text ->
  -- | 'enabled'
  Lude.Bool ->
  DistributionConfig
mkDistributionConfig
  pCallerReference_
  pOrigins_
  pDefaultCacheBehavior_
  pComment_
  pEnabled_ =
    DistributionConfig'
      { hTTPVersion = Lude.Nothing,
        originGroups = Lude.Nothing,
        aliases = Lude.Nothing,
        defaultRootObject = Lude.Nothing,
        priceClass = Lude.Nothing,
        customErrorResponses = Lude.Nothing,
        webACLId = Lude.Nothing,
        viewerCertificate = Lude.Nothing,
        restrictions = Lude.Nothing,
        logging = Lude.Nothing,
        cacheBehaviors = Lude.Nothing,
        isIPV6Enabled = Lude.Nothing,
        callerReference = pCallerReference_,
        origins = pOrigins_,
        defaultCacheBehavior = pDefaultCacheBehavior_,
        comment = pComment_,
        enabled = pEnabled_
      }

-- | (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI).
-- In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
--
-- /Note:/ Consider using 'hTTPVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcHTTPVersion :: Lens.Lens' DistributionConfig (Lude.Maybe HTTPVersion)
dcHTTPVersion = Lens.lens (hTTPVersion :: DistributionConfig -> Lude.Maybe HTTPVersion) (\s a -> s {hTTPVersion = a} :: DistributionConfig)
{-# DEPRECATED dcHTTPVersion "Use generic-lens or generic-optics with 'hTTPVersion' instead." #-}

-- | A complex type that contains information about origin groups for this distribution.
--
-- /Note:/ Consider using 'originGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOriginGroups :: Lens.Lens' DistributionConfig (Lude.Maybe OriginGroups)
dcOriginGroups = Lens.lens (originGroups :: DistributionConfig -> Lude.Maybe OriginGroups) (\s a -> s {originGroups = a} :: DistributionConfig)
{-# DEPRECATED dcOriginGroups "Use generic-lens or generic-optics with 'originGroups' instead." #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAliases :: Lens.Lens' DistributionConfig (Lude.Maybe Aliases)
dcAliases = Lens.lens (aliases :: DistributionConfig -> Lude.Maybe Aliases) (\s a -> s {aliases = a} :: DistributionConfig)
{-# DEPRECATED dcAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution.
--
-- Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name.
-- If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element.
-- To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element.
-- To replace the default root object, update the distribution configuration and specify the new object.
-- For more information about the default root object, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'defaultRootObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDefaultRootObject :: Lens.Lens' DistributionConfig (Lude.Maybe Lude.Text)
dcDefaultRootObject = Lens.lens (defaultRootObject :: DistributionConfig -> Lude.Maybe Lude.Text) (\s a -> s {defaultRootObject = a} :: DistributionConfig)
{-# DEPRECATED dcDefaultRootObject "Use generic-lens or generic-optics with 'defaultRootObject' instead." #-}

-- | The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations.
--
-- If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance.
-- For more information about price classes, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes (such as Price Class 100) map to CloudFront regions, see <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcPriceClass :: Lens.Lens' DistributionConfig (Lude.Maybe PriceClass)
dcPriceClass = Lens.lens (priceClass :: DistributionConfig -> Lude.Maybe PriceClass) (\s a -> s {priceClass = a} :: DistributionConfig)
{-# DEPRECATED dcPriceClass "Use generic-lens or generic-optics with 'priceClass' instead." #-}

-- | A complex type that controls the following:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
-- For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'customErrorResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCustomErrorResponses :: Lens.Lens' DistributionConfig (Lude.Maybe CustomErrorResponses)
dcCustomErrorResponses = Lens.lens (customErrorResponses :: DistributionConfig -> Lude.Maybe CustomErrorResponses) (\s a -> s {customErrorResponses = a} :: DistributionConfig)
{-# DEPRECATED dcCustomErrorResponses "Use generic-lens or generic-optics with 'customErrorResponses' instead." #-}

-- | A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. To specify a web ACL created using the latest version of AWS WAF, use the ACL ARN, for example @arn:aws:wafv2:us-east-1:123456789012:global/webacl/ExampleWebACL/473e64fd-f30b-4765-81a0-62ad96dd167a@ . To specify a web ACL created using AWS WAF Classic, use the ACL ID, for example @473e64fd-f30b-4765-81a0-62ad96dd167a@ .
--
-- AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcWebACLId :: Lens.Lens' DistributionConfig (Lude.Maybe Lude.Text)
dcWebACLId = Lens.lens (webACLId :: DistributionConfig -> Lude.Maybe Lude.Text) (\s a -> s {webACLId = a} :: DistributionConfig)
{-# DEPRECATED dcWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- /Note:/ Consider using 'viewerCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcViewerCertificate :: Lens.Lens' DistributionConfig (Lude.Maybe ViewerCertificate)
dcViewerCertificate = Lens.lens (viewerCertificate :: DistributionConfig -> Lude.Maybe ViewerCertificate) (\s a -> s {viewerCertificate = a} :: DistributionConfig)
{-# DEPRECATED dcViewerCertificate "Use generic-lens or generic-optics with 'viewerCertificate' instead." #-}

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- /Note:/ Consider using 'restrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcRestrictions :: Lens.Lens' DistributionConfig (Lude.Maybe Restrictions)
dcRestrictions = Lens.lens (restrictions :: DistributionConfig -> Lude.Maybe Restrictions) (\s a -> s {restrictions = a} :: DistributionConfig)
{-# DEPRECATED dcRestrictions "Use generic-lens or generic-optics with 'restrictions' instead." #-}

-- | A complex type that controls whether access logs are written for the distribution.
--
-- For more information about logging, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'logging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLogging :: Lens.Lens' DistributionConfig (Lude.Maybe LoggingConfig)
dcLogging = Lens.lens (logging :: DistributionConfig -> Lude.Maybe LoggingConfig) (\s a -> s {logging = a} :: DistributionConfig)
{-# DEPRECATED dcLogging "Use generic-lens or generic-optics with 'logging' instead." #-}

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCacheBehaviors :: Lens.Lens' DistributionConfig (Lude.Maybe CacheBehaviors)
dcCacheBehaviors = Lens.lens (cacheBehaviors :: DistributionConfig -> Lude.Maybe CacheBehaviors) (\s a -> s {cacheBehaviors = a} :: DistributionConfig)
{-# DEPRECATED dcCacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead." #-}

-- | If you want CloudFront to respond to IPv6 DNS requests with an IPv6 address for your distribution, specify @true@ . If you specify @false@ , CloudFront responds to IPv6 DNS requests with the DNS response code @NOERROR@ and with no IP addresses. This allows viewers to submit a second request, for an IPv4 address for your distribution.
--
-- In general, you should enable IPv6 if you have users on IPv6 networks who want to access your content. However, if you're using signed URLs or signed cookies to restrict access to your content, and if you're using a custom policy that includes the @IpAddress@ parameter to restrict the IP addresses that can access your content, don't enable IPv6. If you want to restrict access to some content by IP address and not restrict access to other content (or restrict access but not by IP address), you can create two distributions. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy> in the /Amazon CloudFront Developer Guide/ .
-- If you're using an Amazon Route 53 alias resource record set to route traffic to your CloudFront distribution, you need to create a second alias resource record set when both of the following are true:
--
--     * You enable IPv6 for the distribution
--
--
--     * You're using alternate domain names in the URLs for your objects
--
--
-- For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name> in the /Amazon Route 53 Developer Guide/ .
-- If you created a CNAME resource record set, either with Amazon Route 53 or with another DNS service, you don't need to make any changes. A CNAME record will route traffic to your distribution regardless of the IP address format of the viewer request.
--
-- /Note:/ Consider using 'isIPV6Enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcIsIPV6Enabled :: Lens.Lens' DistributionConfig (Lude.Maybe Lude.Bool)
dcIsIPV6Enabled = Lens.lens (isIPV6Enabled :: DistributionConfig -> Lude.Maybe Lude.Bool) (\s a -> s {isIPV6Enabled = a} :: DistributionConfig)
{-# DEPRECATED dcIsIPV6Enabled "Use generic-lens or generic-optics with 'isIPV6Enabled' instead." #-}

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution.
-- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCallerReference :: Lens.Lens' DistributionConfig Lude.Text
dcCallerReference = Lens.lens (callerReference :: DistributionConfig -> Lude.Text) (\s a -> s {callerReference = a} :: DistributionConfig)
{-# DEPRECATED dcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex type that contains information about origins for this distribution.
--
-- /Note:/ Consider using 'origins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOrigins :: Lens.Lens' DistributionConfig Origins
dcOrigins = Lens.lens (origins :: DistributionConfig -> Origins) (\s a -> s {origins = a} :: DistributionConfig)
{-# DEPRECATED dcOrigins "Use generic-lens or generic-optics with 'origins' instead." #-}

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDefaultCacheBehavior :: Lens.Lens' DistributionConfig DefaultCacheBehavior
dcDefaultCacheBehavior = Lens.lens (defaultCacheBehavior :: DistributionConfig -> DefaultCacheBehavior) (\s a -> s {defaultCacheBehavior = a} :: DistributionConfig)
{-# DEPRECATED dcDefaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead." #-}

-- | Any comments you want to include about the distribution.
--
-- If you don't want to specify a comment, include an empty @Comment@ element.
-- To delete an existing comment, update the distribution configuration and include an empty @Comment@ element.
-- To add or change a comment, update the distribution configuration and specify the new comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcComment :: Lens.Lens' DistributionConfig (Lude.Sensitive Lude.Text)
dcComment = Lens.lens (comment :: DistributionConfig -> Lude.Sensitive Lude.Text) (\s a -> s {comment = a} :: DistributionConfig)
{-# DEPRECATED dcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | From this field, you can enable or disable the selected distribution.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEnabled :: Lens.Lens' DistributionConfig Lude.Bool
dcEnabled = Lens.lens (enabled :: DistributionConfig -> Lude.Bool) (\s a -> s {enabled = a} :: DistributionConfig)
{-# DEPRECATED dcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML DistributionConfig where
  parseXML x =
    DistributionConfig'
      Lude.<$> (x Lude..@? "HttpVersion")
      Lude.<*> (x Lude..@? "OriginGroups")
      Lude.<*> (x Lude..@? "Aliases")
      Lude.<*> (x Lude..@? "DefaultRootObject")
      Lude.<*> (x Lude..@? "PriceClass")
      Lude.<*> (x Lude..@? "CustomErrorResponses")
      Lude.<*> (x Lude..@? "WebACLId")
      Lude.<*> (x Lude..@? "ViewerCertificate")
      Lude.<*> (x Lude..@? "Restrictions")
      Lude.<*> (x Lude..@? "Logging")
      Lude.<*> (x Lude..@? "CacheBehaviors")
      Lude.<*> (x Lude..@? "IsIPV6Enabled")
      Lude.<*> (x Lude..@ "CallerReference")
      Lude.<*> (x Lude..@ "Origins")
      Lude.<*> (x Lude..@ "DefaultCacheBehavior")
      Lude.<*> (x Lude..@ "Comment")
      Lude.<*> (x Lude..@ "Enabled")

instance Lude.ToXML DistributionConfig where
  toXML DistributionConfig' {..} =
    Lude.mconcat
      [ "HttpVersion" Lude.@= hTTPVersion,
        "OriginGroups" Lude.@= originGroups,
        "Aliases" Lude.@= aliases,
        "DefaultRootObject" Lude.@= defaultRootObject,
        "PriceClass" Lude.@= priceClass,
        "CustomErrorResponses" Lude.@= customErrorResponses,
        "WebACLId" Lude.@= webACLId,
        "ViewerCertificate" Lude.@= viewerCertificate,
        "Restrictions" Lude.@= restrictions,
        "Logging" Lude.@= logging,
        "CacheBehaviors" Lude.@= cacheBehaviors,
        "IsIPV6Enabled" Lude.@= isIPV6Enabled,
        "CallerReference" Lude.@= callerReference,
        "Origins" Lude.@= origins,
        "DefaultCacheBehavior" Lude.@= defaultCacheBehavior,
        "Comment" Lude.@= comment,
        "Enabled" Lude.@= enabled
      ]
