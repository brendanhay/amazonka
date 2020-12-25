{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dcCallerReference,
    dcOrigins,
    dcDefaultCacheBehavior,
    dcComment,
    dcEnabled,
    dcAliases,
    dcCacheBehaviors,
    dcCustomErrorResponses,
    dcDefaultRootObject,
    dcHttpVersion,
    dcIsIPV6Enabled,
    dcLogging,
    dcOriginGroups,
    dcPriceClass,
    dcRestrictions,
    dcViewerCertificate,
    dcWebACLId,
  )
where

import qualified Network.AWS.CloudFront.Types.Aliases as Types
import qualified Network.AWS.CloudFront.Types.CacheBehaviors as Types
import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.CloudFront.Types.CustomErrorResponses as Types
import qualified Network.AWS.CloudFront.Types.DefaultCacheBehavior as Types
import qualified Network.AWS.CloudFront.Types.HttpVersion as Types
import qualified Network.AWS.CloudFront.Types.LoggingConfig as Types
import qualified Network.AWS.CloudFront.Types.OriginGroups as Types
import qualified Network.AWS.CloudFront.Types.Origins as Types
import qualified Network.AWS.CloudFront.Types.PriceClass as Types
import qualified Network.AWS.CloudFront.Types.Restrictions as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.CloudFront.Types.ViewerCertificate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A distribution configuration.
--
-- /See:/ 'mkDistributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
  { -- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution.
    -- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
    callerReference :: Types.String,
    -- | A complex type that contains information about origins for this distribution.
    origins :: Types.Origins,
    -- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
    defaultCacheBehavior :: Types.DefaultCacheBehavior,
    -- | Any comments you want to include about the distribution.
    --
    -- If you don't want to specify a comment, include an empty @Comment@ element.
    -- To delete an existing comment, update the distribution configuration and include an empty @Comment@ element.
    -- To add or change a comment, update the distribution configuration and specify the new comment.
    comment :: Types.Comment,
    -- | From this field, you can enable or disable the selected distribution.
    enabled :: Core.Bool,
    -- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
    aliases :: Core.Maybe Types.Aliases,
    -- | A complex type that contains zero or more @CacheBehavior@ elements.
    cacheBehaviors :: Core.Maybe Types.CacheBehaviors,
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
    customErrorResponses :: Core.Maybe Types.CustomErrorResponses,
    -- | The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution.
    --
    -- Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name.
    -- If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element.
    -- To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element.
    -- To replace the default root object, update the distribution configuration and specify the new object.
    -- For more information about the default root object, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
    defaultRootObject :: Core.Maybe Types.String,
    -- | (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version.
    --
    -- For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI).
    -- In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
    httpVersion :: Core.Maybe Types.HttpVersion,
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
    isIPV6Enabled :: Core.Maybe Core.Bool,
    -- | A complex type that controls whether access logs are written for the distribution.
    --
    -- For more information about logging, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
    logging :: Core.Maybe Types.LoggingConfig,
    -- | A complex type that contains information about origin groups for this distribution.
    originGroups :: Core.Maybe Types.OriginGroups,
    -- | The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations.
    --
    -- If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance.
    -- For more information about price classes, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes (such as Price Class 100) map to CloudFront regions, see <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
    priceClass :: Core.Maybe Types.PriceClass,
    -- | A complex type that identifies ways in which you want to restrict distribution of your content.
    restrictions :: Core.Maybe Types.Restrictions,
    -- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
    viewerCertificate :: Core.Maybe Types.ViewerCertificate,
    -- | A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. To specify a web ACL created using the latest version of AWS WAF, use the ACL ARN, for example @arn:aws:wafv2:us-east-1:123456789012:global/webacl/ExampleWebACL/473e64fd-f30b-4765-81a0-62ad96dd167a@ . To specify a web ACL created using AWS WAF Classic, use the ACL ID, for example @473e64fd-f30b-4765-81a0-62ad96dd167a@ .
    --
    -- AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
    webACLId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DistributionConfig' value with any optional fields omitted.
mkDistributionConfig ::
  -- | 'callerReference'
  Types.String ->
  -- | 'origins'
  Types.Origins ->
  -- | 'defaultCacheBehavior'
  Types.DefaultCacheBehavior ->
  -- | 'comment'
  Types.Comment ->
  -- | 'enabled'
  Core.Bool ->
  DistributionConfig
mkDistributionConfig
  callerReference
  origins
  defaultCacheBehavior
  comment
  enabled =
    DistributionConfig'
      { callerReference,
        origins,
        defaultCacheBehavior,
        comment,
        enabled,
        aliases = Core.Nothing,
        cacheBehaviors = Core.Nothing,
        customErrorResponses = Core.Nothing,
        defaultRootObject = Core.Nothing,
        httpVersion = Core.Nothing,
        isIPV6Enabled = Core.Nothing,
        logging = Core.Nothing,
        originGroups = Core.Nothing,
        priceClass = Core.Nothing,
        restrictions = Core.Nothing,
        viewerCertificate = Core.Nothing,
        webACLId = Core.Nothing
      }

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution.
-- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCallerReference :: Lens.Lens' DistributionConfig Types.String
dcCallerReference = Lens.field @"callerReference"
{-# DEPRECATED dcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex type that contains information about origins for this distribution.
--
-- /Note:/ Consider using 'origins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOrigins :: Lens.Lens' DistributionConfig Types.Origins
dcOrigins = Lens.field @"origins"
{-# DEPRECATED dcOrigins "Use generic-lens or generic-optics with 'origins' instead." #-}

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDefaultCacheBehavior :: Lens.Lens' DistributionConfig Types.DefaultCacheBehavior
dcDefaultCacheBehavior = Lens.field @"defaultCacheBehavior"
{-# DEPRECATED dcDefaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead." #-}

-- | Any comments you want to include about the distribution.
--
-- If you don't want to specify a comment, include an empty @Comment@ element.
-- To delete an existing comment, update the distribution configuration and include an empty @Comment@ element.
-- To add or change a comment, update the distribution configuration and specify the new comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcComment :: Lens.Lens' DistributionConfig Types.Comment
dcComment = Lens.field @"comment"
{-# DEPRECATED dcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | From this field, you can enable or disable the selected distribution.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEnabled :: Lens.Lens' DistributionConfig Core.Bool
dcEnabled = Lens.field @"enabled"
{-# DEPRECATED dcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAliases :: Lens.Lens' DistributionConfig (Core.Maybe Types.Aliases)
dcAliases = Lens.field @"aliases"
{-# DEPRECATED dcAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCacheBehaviors :: Lens.Lens' DistributionConfig (Core.Maybe Types.CacheBehaviors)
dcCacheBehaviors = Lens.field @"cacheBehaviors"
{-# DEPRECATED dcCacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead." #-}

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
dcCustomErrorResponses :: Lens.Lens' DistributionConfig (Core.Maybe Types.CustomErrorResponses)
dcCustomErrorResponses = Lens.field @"customErrorResponses"
{-# DEPRECATED dcCustomErrorResponses "Use generic-lens or generic-optics with 'customErrorResponses' instead." #-}

-- | The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution.
--
-- Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name.
-- If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element.
-- To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element.
-- To replace the default root object, update the distribution configuration and specify the new object.
-- For more information about the default root object, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'defaultRootObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDefaultRootObject :: Lens.Lens' DistributionConfig (Core.Maybe Types.String)
dcDefaultRootObject = Lens.field @"defaultRootObject"
{-# DEPRECATED dcDefaultRootObject "Use generic-lens or generic-optics with 'defaultRootObject' instead." #-}

-- | (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI).
-- In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
--
-- /Note:/ Consider using 'httpVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcHttpVersion :: Lens.Lens' DistributionConfig (Core.Maybe Types.HttpVersion)
dcHttpVersion = Lens.field @"httpVersion"
{-# DEPRECATED dcHttpVersion "Use generic-lens or generic-optics with 'httpVersion' instead." #-}

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
dcIsIPV6Enabled :: Lens.Lens' DistributionConfig (Core.Maybe Core.Bool)
dcIsIPV6Enabled = Lens.field @"isIPV6Enabled"
{-# DEPRECATED dcIsIPV6Enabled "Use generic-lens or generic-optics with 'isIPV6Enabled' instead." #-}

-- | A complex type that controls whether access logs are written for the distribution.
--
-- For more information about logging, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'logging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLogging :: Lens.Lens' DistributionConfig (Core.Maybe Types.LoggingConfig)
dcLogging = Lens.field @"logging"
{-# DEPRECATED dcLogging "Use generic-lens or generic-optics with 'logging' instead." #-}

-- | A complex type that contains information about origin groups for this distribution.
--
-- /Note:/ Consider using 'originGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOriginGroups :: Lens.Lens' DistributionConfig (Core.Maybe Types.OriginGroups)
dcOriginGroups = Lens.field @"originGroups"
{-# DEPRECATED dcOriginGroups "Use generic-lens or generic-optics with 'originGroups' instead." #-}

-- | The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations.
--
-- If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance.
-- For more information about price classes, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes (such as Price Class 100) map to CloudFront regions, see <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcPriceClass :: Lens.Lens' DistributionConfig (Core.Maybe Types.PriceClass)
dcPriceClass = Lens.field @"priceClass"
{-# DEPRECATED dcPriceClass "Use generic-lens or generic-optics with 'priceClass' instead." #-}

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- /Note:/ Consider using 'restrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcRestrictions :: Lens.Lens' DistributionConfig (Core.Maybe Types.Restrictions)
dcRestrictions = Lens.field @"restrictions"
{-# DEPRECATED dcRestrictions "Use generic-lens or generic-optics with 'restrictions' instead." #-}

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- /Note:/ Consider using 'viewerCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcViewerCertificate :: Lens.Lens' DistributionConfig (Core.Maybe Types.ViewerCertificate)
dcViewerCertificate = Lens.field @"viewerCertificate"
{-# DEPRECATED dcViewerCertificate "Use generic-lens or generic-optics with 'viewerCertificate' instead." #-}

-- | A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. To specify a web ACL created using the latest version of AWS WAF, use the ACL ARN, for example @arn:aws:wafv2:us-east-1:123456789012:global/webacl/ExampleWebACL/473e64fd-f30b-4765-81a0-62ad96dd167a@ . To specify a web ACL created using AWS WAF Classic, use the ACL ID, for example @473e64fd-f30b-4765-81a0-62ad96dd167a@ .
--
-- AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcWebACLId :: Lens.Lens' DistributionConfig (Core.Maybe Types.String)
dcWebACLId = Lens.field @"webACLId"
{-# DEPRECATED dcWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

instance Core.ToXML DistributionConfig where
  toXML DistributionConfig {..} =
    Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "Origins" origins
      Core.<> Core.toXMLNode "DefaultCacheBehavior" defaultCacheBehavior
      Core.<> Core.toXMLNode "Comment" comment
      Core.<> Core.toXMLNode "Enabled" enabled
      Core.<> Core.toXMLNode "Aliases" Core.<$> aliases
      Core.<> Core.toXMLNode "CacheBehaviors" Core.<$> cacheBehaviors
      Core.<> Core.toXMLNode "CustomErrorResponses" Core.<$> customErrorResponses
      Core.<> Core.toXMLNode "DefaultRootObject" Core.<$> defaultRootObject
      Core.<> Core.toXMLNode "HttpVersion" Core.<$> httpVersion
      Core.<> Core.toXMLNode "IsIPV6Enabled" Core.<$> isIPV6Enabled
      Core.<> Core.toXMLNode "Logging" Core.<$> logging
      Core.<> Core.toXMLNode "OriginGroups" Core.<$> originGroups
      Core.<> Core.toXMLNode "PriceClass" Core.<$> priceClass
      Core.<> Core.toXMLNode "Restrictions" Core.<$> restrictions
      Core.<> Core.toXMLNode "ViewerCertificate" Core.<$> viewerCertificate
      Core.<> Core.toXMLNode "WebACLId" Core.<$> webACLId

instance Core.FromXML DistributionConfig where
  parseXML x =
    DistributionConfig'
      Core.<$> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "Origins")
      Core.<*> (x Core..@ "DefaultCacheBehavior")
      Core.<*> (x Core..@ "Comment")
      Core.<*> (x Core..@ "Enabled")
      Core.<*> (x Core..@? "Aliases")
      Core.<*> (x Core..@? "CacheBehaviors")
      Core.<*> (x Core..@? "CustomErrorResponses")
      Core.<*> (x Core..@? "DefaultRootObject")
      Core.<*> (x Core..@? "HttpVersion")
      Core.<*> (x Core..@? "IsIPV6Enabled")
      Core.<*> (x Core..@? "Logging")
      Core.<*> (x Core..@? "OriginGroups")
      Core.<*> (x Core..@? "PriceClass")
      Core.<*> (x Core..@? "Restrictions")
      Core.<*> (x Core..@? "ViewerCertificate")
      Core.<*> (x Core..@? "WebACLId")
