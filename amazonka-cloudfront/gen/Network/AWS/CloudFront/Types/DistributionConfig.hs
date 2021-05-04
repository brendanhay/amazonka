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
-- Module      : Network.AWS.CloudFront.Types.DistributionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionConfig where

import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.CacheBehaviors
import Network.AWS.CloudFront.Types.CustomErrorResponses
import Network.AWS.CloudFront.Types.DefaultCacheBehavior
import Network.AWS.CloudFront.Types.HttpVersion
import Network.AWS.CloudFront.Types.LoggingConfig
import Network.AWS.CloudFront.Types.OriginGroups
import Network.AWS.CloudFront.Types.Origins
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.Restrictions
import Network.AWS.CloudFront.Types.ViewerCertificate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A distribution configuration.
--
-- /See:/ 'newDistributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
  { -- | A complex type that determines the distribution’s SSL\/TLS configuration
    -- for communicating with viewers.
    viewerCertificate :: Prelude.Maybe ViewerCertificate,
    -- | A complex type that controls the following:
    --
    -- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
    --     range with custom error messages before returning the response to
    --     the viewer.
    --
    -- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
    --     range.
    --
    -- For more information about custom error pages, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
    -- in the /Amazon CloudFront Developer Guide/.
    customErrorResponses :: Prelude.Maybe CustomErrorResponses,
    -- | A unique identifier that specifies the AWS WAF web ACL, if any, to
    -- associate with this distribution. To specify a web ACL created using the
    -- latest version of AWS WAF, use the ACL ARN, for example
    -- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
    -- To specify a web ACL created using AWS WAF Classic, use the ACL ID, for
    -- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
    --
    -- AWS WAF is a web application firewall that lets you monitor the HTTP and
    -- HTTPS requests that are forwarded to CloudFront, and lets you control
    -- access to your content. Based on conditions that you specify, such as
    -- the IP addresses that requests originate from or the values of query
    -- strings, CloudFront responds to requests either with the requested
    -- content or with an HTTP 403 status code (Forbidden). You can also
    -- configure CloudFront to return a custom error page when a request is
    -- blocked. For more information about AWS WAF, see the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide>.
    webACLId :: Prelude.Maybe Prelude.Text,
    -- | The price class that corresponds with the maximum price that you want to
    -- pay for CloudFront service. If you specify @PriceClass_All@, CloudFront
    -- responds to requests for your objects from all CloudFront edge
    -- locations.
    --
    -- If you specify a price class other than @PriceClass_All@, CloudFront
    -- serves your objects from the CloudFront edge location that has the
    -- lowest latency among the edge locations in your price class. Viewers who
    -- are in or near regions that are excluded from your specified price class
    -- may encounter slower performance.
    --
    -- For more information about price classes, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution>
    -- in the /Amazon CloudFront Developer Guide/. For information about
    -- CloudFront pricing, including how price classes (such as Price Class
    -- 100) map to CloudFront regions, see
    -- <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing>.
    priceClass :: Prelude.Maybe PriceClass,
    -- | A complex type that controls whether access logs are written for the
    -- distribution.
    --
    -- For more information about logging, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs>
    -- in the /Amazon CloudFront Developer Guide/.
    logging :: Prelude.Maybe LoggingConfig,
    -- | A complex type that contains information about origin groups for this
    -- distribution.
    originGroups :: Prelude.Maybe OriginGroups,
    -- | A complex type that identifies ways in which you want to restrict
    -- distribution of your content.
    restrictions :: Prelude.Maybe Restrictions,
    -- | If you want CloudFront to respond to IPv6 DNS requests with an IPv6
    -- address for your distribution, specify @true@. If you specify @false@,
    -- CloudFront responds to IPv6 DNS requests with the DNS response code
    -- @NOERROR@ and with no IP addresses. This allows viewers to submit a
    -- second request, for an IPv4 address for your distribution.
    --
    -- In general, you should enable IPv6 if you have users on IPv6 networks
    -- who want to access your content. However, if you\'re using signed URLs
    -- or signed cookies to restrict access to your content, and if you\'re
    -- using a custom policy that includes the @IpAddress@ parameter to
    -- restrict the IP addresses that can access your content, don\'t enable
    -- IPv6. If you want to restrict access to some content by IP address and
    -- not restrict access to other content (or restrict access but not by IP
    -- address), you can create two distributions. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you\'re using an Amazon Route 53 alias resource record set to route
    -- traffic to your CloudFront distribution, you need to create a second
    -- alias resource record set when both of the following are true:
    --
    -- -   You enable IPv6 for the distribution
    --
    -- -   You\'re using alternate domain names in the URLs for your objects
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- If you created a CNAME resource record set, either with Amazon Route 53
    -- or with another DNS service, you don\'t need to make any changes. A
    -- CNAME record will route traffic to your distribution regardless of the
    -- IP address format of the viewer request.
    isIPV6Enabled :: Prelude.Maybe Prelude.Bool,
    -- | A complex type that contains zero or more @CacheBehavior@ elements.
    cacheBehaviors :: Prelude.Maybe CacheBehaviors,
    -- | The object that you want CloudFront to request from your origin (for
    -- example, @index.html@) when a viewer requests the root URL for your
    -- distribution (@http:\/\/www.example.com@) instead of an object in your
    -- distribution (@http:\/\/www.example.com\/product-description.html@).
    -- Specifying a default root object avoids exposing the contents of your
    -- distribution.
    --
    -- Specify only the object name, for example, @index.html@. Don\'t add a
    -- @\/@ before the object name.
    --
    -- If you don\'t want to specify a default root object when you create a
    -- distribution, include an empty @DefaultRootObject@ element.
    --
    -- To delete the default root object from an existing distribution, update
    -- the distribution configuration and include an empty @DefaultRootObject@
    -- element.
    --
    -- To replace the default root object, update the distribution
    -- configuration and specify the new object.
    --
    -- For more information about the default root object, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object>
    -- in the /Amazon CloudFront Developer Guide/.
    defaultRootObject :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information about CNAMEs (alternate domain
    -- names), if any, for this distribution.
    aliases :: Prelude.Maybe Aliases,
    -- | (Optional) Specify the maximum HTTP version that you want viewers to use
    -- to communicate with CloudFront. The default value for new web
    -- distributions is http2. Viewers that don\'t support HTTP\/2
    -- automatically use an earlier HTTP version.
    --
    -- For viewers and CloudFront to use HTTP\/2, viewers must support TLS 1.2
    -- or later, and must support Server Name Identification (SNI).
    --
    -- In general, configuring CloudFront to communicate with viewers using
    -- HTTP\/2 reduces latency. You can improve performance by optimizing for
    -- HTTP\/2. For more information, do an Internet search for \"http\/2
    -- optimization.\"
    httpVersion :: Prelude.Maybe HttpVersion,
    -- | A unique value (for example, a date-time stamp) that ensures that the
    -- request can\'t be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of
    -- the @DistributionConfig@ object), CloudFront creates a new distribution.
    --
    -- If @CallerReference@ is a value that you already sent in a previous
    -- request to create a distribution, CloudFront returns a
    -- @DistributionAlreadyExists@ error.
    callerReference :: Prelude.Text,
    -- | A complex type that contains information about origins for this
    -- distribution.
    origins :: Origins,
    -- | A complex type that describes the default cache behavior if you don\'t
    -- specify a @CacheBehavior@ element or if files don\'t match any of the
    -- values of @PathPattern@ in @CacheBehavior@ elements. You must create
    -- exactly one default cache behavior.
    defaultCacheBehavior :: DefaultCacheBehavior,
    -- | Any comments you want to include about the distribution.
    --
    -- If you don\'t want to specify a comment, include an empty @Comment@
    -- element.
    --
    -- To delete an existing comment, update the distribution configuration and
    -- include an empty @Comment@ element.
    --
    -- To add or change a comment, update the distribution configuration and
    -- specify the new comment.
    comment :: Prelude.Sensitive Prelude.Text,
    -- | From this field, you can enable or disable the selected distribution.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DistributionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewerCertificate', 'distributionConfig_viewerCertificate' - A complex type that determines the distribution’s SSL\/TLS configuration
-- for communicating with viewers.
--
-- 'customErrorResponses', 'distributionConfig_customErrorResponses' - A complex type that controls the following:
--
-- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
--     range with custom error messages before returning the response to
--     the viewer.
--
-- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
--     range.
--
-- For more information about custom error pages, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'webACLId', 'distributionConfig_webACLId' - A unique identifier that specifies the AWS WAF web ACL, if any, to
-- associate with this distribution. To specify a web ACL created using the
-- latest version of AWS WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using AWS WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
--
-- AWS WAF is a web application firewall that lets you monitor the HTTP and
-- HTTPS requests that are forwarded to CloudFront, and lets you control
-- access to your content. Based on conditions that you specify, such as
-- the IP addresses that requests originate from or the values of query
-- strings, CloudFront responds to requests either with the requested
-- content or with an HTTP 403 status code (Forbidden). You can also
-- configure CloudFront to return a custom error page when a request is
-- blocked. For more information about AWS WAF, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide>.
--
-- 'priceClass', 'distributionConfig_priceClass' - The price class that corresponds with the maximum price that you want to
-- pay for CloudFront service. If you specify @PriceClass_All@, CloudFront
-- responds to requests for your objects from all CloudFront edge
-- locations.
--
-- If you specify a price class other than @PriceClass_All@, CloudFront
-- serves your objects from the CloudFront edge location that has the
-- lowest latency among the edge locations in your price class. Viewers who
-- are in or near regions that are excluded from your specified price class
-- may encounter slower performance.
--
-- For more information about price classes, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution>
-- in the /Amazon CloudFront Developer Guide/. For information about
-- CloudFront pricing, including how price classes (such as Price Class
-- 100) map to CloudFront regions, see
-- <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing>.
--
-- 'logging', 'distributionConfig_logging' - A complex type that controls whether access logs are written for the
-- distribution.
--
-- For more information about logging, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'originGroups', 'distributionConfig_originGroups' - A complex type that contains information about origin groups for this
-- distribution.
--
-- 'restrictions', 'distributionConfig_restrictions' - A complex type that identifies ways in which you want to restrict
-- distribution of your content.
--
-- 'isIPV6Enabled', 'distributionConfig_isIPV6Enabled' - If you want CloudFront to respond to IPv6 DNS requests with an IPv6
-- address for your distribution, specify @true@. If you specify @false@,
-- CloudFront responds to IPv6 DNS requests with the DNS response code
-- @NOERROR@ and with no IP addresses. This allows viewers to submit a
-- second request, for an IPv4 address for your distribution.
--
-- In general, you should enable IPv6 if you have users on IPv6 networks
-- who want to access your content. However, if you\'re using signed URLs
-- or signed cookies to restrict access to your content, and if you\'re
-- using a custom policy that includes the @IpAddress@ parameter to
-- restrict the IP addresses that can access your content, don\'t enable
-- IPv6. If you want to restrict access to some content by IP address and
-- not restrict access to other content (or restrict access but not by IP
-- address), you can create two distributions. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you\'re using an Amazon Route 53 alias resource record set to route
-- traffic to your CloudFront distribution, you need to create a second
-- alias resource record set when both of the following are true:
--
-- -   You enable IPv6 for the distribution
--
-- -   You\'re using alternate domain names in the URLs for your objects
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
-- in the /Amazon Route 53 Developer Guide/.
--
-- If you created a CNAME resource record set, either with Amazon Route 53
-- or with another DNS service, you don\'t need to make any changes. A
-- CNAME record will route traffic to your distribution regardless of the
-- IP address format of the viewer request.
--
-- 'cacheBehaviors', 'distributionConfig_cacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- 'defaultRootObject', 'distributionConfig_defaultRootObject' - The object that you want CloudFront to request from your origin (for
-- example, @index.html@) when a viewer requests the root URL for your
-- distribution (@http:\/\/www.example.com@) instead of an object in your
-- distribution (@http:\/\/www.example.com\/product-description.html@).
-- Specifying a default root object avoids exposing the contents of your
-- distribution.
--
-- Specify only the object name, for example, @index.html@. Don\'t add a
-- @\/@ before the object name.
--
-- If you don\'t want to specify a default root object when you create a
-- distribution, include an empty @DefaultRootObject@ element.
--
-- To delete the default root object from an existing distribution, update
-- the distribution configuration and include an empty @DefaultRootObject@
-- element.
--
-- To replace the default root object, update the distribution
-- configuration and specify the new object.
--
-- For more information about the default root object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'aliases', 'distributionConfig_aliases' - A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- 'httpVersion', 'distributionConfig_httpVersion' - (Optional) Specify the maximum HTTP version that you want viewers to use
-- to communicate with CloudFront. The default value for new web
-- distributions is http2. Viewers that don\'t support HTTP\/2
-- automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP\/2, viewers must support TLS 1.2
-- or later, and must support Server Name Identification (SNI).
--
-- In general, configuring CloudFront to communicate with viewers using
-- HTTP\/2 reduces latency. You can improve performance by optimizing for
-- HTTP\/2. For more information, do an Internet search for \"http\/2
-- optimization.\"
--
-- 'callerReference', 'distributionConfig_callerReference' - A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @DistributionConfig@ object), CloudFront creates a new distribution.
--
-- If @CallerReference@ is a value that you already sent in a previous
-- request to create a distribution, CloudFront returns a
-- @DistributionAlreadyExists@ error.
--
-- 'origins', 'distributionConfig_origins' - A complex type that contains information about origins for this
-- distribution.
--
-- 'defaultCacheBehavior', 'distributionConfig_defaultCacheBehavior' - A complex type that describes the default cache behavior if you don\'t
-- specify a @CacheBehavior@ element or if files don\'t match any of the
-- values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
--
-- 'comment', 'distributionConfig_comment' - Any comments you want to include about the distribution.
--
-- If you don\'t want to specify a comment, include an empty @Comment@
-- element.
--
-- To delete an existing comment, update the distribution configuration and
-- include an empty @Comment@ element.
--
-- To add or change a comment, update the distribution configuration and
-- specify the new comment.
--
-- 'enabled', 'distributionConfig_enabled' - From this field, you can enable or disable the selected distribution.
newDistributionConfig ::
  -- | 'callerReference'
  Prelude.Text ->
  -- | 'origins'
  Origins ->
  -- | 'defaultCacheBehavior'
  DefaultCacheBehavior ->
  -- | 'comment'
  Prelude.Text ->
  -- | 'enabled'
  Prelude.Bool ->
  DistributionConfig
newDistributionConfig
  pCallerReference_
  pOrigins_
  pDefaultCacheBehavior_
  pComment_
  pEnabled_ =
    DistributionConfig'
      { viewerCertificate =
          Prelude.Nothing,
        customErrorResponses = Prelude.Nothing,
        webACLId = Prelude.Nothing,
        priceClass = Prelude.Nothing,
        logging = Prelude.Nothing,
        originGroups = Prelude.Nothing,
        restrictions = Prelude.Nothing,
        isIPV6Enabled = Prelude.Nothing,
        cacheBehaviors = Prelude.Nothing,
        defaultRootObject = Prelude.Nothing,
        aliases = Prelude.Nothing,
        httpVersion = Prelude.Nothing,
        callerReference = pCallerReference_,
        origins = pOrigins_,
        defaultCacheBehavior = pDefaultCacheBehavior_,
        comment = Prelude._Sensitive Lens.# pComment_,
        enabled = pEnabled_
      }

-- | A complex type that determines the distribution’s SSL\/TLS configuration
-- for communicating with viewers.
distributionConfig_viewerCertificate :: Lens.Lens' DistributionConfig (Prelude.Maybe ViewerCertificate)
distributionConfig_viewerCertificate = Lens.lens (\DistributionConfig' {viewerCertificate} -> viewerCertificate) (\s@DistributionConfig' {} a -> s {viewerCertificate = a} :: DistributionConfig)

-- | A complex type that controls the following:
--
-- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
--     range with custom error messages before returning the response to
--     the viewer.
--
-- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
--     range.
--
-- For more information about custom error pages, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
distributionConfig_customErrorResponses :: Lens.Lens' DistributionConfig (Prelude.Maybe CustomErrorResponses)
distributionConfig_customErrorResponses = Lens.lens (\DistributionConfig' {customErrorResponses} -> customErrorResponses) (\s@DistributionConfig' {} a -> s {customErrorResponses = a} :: DistributionConfig)

-- | A unique identifier that specifies the AWS WAF web ACL, if any, to
-- associate with this distribution. To specify a web ACL created using the
-- latest version of AWS WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using AWS WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
--
-- AWS WAF is a web application firewall that lets you monitor the HTTP and
-- HTTPS requests that are forwarded to CloudFront, and lets you control
-- access to your content. Based on conditions that you specify, such as
-- the IP addresses that requests originate from or the values of query
-- strings, CloudFront responds to requests either with the requested
-- content or with an HTTP 403 status code (Forbidden). You can also
-- configure CloudFront to return a custom error page when a request is
-- blocked. For more information about AWS WAF, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide>.
distributionConfig_webACLId :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Text)
distributionConfig_webACLId = Lens.lens (\DistributionConfig' {webACLId} -> webACLId) (\s@DistributionConfig' {} a -> s {webACLId = a} :: DistributionConfig)

-- | The price class that corresponds with the maximum price that you want to
-- pay for CloudFront service. If you specify @PriceClass_All@, CloudFront
-- responds to requests for your objects from all CloudFront edge
-- locations.
--
-- If you specify a price class other than @PriceClass_All@, CloudFront
-- serves your objects from the CloudFront edge location that has the
-- lowest latency among the edge locations in your price class. Viewers who
-- are in or near regions that are excluded from your specified price class
-- may encounter slower performance.
--
-- For more information about price classes, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution>
-- in the /Amazon CloudFront Developer Guide/. For information about
-- CloudFront pricing, including how price classes (such as Price Class
-- 100) map to CloudFront regions, see
-- <http://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing>.
distributionConfig_priceClass :: Lens.Lens' DistributionConfig (Prelude.Maybe PriceClass)
distributionConfig_priceClass = Lens.lens (\DistributionConfig' {priceClass} -> priceClass) (\s@DistributionConfig' {} a -> s {priceClass = a} :: DistributionConfig)

-- | A complex type that controls whether access logs are written for the
-- distribution.
--
-- For more information about logging, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs>
-- in the /Amazon CloudFront Developer Guide/.
distributionConfig_logging :: Lens.Lens' DistributionConfig (Prelude.Maybe LoggingConfig)
distributionConfig_logging = Lens.lens (\DistributionConfig' {logging} -> logging) (\s@DistributionConfig' {} a -> s {logging = a} :: DistributionConfig)

-- | A complex type that contains information about origin groups for this
-- distribution.
distributionConfig_originGroups :: Lens.Lens' DistributionConfig (Prelude.Maybe OriginGroups)
distributionConfig_originGroups = Lens.lens (\DistributionConfig' {originGroups} -> originGroups) (\s@DistributionConfig' {} a -> s {originGroups = a} :: DistributionConfig)

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
distributionConfig_restrictions :: Lens.Lens' DistributionConfig (Prelude.Maybe Restrictions)
distributionConfig_restrictions = Lens.lens (\DistributionConfig' {restrictions} -> restrictions) (\s@DistributionConfig' {} a -> s {restrictions = a} :: DistributionConfig)

-- | If you want CloudFront to respond to IPv6 DNS requests with an IPv6
-- address for your distribution, specify @true@. If you specify @false@,
-- CloudFront responds to IPv6 DNS requests with the DNS response code
-- @NOERROR@ and with no IP addresses. This allows viewers to submit a
-- second request, for an IPv4 address for your distribution.
--
-- In general, you should enable IPv6 if you have users on IPv6 networks
-- who want to access your content. However, if you\'re using signed URLs
-- or signed cookies to restrict access to your content, and if you\'re
-- using a custom policy that includes the @IpAddress@ parameter to
-- restrict the IP addresses that can access your content, don\'t enable
-- IPv6. If you want to restrict access to some content by IP address and
-- not restrict access to other content (or restrict access but not by IP
-- address), you can create two distributions. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you\'re using an Amazon Route 53 alias resource record set to route
-- traffic to your CloudFront distribution, you need to create a second
-- alias resource record set when both of the following are true:
--
-- -   You enable IPv6 for the distribution
--
-- -   You\'re using alternate domain names in the URLs for your objects
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
-- in the /Amazon Route 53 Developer Guide/.
--
-- If you created a CNAME resource record set, either with Amazon Route 53
-- or with another DNS service, you don\'t need to make any changes. A
-- CNAME record will route traffic to your distribution regardless of the
-- IP address format of the viewer request.
distributionConfig_isIPV6Enabled :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Bool)
distributionConfig_isIPV6Enabled = Lens.lens (\DistributionConfig' {isIPV6Enabled} -> isIPV6Enabled) (\s@DistributionConfig' {} a -> s {isIPV6Enabled = a} :: DistributionConfig)

-- | A complex type that contains zero or more @CacheBehavior@ elements.
distributionConfig_cacheBehaviors :: Lens.Lens' DistributionConfig (Prelude.Maybe CacheBehaviors)
distributionConfig_cacheBehaviors = Lens.lens (\DistributionConfig' {cacheBehaviors} -> cacheBehaviors) (\s@DistributionConfig' {} a -> s {cacheBehaviors = a} :: DistributionConfig)

-- | The object that you want CloudFront to request from your origin (for
-- example, @index.html@) when a viewer requests the root URL for your
-- distribution (@http:\/\/www.example.com@) instead of an object in your
-- distribution (@http:\/\/www.example.com\/product-description.html@).
-- Specifying a default root object avoids exposing the contents of your
-- distribution.
--
-- Specify only the object name, for example, @index.html@. Don\'t add a
-- @\/@ before the object name.
--
-- If you don\'t want to specify a default root object when you create a
-- distribution, include an empty @DefaultRootObject@ element.
--
-- To delete the default root object from an existing distribution, update
-- the distribution configuration and include an empty @DefaultRootObject@
-- element.
--
-- To replace the default root object, update the distribution
-- configuration and specify the new object.
--
-- For more information about the default root object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object>
-- in the /Amazon CloudFront Developer Guide/.
distributionConfig_defaultRootObject :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Text)
distributionConfig_defaultRootObject = Lens.lens (\DistributionConfig' {defaultRootObject} -> defaultRootObject) (\s@DistributionConfig' {} a -> s {defaultRootObject = a} :: DistributionConfig)

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
distributionConfig_aliases :: Lens.Lens' DistributionConfig (Prelude.Maybe Aliases)
distributionConfig_aliases = Lens.lens (\DistributionConfig' {aliases} -> aliases) (\s@DistributionConfig' {} a -> s {aliases = a} :: DistributionConfig)

-- | (Optional) Specify the maximum HTTP version that you want viewers to use
-- to communicate with CloudFront. The default value for new web
-- distributions is http2. Viewers that don\'t support HTTP\/2
-- automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP\/2, viewers must support TLS 1.2
-- or later, and must support Server Name Identification (SNI).
--
-- In general, configuring CloudFront to communicate with viewers using
-- HTTP\/2 reduces latency. You can improve performance by optimizing for
-- HTTP\/2. For more information, do an Internet search for \"http\/2
-- optimization.\"
distributionConfig_httpVersion :: Lens.Lens' DistributionConfig (Prelude.Maybe HttpVersion)
distributionConfig_httpVersion = Lens.lens (\DistributionConfig' {httpVersion} -> httpVersion) (\s@DistributionConfig' {} a -> s {httpVersion = a} :: DistributionConfig)

-- | A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @DistributionConfig@ object), CloudFront creates a new distribution.
--
-- If @CallerReference@ is a value that you already sent in a previous
-- request to create a distribution, CloudFront returns a
-- @DistributionAlreadyExists@ error.
distributionConfig_callerReference :: Lens.Lens' DistributionConfig Prelude.Text
distributionConfig_callerReference = Lens.lens (\DistributionConfig' {callerReference} -> callerReference) (\s@DistributionConfig' {} a -> s {callerReference = a} :: DistributionConfig)

-- | A complex type that contains information about origins for this
-- distribution.
distributionConfig_origins :: Lens.Lens' DistributionConfig Origins
distributionConfig_origins = Lens.lens (\DistributionConfig' {origins} -> origins) (\s@DistributionConfig' {} a -> s {origins = a} :: DistributionConfig)

-- | A complex type that describes the default cache behavior if you don\'t
-- specify a @CacheBehavior@ element or if files don\'t match any of the
-- values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
distributionConfig_defaultCacheBehavior :: Lens.Lens' DistributionConfig DefaultCacheBehavior
distributionConfig_defaultCacheBehavior = Lens.lens (\DistributionConfig' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@DistributionConfig' {} a -> s {defaultCacheBehavior = a} :: DistributionConfig)

-- | Any comments you want to include about the distribution.
--
-- If you don\'t want to specify a comment, include an empty @Comment@
-- element.
--
-- To delete an existing comment, update the distribution configuration and
-- include an empty @Comment@ element.
--
-- To add or change a comment, update the distribution configuration and
-- specify the new comment.
distributionConfig_comment :: Lens.Lens' DistributionConfig Prelude.Text
distributionConfig_comment = Lens.lens (\DistributionConfig' {comment} -> comment) (\s@DistributionConfig' {} a -> s {comment = a} :: DistributionConfig) Prelude.. Prelude._Sensitive

-- | From this field, you can enable or disable the selected distribution.
distributionConfig_enabled :: Lens.Lens' DistributionConfig Prelude.Bool
distributionConfig_enabled = Lens.lens (\DistributionConfig' {enabled} -> enabled) (\s@DistributionConfig' {} a -> s {enabled = a} :: DistributionConfig)

instance Prelude.FromXML DistributionConfig where
  parseXML x =
    DistributionConfig'
      Prelude.<$> (x Prelude..@? "ViewerCertificate")
      Prelude.<*> (x Prelude..@? "CustomErrorResponses")
      Prelude.<*> (x Prelude..@? "WebACLId")
      Prelude.<*> (x Prelude..@? "PriceClass")
      Prelude.<*> (x Prelude..@? "Logging")
      Prelude.<*> (x Prelude..@? "OriginGroups")
      Prelude.<*> (x Prelude..@? "Restrictions")
      Prelude.<*> (x Prelude..@? "IsIPV6Enabled")
      Prelude.<*> (x Prelude..@? "CacheBehaviors")
      Prelude.<*> (x Prelude..@? "DefaultRootObject")
      Prelude.<*> (x Prelude..@? "Aliases")
      Prelude.<*> (x Prelude..@? "HttpVersion")
      Prelude.<*> (x Prelude..@ "CallerReference")
      Prelude.<*> (x Prelude..@ "Origins")
      Prelude.<*> (x Prelude..@ "DefaultCacheBehavior")
      Prelude.<*> (x Prelude..@ "Comment")
      Prelude.<*> (x Prelude..@ "Enabled")

instance Prelude.Hashable DistributionConfig

instance Prelude.NFData DistributionConfig

instance Prelude.ToXML DistributionConfig where
  toXML DistributionConfig' {..} =
    Prelude.mconcat
      [ "ViewerCertificate" Prelude.@= viewerCertificate,
        "CustomErrorResponses"
          Prelude.@= customErrorResponses,
        "WebACLId" Prelude.@= webACLId,
        "PriceClass" Prelude.@= priceClass,
        "Logging" Prelude.@= logging,
        "OriginGroups" Prelude.@= originGroups,
        "Restrictions" Prelude.@= restrictions,
        "IsIPV6Enabled" Prelude.@= isIPV6Enabled,
        "CacheBehaviors" Prelude.@= cacheBehaviors,
        "DefaultRootObject" Prelude.@= defaultRootObject,
        "Aliases" Prelude.@= aliases,
        "HttpVersion" Prelude.@= httpVersion,
        "CallerReference" Prelude.@= callerReference,
        "Origins" Prelude.@= origins,
        "DefaultCacheBehavior"
          Prelude.@= defaultCacheBehavior,
        "Comment" Prelude.@= comment,
        "Enabled" Prelude.@= enabled
      ]
