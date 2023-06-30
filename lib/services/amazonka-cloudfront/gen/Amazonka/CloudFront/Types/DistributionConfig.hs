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
-- Module      : Amazonka.CloudFront.Types.DistributionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.DistributionConfig where

import Amazonka.CloudFront.Types.Aliases
import Amazonka.CloudFront.Types.CacheBehaviors
import Amazonka.CloudFront.Types.CustomErrorResponses
import Amazonka.CloudFront.Types.DefaultCacheBehavior
import Amazonka.CloudFront.Types.HttpVersion
import Amazonka.CloudFront.Types.LoggingConfig
import Amazonka.CloudFront.Types.OriginGroups
import Amazonka.CloudFront.Types.Origins
import Amazonka.CloudFront.Types.PriceClass
import Amazonka.CloudFront.Types.Restrictions
import Amazonka.CloudFront.Types.ViewerCertificate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A distribution configuration.
--
-- /See:/ 'newDistributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
  { -- | A complex type that contains information about CNAMEs (alternate domain
    -- names), if any, for this distribution.
    aliases :: Prelude.Maybe Aliases,
    -- | A complex type that contains zero or more @CacheBehavior@ elements.
    cacheBehaviors :: Prelude.Maybe CacheBehaviors,
    -- | The identifier of a continuous deployment policy. For more information,
    -- see @CreateContinuousDeploymentPolicy@.
    continuousDeploymentPolicyId :: Prelude.Maybe Prelude.Text,
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
    -- | The object that you want CloudFront to request from your origin (for
    -- example, @index.html@) when a viewer requests the root URL for your
    -- distribution (@https:\/\/www.example.com@) instead of an object in your
    -- distribution (@https:\/\/www.example.com\/product-description.html@).
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
    -- | (Optional) Specify the maximum HTTP version(s) that you want viewers to
    -- use to communicate with CloudFront. The default value for new web
    -- distributions is @http2@. Viewers that don\'t support HTTP\/2
    -- automatically use an earlier HTTP version.
    --
    -- For viewers and CloudFront to use HTTP\/2, viewers must support TLSv1.2
    -- or later, and must support Server Name Indication (SNI).
    --
    -- For viewers and CloudFront to use HTTP\/3, viewers must support TLSv1.3
    -- and Server Name Indication (SNI). CloudFront supports HTTP\/3 connection
    -- migration to allow the viewer to switch networks without losing
    -- connection. For more information about connection migration, see
    -- <https://www.rfc-editor.org/rfc/rfc9000.html#name-connection-migration Connection Migration>
    -- at RFC 9000. For more information about supported TLSv1.3 ciphers, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html Supported protocols and ciphers between viewers and CloudFront>.
    httpVersion :: Prelude.Maybe HttpVersion,
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
    -- If you\'re using an Route 53 Amazon Web Services Integration alias
    -- resource record set to route traffic to your CloudFront distribution,
    -- you need to create a second alias resource record set when both of the
    -- following are true:
    --
    -- -   You enable IPv6 for the distribution
    --
    -- -   You\'re using alternate domain names in the URLs for your objects
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
    -- in the /Route 53 Amazon Web Services Integration Developer Guide/.
    --
    -- If you created a CNAME resource record set, either with Route 53 Amazon
    -- Web Services Integration or with another DNS service, you don\'t need to
    -- make any changes. A CNAME record will route traffic to your distribution
    -- regardless of the IP address format of the viewer request.
    isIPV6Enabled :: Prelude.Maybe Prelude.Bool,
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
    -- | A complex type that identifies ways in which you want to restrict
    -- distribution of your content.
    restrictions :: Prelude.Maybe Restrictions,
    -- | A Boolean that indicates whether this is a staging distribution. When
    -- this value is @true@, this is a staging distribution. When this value is
    -- @false@, this is not a staging distribution.
    staging :: Prelude.Maybe Prelude.Bool,
    -- | A complex type that determines the distribution\'s SSL\/TLS
    -- configuration for communicating with viewers.
    viewerCertificate :: Prelude.Maybe ViewerCertificate,
    -- | A unique identifier that specifies the WAF web ACL, if any, to associate
    -- with this distribution. To specify a web ACL created using the latest
    -- version of WAF, use the ACL ARN, for example
    -- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
    -- To specify a web ACL created using WAF Classic, use the ACL ID, for
    -- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
    --
    -- WAF is a web application firewall that lets you monitor the HTTP and
    -- HTTPS requests that are forwarded to CloudFront, and lets you control
    -- access to your content. Based on conditions that you specify, such as
    -- the IP addresses that requests originate from or the values of query
    -- strings, CloudFront responds to requests either with the requested
    -- content or with an HTTP 403 status code (Forbidden). You can also
    -- configure CloudFront to return a custom error page when a request is
    -- blocked. For more information about WAF, see the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html WAF Developer Guide>.
    webACLId :: Prelude.Maybe Prelude.Text,
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
    -- | A comment to describe the distribution. The comment cannot be longer
    -- than 128 characters.
    comment :: Data.Sensitive Prelude.Text,
    -- | From this field, you can enable or disable the selected distribution.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliases', 'distributionConfig_aliases' - A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- 'cacheBehaviors', 'distributionConfig_cacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- 'continuousDeploymentPolicyId', 'distributionConfig_continuousDeploymentPolicyId' - The identifier of a continuous deployment policy. For more information,
-- see @CreateContinuousDeploymentPolicy@.
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
-- 'defaultRootObject', 'distributionConfig_defaultRootObject' - The object that you want CloudFront to request from your origin (for
-- example, @index.html@) when a viewer requests the root URL for your
-- distribution (@https:\/\/www.example.com@) instead of an object in your
-- distribution (@https:\/\/www.example.com\/product-description.html@).
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
-- 'httpVersion', 'distributionConfig_httpVersion' - (Optional) Specify the maximum HTTP version(s) that you want viewers to
-- use to communicate with CloudFront. The default value for new web
-- distributions is @http2@. Viewers that don\'t support HTTP\/2
-- automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP\/2, viewers must support TLSv1.2
-- or later, and must support Server Name Indication (SNI).
--
-- For viewers and CloudFront to use HTTP\/3, viewers must support TLSv1.3
-- and Server Name Indication (SNI). CloudFront supports HTTP\/3 connection
-- migration to allow the viewer to switch networks without losing
-- connection. For more information about connection migration, see
-- <https://www.rfc-editor.org/rfc/rfc9000.html#name-connection-migration Connection Migration>
-- at RFC 9000. For more information about supported TLSv1.3 ciphers, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html Supported protocols and ciphers between viewers and CloudFront>.
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
-- If you\'re using an Route 53 Amazon Web Services Integration alias
-- resource record set to route traffic to your CloudFront distribution,
-- you need to create a second alias resource record set when both of the
-- following are true:
--
-- -   You enable IPv6 for the distribution
--
-- -   You\'re using alternate domain names in the URLs for your objects
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
-- in the /Route 53 Amazon Web Services Integration Developer Guide/.
--
-- If you created a CNAME resource record set, either with Route 53 Amazon
-- Web Services Integration or with another DNS service, you don\'t need to
-- make any changes. A CNAME record will route traffic to your distribution
-- regardless of the IP address format of the viewer request.
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
-- 'restrictions', 'distributionConfig_restrictions' - A complex type that identifies ways in which you want to restrict
-- distribution of your content.
--
-- 'staging', 'distributionConfig_staging' - A Boolean that indicates whether this is a staging distribution. When
-- this value is @true@, this is a staging distribution. When this value is
-- @false@, this is not a staging distribution.
--
-- 'viewerCertificate', 'distributionConfig_viewerCertificate' - A complex type that determines the distribution\'s SSL\/TLS
-- configuration for communicating with viewers.
--
-- 'webACLId', 'distributionConfig_webACLId' - A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution. To specify a web ACL created using the latest
-- version of WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
--
-- WAF is a web application firewall that lets you monitor the HTTP and
-- HTTPS requests that are forwarded to CloudFront, and lets you control
-- access to your content. Based on conditions that you specify, such as
-- the IP addresses that requests originate from or the values of query
-- strings, CloudFront responds to requests either with the requested
-- content or with an HTTP 403 status code (Forbidden). You can also
-- configure CloudFront to return a custom error page when a request is
-- blocked. For more information about WAF, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html WAF Developer Guide>.
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
-- 'comment', 'distributionConfig_comment' - A comment to describe the distribution. The comment cannot be longer
-- than 128 characters.
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
      { aliases = Prelude.Nothing,
        cacheBehaviors = Prelude.Nothing,
        continuousDeploymentPolicyId = Prelude.Nothing,
        customErrorResponses = Prelude.Nothing,
        defaultRootObject = Prelude.Nothing,
        httpVersion = Prelude.Nothing,
        isIPV6Enabled = Prelude.Nothing,
        logging = Prelude.Nothing,
        originGroups = Prelude.Nothing,
        priceClass = Prelude.Nothing,
        restrictions = Prelude.Nothing,
        staging = Prelude.Nothing,
        viewerCertificate = Prelude.Nothing,
        webACLId = Prelude.Nothing,
        callerReference = pCallerReference_,
        origins = pOrigins_,
        defaultCacheBehavior = pDefaultCacheBehavior_,
        comment = Data._Sensitive Lens.# pComment_,
        enabled = pEnabled_
      }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
distributionConfig_aliases :: Lens.Lens' DistributionConfig (Prelude.Maybe Aliases)
distributionConfig_aliases = Lens.lens (\DistributionConfig' {aliases} -> aliases) (\s@DistributionConfig' {} a -> s {aliases = a} :: DistributionConfig)

-- | A complex type that contains zero or more @CacheBehavior@ elements.
distributionConfig_cacheBehaviors :: Lens.Lens' DistributionConfig (Prelude.Maybe CacheBehaviors)
distributionConfig_cacheBehaviors = Lens.lens (\DistributionConfig' {cacheBehaviors} -> cacheBehaviors) (\s@DistributionConfig' {} a -> s {cacheBehaviors = a} :: DistributionConfig)

-- | The identifier of a continuous deployment policy. For more information,
-- see @CreateContinuousDeploymentPolicy@.
distributionConfig_continuousDeploymentPolicyId :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Text)
distributionConfig_continuousDeploymentPolicyId = Lens.lens (\DistributionConfig' {continuousDeploymentPolicyId} -> continuousDeploymentPolicyId) (\s@DistributionConfig' {} a -> s {continuousDeploymentPolicyId = a} :: DistributionConfig)

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

-- | The object that you want CloudFront to request from your origin (for
-- example, @index.html@) when a viewer requests the root URL for your
-- distribution (@https:\/\/www.example.com@) instead of an object in your
-- distribution (@https:\/\/www.example.com\/product-description.html@).
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

-- | (Optional) Specify the maximum HTTP version(s) that you want viewers to
-- use to communicate with CloudFront. The default value for new web
-- distributions is @http2@. Viewers that don\'t support HTTP\/2
-- automatically use an earlier HTTP version.
--
-- For viewers and CloudFront to use HTTP\/2, viewers must support TLSv1.2
-- or later, and must support Server Name Indication (SNI).
--
-- For viewers and CloudFront to use HTTP\/3, viewers must support TLSv1.3
-- and Server Name Indication (SNI). CloudFront supports HTTP\/3 connection
-- migration to allow the viewer to switch networks without losing
-- connection. For more information about connection migration, see
-- <https://www.rfc-editor.org/rfc/rfc9000.html#name-connection-migration Connection Migration>
-- at RFC 9000. For more information about supported TLSv1.3 ciphers, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html Supported protocols and ciphers between viewers and CloudFront>.
distributionConfig_httpVersion :: Lens.Lens' DistributionConfig (Prelude.Maybe HttpVersion)
distributionConfig_httpVersion = Lens.lens (\DistributionConfig' {httpVersion} -> httpVersion) (\s@DistributionConfig' {} a -> s {httpVersion = a} :: DistributionConfig)

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
-- If you\'re using an Route 53 Amazon Web Services Integration alias
-- resource record set to route traffic to your CloudFront distribution,
-- you need to create a second alias resource record set when both of the
-- following are true:
--
-- -   You enable IPv6 for the distribution
--
-- -   You\'re using alternate domain names in the URLs for your objects
--
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name>
-- in the /Route 53 Amazon Web Services Integration Developer Guide/.
--
-- If you created a CNAME resource record set, either with Route 53 Amazon
-- Web Services Integration or with another DNS service, you don\'t need to
-- make any changes. A CNAME record will route traffic to your distribution
-- regardless of the IP address format of the viewer request.
distributionConfig_isIPV6Enabled :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Bool)
distributionConfig_isIPV6Enabled = Lens.lens (\DistributionConfig' {isIPV6Enabled} -> isIPV6Enabled) (\s@DistributionConfig' {} a -> s {isIPV6Enabled = a} :: DistributionConfig)

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

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
distributionConfig_restrictions :: Lens.Lens' DistributionConfig (Prelude.Maybe Restrictions)
distributionConfig_restrictions = Lens.lens (\DistributionConfig' {restrictions} -> restrictions) (\s@DistributionConfig' {} a -> s {restrictions = a} :: DistributionConfig)

-- | A Boolean that indicates whether this is a staging distribution. When
-- this value is @true@, this is a staging distribution. When this value is
-- @false@, this is not a staging distribution.
distributionConfig_staging :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Bool)
distributionConfig_staging = Lens.lens (\DistributionConfig' {staging} -> staging) (\s@DistributionConfig' {} a -> s {staging = a} :: DistributionConfig)

-- | A complex type that determines the distribution\'s SSL\/TLS
-- configuration for communicating with viewers.
distributionConfig_viewerCertificate :: Lens.Lens' DistributionConfig (Prelude.Maybe ViewerCertificate)
distributionConfig_viewerCertificate = Lens.lens (\DistributionConfig' {viewerCertificate} -> viewerCertificate) (\s@DistributionConfig' {} a -> s {viewerCertificate = a} :: DistributionConfig)

-- | A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution. To specify a web ACL created using the latest
-- version of WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
--
-- WAF is a web application firewall that lets you monitor the HTTP and
-- HTTPS requests that are forwarded to CloudFront, and lets you control
-- access to your content. Based on conditions that you specify, such as
-- the IP addresses that requests originate from or the values of query
-- strings, CloudFront responds to requests either with the requested
-- content or with an HTTP 403 status code (Forbidden). You can also
-- configure CloudFront to return a custom error page when a request is
-- blocked. For more information about WAF, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html WAF Developer Guide>.
distributionConfig_webACLId :: Lens.Lens' DistributionConfig (Prelude.Maybe Prelude.Text)
distributionConfig_webACLId = Lens.lens (\DistributionConfig' {webACLId} -> webACLId) (\s@DistributionConfig' {} a -> s {webACLId = a} :: DistributionConfig)

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

-- | A comment to describe the distribution. The comment cannot be longer
-- than 128 characters.
distributionConfig_comment :: Lens.Lens' DistributionConfig Prelude.Text
distributionConfig_comment = Lens.lens (\DistributionConfig' {comment} -> comment) (\s@DistributionConfig' {} a -> s {comment = a} :: DistributionConfig) Prelude.. Data._Sensitive

-- | From this field, you can enable or disable the selected distribution.
distributionConfig_enabled :: Lens.Lens' DistributionConfig Prelude.Bool
distributionConfig_enabled = Lens.lens (\DistributionConfig' {enabled} -> enabled) (\s@DistributionConfig' {} a -> s {enabled = a} :: DistributionConfig)

instance Data.FromXML DistributionConfig where
  parseXML x =
    DistributionConfig'
      Prelude.<$> (x Data..@? "Aliases")
      Prelude.<*> (x Data..@? "CacheBehaviors")
      Prelude.<*> (x Data..@? "ContinuousDeploymentPolicyId")
      Prelude.<*> (x Data..@? "CustomErrorResponses")
      Prelude.<*> (x Data..@? "DefaultRootObject")
      Prelude.<*> (x Data..@? "HttpVersion")
      Prelude.<*> (x Data..@? "IsIPV6Enabled")
      Prelude.<*> (x Data..@? "Logging")
      Prelude.<*> (x Data..@? "OriginGroups")
      Prelude.<*> (x Data..@? "PriceClass")
      Prelude.<*> (x Data..@? "Restrictions")
      Prelude.<*> (x Data..@? "Staging")
      Prelude.<*> (x Data..@? "ViewerCertificate")
      Prelude.<*> (x Data..@? "WebACLId")
      Prelude.<*> (x Data..@ "CallerReference")
      Prelude.<*> (x Data..@ "Origins")
      Prelude.<*> (x Data..@ "DefaultCacheBehavior")
      Prelude.<*> (x Data..@ "Comment")
      Prelude.<*> (x Data..@ "Enabled")

instance Prelude.Hashable DistributionConfig where
  hashWithSalt _salt DistributionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` aliases
      `Prelude.hashWithSalt` cacheBehaviors
      `Prelude.hashWithSalt` continuousDeploymentPolicyId
      `Prelude.hashWithSalt` customErrorResponses
      `Prelude.hashWithSalt` defaultRootObject
      `Prelude.hashWithSalt` httpVersion
      `Prelude.hashWithSalt` isIPV6Enabled
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` originGroups
      `Prelude.hashWithSalt` priceClass
      `Prelude.hashWithSalt` restrictions
      `Prelude.hashWithSalt` staging
      `Prelude.hashWithSalt` viewerCertificate
      `Prelude.hashWithSalt` webACLId
      `Prelude.hashWithSalt` callerReference
      `Prelude.hashWithSalt` origins
      `Prelude.hashWithSalt` defaultCacheBehavior
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData DistributionConfig where
  rnf DistributionConfig' {..} =
    Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf cacheBehaviors
      `Prelude.seq` Prelude.rnf continuousDeploymentPolicyId
      `Prelude.seq` Prelude.rnf customErrorResponses
      `Prelude.seq` Prelude.rnf defaultRootObject
      `Prelude.seq` Prelude.rnf httpVersion
      `Prelude.seq` Prelude.rnf isIPV6Enabled
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf originGroups
      `Prelude.seq` Prelude.rnf priceClass
      `Prelude.seq` Prelude.rnf restrictions
      `Prelude.seq` Prelude.rnf staging
      `Prelude.seq` Prelude.rnf viewerCertificate
      `Prelude.seq` Prelude.rnf webACLId
      `Prelude.seq` Prelude.rnf callerReference
      `Prelude.seq` Prelude.rnf origins
      `Prelude.seq` Prelude.rnf defaultCacheBehavior
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToXML DistributionConfig where
  toXML DistributionConfig' {..} =
    Prelude.mconcat
      [ "Aliases" Data.@= aliases,
        "CacheBehaviors" Data.@= cacheBehaviors,
        "ContinuousDeploymentPolicyId"
          Data.@= continuousDeploymentPolicyId,
        "CustomErrorResponses" Data.@= customErrorResponses,
        "DefaultRootObject" Data.@= defaultRootObject,
        "HttpVersion" Data.@= httpVersion,
        "IsIPV6Enabled" Data.@= isIPV6Enabled,
        "Logging" Data.@= logging,
        "OriginGroups" Data.@= originGroups,
        "PriceClass" Data.@= priceClass,
        "Restrictions" Data.@= restrictions,
        "Staging" Data.@= staging,
        "ViewerCertificate" Data.@= viewerCertificate,
        "WebACLId" Data.@= webACLId,
        "CallerReference" Data.@= callerReference,
        "Origins" Data.@= origins,
        "DefaultCacheBehavior" Data.@= defaultCacheBehavior,
        "Comment" Data.@= comment,
        "Enabled" Data.@= enabled
      ]
