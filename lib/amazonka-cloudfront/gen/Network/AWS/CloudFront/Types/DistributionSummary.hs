{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionSummary
  ( DistributionSummary (..),

    -- * Smart constructor
    mkDistributionSummary,

    -- * Lenses
    dsId,
    dsARN,
    dsStatus,
    dsLastModifiedTime,
    dsDomainName,
    dsAliases,
    dsOrigins,
    dsDefaultCacheBehavior,
    dsCacheBehaviors,
    dsCustomErrorResponses,
    dsComment,
    dsPriceClass,
    dsEnabled,
    dsViewerCertificate,
    dsRestrictions,
    dsWebACLId,
    dsHttpVersion,
    dsIsIPV6Enabled,
    dsAliasICPRecordals,
    dsOriginGroups,
  )
where

import qualified Network.AWS.CloudFront.Types.AliasICPRecordal as Types
import qualified Network.AWS.CloudFront.Types.Aliases as Types
import qualified Network.AWS.CloudFront.Types.CacheBehaviors as Types
import qualified Network.AWS.CloudFront.Types.CustomErrorResponses as Types
import qualified Network.AWS.CloudFront.Types.DefaultCacheBehavior as Types
import qualified Network.AWS.CloudFront.Types.HttpVersion as Types
import qualified Network.AWS.CloudFront.Types.OriginGroups as Types
import qualified Network.AWS.CloudFront.Types.Origins as Types
import qualified Network.AWS.CloudFront.Types.PriceClass as Types
import qualified Network.AWS.CloudFront.Types.Restrictions as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.CloudFront.Types.ViewerCertificate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of the information about a CloudFront distribution.
--
-- /See:/ 'mkDistributionSummary' smart constructor.
data DistributionSummary = DistributionSummary'
  { -- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
    id :: Types.String,
    -- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
    arn :: Types.String,
    -- | The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
    status :: Types.String,
    -- | The date and time the distribution was last modified.
    lastModifiedTime :: Core.UTCTime,
    -- | The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
    domainName :: Types.String,
    -- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
    aliases :: Types.Aliases,
    -- | A complex type that contains information about origins for this distribution.
    origins :: Types.Origins,
    -- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
    defaultCacheBehavior :: Types.DefaultCacheBehavior,
    -- | A complex type that contains zero or more @CacheBehavior@ elements.
    cacheBehaviors :: Types.CacheBehaviors,
    -- | A complex type that contains zero or more @CustomErrorResponses@ elements.
    customErrorResponses :: Types.CustomErrorResponses,
    -- | The comment originally specified when this distribution was created.
    comment :: Types.String,
    -- | A complex type that contains information about price class for this streaming distribution.
    priceClass :: Types.PriceClass,
    -- | Whether the distribution is enabled to accept user requests for content.
    enabled :: Core.Bool,
    -- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
    viewerCertificate :: Types.ViewerCertificate,
    -- | A complex type that identifies ways in which you want to restrict distribution of your content.
    restrictions :: Types.Restrictions,
    -- | The Web ACL Id (if any) associated with the distribution.
    webACLId :: Types.String,
    -- | Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
    httpVersion :: Types.HttpVersion,
    -- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
    isIPV6Enabled :: Core.Bool,
    -- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
    --
    -- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
    aliasICPRecordals :: Core.Maybe [Types.AliasICPRecordal],
    -- | A complex type that contains information about origin groups for this distribution.
    originGroups :: Core.Maybe Types.OriginGroups
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DistributionSummary' value with any optional fields omitted.
mkDistributionSummary ::
  -- | 'id'
  Types.String ->
  -- | 'arn'
  Types.String ->
  -- | 'status'
  Types.String ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'domainName'
  Types.String ->
  -- | 'aliases'
  Types.Aliases ->
  -- | 'origins'
  Types.Origins ->
  -- | 'defaultCacheBehavior'
  Types.DefaultCacheBehavior ->
  -- | 'cacheBehaviors'
  Types.CacheBehaviors ->
  -- | 'customErrorResponses'
  Types.CustomErrorResponses ->
  -- | 'comment'
  Types.String ->
  -- | 'priceClass'
  Types.PriceClass ->
  -- | 'enabled'
  Core.Bool ->
  -- | 'viewerCertificate'
  Types.ViewerCertificate ->
  -- | 'restrictions'
  Types.Restrictions ->
  -- | 'webACLId'
  Types.String ->
  -- | 'httpVersion'
  Types.HttpVersion ->
  -- | 'isIPV6Enabled'
  Core.Bool ->
  DistributionSummary
mkDistributionSummary
  id
  arn
  status
  lastModifiedTime
  domainName
  aliases
  origins
  defaultCacheBehavior
  cacheBehaviors
  customErrorResponses
  comment
  priceClass
  enabled
  viewerCertificate
  restrictions
  webACLId
  httpVersion
  isIPV6Enabled =
    DistributionSummary'
      { id,
        arn,
        status,
        lastModifiedTime,
        domainName,
        aliases,
        origins,
        defaultCacheBehavior,
        cacheBehaviors,
        customErrorResponses,
        comment,
        priceClass,
        enabled,
        viewerCertificate,
        restrictions,
        webACLId,
        httpVersion,
        isIPV6Enabled,
        aliasICPRecordals = Core.Nothing,
        originGroups = Core.Nothing
      }

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsId :: Lens.Lens' DistributionSummary Types.String
dsId = Lens.field @"id"
{-# DEPRECATED dsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsARN :: Lens.Lens' DistributionSummary Types.String
dsARN = Lens.field @"arn"
{-# DEPRECATED dsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStatus :: Lens.Lens' DistributionSummary Types.String
dsStatus = Lens.field @"status"
{-# DEPRECATED dsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the distribution was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastModifiedTime :: Lens.Lens' DistributionSummary Core.UTCTime
dsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DistributionSummary Types.String
dsDomainName = Lens.field @"domainName"
{-# DEPRECATED dsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAliases :: Lens.Lens' DistributionSummary Types.Aliases
dsAliases = Lens.field @"aliases"
{-# DEPRECATED dsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A complex type that contains information about origins for this distribution.
--
-- /Note:/ Consider using 'origins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsOrigins :: Lens.Lens' DistributionSummary Types.Origins
dsOrigins = Lens.field @"origins"
{-# DEPRECATED dsOrigins "Use generic-lens or generic-optics with 'origins' instead." #-}

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDefaultCacheBehavior :: Lens.Lens' DistributionSummary Types.DefaultCacheBehavior
dsDefaultCacheBehavior = Lens.field @"defaultCacheBehavior"
{-# DEPRECATED dsDefaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead." #-}

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCacheBehaviors :: Lens.Lens' DistributionSummary Types.CacheBehaviors
dsCacheBehaviors = Lens.field @"cacheBehaviors"
{-# DEPRECATED dsCacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead." #-}

-- | A complex type that contains zero or more @CustomErrorResponses@ elements.
--
-- /Note:/ Consider using 'customErrorResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCustomErrorResponses :: Lens.Lens' DistributionSummary Types.CustomErrorResponses
dsCustomErrorResponses = Lens.field @"customErrorResponses"
{-# DEPRECATED dsCustomErrorResponses "Use generic-lens or generic-optics with 'customErrorResponses' instead." #-}

-- | The comment originally specified when this distribution was created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsComment :: Lens.Lens' DistributionSummary Types.String
dsComment = Lens.field @"comment"
{-# DEPRECATED dsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A complex type that contains information about price class for this streaming distribution.
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsPriceClass :: Lens.Lens' DistributionSummary Types.PriceClass
dsPriceClass = Lens.field @"priceClass"
{-# DEPRECATED dsPriceClass "Use generic-lens or generic-optics with 'priceClass' instead." #-}

-- | Whether the distribution is enabled to accept user requests for content.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEnabled :: Lens.Lens' DistributionSummary Core.Bool
dsEnabled = Lens.field @"enabled"
{-# DEPRECATED dsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- /Note:/ Consider using 'viewerCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsViewerCertificate :: Lens.Lens' DistributionSummary Types.ViewerCertificate
dsViewerCertificate = Lens.field @"viewerCertificate"
{-# DEPRECATED dsViewerCertificate "Use generic-lens or generic-optics with 'viewerCertificate' instead." #-}

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- /Note:/ Consider using 'restrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRestrictions :: Lens.Lens' DistributionSummary Types.Restrictions
dsRestrictions = Lens.field @"restrictions"
{-# DEPRECATED dsRestrictions "Use generic-lens or generic-optics with 'restrictions' instead." #-}

-- | The Web ACL Id (if any) associated with the distribution.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsWebACLId :: Lens.Lens' DistributionSummary Types.String
dsWebACLId = Lens.field @"webACLId"
{-# DEPRECATED dsWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
--
-- /Note:/ Consider using 'httpVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsHttpVersion :: Lens.Lens' DistributionSummary Types.HttpVersion
dsHttpVersion = Lens.field @"httpVersion"
{-# DEPRECATED dsHttpVersion "Use generic-lens or generic-optics with 'httpVersion' instead." #-}

-- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
--
-- /Note:/ Consider using 'isIPV6Enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsIsIPV6Enabled :: Lens.Lens' DistributionSummary Core.Bool
dsIsIPV6Enabled = Lens.field @"isIPV6Enabled"
{-# DEPRECATED dsIsIPV6Enabled "Use generic-lens or generic-optics with 'isIPV6Enabled' instead." #-}

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- /Note:/ Consider using 'aliasICPRecordals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAliasICPRecordals :: Lens.Lens' DistributionSummary (Core.Maybe [Types.AliasICPRecordal])
dsAliasICPRecordals = Lens.field @"aliasICPRecordals"
{-# DEPRECATED dsAliasICPRecordals "Use generic-lens or generic-optics with 'aliasICPRecordals' instead." #-}

-- | A complex type that contains information about origin groups for this distribution.
--
-- /Note:/ Consider using 'originGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsOriginGroups :: Lens.Lens' DistributionSummary (Core.Maybe Types.OriginGroups)
dsOriginGroups = Lens.field @"originGroups"
{-# DEPRECATED dsOriginGroups "Use generic-lens or generic-optics with 'originGroups' instead." #-}

instance Core.FromXML DistributionSummary where
  parseXML x =
    DistributionSummary'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "ARN")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "DomainName")
      Core.<*> (x Core..@ "Aliases")
      Core.<*> (x Core..@ "Origins")
      Core.<*> (x Core..@ "DefaultCacheBehavior")
      Core.<*> (x Core..@ "CacheBehaviors")
      Core.<*> (x Core..@ "CustomErrorResponses")
      Core.<*> (x Core..@ "Comment")
      Core.<*> (x Core..@ "PriceClass")
      Core.<*> (x Core..@ "Enabled")
      Core.<*> (x Core..@ "ViewerCertificate")
      Core.<*> (x Core..@ "Restrictions")
      Core.<*> (x Core..@ "WebACLId")
      Core.<*> (x Core..@ "HttpVersion")
      Core.<*> (x Core..@ "IsIPV6Enabled")
      Core.<*> ( x Core..@? "AliasICPRecordals"
                   Core..<@> Core.parseXMLList "AliasICPRecordal"
               )
      Core.<*> (x Core..@? "OriginGroups")
