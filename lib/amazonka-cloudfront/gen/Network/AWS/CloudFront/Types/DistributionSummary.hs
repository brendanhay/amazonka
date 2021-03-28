{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.DistributionSummary
  ( DistributionSummary (..)
  -- * Smart constructor
  , mkDistributionSummary
  -- * Lenses
  , dsId
  , dsARN
  , dsStatus
  , dsLastModifiedTime
  , dsDomainName
  , dsAliases
  , dsOrigins
  , dsDefaultCacheBehavior
  , dsCacheBehaviors
  , dsCustomErrorResponses
  , dsComment
  , dsPriceClass
  , dsEnabled
  , dsViewerCertificate
  , dsRestrictions
  , dsWebACLId
  , dsHttpVersion
  , dsIsIPV6Enabled
  , dsAliasICPRecordals
  , dsOriginGroups
  ) where

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
import qualified Network.AWS.CloudFront.Types.ViewerCertificate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of the information about a CloudFront distribution.
--
-- /See:/ 'mkDistributionSummary' smart constructor.
data DistributionSummary = DistributionSummary'
  { id :: Core.Text
    -- ^ The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
  , arn :: Core.Text
    -- ^ The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
  , status :: Core.Text
    -- ^ The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
  , lastModifiedTime :: Core.UTCTime
    -- ^ The date and time the distribution was last modified.
  , domainName :: Core.Text
    -- ^ The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
  , aliases :: Types.Aliases
    -- ^ A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
  , origins :: Types.Origins
    -- ^ A complex type that contains information about origins for this distribution.
  , defaultCacheBehavior :: Types.DefaultCacheBehavior
    -- ^ A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
  , cacheBehaviors :: Types.CacheBehaviors
    -- ^ A complex type that contains zero or more @CacheBehavior@ elements.
  , customErrorResponses :: Types.CustomErrorResponses
    -- ^ A complex type that contains zero or more @CustomErrorResponses@ elements.
  , comment :: Core.Text
    -- ^ The comment originally specified when this distribution was created.
  , priceClass :: Types.PriceClass
    -- ^ A complex type that contains information about price class for this streaming distribution. 
  , enabled :: Core.Bool
    -- ^ Whether the distribution is enabled to accept user requests for content.
  , viewerCertificate :: Types.ViewerCertificate
    -- ^ A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
  , restrictions :: Types.Restrictions
    -- ^ A complex type that identifies ways in which you want to restrict distribution of your content.
  , webACLId :: Core.Text
    -- ^ The Web ACL Id (if any) associated with the distribution.
  , httpVersion :: Types.HttpVersion
    -- ^ Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
  , isIPV6Enabled :: Core.Bool
    -- ^ Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
  , aliasICPRecordals :: Core.Maybe [Types.AliasICPRecordal]
    -- ^ AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
  , originGroups :: Core.Maybe Types.OriginGroups
    -- ^ A complex type that contains information about origin groups for this distribution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DistributionSummary' value with any optional fields omitted.
mkDistributionSummary
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'arn'
    -> Core.Text -- ^ 'status'
    -> Core.UTCTime -- ^ 'lastModifiedTime'
    -> Core.Text -- ^ 'domainName'
    -> Types.Aliases -- ^ 'aliases'
    -> Types.Origins -- ^ 'origins'
    -> Types.DefaultCacheBehavior -- ^ 'defaultCacheBehavior'
    -> Types.CacheBehaviors -- ^ 'cacheBehaviors'
    -> Types.CustomErrorResponses -- ^ 'customErrorResponses'
    -> Core.Text -- ^ 'comment'
    -> Types.PriceClass -- ^ 'priceClass'
    -> Core.Bool -- ^ 'enabled'
    -> Types.ViewerCertificate -- ^ 'viewerCertificate'
    -> Types.Restrictions -- ^ 'restrictions'
    -> Core.Text -- ^ 'webACLId'
    -> Types.HttpVersion -- ^ 'httpVersion'
    -> Core.Bool -- ^ 'isIPV6Enabled'
    -> DistributionSummary
mkDistributionSummary id arn status lastModifiedTime domainName
  aliases origins defaultCacheBehavior cacheBehaviors
  customErrorResponses comment priceClass enabled viewerCertificate
  restrictions webACLId httpVersion isIPV6Enabled
  = DistributionSummary'{id, arn, status, lastModifiedTime,
                         domainName, aliases, origins, defaultCacheBehavior, cacheBehaviors,
                         customErrorResponses, comment, priceClass, enabled,
                         viewerCertificate, restrictions, webACLId, httpVersion,
                         isIPV6Enabled, aliasICPRecordals = Core.Nothing,
                         originGroups = Core.Nothing}

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsId :: Lens.Lens' DistributionSummary Core.Text
dsId = Lens.field @"id"
{-# INLINEABLE dsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsARN :: Lens.Lens' DistributionSummary Core.Text
dsARN = Lens.field @"arn"
{-# INLINEABLE dsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStatus :: Lens.Lens' DistributionSummary Core.Text
dsStatus = Lens.field @"status"
{-# INLINEABLE dsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date and time the distribution was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastModifiedTime :: Lens.Lens' DistributionSummary Core.UTCTime
dsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DistributionSummary Core.Text
dsDomainName = Lens.field @"domainName"
{-# INLINEABLE dsDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAliases :: Lens.Lens' DistributionSummary Types.Aliases
dsAliases = Lens.field @"aliases"
{-# INLINEABLE dsAliases #-}
{-# DEPRECATED aliases "Use generic-lens or generic-optics with 'aliases' instead"  #-}

-- | A complex type that contains information about origins for this distribution.
--
-- /Note:/ Consider using 'origins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsOrigins :: Lens.Lens' DistributionSummary Types.Origins
dsOrigins = Lens.field @"origins"
{-# INLINEABLE dsOrigins #-}
{-# DEPRECATED origins "Use generic-lens or generic-optics with 'origins' instead"  #-}

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDefaultCacheBehavior :: Lens.Lens' DistributionSummary Types.DefaultCacheBehavior
dsDefaultCacheBehavior = Lens.field @"defaultCacheBehavior"
{-# INLINEABLE dsDefaultCacheBehavior #-}
{-# DEPRECATED defaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead"  #-}

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCacheBehaviors :: Lens.Lens' DistributionSummary Types.CacheBehaviors
dsCacheBehaviors = Lens.field @"cacheBehaviors"
{-# INLINEABLE dsCacheBehaviors #-}
{-# DEPRECATED cacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead"  #-}

-- | A complex type that contains zero or more @CustomErrorResponses@ elements.
--
-- /Note:/ Consider using 'customErrorResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCustomErrorResponses :: Lens.Lens' DistributionSummary Types.CustomErrorResponses
dsCustomErrorResponses = Lens.field @"customErrorResponses"
{-# INLINEABLE dsCustomErrorResponses #-}
{-# DEPRECATED customErrorResponses "Use generic-lens or generic-optics with 'customErrorResponses' instead"  #-}

-- | The comment originally specified when this distribution was created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsComment :: Lens.Lens' DistributionSummary Core.Text
dsComment = Lens.field @"comment"
{-# INLINEABLE dsComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | A complex type that contains information about price class for this streaming distribution. 
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsPriceClass :: Lens.Lens' DistributionSummary Types.PriceClass
dsPriceClass = Lens.field @"priceClass"
{-# INLINEABLE dsPriceClass #-}
{-# DEPRECATED priceClass "Use generic-lens or generic-optics with 'priceClass' instead"  #-}

-- | Whether the distribution is enabled to accept user requests for content.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEnabled :: Lens.Lens' DistributionSummary Core.Bool
dsEnabled = Lens.field @"enabled"
{-# INLINEABLE dsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- /Note:/ Consider using 'viewerCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsViewerCertificate :: Lens.Lens' DistributionSummary Types.ViewerCertificate
dsViewerCertificate = Lens.field @"viewerCertificate"
{-# INLINEABLE dsViewerCertificate #-}
{-# DEPRECATED viewerCertificate "Use generic-lens or generic-optics with 'viewerCertificate' instead"  #-}

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- /Note:/ Consider using 'restrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRestrictions :: Lens.Lens' DistributionSummary Types.Restrictions
dsRestrictions = Lens.field @"restrictions"
{-# INLINEABLE dsRestrictions #-}
{-# DEPRECATED restrictions "Use generic-lens or generic-optics with 'restrictions' instead"  #-}

-- | The Web ACL Id (if any) associated with the distribution.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsWebACLId :: Lens.Lens' DistributionSummary Core.Text
dsWebACLId = Lens.field @"webACLId"
{-# INLINEABLE dsWebACLId #-}
{-# DEPRECATED webACLId "Use generic-lens or generic-optics with 'webACLId' instead"  #-}

-- | Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
--
-- /Note:/ Consider using 'httpVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsHttpVersion :: Lens.Lens' DistributionSummary Types.HttpVersion
dsHttpVersion = Lens.field @"httpVersion"
{-# INLINEABLE dsHttpVersion #-}
{-# DEPRECATED httpVersion "Use generic-lens or generic-optics with 'httpVersion' instead"  #-}

-- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
--
-- /Note:/ Consider using 'isIPV6Enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsIsIPV6Enabled :: Lens.Lens' DistributionSummary Core.Bool
dsIsIPV6Enabled = Lens.field @"isIPV6Enabled"
{-# INLINEABLE dsIsIPV6Enabled #-}
{-# DEPRECATED isIPV6Enabled "Use generic-lens or generic-optics with 'isIPV6Enabled' instead"  #-}

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- /Note:/ Consider using 'aliasICPRecordals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAliasICPRecordals :: Lens.Lens' DistributionSummary (Core.Maybe [Types.AliasICPRecordal])
dsAliasICPRecordals = Lens.field @"aliasICPRecordals"
{-# INLINEABLE dsAliasICPRecordals #-}
{-# DEPRECATED aliasICPRecordals "Use generic-lens or generic-optics with 'aliasICPRecordals' instead"  #-}

-- | A complex type that contains information about origin groups for this distribution.
--
-- /Note:/ Consider using 'originGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsOriginGroups :: Lens.Lens' DistributionSummary (Core.Maybe Types.OriginGroups)
dsOriginGroups = Lens.field @"originGroups"
{-# INLINEABLE dsOriginGroups #-}
{-# DEPRECATED originGroups "Use generic-lens or generic-optics with 'originGroups' instead"  #-}

instance Core.FromXML DistributionSummary where
        parseXML x
          = DistributionSummary' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "ARN" Core.<*>
                x Core..@ "Status"
                Core.<*> x Core..@ "LastModifiedTime"
                Core.<*> x Core..@ "DomainName"
                Core.<*> x Core..@ "Aliases"
                Core.<*> x Core..@ "Origins"
                Core.<*> x Core..@ "DefaultCacheBehavior"
                Core.<*> x Core..@ "CacheBehaviors"
                Core.<*> x Core..@ "CustomErrorResponses"
                Core.<*> x Core..@ "Comment"
                Core.<*> x Core..@ "PriceClass"
                Core.<*> x Core..@ "Enabled"
                Core.<*> x Core..@ "ViewerCertificate"
                Core.<*> x Core..@ "Restrictions"
                Core.<*> x Core..@ "WebACLId"
                Core.<*> x Core..@ "HttpVersion"
                Core.<*> x Core..@ "IsIPV6Enabled"
                Core.<*>
                x Core..@? "AliasICPRecordals" Core..<@>
                  Core.parseXMLList "AliasICPRecordal"
                Core.<*> x Core..@? "OriginGroups"
