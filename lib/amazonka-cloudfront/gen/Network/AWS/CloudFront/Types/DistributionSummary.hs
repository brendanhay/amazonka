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
    dsOriginGroups,
    dsAliasICPRecordals,
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
    dsHTTPVersion,
    dsIsIPV6Enabled,
  )
where

import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.CacheBehaviors
import Network.AWS.CloudFront.Types.CustomErrorResponses
import Network.AWS.CloudFront.Types.DefaultCacheBehavior
import Network.AWS.CloudFront.Types.HTTPVersion
import Network.AWS.CloudFront.Types.OriginGroups
import Network.AWS.CloudFront.Types.Origins
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.Restrictions
import Network.AWS.CloudFront.Types.ViewerCertificate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of the information about a CloudFront distribution.
--
-- /See:/ 'mkDistributionSummary' smart constructor.
data DistributionSummary = DistributionSummary'
  { originGroups ::
      Lude.Maybe OriginGroups,
    aliasICPRecordals :: Lude.Maybe [AliasICPRecordal],
    id :: Lude.Text,
    arn :: Lude.Text,
    status :: Lude.Text,
    lastModifiedTime :: Lude.ISO8601,
    domainName :: Lude.Text,
    aliases :: Aliases,
    origins :: Origins,
    defaultCacheBehavior :: DefaultCacheBehavior,
    cacheBehaviors :: CacheBehaviors,
    customErrorResponses :: CustomErrorResponses,
    comment :: Lude.Text,
    priceClass :: PriceClass,
    enabled :: Lude.Bool,
    viewerCertificate :: ViewerCertificate,
    restrictions :: Restrictions,
    webACLId :: Lude.Text,
    hTTPVersion :: HTTPVersion,
    isIPV6Enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DistributionSummary' with the minimum fields required to make a request.
--
-- * 'aliasICPRecordals' - AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
-- * 'aliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
-- * 'arn' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
-- * 'cacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
-- * 'comment' - The comment originally specified when this distribution was created.
-- * 'customErrorResponses' - A complex type that contains zero or more @CustomErrorResponses@ elements.
-- * 'defaultCacheBehavior' - A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
-- * 'domainName' - The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
-- * 'enabled' - Whether the distribution is enabled to accept user requests for content.
-- * 'hTTPVersion' - Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
-- * 'id' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
-- * 'isIPV6Enabled' - Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
-- * 'lastModifiedTime' - The date and time the distribution was last modified.
-- * 'originGroups' - A complex type that contains information about origin groups for this distribution.
-- * 'origins' - A complex type that contains information about origins for this distribution.
-- * 'priceClass' - A complex type that contains information about price class for this streaming distribution.
-- * 'restrictions' - A complex type that identifies ways in which you want to restrict distribution of your content.
-- * 'status' - The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
-- * 'viewerCertificate' - A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
-- * 'webACLId' - The Web ACL Id (if any) associated with the distribution.
mkDistributionSummary ::
  -- | 'id'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'status'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.ISO8601 ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'aliases'
  Aliases ->
  -- | 'origins'
  Origins ->
  -- | 'defaultCacheBehavior'
  DefaultCacheBehavior ->
  -- | 'cacheBehaviors'
  CacheBehaviors ->
  -- | 'customErrorResponses'
  CustomErrorResponses ->
  -- | 'comment'
  Lude.Text ->
  -- | 'priceClass'
  PriceClass ->
  -- | 'enabled'
  Lude.Bool ->
  -- | 'viewerCertificate'
  ViewerCertificate ->
  -- | 'restrictions'
  Restrictions ->
  -- | 'webACLId'
  Lude.Text ->
  -- | 'hTTPVersion'
  HTTPVersion ->
  -- | 'isIPV6Enabled'
  Lude.Bool ->
  DistributionSummary
mkDistributionSummary
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pDomainName_
  pAliases_
  pOrigins_
  pDefaultCacheBehavior_
  pCacheBehaviors_
  pCustomErrorResponses_
  pComment_
  pPriceClass_
  pEnabled_
  pViewerCertificate_
  pRestrictions_
  pWebACLId_
  pHTTPVersion_
  pIsIPV6Enabled_ =
    DistributionSummary'
      { originGroups = Lude.Nothing,
        aliasICPRecordals = Lude.Nothing,
        id = pId_,
        arn = pARN_,
        status = pStatus_,
        lastModifiedTime = pLastModifiedTime_,
        domainName = pDomainName_,
        aliases = pAliases_,
        origins = pOrigins_,
        defaultCacheBehavior = pDefaultCacheBehavior_,
        cacheBehaviors = pCacheBehaviors_,
        customErrorResponses = pCustomErrorResponses_,
        comment = pComment_,
        priceClass = pPriceClass_,
        enabled = pEnabled_,
        viewerCertificate = pViewerCertificate_,
        restrictions = pRestrictions_,
        webACLId = pWebACLId_,
        hTTPVersion = pHTTPVersion_,
        isIPV6Enabled = pIsIPV6Enabled_
      }

-- | A complex type that contains information about origin groups for this distribution.
--
-- /Note:/ Consider using 'originGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsOriginGroups :: Lens.Lens' DistributionSummary (Lude.Maybe OriginGroups)
dsOriginGroups = Lens.lens (originGroups :: DistributionSummary -> Lude.Maybe OriginGroups) (\s a -> s {originGroups = a} :: DistributionSummary)
{-# DEPRECATED dsOriginGroups "Use generic-lens or generic-optics with 'originGroups' instead." #-}

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- /Note:/ Consider using 'aliasICPRecordals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAliasICPRecordals :: Lens.Lens' DistributionSummary (Lude.Maybe [AliasICPRecordal])
dsAliasICPRecordals = Lens.lens (aliasICPRecordals :: DistributionSummary -> Lude.Maybe [AliasICPRecordal]) (\s a -> s {aliasICPRecordals = a} :: DistributionSummary)
{-# DEPRECATED dsAliasICPRecordals "Use generic-lens or generic-optics with 'aliasICPRecordals' instead." #-}

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsId :: Lens.Lens' DistributionSummary Lude.Text
dsId = Lens.lens (id :: DistributionSummary -> Lude.Text) (\s a -> s {id = a} :: DistributionSummary)
{-# DEPRECATED dsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsARN :: Lens.Lens' DistributionSummary Lude.Text
dsARN = Lens.lens (arn :: DistributionSummary -> Lude.Text) (\s a -> s {arn = a} :: DistributionSummary)
{-# DEPRECATED dsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStatus :: Lens.Lens' DistributionSummary Lude.Text
dsStatus = Lens.lens (status :: DistributionSummary -> Lude.Text) (\s a -> s {status = a} :: DistributionSummary)
{-# DEPRECATED dsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the distribution was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastModifiedTime :: Lens.Lens' DistributionSummary Lude.ISO8601
dsLastModifiedTime = Lens.lens (lastModifiedTime :: DistributionSummary -> Lude.ISO8601) (\s a -> s {lastModifiedTime = a} :: DistributionSummary)
{-# DEPRECATED dsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DistributionSummary Lude.Text
dsDomainName = Lens.lens (domainName :: DistributionSummary -> Lude.Text) (\s a -> s {domainName = a} :: DistributionSummary)
{-# DEPRECATED dsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAliases :: Lens.Lens' DistributionSummary Aliases
dsAliases = Lens.lens (aliases :: DistributionSummary -> Aliases) (\s a -> s {aliases = a} :: DistributionSummary)
{-# DEPRECATED dsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A complex type that contains information about origins for this distribution.
--
-- /Note:/ Consider using 'origins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsOrigins :: Lens.Lens' DistributionSummary Origins
dsOrigins = Lens.lens (origins :: DistributionSummary -> Origins) (\s a -> s {origins = a} :: DistributionSummary)
{-# DEPRECATED dsOrigins "Use generic-lens or generic-optics with 'origins' instead." #-}

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDefaultCacheBehavior :: Lens.Lens' DistributionSummary DefaultCacheBehavior
dsDefaultCacheBehavior = Lens.lens (defaultCacheBehavior :: DistributionSummary -> DefaultCacheBehavior) (\s a -> s {defaultCacheBehavior = a} :: DistributionSummary)
{-# DEPRECATED dsDefaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead." #-}

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCacheBehaviors :: Lens.Lens' DistributionSummary CacheBehaviors
dsCacheBehaviors = Lens.lens (cacheBehaviors :: DistributionSummary -> CacheBehaviors) (\s a -> s {cacheBehaviors = a} :: DistributionSummary)
{-# DEPRECATED dsCacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead." #-}

-- | A complex type that contains zero or more @CustomErrorResponses@ elements.
--
-- /Note:/ Consider using 'customErrorResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCustomErrorResponses :: Lens.Lens' DistributionSummary CustomErrorResponses
dsCustomErrorResponses = Lens.lens (customErrorResponses :: DistributionSummary -> CustomErrorResponses) (\s a -> s {customErrorResponses = a} :: DistributionSummary)
{-# DEPRECATED dsCustomErrorResponses "Use generic-lens or generic-optics with 'customErrorResponses' instead." #-}

-- | The comment originally specified when this distribution was created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsComment :: Lens.Lens' DistributionSummary Lude.Text
dsComment = Lens.lens (comment :: DistributionSummary -> Lude.Text) (\s a -> s {comment = a} :: DistributionSummary)
{-# DEPRECATED dsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A complex type that contains information about price class for this streaming distribution.
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsPriceClass :: Lens.Lens' DistributionSummary PriceClass
dsPriceClass = Lens.lens (priceClass :: DistributionSummary -> PriceClass) (\s a -> s {priceClass = a} :: DistributionSummary)
{-# DEPRECATED dsPriceClass "Use generic-lens or generic-optics with 'priceClass' instead." #-}

-- | Whether the distribution is enabled to accept user requests for content.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEnabled :: Lens.Lens' DistributionSummary Lude.Bool
dsEnabled = Lens.lens (enabled :: DistributionSummary -> Lude.Bool) (\s a -> s {enabled = a} :: DistributionSummary)
{-# DEPRECATED dsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- /Note:/ Consider using 'viewerCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsViewerCertificate :: Lens.Lens' DistributionSummary ViewerCertificate
dsViewerCertificate = Lens.lens (viewerCertificate :: DistributionSummary -> ViewerCertificate) (\s a -> s {viewerCertificate = a} :: DistributionSummary)
{-# DEPRECATED dsViewerCertificate "Use generic-lens or generic-optics with 'viewerCertificate' instead." #-}

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- /Note:/ Consider using 'restrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRestrictions :: Lens.Lens' DistributionSummary Restrictions
dsRestrictions = Lens.lens (restrictions :: DistributionSummary -> Restrictions) (\s a -> s {restrictions = a} :: DistributionSummary)
{-# DEPRECATED dsRestrictions "Use generic-lens or generic-optics with 'restrictions' instead." #-}

-- | The Web ACL Id (if any) associated with the distribution.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsWebACLId :: Lens.Lens' DistributionSummary Lude.Text
dsWebACLId = Lens.lens (webACLId :: DistributionSummary -> Lude.Text) (\s a -> s {webACLId = a} :: DistributionSummary)
{-# DEPRECATED dsWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
--
-- /Note:/ Consider using 'hTTPVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsHTTPVersion :: Lens.Lens' DistributionSummary HTTPVersion
dsHTTPVersion = Lens.lens (hTTPVersion :: DistributionSummary -> HTTPVersion) (\s a -> s {hTTPVersion = a} :: DistributionSummary)
{-# DEPRECATED dsHTTPVersion "Use generic-lens or generic-optics with 'hTTPVersion' instead." #-}

-- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
--
-- /Note:/ Consider using 'isIPV6Enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsIsIPV6Enabled :: Lens.Lens' DistributionSummary Lude.Bool
dsIsIPV6Enabled = Lens.lens (isIPV6Enabled :: DistributionSummary -> Lude.Bool) (\s a -> s {isIPV6Enabled = a} :: DistributionSummary)
{-# DEPRECATED dsIsIPV6Enabled "Use generic-lens or generic-optics with 'isIPV6Enabled' instead." #-}

instance Lude.FromXML DistributionSummary where
  parseXML x =
    DistributionSummary'
      Lude.<$> (x Lude..@? "OriginGroups")
      Lude.<*> ( x Lude..@? "AliasICPRecordals" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AliasICPRecordal")
               )
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "ARN")
      Lude.<*> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "DomainName")
      Lude.<*> (x Lude..@ "Aliases")
      Lude.<*> (x Lude..@ "Origins")
      Lude.<*> (x Lude..@ "DefaultCacheBehavior")
      Lude.<*> (x Lude..@ "CacheBehaviors")
      Lude.<*> (x Lude..@ "CustomErrorResponses")
      Lude.<*> (x Lude..@ "Comment")
      Lude.<*> (x Lude..@ "PriceClass")
      Lude.<*> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@ "ViewerCertificate")
      Lude.<*> (x Lude..@ "Restrictions")
      Lude.<*> (x Lude..@ "WebACLId")
      Lude.<*> (x Lude..@ "HttpVersion")
      Lude.<*> (x Lude..@ "IsIPV6Enabled")
