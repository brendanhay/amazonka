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
-- Module      : Network.AWS.CloudFront.Types.DistributionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionSummary where

import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.CacheBehaviors
import Network.AWS.CloudFront.Types.CustomErrorResponses
import Network.AWS.CloudFront.Types.DefaultCacheBehavior
import Network.AWS.CloudFront.Types.HttpVersion
import Network.AWS.CloudFront.Types.OriginGroups
import Network.AWS.CloudFront.Types.Origins
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.Restrictions
import Network.AWS.CloudFront.Types.ViewerCertificate
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A summary of the information about a CloudFront distribution.
--
-- /See:/ 'newDistributionSummary' smart constructor.
data DistributionSummary = DistributionSummary'
  { -- | AWS services in China customers must file for an Internet Content
    -- Provider (ICP) recordal if they want to serve content publicly on an
    -- alternate domain name, also known as a CNAME, that they\'ve added to
    -- CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs
    -- associated with distributions.
    --
    -- For more information about ICP recordals, see
    -- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
    -- in /Getting Started with AWS services in China/.
    aliasICPRecordals :: Core.Maybe [AliasICPRecordal],
    -- | A complex type that contains information about origin groups for this
    -- distribution.
    originGroups :: Core.Maybe OriginGroups,
    -- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@.
    id :: Core.Text,
    -- | The ARN (Amazon Resource Name) for the distribution. For example:
    -- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
    -- @123456789012@ is your AWS account ID.
    arn :: Core.Text,
    -- | The current status of the distribution. When the status is @Deployed@,
    -- the distribution\'s information is propagated to all CloudFront edge
    -- locations.
    status :: Core.Text,
    -- | The date and time the distribution was last modified.
    lastModifiedTime :: Core.ISO8601,
    -- | The domain name that corresponds to the distribution, for example,
    -- @d111111abcdef8.cloudfront.net@.
    domainName :: Core.Text,
    -- | A complex type that contains information about CNAMEs (alternate domain
    -- names), if any, for this distribution.
    aliases :: Aliases,
    -- | A complex type that contains information about origins for this
    -- distribution.
    origins :: Origins,
    -- | A complex type that describes the default cache behavior if you don\'t
    -- specify a @CacheBehavior@ element or if files don\'t match any of the
    -- values of @PathPattern@ in @CacheBehavior@ elements. You must create
    -- exactly one default cache behavior.
    defaultCacheBehavior :: DefaultCacheBehavior,
    -- | A complex type that contains zero or more @CacheBehavior@ elements.
    cacheBehaviors :: CacheBehaviors,
    -- | A complex type that contains zero or more @CustomErrorResponses@
    -- elements.
    customErrorResponses :: CustomErrorResponses,
    -- | The comment originally specified when this distribution was created.
    comment :: Core.Text,
    -- | A complex type that contains information about price class for this
    -- streaming distribution.
    priceClass :: PriceClass,
    -- | Whether the distribution is enabled to accept user requests for content.
    enabled :: Core.Bool,
    -- | A complex type that determines the distribution’s SSL\/TLS configuration
    -- for communicating with viewers.
    viewerCertificate :: ViewerCertificate,
    -- | A complex type that identifies ways in which you want to restrict
    -- distribution of your content.
    restrictions :: Restrictions,
    -- | The Web ACL Id (if any) associated with the distribution.
    webACLId :: Core.Text,
    -- | Specify the maximum HTTP version that you want viewers to use to
    -- communicate with CloudFront. The default value for new web distributions
    -- is @http2@. Viewers that don\'t support @HTTP\/2@ will automatically use
    -- an earlier version.
    httpVersion :: HttpVersion,
    -- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address
    -- for your distribution.
    isIPV6Enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DistributionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasICPRecordals', 'distributionSummary_aliasICPRecordals' - AWS services in China customers must file for an Internet Content
-- Provider (ICP) recordal if they want to serve content publicly on an
-- alternate domain name, also known as a CNAME, that they\'ve added to
-- CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs
-- associated with distributions.
--
-- For more information about ICP recordals, see
-- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
-- in /Getting Started with AWS services in China/.
--
-- 'originGroups', 'distributionSummary_originGroups' - A complex type that contains information about origin groups for this
-- distribution.
--
-- 'id', 'distributionSummary_id' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@.
--
-- 'arn', 'distributionSummary_arn' - The ARN (Amazon Resource Name) for the distribution. For example:
-- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
-- @123456789012@ is your AWS account ID.
--
-- 'status', 'distributionSummary_status' - The current status of the distribution. When the status is @Deployed@,
-- the distribution\'s information is propagated to all CloudFront edge
-- locations.
--
-- 'lastModifiedTime', 'distributionSummary_lastModifiedTime' - The date and time the distribution was last modified.
--
-- 'domainName', 'distributionSummary_domainName' - The domain name that corresponds to the distribution, for example,
-- @d111111abcdef8.cloudfront.net@.
--
-- 'aliases', 'distributionSummary_aliases' - A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- 'origins', 'distributionSummary_origins' - A complex type that contains information about origins for this
-- distribution.
--
-- 'defaultCacheBehavior', 'distributionSummary_defaultCacheBehavior' - A complex type that describes the default cache behavior if you don\'t
-- specify a @CacheBehavior@ element or if files don\'t match any of the
-- values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
--
-- 'cacheBehaviors', 'distributionSummary_cacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- 'customErrorResponses', 'distributionSummary_customErrorResponses' - A complex type that contains zero or more @CustomErrorResponses@
-- elements.
--
-- 'comment', 'distributionSummary_comment' - The comment originally specified when this distribution was created.
--
-- 'priceClass', 'distributionSummary_priceClass' - A complex type that contains information about price class for this
-- streaming distribution.
--
-- 'enabled', 'distributionSummary_enabled' - Whether the distribution is enabled to accept user requests for content.
--
-- 'viewerCertificate', 'distributionSummary_viewerCertificate' - A complex type that determines the distribution’s SSL\/TLS configuration
-- for communicating with viewers.
--
-- 'restrictions', 'distributionSummary_restrictions' - A complex type that identifies ways in which you want to restrict
-- distribution of your content.
--
-- 'webACLId', 'distributionSummary_webACLId' - The Web ACL Id (if any) associated with the distribution.
--
-- 'httpVersion', 'distributionSummary_httpVersion' - Specify the maximum HTTP version that you want viewers to use to
-- communicate with CloudFront. The default value for new web distributions
-- is @http2@. Viewers that don\'t support @HTTP\/2@ will automatically use
-- an earlier version.
--
-- 'isIPV6Enabled', 'distributionSummary_isIPV6Enabled' - Whether CloudFront responds to IPv6 DNS requests with an IPv6 address
-- for your distribution.
newDistributionSummary ::
  -- | 'id'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  -- | 'status'
  Core.Text ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'domainName'
  Core.Text ->
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
  Core.Text ->
  -- | 'priceClass'
  PriceClass ->
  -- | 'enabled'
  Core.Bool ->
  -- | 'viewerCertificate'
  ViewerCertificate ->
  -- | 'restrictions'
  Restrictions ->
  -- | 'webACLId'
  Core.Text ->
  -- | 'httpVersion'
  HttpVersion ->
  -- | 'isIPV6Enabled'
  Core.Bool ->
  DistributionSummary
newDistributionSummary
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
  pHttpVersion_
  pIsIPV6Enabled_ =
    DistributionSummary'
      { aliasICPRecordals =
          Core.Nothing,
        originGroups = Core.Nothing,
        id = pId_,
        arn = pARN_,
        status = pStatus_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
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
        httpVersion = pHttpVersion_,
        isIPV6Enabled = pIsIPV6Enabled_
      }

-- | AWS services in China customers must file for an Internet Content
-- Provider (ICP) recordal if they want to serve content publicly on an
-- alternate domain name, also known as a CNAME, that they\'ve added to
-- CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs
-- associated with distributions.
--
-- For more information about ICP recordals, see
-- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
-- in /Getting Started with AWS services in China/.
distributionSummary_aliasICPRecordals :: Lens.Lens' DistributionSummary (Core.Maybe [AliasICPRecordal])
distributionSummary_aliasICPRecordals = Lens.lens (\DistributionSummary' {aliasICPRecordals} -> aliasICPRecordals) (\s@DistributionSummary' {} a -> s {aliasICPRecordals = a} :: DistributionSummary) Core.. Lens.mapping Lens._Coerce

-- | A complex type that contains information about origin groups for this
-- distribution.
distributionSummary_originGroups :: Lens.Lens' DistributionSummary (Core.Maybe OriginGroups)
distributionSummary_originGroups = Lens.lens (\DistributionSummary' {originGroups} -> originGroups) (\s@DistributionSummary' {} a -> s {originGroups = a} :: DistributionSummary)

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@.
distributionSummary_id :: Lens.Lens' DistributionSummary Core.Text
distributionSummary_id = Lens.lens (\DistributionSummary' {id} -> id) (\s@DistributionSummary' {} a -> s {id = a} :: DistributionSummary)

-- | The ARN (Amazon Resource Name) for the distribution. For example:
-- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
-- @123456789012@ is your AWS account ID.
distributionSummary_arn :: Lens.Lens' DistributionSummary Core.Text
distributionSummary_arn = Lens.lens (\DistributionSummary' {arn} -> arn) (\s@DistributionSummary' {} a -> s {arn = a} :: DistributionSummary)

-- | The current status of the distribution. When the status is @Deployed@,
-- the distribution\'s information is propagated to all CloudFront edge
-- locations.
distributionSummary_status :: Lens.Lens' DistributionSummary Core.Text
distributionSummary_status = Lens.lens (\DistributionSummary' {status} -> status) (\s@DistributionSummary' {} a -> s {status = a} :: DistributionSummary)

-- | The date and time the distribution was last modified.
distributionSummary_lastModifiedTime :: Lens.Lens' DistributionSummary Core.UTCTime
distributionSummary_lastModifiedTime = Lens.lens (\DistributionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@DistributionSummary' {} a -> s {lastModifiedTime = a} :: DistributionSummary) Core.. Core._Time

-- | The domain name that corresponds to the distribution, for example,
-- @d111111abcdef8.cloudfront.net@.
distributionSummary_domainName :: Lens.Lens' DistributionSummary Core.Text
distributionSummary_domainName = Lens.lens (\DistributionSummary' {domainName} -> domainName) (\s@DistributionSummary' {} a -> s {domainName = a} :: DistributionSummary)

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
distributionSummary_aliases :: Lens.Lens' DistributionSummary Aliases
distributionSummary_aliases = Lens.lens (\DistributionSummary' {aliases} -> aliases) (\s@DistributionSummary' {} a -> s {aliases = a} :: DistributionSummary)

-- | A complex type that contains information about origins for this
-- distribution.
distributionSummary_origins :: Lens.Lens' DistributionSummary Origins
distributionSummary_origins = Lens.lens (\DistributionSummary' {origins} -> origins) (\s@DistributionSummary' {} a -> s {origins = a} :: DistributionSummary)

-- | A complex type that describes the default cache behavior if you don\'t
-- specify a @CacheBehavior@ element or if files don\'t match any of the
-- values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
distributionSummary_defaultCacheBehavior :: Lens.Lens' DistributionSummary DefaultCacheBehavior
distributionSummary_defaultCacheBehavior = Lens.lens (\DistributionSummary' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@DistributionSummary' {} a -> s {defaultCacheBehavior = a} :: DistributionSummary)

-- | A complex type that contains zero or more @CacheBehavior@ elements.
distributionSummary_cacheBehaviors :: Lens.Lens' DistributionSummary CacheBehaviors
distributionSummary_cacheBehaviors = Lens.lens (\DistributionSummary' {cacheBehaviors} -> cacheBehaviors) (\s@DistributionSummary' {} a -> s {cacheBehaviors = a} :: DistributionSummary)

-- | A complex type that contains zero or more @CustomErrorResponses@
-- elements.
distributionSummary_customErrorResponses :: Lens.Lens' DistributionSummary CustomErrorResponses
distributionSummary_customErrorResponses = Lens.lens (\DistributionSummary' {customErrorResponses} -> customErrorResponses) (\s@DistributionSummary' {} a -> s {customErrorResponses = a} :: DistributionSummary)

-- | The comment originally specified when this distribution was created.
distributionSummary_comment :: Lens.Lens' DistributionSummary Core.Text
distributionSummary_comment = Lens.lens (\DistributionSummary' {comment} -> comment) (\s@DistributionSummary' {} a -> s {comment = a} :: DistributionSummary)

-- | A complex type that contains information about price class for this
-- streaming distribution.
distributionSummary_priceClass :: Lens.Lens' DistributionSummary PriceClass
distributionSummary_priceClass = Lens.lens (\DistributionSummary' {priceClass} -> priceClass) (\s@DistributionSummary' {} a -> s {priceClass = a} :: DistributionSummary)

-- | Whether the distribution is enabled to accept user requests for content.
distributionSummary_enabled :: Lens.Lens' DistributionSummary Core.Bool
distributionSummary_enabled = Lens.lens (\DistributionSummary' {enabled} -> enabled) (\s@DistributionSummary' {} a -> s {enabled = a} :: DistributionSummary)

-- | A complex type that determines the distribution’s SSL\/TLS configuration
-- for communicating with viewers.
distributionSummary_viewerCertificate :: Lens.Lens' DistributionSummary ViewerCertificate
distributionSummary_viewerCertificate = Lens.lens (\DistributionSummary' {viewerCertificate} -> viewerCertificate) (\s@DistributionSummary' {} a -> s {viewerCertificate = a} :: DistributionSummary)

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
distributionSummary_restrictions :: Lens.Lens' DistributionSummary Restrictions
distributionSummary_restrictions = Lens.lens (\DistributionSummary' {restrictions} -> restrictions) (\s@DistributionSummary' {} a -> s {restrictions = a} :: DistributionSummary)

-- | The Web ACL Id (if any) associated with the distribution.
distributionSummary_webACLId :: Lens.Lens' DistributionSummary Core.Text
distributionSummary_webACLId = Lens.lens (\DistributionSummary' {webACLId} -> webACLId) (\s@DistributionSummary' {} a -> s {webACLId = a} :: DistributionSummary)

-- | Specify the maximum HTTP version that you want viewers to use to
-- communicate with CloudFront. The default value for new web distributions
-- is @http2@. Viewers that don\'t support @HTTP\/2@ will automatically use
-- an earlier version.
distributionSummary_httpVersion :: Lens.Lens' DistributionSummary HttpVersion
distributionSummary_httpVersion = Lens.lens (\DistributionSummary' {httpVersion} -> httpVersion) (\s@DistributionSummary' {} a -> s {httpVersion = a} :: DistributionSummary)

-- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address
-- for your distribution.
distributionSummary_isIPV6Enabled :: Lens.Lens' DistributionSummary Core.Bool
distributionSummary_isIPV6Enabled = Lens.lens (\DistributionSummary' {isIPV6Enabled} -> isIPV6Enabled) (\s@DistributionSummary' {} a -> s {isIPV6Enabled = a} :: DistributionSummary)

instance Core.FromXML DistributionSummary where
  parseXML x =
    DistributionSummary'
      Core.<$> ( x Core..@? "AliasICPRecordals" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "AliasICPRecordal")
               )
      Core.<*> (x Core..@? "OriginGroups")
      Core.<*> (x Core..@ "Id")
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

instance Core.Hashable DistributionSummary

instance Core.NFData DistributionSummary
