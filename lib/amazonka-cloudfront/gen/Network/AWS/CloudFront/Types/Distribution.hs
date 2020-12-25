{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Distribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Distribution
  ( Distribution (..),

    -- * Smart constructor
    mkDistribution,

    -- * Lenses
    dId,
    dARN,
    dStatus,
    dLastModifiedTime,
    dInProgressInvalidationBatches,
    dDomainName,
    dDistributionConfig,
    dActiveTrustedKeyGroups,
    dActiveTrustedSigners,
    dAliasICPRecordals,
  )
where

import qualified Network.AWS.CloudFront.Types.ARN as Types
import qualified Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups as Types
import qualified Network.AWS.CloudFront.Types.ActiveTrustedSigners as Types
import qualified Network.AWS.CloudFront.Types.AliasICPRecordal as Types
import qualified Network.AWS.CloudFront.Types.DistributionConfig as Types
import qualified Network.AWS.CloudFront.Types.DomainName as Types
import qualified Network.AWS.CloudFront.Types.Id as Types
import qualified Network.AWS.CloudFront.Types.Status as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A distribution tells CloudFront where you want content to be delivered from, and the details about how to track and manage content delivery.
--
-- /See:/ 'mkDistribution' smart constructor.
data Distribution = Distribution'
  { -- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
    id :: Types.Id,
    -- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
    arn :: Types.ARN,
    -- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
    status :: Types.Status,
    -- | The date and time the distribution was last modified.
    lastModifiedTime :: Core.UTCTime,
    -- | The number of invalidation batches currently in progress.
    inProgressInvalidationBatches :: Core.Int,
    -- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
    domainName :: Types.DomainName,
    -- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
    distributionConfig :: Types.DistributionConfig,
    -- | CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
    activeTrustedKeyGroups :: Core.Maybe Types.ActiveTrustedKeyGroups,
    -- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
    --
    -- CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
    activeTrustedSigners :: Core.Maybe Types.ActiveTrustedSigners,
    -- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
    --
    -- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
    aliasICPRecordals :: Core.Maybe [Types.AliasICPRecordal]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Distribution' value with any optional fields omitted.
mkDistribution ::
  -- | 'id'
  Types.Id ->
  -- | 'arn'
  Types.ARN ->
  -- | 'status'
  Types.Status ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'inProgressInvalidationBatches'
  Core.Int ->
  -- | 'domainName'
  Types.DomainName ->
  -- | 'distributionConfig'
  Types.DistributionConfig ->
  Distribution
mkDistribution
  id
  arn
  status
  lastModifiedTime
  inProgressInvalidationBatches
  domainName
  distributionConfig =
    Distribution'
      { id,
        arn,
        status,
        lastModifiedTime,
        inProgressInvalidationBatches,
        domainName,
        distributionConfig,
        activeTrustedKeyGroups = Core.Nothing,
        activeTrustedSigners = Core.Nothing,
        aliasICPRecordals = Core.Nothing
      }

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Distribution Types.Id
dId = Lens.field @"id"
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dARN :: Lens.Lens' Distribution Types.ARN
dARN = Lens.field @"arn"
{-# DEPRECATED dARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Distribution Types.Status
dStatus = Lens.field @"status"
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the distribution was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastModifiedTime :: Lens.Lens' Distribution Core.UTCTime
dLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The number of invalidation batches currently in progress.
--
-- /Note:/ Consider using 'inProgressInvalidationBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInProgressInvalidationBatches :: Lens.Lens' Distribution Core.Int
dInProgressInvalidationBatches = Lens.field @"inProgressInvalidationBatches"
{-# DEPRECATED dInProgressInvalidationBatches "Use generic-lens or generic-optics with 'inProgressInvalidationBatches' instead." #-}

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' Distribution Types.DomainName
dDomainName = Lens.field @"domainName"
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDistributionConfig :: Lens.Lens' Distribution Types.DistributionConfig
dDistributionConfig = Lens.field @"distributionConfig"
{-# DEPRECATED dDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

-- | CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- /Note:/ Consider using 'activeTrustedKeyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActiveTrustedKeyGroups :: Lens.Lens' Distribution (Core.Maybe Types.ActiveTrustedKeyGroups)
dActiveTrustedKeyGroups = Lens.field @"activeTrustedKeyGroups"
{-# DEPRECATED dActiveTrustedKeyGroups "Use generic-lens or generic-optics with 'activeTrustedKeyGroups' instead." #-}

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- /Note:/ Consider using 'activeTrustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActiveTrustedSigners :: Lens.Lens' Distribution (Core.Maybe Types.ActiveTrustedSigners)
dActiveTrustedSigners = Lens.field @"activeTrustedSigners"
{-# DEPRECATED dActiveTrustedSigners "Use generic-lens or generic-optics with 'activeTrustedSigners' instead." #-}

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- /Note:/ Consider using 'aliasICPRecordals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAliasICPRecordals :: Lens.Lens' Distribution (Core.Maybe [Types.AliasICPRecordal])
dAliasICPRecordals = Lens.field @"aliasICPRecordals"
{-# DEPRECATED dAliasICPRecordals "Use generic-lens or generic-optics with 'aliasICPRecordals' instead." #-}

instance Core.FromXML Distribution where
  parseXML x =
    Distribution'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "ARN")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "InProgressInvalidationBatches")
      Core.<*> (x Core..@ "DomainName")
      Core.<*> (x Core..@ "DistributionConfig")
      Core.<*> (x Core..@? "ActiveTrustedKeyGroups")
      Core.<*> (x Core..@? "ActiveTrustedSigners")
      Core.<*> ( x Core..@? "AliasICPRecordals"
                   Core..<@> Core.parseXMLList "AliasICPRecordal"
               )
