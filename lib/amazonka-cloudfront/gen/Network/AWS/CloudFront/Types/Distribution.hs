{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Distribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Distribution
  ( Distribution (..)
  -- * Smart constructor
  , mkDistribution
  -- * Lenses
  , dId
  , dARN
  , dStatus
  , dLastModifiedTime
  , dInProgressInvalidationBatches
  , dDomainName
  , dDistributionConfig
  , dActiveTrustedKeyGroups
  , dActiveTrustedSigners
  , dAliasICPRecordals
  ) where

import qualified Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups as Types
import qualified Network.AWS.CloudFront.Types.ActiveTrustedSigners as Types
import qualified Network.AWS.CloudFront.Types.AliasICPRecordal as Types
import qualified Network.AWS.CloudFront.Types.DistributionConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A distribution tells CloudFront where you want content to be delivered from, and the details about how to track and manage content delivery.
--
-- /See:/ 'mkDistribution' smart constructor.
data Distribution = Distribution'
  { id :: Core.Text
    -- ^ The identifier for the distribution. For example: @EDFDVBD632BHDS5@ . 
  , arn :: Core.Text
    -- ^ The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
  , status :: Core.Text
    -- ^ This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations. 
  , lastModifiedTime :: Core.UTCTime
    -- ^ The date and time the distribution was last modified. 
  , inProgressInvalidationBatches :: Core.Int
    -- ^ The number of invalidation batches currently in progress. 
  , domainName :: Core.Text
    -- ^ The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ . 
  , distributionConfig :: Types.DistributionConfig
    -- ^ The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
  , activeTrustedKeyGroups :: Core.Maybe Types.ActiveTrustedKeyGroups
    -- ^ CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
  , activeTrustedSigners :: Core.Maybe Types.ActiveTrustedSigners
    -- ^ /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
  , aliasICPRecordals :: Core.Maybe [Types.AliasICPRecordal]
    -- ^ AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Distribution' value with any optional fields omitted.
mkDistribution
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'arn'
    -> Core.Text -- ^ 'status'
    -> Core.UTCTime -- ^ 'lastModifiedTime'
    -> Core.Int -- ^ 'inProgressInvalidationBatches'
    -> Core.Text -- ^ 'domainName'
    -> Types.DistributionConfig -- ^ 'distributionConfig'
    -> Distribution
mkDistribution id arn status lastModifiedTime
  inProgressInvalidationBatches domainName distributionConfig
  = Distribution'{id, arn, status, lastModifiedTime,
                  inProgressInvalidationBatches, domainName, distributionConfig,
                  activeTrustedKeyGroups = Core.Nothing,
                  activeTrustedSigners = Core.Nothing,
                  aliasICPRecordals = Core.Nothing}

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ . 
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Distribution Core.Text
dId = Lens.field @"id"
{-# INLINEABLE dId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dARN :: Lens.Lens' Distribution Core.Text
dARN = Lens.field @"arn"
{-# INLINEABLE dARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Distribution Core.Text
dStatus = Lens.field @"status"
{-# INLINEABLE dStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date and time the distribution was last modified. 
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastModifiedTime :: Lens.Lens' Distribution Core.UTCTime
dLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The number of invalidation batches currently in progress. 
--
-- /Note:/ Consider using 'inProgressInvalidationBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInProgressInvalidationBatches :: Lens.Lens' Distribution Core.Int
dInProgressInvalidationBatches = Lens.field @"inProgressInvalidationBatches"
{-# INLINEABLE dInProgressInvalidationBatches #-}
{-# DEPRECATED inProgressInvalidationBatches "Use generic-lens or generic-optics with 'inProgressInvalidationBatches' instead"  #-}

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ . 
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' Distribution Core.Text
dDomainName = Lens.field @"domainName"
{-# INLINEABLE dDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDistributionConfig :: Lens.Lens' Distribution Types.DistributionConfig
dDistributionConfig = Lens.field @"distributionConfig"
{-# INLINEABLE dDistributionConfig #-}
{-# DEPRECATED distributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead"  #-}

-- | CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- /Note:/ Consider using 'activeTrustedKeyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActiveTrustedKeyGroups :: Lens.Lens' Distribution (Core.Maybe Types.ActiveTrustedKeyGroups)
dActiveTrustedKeyGroups = Lens.field @"activeTrustedKeyGroups"
{-# INLINEABLE dActiveTrustedKeyGroups #-}
{-# DEPRECATED activeTrustedKeyGroups "Use generic-lens or generic-optics with 'activeTrustedKeyGroups' instead"  #-}

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- /Note:/ Consider using 'activeTrustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActiveTrustedSigners :: Lens.Lens' Distribution (Core.Maybe Types.ActiveTrustedSigners)
dActiveTrustedSigners = Lens.field @"activeTrustedSigners"
{-# INLINEABLE dActiveTrustedSigners #-}
{-# DEPRECATED activeTrustedSigners "Use generic-lens or generic-optics with 'activeTrustedSigners' instead"  #-}

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- /Note:/ Consider using 'aliasICPRecordals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAliasICPRecordals :: Lens.Lens' Distribution (Core.Maybe [Types.AliasICPRecordal])
dAliasICPRecordals = Lens.field @"aliasICPRecordals"
{-# INLINEABLE dAliasICPRecordals #-}
{-# DEPRECATED aliasICPRecordals "Use generic-lens or generic-optics with 'aliasICPRecordals' instead"  #-}

instance Core.FromXML Distribution where
        parseXML x
          = Distribution' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "ARN" Core.<*>
                x Core..@ "Status"
                Core.<*> x Core..@ "LastModifiedTime"
                Core.<*> x Core..@ "InProgressInvalidationBatches"
                Core.<*> x Core..@ "DomainName"
                Core.<*> x Core..@ "DistributionConfig"
                Core.<*> x Core..@? "ActiveTrustedKeyGroups"
                Core.<*> x Core..@? "ActiveTrustedSigners"
                Core.<*>
                x Core..@? "AliasICPRecordals" Core..<@>
                  Core.parseXMLList "AliasICPRecordal"
