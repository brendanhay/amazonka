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
    dInProgressInvalidationBatches,
    dStatus,
    dActiveTrustedKeyGroups,
    dARN,
    dDistributionConfig,
    dLastModifiedTime,
    dDomainName,
    dAliasICPRecordals,
    dId,
    dActiveTrustedSigners,
  )
where

import Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
import Network.AWS.CloudFront.Types.ActiveTrustedSigners
import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.DistributionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A distribution tells CloudFront where you want content to be delivered from, and the details about how to track and manage content delivery.
--
-- /See:/ 'mkDistribution' smart constructor.
data Distribution = Distribution'
  { -- | The number of invalidation batches currently in progress.
    inProgressInvalidationBatches :: Lude.Int,
    -- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
    status :: Lude.Text,
    -- | CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
    activeTrustedKeyGroups :: Lude.Maybe ActiveTrustedKeyGroups,
    -- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
    arn :: Lude.Text,
    -- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
    distributionConfig :: DistributionConfig,
    -- | The date and time the distribution was last modified.
    lastModifiedTime :: Lude.DateTime,
    -- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
    domainName :: Lude.Text,
    -- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
    --
    -- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
    aliasICPRecordals :: Lude.Maybe [AliasICPRecordal],
    -- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
    id :: Lude.Text,
    -- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
    --
    -- CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
    activeTrustedSigners :: Lude.Maybe ActiveTrustedSigners
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Distribution' with the minimum fields required to make a request.
--
-- * 'inProgressInvalidationBatches' - The number of invalidation batches currently in progress.
-- * 'status' - This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
-- * 'activeTrustedKeyGroups' - CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
-- * 'arn' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
-- * 'distributionConfig' - The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
-- * 'lastModifiedTime' - The date and time the distribution was last modified.
-- * 'domainName' - The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
-- * 'aliasICPRecordals' - AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
-- * 'id' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
-- * 'activeTrustedSigners' - /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
mkDistribution ::
  -- | 'inProgressInvalidationBatches'
  Lude.Int ->
  -- | 'status'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'distributionConfig'
  DistributionConfig ->
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  Distribution
mkDistribution
  pInProgressInvalidationBatches_
  pStatus_
  pARN_
  pDistributionConfig_
  pLastModifiedTime_
  pDomainName_
  pId_ =
    Distribution'
      { inProgressInvalidationBatches =
          pInProgressInvalidationBatches_,
        status = pStatus_,
        activeTrustedKeyGroups = Lude.Nothing,
        arn = pARN_,
        distributionConfig = pDistributionConfig_,
        lastModifiedTime = pLastModifiedTime_,
        domainName = pDomainName_,
        aliasICPRecordals = Lude.Nothing,
        id = pId_,
        activeTrustedSigners = Lude.Nothing
      }

-- | The number of invalidation batches currently in progress.
--
-- /Note:/ Consider using 'inProgressInvalidationBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInProgressInvalidationBatches :: Lens.Lens' Distribution Lude.Int
dInProgressInvalidationBatches = Lens.lens (inProgressInvalidationBatches :: Distribution -> Lude.Int) (\s a -> s {inProgressInvalidationBatches = a} :: Distribution)
{-# DEPRECATED dInProgressInvalidationBatches "Use generic-lens or generic-optics with 'inProgressInvalidationBatches' instead." #-}

-- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Distribution Lude.Text
dStatus = Lens.lens (status :: Distribution -> Lude.Text) (\s a -> s {status = a} :: Distribution)
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- /Note:/ Consider using 'activeTrustedKeyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActiveTrustedKeyGroups :: Lens.Lens' Distribution (Lude.Maybe ActiveTrustedKeyGroups)
dActiveTrustedKeyGroups = Lens.lens (activeTrustedKeyGroups :: Distribution -> Lude.Maybe ActiveTrustedKeyGroups) (\s a -> s {activeTrustedKeyGroups = a} :: Distribution)
{-# DEPRECATED dActiveTrustedKeyGroups "Use generic-lens or generic-optics with 'activeTrustedKeyGroups' instead." #-}

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dARN :: Lens.Lens' Distribution Lude.Text
dARN = Lens.lens (arn :: Distribution -> Lude.Text) (\s a -> s {arn = a} :: Distribution)
{-# DEPRECATED dARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDistributionConfig :: Lens.Lens' Distribution DistributionConfig
dDistributionConfig = Lens.lens (distributionConfig :: Distribution -> DistributionConfig) (\s a -> s {distributionConfig = a} :: Distribution)
{-# DEPRECATED dDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

-- | The date and time the distribution was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLastModifiedTime :: Lens.Lens' Distribution Lude.DateTime
dLastModifiedTime = Lens.lens (lastModifiedTime :: Distribution -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: Distribution)
{-# DEPRECATED dLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' Distribution Lude.Text
dDomainName = Lens.lens (domainName :: Distribution -> Lude.Text) (\s a -> s {domainName = a} :: Distribution)
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- /Note:/ Consider using 'aliasICPRecordals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAliasICPRecordals :: Lens.Lens' Distribution (Lude.Maybe [AliasICPRecordal])
dAliasICPRecordals = Lens.lens (aliasICPRecordals :: Distribution -> Lude.Maybe [AliasICPRecordal]) (\s a -> s {aliasICPRecordals = a} :: Distribution)
{-# DEPRECATED dAliasICPRecordals "Use generic-lens or generic-optics with 'aliasICPRecordals' instead." #-}

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Distribution Lude.Text
dId = Lens.lens (id :: Distribution -> Lude.Text) (\s a -> s {id = a} :: Distribution)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- /Note:/ Consider using 'activeTrustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActiveTrustedSigners :: Lens.Lens' Distribution (Lude.Maybe ActiveTrustedSigners)
dActiveTrustedSigners = Lens.lens (activeTrustedSigners :: Distribution -> Lude.Maybe ActiveTrustedSigners) (\s a -> s {activeTrustedSigners = a} :: Distribution)
{-# DEPRECATED dActiveTrustedSigners "Use generic-lens or generic-optics with 'activeTrustedSigners' instead." #-}

instance Lude.FromXML Distribution where
  parseXML x =
    Distribution'
      Lude.<$> (x Lude..@ "InProgressInvalidationBatches")
      Lude.<*> (x Lude..@ "Status")
      Lude.<*> (x Lude..@? "ActiveTrustedKeyGroups")
      Lude.<*> (x Lude..@ "ARN")
      Lude.<*> (x Lude..@ "DistributionConfig")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "DomainName")
      Lude.<*> ( x Lude..@? "AliasICPRecordals" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AliasICPRecordal")
               )
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@? "ActiveTrustedSigners")
