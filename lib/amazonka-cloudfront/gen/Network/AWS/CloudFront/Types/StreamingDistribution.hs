{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistribution
  ( StreamingDistribution (..),

    -- * Smart constructor
    mkStreamingDistribution,

    -- * Lenses
    sdId,
    sdARN,
    sdStatus,
    sdDomainName,
    sdActiveTrustedSigners,
    sdStreamingDistributionConfig,
    sdLastModifiedTime,
  )
where

import qualified Network.AWS.CloudFront.Types.ActiveTrustedSigners as Types
import qualified Network.AWS.CloudFront.Types.StreamingDistributionConfig as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A streaming distribution tells CloudFront where you want RTMP content to be delivered from, and the details about how to track and manage content delivery.
--
-- /See:/ 'mkStreamingDistribution' smart constructor.
data StreamingDistribution = StreamingDistribution'
  { -- | The identifier for the RTMP distribution. For example: @EGTXBD79EXAMPLE@ .
    id :: Types.String,
    -- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
    arn :: Types.String,
    -- | The current status of the RTMP distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
    status :: Types.String,
    -- | The domain name that corresponds to the streaming distribution, for example, @s5c39gqb8ow64r.cloudfront.net@ .
    domainName :: Types.String,
    -- | A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content.
    --
    -- The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs.
    -- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
    activeTrustedSigners :: Types.ActiveTrustedSigners,
    -- | The current configuration information for the RTMP distribution.
    streamingDistributionConfig :: Types.StreamingDistributionConfig,
    -- | The date and time that the distribution was last modified.
    lastModifiedTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StreamingDistribution' value with any optional fields omitted.
mkStreamingDistribution ::
  -- | 'id'
  Types.String ->
  -- | 'arn'
  Types.String ->
  -- | 'status'
  Types.String ->
  -- | 'domainName'
  Types.String ->
  -- | 'activeTrustedSigners'
  Types.ActiveTrustedSigners ->
  -- | 'streamingDistributionConfig'
  Types.StreamingDistributionConfig ->
  StreamingDistribution
mkStreamingDistribution
  id
  arn
  status
  domainName
  activeTrustedSigners
  streamingDistributionConfig =
    StreamingDistribution'
      { id,
        arn,
        status,
        domainName,
        activeTrustedSigners,
        streamingDistributionConfig,
        lastModifiedTime = Core.Nothing
      }

-- | The identifier for the RTMP distribution. For example: @EGTXBD79EXAMPLE@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdId :: Lens.Lens' StreamingDistribution Types.String
sdId = Lens.field @"id"
{-# DEPRECATED sdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdARN :: Lens.Lens' StreamingDistribution Types.String
sdARN = Lens.field @"arn"
{-# DEPRECATED sdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The current status of the RTMP distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStatus :: Lens.Lens' StreamingDistribution Types.String
sdStatus = Lens.field @"status"
{-# DEPRECATED sdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The domain name that corresponds to the streaming distribution, for example, @s5c39gqb8ow64r.cloudfront.net@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDomainName :: Lens.Lens' StreamingDistribution Types.String
sdDomainName = Lens.field @"domainName"
{-# DEPRECATED sdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content.
--
-- The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'activeTrustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdActiveTrustedSigners :: Lens.Lens' StreamingDistribution Types.ActiveTrustedSigners
sdActiveTrustedSigners = Lens.field @"activeTrustedSigners"
{-# DEPRECATED sdActiveTrustedSigners "Use generic-lens or generic-optics with 'activeTrustedSigners' instead." #-}

-- | The current configuration information for the RTMP distribution.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamingDistributionConfig :: Lens.Lens' StreamingDistribution Types.StreamingDistributionConfig
sdStreamingDistributionConfig = Lens.field @"streamingDistributionConfig"
{-# DEPRECATED sdStreamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead." #-}

-- | The date and time that the distribution was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLastModifiedTime :: Lens.Lens' StreamingDistribution (Core.Maybe Core.UTCTime)
sdLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED sdLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Core.FromXML StreamingDistribution where
  parseXML x =
    StreamingDistribution'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "ARN")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "DomainName")
      Core.<*> (x Core..@ "ActiveTrustedSigners")
      Core.<*> (x Core..@ "StreamingDistributionConfig")
      Core.<*> (x Core..@? "LastModifiedTime")
