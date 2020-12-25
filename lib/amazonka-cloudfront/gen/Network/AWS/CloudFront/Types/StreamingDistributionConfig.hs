{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionConfig
  ( StreamingDistributionConfig (..),

    -- * Smart constructor
    mkStreamingDistributionConfig,

    -- * Lenses
    sdcCallerReference,
    sdcS3Origin,
    sdcComment,
    sdcTrustedSigners,
    sdcEnabled,
    sdcAliases,
    sdcLogging,
    sdcPriceClass,
  )
where

import qualified Network.AWS.CloudFront.Types.Aliases as Types
import qualified Network.AWS.CloudFront.Types.CallerReference as Types
import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.CloudFront.Types.PriceClass as Types
import qualified Network.AWS.CloudFront.Types.S3Origin as Types
import qualified Network.AWS.CloudFront.Types.StreamingLoggingConfig as Types
import qualified Network.AWS.CloudFront.Types.TrustedSigners as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The RTMP distribution's configuration information.
--
-- /See:/ 'mkStreamingDistributionConfig' smart constructor.
data StreamingDistributionConfig = StreamingDistributionConfig'
  { -- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of the @StreamingDistributionConfig@ object), CloudFront creates a new distribution.
    -- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
    callerReference :: Types.CallerReference,
    -- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
    s3Origin :: Types.S3Origin,
    -- | Any comments you want to include about the streaming distribution.
    comment :: Types.Comment,
    -- | A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
    trustedSigners :: Types.TrustedSigners,
    -- | Whether the streaming distribution is enabled to accept user requests for content.
    enabled :: Core.Bool,
    -- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
    aliases :: Core.Maybe Types.Aliases,
    -- | A complex type that controls whether access logs are written for the streaming distribution.
    logging :: Core.Maybe Types.StreamingLoggingConfig,
    -- | A complex type that contains information about price class for this streaming distribution.
    priceClass :: Core.Maybe Types.PriceClass
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamingDistributionConfig' value with any optional fields omitted.
mkStreamingDistributionConfig ::
  -- | 'callerReference'
  Types.CallerReference ->
  -- | 's3Origin'
  Types.S3Origin ->
  -- | 'comment'
  Types.Comment ->
  -- | 'trustedSigners'
  Types.TrustedSigners ->
  -- | 'enabled'
  Core.Bool ->
  StreamingDistributionConfig
mkStreamingDistributionConfig
  callerReference
  s3Origin
  comment
  trustedSigners
  enabled =
    StreamingDistributionConfig'
      { callerReference,
        s3Origin,
        comment,
        trustedSigners,
        enabled,
        aliases = Core.Nothing,
        logging = Core.Nothing,
        priceClass = Core.Nothing
      }

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @StreamingDistributionConfig@ object), CloudFront creates a new distribution.
-- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcCallerReference :: Lens.Lens' StreamingDistributionConfig Types.CallerReference
sdcCallerReference = Lens.field @"callerReference"
{-# DEPRECATED sdcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- /Note:/ Consider using 's3Origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcS3Origin :: Lens.Lens' StreamingDistributionConfig Types.S3Origin
sdcS3Origin = Lens.field @"s3Origin"
{-# DEPRECATED sdcS3Origin "Use generic-lens or generic-optics with 's3Origin' instead." #-}

-- | Any comments you want to include about the streaming distribution.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcComment :: Lens.Lens' StreamingDistributionConfig Types.Comment
sdcComment = Lens.field @"comment"
{-# DEPRECATED sdcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcTrustedSigners :: Lens.Lens' StreamingDistributionConfig Types.TrustedSigners
sdcTrustedSigners = Lens.field @"trustedSigners"
{-# DEPRECATED sdcTrustedSigners "Use generic-lens or generic-optics with 'trustedSigners' instead." #-}

-- | Whether the streaming distribution is enabled to accept user requests for content.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcEnabled :: Lens.Lens' StreamingDistributionConfig Core.Bool
sdcEnabled = Lens.field @"enabled"
{-# DEPRECATED sdcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcAliases :: Lens.Lens' StreamingDistributionConfig (Core.Maybe Types.Aliases)
sdcAliases = Lens.field @"aliases"
{-# DEPRECATED sdcAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A complex type that controls whether access logs are written for the streaming distribution.
--
-- /Note:/ Consider using 'logging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcLogging :: Lens.Lens' StreamingDistributionConfig (Core.Maybe Types.StreamingLoggingConfig)
sdcLogging = Lens.field @"logging"
{-# DEPRECATED sdcLogging "Use generic-lens or generic-optics with 'logging' instead." #-}

-- | A complex type that contains information about price class for this streaming distribution.
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcPriceClass :: Lens.Lens' StreamingDistributionConfig (Core.Maybe Types.PriceClass)
sdcPriceClass = Lens.field @"priceClass"
{-# DEPRECATED sdcPriceClass "Use generic-lens or generic-optics with 'priceClass' instead." #-}

instance Core.ToXML StreamingDistributionConfig where
  toXML StreamingDistributionConfig {..} =
    Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "S3Origin" s3Origin
      Core.<> Core.toXMLNode "Comment" comment
      Core.<> Core.toXMLNode "TrustedSigners" trustedSigners
      Core.<> Core.toXMLNode "Enabled" enabled
      Core.<> Core.toXMLNode "Aliases" Core.<$> aliases
      Core.<> Core.toXMLNode "Logging" Core.<$> logging
      Core.<> Core.toXMLNode "PriceClass" Core.<$> priceClass

instance Core.FromXML StreamingDistributionConfig where
  parseXML x =
    StreamingDistributionConfig'
      Core.<$> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "S3Origin")
      Core.<*> (x Core..@ "Comment")
      Core.<*> (x Core..@ "TrustedSigners")
      Core.<*> (x Core..@ "Enabled")
      Core.<*> (x Core..@? "Aliases")
      Core.<*> (x Core..@? "Logging")
      Core.<*> (x Core..@? "PriceClass")
