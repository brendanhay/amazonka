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
    sdcEnabled,
    sdcAliases,
    sdcPriceClass,
    sdcS3Origin,
    sdcTrustedSigners,
    sdcLogging,
    sdcComment,
    sdcCallerReference,
  )
where

import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.S3Origin
import Network.AWS.CloudFront.Types.StreamingLoggingConfig
import Network.AWS.CloudFront.Types.TrustedSigners
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The RTMP distribution's configuration information.
--
-- /See:/ 'mkStreamingDistributionConfig' smart constructor.
data StreamingDistributionConfig = StreamingDistributionConfig'
  { -- | Whether the streaming distribution is enabled to accept user requests for content.
    enabled :: Lude.Bool,
    -- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
    aliases :: Lude.Maybe Aliases,
    -- | A complex type that contains information about price class for this streaming distribution.
    priceClass :: Lude.Maybe PriceClass,
    -- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
    s3Origin :: S3Origin,
    -- | A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
    trustedSigners :: TrustedSigners,
    -- | A complex type that controls whether access logs are written for the streaming distribution.
    logging :: Lude.Maybe StreamingLoggingConfig,
    -- | Any comments you want to include about the streaming distribution.
    comment :: Lude.Text,
    -- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of the @StreamingDistributionConfig@ object), CloudFront creates a new distribution.
    -- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
    callerReference :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamingDistributionConfig' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether the streaming distribution is enabled to accept user requests for content.
-- * 'aliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
-- * 'priceClass' - A complex type that contains information about price class for this streaming distribution.
-- * 's3Origin' - A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
-- * 'trustedSigners' - A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
-- * 'logging' - A complex type that controls whether access logs are written for the streaming distribution.
-- * 'comment' - Any comments you want to include about the streaming distribution.
-- * 'callerReference' - A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @StreamingDistributionConfig@ object), CloudFront creates a new distribution.
-- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
mkStreamingDistributionConfig ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 's3Origin'
  S3Origin ->
  -- | 'trustedSigners'
  TrustedSigners ->
  -- | 'comment'
  Lude.Text ->
  -- | 'callerReference'
  Lude.Text ->
  StreamingDistributionConfig
mkStreamingDistributionConfig
  pEnabled_
  pS3Origin_
  pTrustedSigners_
  pComment_
  pCallerReference_ =
    StreamingDistributionConfig'
      { enabled = pEnabled_,
        aliases = Lude.Nothing,
        priceClass = Lude.Nothing,
        s3Origin = pS3Origin_,
        trustedSigners = pTrustedSigners_,
        logging = Lude.Nothing,
        comment = pComment_,
        callerReference = pCallerReference_
      }

-- | Whether the streaming distribution is enabled to accept user requests for content.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcEnabled :: Lens.Lens' StreamingDistributionConfig Lude.Bool
sdcEnabled = Lens.lens (enabled :: StreamingDistributionConfig -> Lude.Bool) (\s a -> s {enabled = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcAliases :: Lens.Lens' StreamingDistributionConfig (Lude.Maybe Aliases)
sdcAliases = Lens.lens (aliases :: StreamingDistributionConfig -> Lude.Maybe Aliases) (\s a -> s {aliases = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A complex type that contains information about price class for this streaming distribution.
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcPriceClass :: Lens.Lens' StreamingDistributionConfig (Lude.Maybe PriceClass)
sdcPriceClass = Lens.lens (priceClass :: StreamingDistributionConfig -> Lude.Maybe PriceClass) (\s a -> s {priceClass = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcPriceClass "Use generic-lens or generic-optics with 'priceClass' instead." #-}

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- /Note:/ Consider using 's3Origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcS3Origin :: Lens.Lens' StreamingDistributionConfig S3Origin
sdcS3Origin = Lens.lens (s3Origin :: StreamingDistributionConfig -> S3Origin) (\s a -> s {s3Origin = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcS3Origin "Use generic-lens or generic-optics with 's3Origin' instead." #-}

-- | A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcTrustedSigners :: Lens.Lens' StreamingDistributionConfig TrustedSigners
sdcTrustedSigners = Lens.lens (trustedSigners :: StreamingDistributionConfig -> TrustedSigners) (\s a -> s {trustedSigners = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcTrustedSigners "Use generic-lens or generic-optics with 'trustedSigners' instead." #-}

-- | A complex type that controls whether access logs are written for the streaming distribution.
--
-- /Note:/ Consider using 'logging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcLogging :: Lens.Lens' StreamingDistributionConfig (Lude.Maybe StreamingLoggingConfig)
sdcLogging = Lens.lens (logging :: StreamingDistributionConfig -> Lude.Maybe StreamingLoggingConfig) (\s a -> s {logging = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcLogging "Use generic-lens or generic-optics with 'logging' instead." #-}

-- | Any comments you want to include about the streaming distribution.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcComment :: Lens.Lens' StreamingDistributionConfig Lude.Text
sdcComment = Lens.lens (comment :: StreamingDistributionConfig -> Lude.Text) (\s a -> s {comment = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @StreamingDistributionConfig@ object), CloudFront creates a new distribution.
-- If @CallerReference@ is a value that you already sent in a previous request to create a distribution, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcCallerReference :: Lens.Lens' StreamingDistributionConfig Lude.Text
sdcCallerReference = Lens.lens (callerReference :: StreamingDistributionConfig -> Lude.Text) (\s a -> s {callerReference = a} :: StreamingDistributionConfig)
{-# DEPRECATED sdcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.FromXML StreamingDistributionConfig where
  parseXML x =
    StreamingDistributionConfig'
      Lude.<$> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@? "Aliases")
      Lude.<*> (x Lude..@? "PriceClass")
      Lude.<*> (x Lude..@ "S3Origin")
      Lude.<*> (x Lude..@ "TrustedSigners")
      Lude.<*> (x Lude..@? "Logging")
      Lude.<*> (x Lude..@ "Comment")
      Lude.<*> (x Lude..@ "CallerReference")

instance Lude.ToXML StreamingDistributionConfig where
  toXML StreamingDistributionConfig' {..} =
    Lude.mconcat
      [ "Enabled" Lude.@= enabled,
        "Aliases" Lude.@= aliases,
        "PriceClass" Lude.@= priceClass,
        "S3Origin" Lude.@= s3Origin,
        "TrustedSigners" Lude.@= trustedSigners,
        "Logging" Lude.@= logging,
        "Comment" Lude.@= comment,
        "CallerReference" Lude.@= callerReference
      ]
