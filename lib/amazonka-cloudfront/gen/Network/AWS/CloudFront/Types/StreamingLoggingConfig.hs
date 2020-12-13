{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingLoggingConfig
  ( StreamingLoggingConfig (..),

    -- * Smart constructor
    mkStreamingLoggingConfig,

    -- * Lenses
    slcEnabled,
    slcPrefix,
    slcBucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that controls whether access logs are written for this streaming distribution.
--
-- /See:/ 'mkStreamingLoggingConfig' smart constructor.
data StreamingLoggingConfig = StreamingLoggingConfig'
  { -- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
    enabled :: Lude.Bool,
    -- | An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
    prefix :: Lude.Text,
    -- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
    bucket :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamingLoggingConfig' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
-- * 'prefix' - An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
-- * 'bucket' - The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
mkStreamingLoggingConfig ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'prefix'
  Lude.Text ->
  -- | 'bucket'
  Lude.Text ->
  StreamingLoggingConfig
mkStreamingLoggingConfig pEnabled_ pPrefix_ pBucket_ =
  StreamingLoggingConfig'
    { enabled = pEnabled_,
      prefix = pPrefix_,
      bucket = pBucket_
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEnabled :: Lens.Lens' StreamingLoggingConfig Lude.Bool
slcEnabled = Lens.lens (enabled :: StreamingLoggingConfig -> Lude.Bool) (\s a -> s {enabled = a} :: StreamingLoggingConfig)
{-# DEPRECATED slcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcPrefix :: Lens.Lens' StreamingLoggingConfig Lude.Text
slcPrefix = Lens.lens (prefix :: StreamingLoggingConfig -> Lude.Text) (\s a -> s {prefix = a} :: StreamingLoggingConfig)
{-# DEPRECATED slcPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcBucket :: Lens.Lens' StreamingLoggingConfig Lude.Text
slcBucket = Lens.lens (bucket :: StreamingLoggingConfig -> Lude.Text) (\s a -> s {bucket = a} :: StreamingLoggingConfig)
{-# DEPRECATED slcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.FromXML StreamingLoggingConfig where
  parseXML x =
    StreamingLoggingConfig'
      Lude.<$> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@ "Prefix")
      Lude.<*> (x Lude..@ "Bucket")

instance Lude.ToXML StreamingLoggingConfig where
  toXML StreamingLoggingConfig' {..} =
    Lude.mconcat
      [ "Enabled" Lude.@= enabled,
        "Prefix" Lude.@= prefix,
        "Bucket" Lude.@= bucket
      ]
