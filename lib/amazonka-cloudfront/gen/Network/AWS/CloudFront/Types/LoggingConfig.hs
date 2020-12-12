{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.LoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LoggingConfig
  ( LoggingConfig (..),

    -- * Smart constructor
    mkLoggingConfig,

    -- * Lenses
    lcEnabled,
    lcIncludeCookies,
    lcBucket,
    lcPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that controls whether access logs are written for the distribution.
--
-- /See:/ 'mkLoggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { enabled :: Lude.Bool,
    includeCookies :: Lude.Bool,
    bucket :: Lude.Text,
    prefix :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingConfig' with the minimum fields required to make a request.
--
-- * 'bucket' - The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
-- * 'enabled' - Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
-- * 'includeCookies' - Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
-- * 'prefix' - An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
mkLoggingConfig ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'includeCookies'
  Lude.Bool ->
  -- | 'bucket'
  Lude.Text ->
  -- | 'prefix'
  Lude.Text ->
  LoggingConfig
mkLoggingConfig pEnabled_ pIncludeCookies_ pBucket_ pPrefix_ =
  LoggingConfig'
    { enabled = pEnabled_,
      includeCookies = pIncludeCookies_,
      bucket = pBucket_,
      prefix = pPrefix_
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcEnabled :: Lens.Lens' LoggingConfig Lude.Bool
lcEnabled = Lens.lens (enabled :: LoggingConfig -> Lude.Bool) (\s a -> s {enabled = a} :: LoggingConfig)
{-# DEPRECATED lcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
--
-- /Note:/ Consider using 'includeCookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcIncludeCookies :: Lens.Lens' LoggingConfig Lude.Bool
lcIncludeCookies = Lens.lens (includeCookies :: LoggingConfig -> Lude.Bool) (\s a -> s {includeCookies = a} :: LoggingConfig)
{-# DEPRECATED lcIncludeCookies "Use generic-lens or generic-optics with 'includeCookies' instead." #-}

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcBucket :: Lens.Lens' LoggingConfig Lude.Text
lcBucket = Lens.lens (bucket :: LoggingConfig -> Lude.Text) (\s a -> s {bucket = a} :: LoggingConfig)
{-# DEPRECATED lcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcPrefix :: Lens.Lens' LoggingConfig Lude.Text
lcPrefix = Lens.lens (prefix :: LoggingConfig -> Lude.Text) (\s a -> s {prefix = a} :: LoggingConfig)
{-# DEPRECATED lcPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Lude.FromXML LoggingConfig where
  parseXML x =
    LoggingConfig'
      Lude.<$> (x Lude..@ "Enabled")
      Lude.<*> (x Lude..@ "IncludeCookies")
      Lude.<*> (x Lude..@ "Bucket")
      Lude.<*> (x Lude..@ "Prefix")

instance Lude.ToXML LoggingConfig where
  toXML LoggingConfig' {..} =
    Lude.mconcat
      [ "Enabled" Lude.@= enabled,
        "IncludeCookies" Lude.@= includeCookies,
        "Bucket" Lude.@= bucket,
        "Prefix" Lude.@= prefix
      ]
