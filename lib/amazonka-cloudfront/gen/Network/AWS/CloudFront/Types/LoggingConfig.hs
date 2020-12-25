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

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that controls whether access logs are written for the distribution.
--
-- /See:/ 'mkLoggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { -- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
    enabled :: Core.Bool,
    -- | Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
    includeCookies :: Core.Bool,
    -- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
    bucket :: Types.String,
    -- | An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
    prefix :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingConfig' value with any optional fields omitted.
mkLoggingConfig ::
  -- | 'enabled'
  Core.Bool ->
  -- | 'includeCookies'
  Core.Bool ->
  -- | 'bucket'
  Types.String ->
  -- | 'prefix'
  Types.String ->
  LoggingConfig
mkLoggingConfig enabled includeCookies bucket prefix =
  LoggingConfig' {enabled, includeCookies, bucket, prefix}

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcEnabled :: Lens.Lens' LoggingConfig Core.Bool
lcEnabled = Lens.field @"enabled"
{-# DEPRECATED lcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
--
-- /Note:/ Consider using 'includeCookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcIncludeCookies :: Lens.Lens' LoggingConfig Core.Bool
lcIncludeCookies = Lens.field @"includeCookies"
{-# DEPRECATED lcIncludeCookies "Use generic-lens or generic-optics with 'includeCookies' instead." #-}

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcBucket :: Lens.Lens' LoggingConfig Types.String
lcBucket = Lens.field @"bucket"
{-# DEPRECATED lcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcPrefix :: Lens.Lens' LoggingConfig Types.String
lcPrefix = Lens.field @"prefix"
{-# DEPRECATED lcPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Core.ToXML LoggingConfig where
  toXML LoggingConfig {..} =
    Core.toXMLNode "Enabled" enabled
      Core.<> Core.toXMLNode "IncludeCookies" includeCookies
      Core.<> Core.toXMLNode "Bucket" bucket
      Core.<> Core.toXMLNode "Prefix" prefix

instance Core.FromXML LoggingConfig where
  parseXML x =
    LoggingConfig'
      Core.<$> (x Core..@ "Enabled")
      Core.<*> (x Core..@ "IncludeCookies")
      Core.<*> (x Core..@ "Bucket")
      Core.<*> (x Core..@ "Prefix")
