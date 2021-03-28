{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.StreamingLoggingConfig
  ( StreamingLoggingConfig (..)
  -- * Smart constructor
  , mkStreamingLoggingConfig
  -- * Lenses
  , slcEnabled
  , slcBucket
  , slcPrefix
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that controls whether access logs are written for this streaming distribution.
--
-- /See:/ 'mkStreamingLoggingConfig' smart constructor.
data StreamingLoggingConfig = StreamingLoggingConfig'
  { enabled :: Core.Bool
    -- ^ Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted. 
  , bucket :: Core.Text
    -- ^ The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
  , prefix :: Core.Text
    -- ^ An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamingLoggingConfig' value with any optional fields omitted.
mkStreamingLoggingConfig
    :: Core.Bool -- ^ 'enabled'
    -> Core.Text -- ^ 'bucket'
    -> Core.Text -- ^ 'prefix'
    -> StreamingLoggingConfig
mkStreamingLoggingConfig enabled bucket prefix
  = StreamingLoggingConfig'{enabled, bucket, prefix}

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted. 
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEnabled :: Lens.Lens' StreamingLoggingConfig Core.Bool
slcEnabled = Lens.field @"enabled"
{-# INLINEABLE slcEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcBucket :: Lens.Lens' StreamingLoggingConfig Core.Text
slcBucket = Lens.field @"bucket"
{-# INLINEABLE slcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcPrefix :: Lens.Lens' StreamingLoggingConfig Core.Text
slcPrefix = Lens.field @"prefix"
{-# INLINEABLE slcPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.ToXML StreamingLoggingConfig where
        toXML StreamingLoggingConfig{..}
          = Core.toXMLElement "Enabled" enabled Core.<>
              Core.toXMLElement "Bucket" bucket
              Core.<> Core.toXMLElement "Prefix" prefix

instance Core.FromXML StreamingLoggingConfig where
        parseXML x
          = StreamingLoggingConfig' Core.<$>
              (x Core..@ "Enabled") Core.<*> x Core..@ "Bucket" Core.<*>
                x Core..@ "Prefix"
