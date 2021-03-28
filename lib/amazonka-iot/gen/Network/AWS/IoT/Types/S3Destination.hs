{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.S3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.S3Destination
  ( S3Destination (..)
  -- * Smart constructor
  , mkS3Destination
  -- * Lenses
  , sdBucket
  , sdPrefix
  ) where

import qualified Network.AWS.IoT.Types.Bucket as Types
import qualified Network.AWS.IoT.Types.Prefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the location of updated firmware in S3.
--
-- /See:/ 'mkS3Destination' smart constructor.
data S3Destination = S3Destination'
  { bucket :: Core.Maybe Types.Bucket
    -- ^ The S3 bucket that contains the updated firmware.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ The S3 prefix.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Destination' value with any optional fields omitted.
mkS3Destination
    :: S3Destination
mkS3Destination
  = S3Destination'{bucket = Core.Nothing, prefix = Core.Nothing}

-- | The S3 bucket that contains the updated firmware.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBucket :: Lens.Lens' S3Destination (Core.Maybe Types.Bucket)
sdBucket = Lens.field @"bucket"
{-# INLINEABLE sdBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The S3 prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPrefix :: Lens.Lens' S3Destination (Core.Maybe Types.Prefix)
sdPrefix = Lens.field @"prefix"
{-# INLINEABLE sdPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.FromJSON S3Destination where
        toJSON S3Destination{..}
          = Core.object
              (Core.catMaybes
                 [("bucket" Core..=) Core.<$> bucket,
                  ("prefix" Core..=) Core.<$> prefix])

instance Core.FromJSON S3Destination where
        parseJSON
          = Core.withObject "S3Destination" Core.$
              \ x ->
                S3Destination' Core.<$>
                  (x Core..:? "bucket") Core.<*> x Core..:? "prefix"
