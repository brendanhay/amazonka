{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.S3Location
  ( S3Location (..)
  -- * Smart constructor
  , mkS3Location
  -- * Lenses
  , slBucket
  , slKey
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.Bucket as Types
import qualified Network.AWS.SMS.Types.Key as Types

-- | Location of an Amazon S3 object.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { bucket :: Core.Maybe Types.Bucket
    -- ^ The Amazon S3 bucket name.
  , key :: Core.Maybe Types.Key
    -- ^ The Amazon S3 bucket key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location
    :: S3Location
mkS3Location
  = S3Location'{bucket = Core.Nothing, key = Core.Nothing}

-- | The Amazon S3 bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' S3Location (Core.Maybe Types.Bucket)
slBucket = Lens.field @"bucket"
{-# INLINEABLE slBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The Amazon S3 bucket key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' S3Location (Core.Maybe Types.Key)
slKey = Lens.field @"key"
{-# INLINEABLE slKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

instance Core.FromJSON S3Location where
        toJSON S3Location{..}
          = Core.object
              (Core.catMaybes
                 [("bucket" Core..=) Core.<$> bucket, ("key" Core..=) Core.<$> key])

instance Core.FromJSON S3Location where
        parseJSON
          = Core.withObject "S3Location" Core.$
              \ x ->
                S3Location' Core.<$>
                  (x Core..:? "bucket") Core.<*> x Core..:? "key"
