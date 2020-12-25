{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slBucket,
    slKey,
    slVersion,
  )
where

import qualified Network.AWS.IoT.Types.Bucket as Types
import qualified Network.AWS.IoT.Types.S3Key as Types
import qualified Network.AWS.IoT.Types.S3Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The S3 location.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The S3 bucket.
    bucket :: Core.Maybe Types.Bucket,
    -- | The S3 key.
    key :: Core.Maybe Types.S3Key,
    -- | The S3 bucket version.
    version :: Core.Maybe Types.S3Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location'
    { bucket = Core.Nothing,
      key = Core.Nothing,
      version = Core.Nothing
    }

-- | The S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' S3Location (Core.Maybe Types.Bucket)
slBucket = Lens.field @"bucket"
{-# DEPRECATED slBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The S3 key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' S3Location (Core.Maybe Types.S3Key)
slKey = Lens.field @"key"
{-# DEPRECATED slKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The S3 bucket version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slVersion :: Lens.Lens' S3Location (Core.Maybe Types.S3Version)
slVersion = Lens.field @"version"
{-# DEPRECATED slVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON S3Location where
  toJSON S3Location {..} =
    Core.object
      ( Core.catMaybes
          [ ("bucket" Core..=) Core.<$> bucket,
            ("key" Core..=) Core.<$> key,
            ("version" Core..=) Core.<$> version
          ]
      )

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject "S3Location" Core.$
      \x ->
        S3Location'
          Core.<$> (x Core..:? "bucket")
          Core.<*> (x Core..:? "key")
          Core.<*> (x Core..:? "version")
