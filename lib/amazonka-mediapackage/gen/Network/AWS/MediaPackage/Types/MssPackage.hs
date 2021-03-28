{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.MssPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.MssPackage
  ( MssPackage (..)
  -- * Smart constructor
  , mkMssPackage
  -- * Lenses
  , mpEncryption
  , mpManifestWindowSeconds
  , mpSegmentDurationSeconds
  , mpStreamSelection
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.MssEncryption as Types
import qualified Network.AWS.MediaPackage.Types.StreamSelection as Types
import qualified Network.AWS.Prelude as Core

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'mkMssPackage' smart constructor.
data MssPackage = MssPackage'
  { encryption :: Core.Maybe Types.MssEncryption
  , manifestWindowSeconds :: Core.Maybe Core.Int
    -- ^ The time window (in seconds) contained in each manifest.
  , segmentDurationSeconds :: Core.Maybe Core.Int
    -- ^ The duration (in seconds) of each segment.
  , streamSelection :: Core.Maybe Types.StreamSelection
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MssPackage' value with any optional fields omitted.
mkMssPackage
    :: MssPackage
mkMssPackage
  = MssPackage'{encryption = Core.Nothing,
                manifestWindowSeconds = Core.Nothing,
                segmentDurationSeconds = Core.Nothing,
                streamSelection = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpEncryption :: Lens.Lens' MssPackage (Core.Maybe Types.MssEncryption)
mpEncryption = Lens.field @"encryption"
{-# INLINEABLE mpEncryption #-}
{-# DEPRECATED encryption "Use generic-lens or generic-optics with 'encryption' instead"  #-}

-- | The time window (in seconds) contained in each manifest.
--
-- /Note:/ Consider using 'manifestWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpManifestWindowSeconds :: Lens.Lens' MssPackage (Core.Maybe Core.Int)
mpManifestWindowSeconds = Lens.field @"manifestWindowSeconds"
{-# INLINEABLE mpManifestWindowSeconds #-}
{-# DEPRECATED manifestWindowSeconds "Use generic-lens or generic-optics with 'manifestWindowSeconds' instead"  #-}

-- | The duration (in seconds) of each segment.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpSegmentDurationSeconds :: Lens.Lens' MssPackage (Core.Maybe Core.Int)
mpSegmentDurationSeconds = Lens.field @"segmentDurationSeconds"
{-# INLINEABLE mpSegmentDurationSeconds #-}
{-# DEPRECATED segmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpStreamSelection :: Lens.Lens' MssPackage (Core.Maybe Types.StreamSelection)
mpStreamSelection = Lens.field @"streamSelection"
{-# INLINEABLE mpStreamSelection #-}
{-# DEPRECATED streamSelection "Use generic-lens or generic-optics with 'streamSelection' instead"  #-}

instance Core.FromJSON MssPackage where
        toJSON MssPackage{..}
          = Core.object
              (Core.catMaybes
                 [("encryption" Core..=) Core.<$> encryption,
                  ("manifestWindowSeconds" Core..=) Core.<$> manifestWindowSeconds,
                  ("segmentDurationSeconds" Core..=) Core.<$> segmentDurationSeconds,
                  ("streamSelection" Core..=) Core.<$> streamSelection])

instance Core.FromJSON MssPackage where
        parseJSON
          = Core.withObject "MssPackage" Core.$
              \ x ->
                MssPackage' Core.<$>
                  (x Core..:? "encryption") Core.<*>
                    x Core..:? "manifestWindowSeconds"
                    Core.<*> x Core..:? "segmentDurationSeconds"
                    Core.<*> x Core..:? "streamSelection"
