{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafPackage
  ( CmafPackage (..),

    -- * Smart constructor
    mkCmafPackage,

    -- * Lenses
    cpEncryption,
    cpHlsManifests,
    cpSegmentDurationSeconds,
    cpSegmentPrefix,
    cpStreamSelection,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.CmafEncryption as Types
import qualified Network.AWS.MediaPackage.Types.HlsManifest as Types
import qualified Network.AWS.MediaPackage.Types.StreamSelection as Types
import qualified Network.AWS.Prelude as Core

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'mkCmafPackage' smart constructor.
data CmafPackage = CmafPackage'
  { encryption :: Core.Maybe Types.CmafEncryption,
    -- | A list of HLS manifest configurations
    hlsManifests :: Core.Maybe [Types.HlsManifest],
    -- | Duration (in seconds) of each segment. Actual segments will be
    --
    -- rounded to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Core.Maybe Core.Int,
    -- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
    segmentPrefix :: Core.Maybe Core.Text,
    streamSelection :: Core.Maybe Types.StreamSelection
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CmafPackage' value with any optional fields omitted.
mkCmafPackage ::
  CmafPackage
mkCmafPackage =
  CmafPackage'
    { encryption = Core.Nothing,
      hlsManifests = Core.Nothing,
      segmentDurationSeconds = Core.Nothing,
      segmentPrefix = Core.Nothing,
      streamSelection = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEncryption :: Lens.Lens' CmafPackage (Core.Maybe Types.CmafEncryption)
cpEncryption = Lens.field @"encryption"
{-# DEPRECATED cpEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | A list of HLS manifest configurations
--
-- /Note:/ Consider using 'hlsManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpHlsManifests :: Lens.Lens' CmafPackage (Core.Maybe [Types.HlsManifest])
cpHlsManifests = Lens.field @"hlsManifests"
{-# DEPRECATED cpHlsManifests "Use generic-lens or generic-optics with 'hlsManifests' instead." #-}

-- | Duration (in seconds) of each segment. Actual segments will be
--
-- rounded to the nearest multiple of the source segment duration.
--
-- /Note:/ Consider using 'segmentDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSegmentDurationSeconds :: Lens.Lens' CmafPackage (Core.Maybe Core.Int)
cpSegmentDurationSeconds = Lens.field @"segmentDurationSeconds"
{-# DEPRECATED cpSegmentDurationSeconds "Use generic-lens or generic-optics with 'segmentDurationSeconds' instead." #-}

-- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
--
-- /Note:/ Consider using 'segmentPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSegmentPrefix :: Lens.Lens' CmafPackage (Core.Maybe Core.Text)
cpSegmentPrefix = Lens.field @"segmentPrefix"
{-# DEPRECATED cpSegmentPrefix "Use generic-lens or generic-optics with 'segmentPrefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'streamSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStreamSelection :: Lens.Lens' CmafPackage (Core.Maybe Types.StreamSelection)
cpStreamSelection = Lens.field @"streamSelection"
{-# DEPRECATED cpStreamSelection "Use generic-lens or generic-optics with 'streamSelection' instead." #-}

instance Core.FromJSON CmafPackage where
  parseJSON =
    Core.withObject "CmafPackage" Core.$
      \x ->
        CmafPackage'
          Core.<$> (x Core..:? "encryption")
          Core.<*> (x Core..:? "hlsManifests")
          Core.<*> (x Core..:? "segmentDurationSeconds")
          Core.<*> (x Core..:? "segmentPrefix")
          Core.<*> (x Core..:? "streamSelection")
