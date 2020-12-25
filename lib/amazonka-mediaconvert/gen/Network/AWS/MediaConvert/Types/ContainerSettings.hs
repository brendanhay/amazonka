{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ContainerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ContainerSettings
  ( ContainerSettings (..),

    -- * Smart constructor
    mkContainerSettings,

    -- * Lenses
    csCmfcSettings,
    csContainer,
    csF4vSettings,
    csM2tsSettings,
    csM3u8Settings,
    csMovSettings,
    csMp4Settings,
    csMpdSettings,
    csMxfSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CmfcSettings as Types
import qualified Network.AWS.MediaConvert.Types.ContainerType as Types
import qualified Network.AWS.MediaConvert.Types.F4vSettings as Types
import qualified Network.AWS.MediaConvert.Types.M2tsSettings as Types
import qualified Network.AWS.MediaConvert.Types.M3u8Settings as Types
import qualified Network.AWS.MediaConvert.Types.MovSettings as Types
import qualified Network.AWS.MediaConvert.Types.Mp4Settings as Types
import qualified Network.AWS.MediaConvert.Types.MpdSettings as Types
import qualified Network.AWS.MediaConvert.Types.MxfSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Container specific settings.
--
-- /See:/ 'mkContainerSettings' smart constructor.
data ContainerSettings = ContainerSettings'
  { -- | Settings for MP4 segments in CMAF
    cmfcSettings :: Core.Maybe Types.CmfcSettings,
    -- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
    container :: Core.Maybe Types.ContainerType,
    -- | Settings for F4v container
    f4vSettings :: Core.Maybe Types.F4vSettings,
    -- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
    m2tsSettings :: Core.Maybe Types.M2tsSettings,
    -- | Settings for TS segments in HLS
    m3u8Settings :: Core.Maybe Types.M3u8Settings,
    -- | Settings for MOV Container.
    movSettings :: Core.Maybe Types.MovSettings,
    -- | Settings for MP4 container. You can create audio-only AAC outputs with this container.
    mp4Settings :: Core.Maybe Types.Mp4Settings,
    -- | Settings for MP4 segments in DASH
    mpdSettings :: Core.Maybe Types.MpdSettings,
    -- | MXF settings
    mxfSettings :: Core.Maybe Types.MxfSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerSettings' value with any optional fields omitted.
mkContainerSettings ::
  ContainerSettings
mkContainerSettings =
  ContainerSettings'
    { cmfcSettings = Core.Nothing,
      container = Core.Nothing,
      f4vSettings = Core.Nothing,
      m2tsSettings = Core.Nothing,
      m3u8Settings = Core.Nothing,
      movSettings = Core.Nothing,
      mp4Settings = Core.Nothing,
      mpdSettings = Core.Nothing,
      mxfSettings = Core.Nothing
    }

-- | Settings for MP4 segments in CMAF
--
-- /Note:/ Consider using 'cmfcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCmfcSettings :: Lens.Lens' ContainerSettings (Core.Maybe Types.CmfcSettings)
csCmfcSettings = Lens.field @"cmfcSettings"
{-# DEPRECATED csCmfcSettings "Use generic-lens or generic-optics with 'cmfcSettings' instead." #-}

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csContainer :: Lens.Lens' ContainerSettings (Core.Maybe Types.ContainerType)
csContainer = Lens.field @"container"
{-# DEPRECATED csContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | Settings for F4v container
--
-- /Note:/ Consider using 'f4vSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csF4vSettings :: Lens.Lens' ContainerSettings (Core.Maybe Types.F4vSettings)
csF4vSettings = Lens.field @"f4vSettings"
{-# DEPRECATED csF4vSettings "Use generic-lens or generic-optics with 'f4vSettings' instead." #-}

-- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
--
-- /Note:/ Consider using 'm2tsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csM2tsSettings :: Lens.Lens' ContainerSettings (Core.Maybe Types.M2tsSettings)
csM2tsSettings = Lens.field @"m2tsSettings"
{-# DEPRECATED csM2tsSettings "Use generic-lens or generic-optics with 'm2tsSettings' instead." #-}

-- | Settings for TS segments in HLS
--
-- /Note:/ Consider using 'm3u8Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csM3u8Settings :: Lens.Lens' ContainerSettings (Core.Maybe Types.M3u8Settings)
csM3u8Settings = Lens.field @"m3u8Settings"
{-# DEPRECATED csM3u8Settings "Use generic-lens or generic-optics with 'm3u8Settings' instead." #-}

-- | Settings for MOV Container.
--
-- /Note:/ Consider using 'movSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMovSettings :: Lens.Lens' ContainerSettings (Core.Maybe Types.MovSettings)
csMovSettings = Lens.field @"movSettings"
{-# DEPRECATED csMovSettings "Use generic-lens or generic-optics with 'movSettings' instead." #-}

-- | Settings for MP4 container. You can create audio-only AAC outputs with this container.
--
-- /Note:/ Consider using 'mp4Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMp4Settings :: Lens.Lens' ContainerSettings (Core.Maybe Types.Mp4Settings)
csMp4Settings = Lens.field @"mp4Settings"
{-# DEPRECATED csMp4Settings "Use generic-lens or generic-optics with 'mp4Settings' instead." #-}

-- | Settings for MP4 segments in DASH
--
-- /Note:/ Consider using 'mpdSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMpdSettings :: Lens.Lens' ContainerSettings (Core.Maybe Types.MpdSettings)
csMpdSettings = Lens.field @"mpdSettings"
{-# DEPRECATED csMpdSettings "Use generic-lens or generic-optics with 'mpdSettings' instead." #-}

-- | MXF settings
--
-- /Note:/ Consider using 'mxfSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMxfSettings :: Lens.Lens' ContainerSettings (Core.Maybe Types.MxfSettings)
csMxfSettings = Lens.field @"mxfSettings"
{-# DEPRECATED csMxfSettings "Use generic-lens or generic-optics with 'mxfSettings' instead." #-}

instance Core.FromJSON ContainerSettings where
  toJSON ContainerSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("cmfcSettings" Core..=) Core.<$> cmfcSettings,
            ("container" Core..=) Core.<$> container,
            ("f4vSettings" Core..=) Core.<$> f4vSettings,
            ("m2tsSettings" Core..=) Core.<$> m2tsSettings,
            ("m3u8Settings" Core..=) Core.<$> m3u8Settings,
            ("movSettings" Core..=) Core.<$> movSettings,
            ("mp4Settings" Core..=) Core.<$> mp4Settings,
            ("mpdSettings" Core..=) Core.<$> mpdSettings,
            ("mxfSettings" Core..=) Core.<$> mxfSettings
          ]
      )

instance Core.FromJSON ContainerSettings where
  parseJSON =
    Core.withObject "ContainerSettings" Core.$
      \x ->
        ContainerSettings'
          Core.<$> (x Core..:? "cmfcSettings")
          Core.<*> (x Core..:? "container")
          Core.<*> (x Core..:? "f4vSettings")
          Core.<*> (x Core..:? "m2tsSettings")
          Core.<*> (x Core..:? "m3u8Settings")
          Core.<*> (x Core..:? "movSettings")
          Core.<*> (x Core..:? "mp4Settings")
          Core.<*> (x Core..:? "mpdSettings")
          Core.<*> (x Core..:? "mxfSettings")
