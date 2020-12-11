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
    csM2tsSettings,
    csMxfSettings,
    csM3u8Settings,
    csCmfcSettings,
    csMovSettings,
    csMp4Settings,
    csMpdSettings,
    csContainer,
    csF4vSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmfcSettings
import Network.AWS.MediaConvert.Types.ContainerType
import Network.AWS.MediaConvert.Types.F4vSettings
import Network.AWS.MediaConvert.Types.M2tsSettings
import Network.AWS.MediaConvert.Types.M3u8Settings
import Network.AWS.MediaConvert.Types.MovSettings
import Network.AWS.MediaConvert.Types.Mp4Settings
import Network.AWS.MediaConvert.Types.MpdSettings
import Network.AWS.MediaConvert.Types.MxfSettings
import qualified Network.AWS.Prelude as Lude

-- | Container specific settings.
--
-- /See:/ 'mkContainerSettings' smart constructor.
data ContainerSettings = ContainerSettings'
  { m2tsSettings ::
      Lude.Maybe M2tsSettings,
    mxfSettings :: Lude.Maybe MxfSettings,
    m3u8Settings :: Lude.Maybe M3u8Settings,
    cmfcSettings :: Lude.Maybe CmfcSettings,
    movSettings :: Lude.Maybe MovSettings,
    mp4Settings :: Lude.Maybe Mp4Settings,
    mpdSettings :: Lude.Maybe MpdSettings,
    container :: Lude.Maybe ContainerType,
    f4vSettings :: Lude.Maybe F4vSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerSettings' with the minimum fields required to make a request.
--
-- * 'cmfcSettings' - Settings for MP4 segments in CMAF
-- * 'container' - Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
-- * 'f4vSettings' - Settings for F4v container
-- * 'm2tsSettings' - MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
-- * 'm3u8Settings' - Settings for TS segments in HLS
-- * 'movSettings' - Settings for MOV Container.
-- * 'mp4Settings' - Settings for MP4 container. You can create audio-only AAC outputs with this container.
-- * 'mpdSettings' - Settings for MP4 segments in DASH
-- * 'mxfSettings' - MXF settings
mkContainerSettings ::
  ContainerSettings
mkContainerSettings =
  ContainerSettings'
    { m2tsSettings = Lude.Nothing,
      mxfSettings = Lude.Nothing,
      m3u8Settings = Lude.Nothing,
      cmfcSettings = Lude.Nothing,
      movSettings = Lude.Nothing,
      mp4Settings = Lude.Nothing,
      mpdSettings = Lude.Nothing,
      container = Lude.Nothing,
      f4vSettings = Lude.Nothing
    }

-- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
--
-- /Note:/ Consider using 'm2tsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csM2tsSettings :: Lens.Lens' ContainerSettings (Lude.Maybe M2tsSettings)
csM2tsSettings = Lens.lens (m2tsSettings :: ContainerSettings -> Lude.Maybe M2tsSettings) (\s a -> s {m2tsSettings = a} :: ContainerSettings)
{-# DEPRECATED csM2tsSettings "Use generic-lens or generic-optics with 'm2tsSettings' instead." #-}

-- | MXF settings
--
-- /Note:/ Consider using 'mxfSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMxfSettings :: Lens.Lens' ContainerSettings (Lude.Maybe MxfSettings)
csMxfSettings = Lens.lens (mxfSettings :: ContainerSettings -> Lude.Maybe MxfSettings) (\s a -> s {mxfSettings = a} :: ContainerSettings)
{-# DEPRECATED csMxfSettings "Use generic-lens or generic-optics with 'mxfSettings' instead." #-}

-- | Settings for TS segments in HLS
--
-- /Note:/ Consider using 'm3u8Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csM3u8Settings :: Lens.Lens' ContainerSettings (Lude.Maybe M3u8Settings)
csM3u8Settings = Lens.lens (m3u8Settings :: ContainerSettings -> Lude.Maybe M3u8Settings) (\s a -> s {m3u8Settings = a} :: ContainerSettings)
{-# DEPRECATED csM3u8Settings "Use generic-lens or generic-optics with 'm3u8Settings' instead." #-}

-- | Settings for MP4 segments in CMAF
--
-- /Note:/ Consider using 'cmfcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCmfcSettings :: Lens.Lens' ContainerSettings (Lude.Maybe CmfcSettings)
csCmfcSettings = Lens.lens (cmfcSettings :: ContainerSettings -> Lude.Maybe CmfcSettings) (\s a -> s {cmfcSettings = a} :: ContainerSettings)
{-# DEPRECATED csCmfcSettings "Use generic-lens or generic-optics with 'cmfcSettings' instead." #-}

-- | Settings for MOV Container.
--
-- /Note:/ Consider using 'movSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMovSettings :: Lens.Lens' ContainerSettings (Lude.Maybe MovSettings)
csMovSettings = Lens.lens (movSettings :: ContainerSettings -> Lude.Maybe MovSettings) (\s a -> s {movSettings = a} :: ContainerSettings)
{-# DEPRECATED csMovSettings "Use generic-lens or generic-optics with 'movSettings' instead." #-}

-- | Settings for MP4 container. You can create audio-only AAC outputs with this container.
--
-- /Note:/ Consider using 'mp4Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMp4Settings :: Lens.Lens' ContainerSettings (Lude.Maybe Mp4Settings)
csMp4Settings = Lens.lens (mp4Settings :: ContainerSettings -> Lude.Maybe Mp4Settings) (\s a -> s {mp4Settings = a} :: ContainerSettings)
{-# DEPRECATED csMp4Settings "Use generic-lens or generic-optics with 'mp4Settings' instead." #-}

-- | Settings for MP4 segments in DASH
--
-- /Note:/ Consider using 'mpdSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMpdSettings :: Lens.Lens' ContainerSettings (Lude.Maybe MpdSettings)
csMpdSettings = Lens.lens (mpdSettings :: ContainerSettings -> Lude.Maybe MpdSettings) (\s a -> s {mpdSettings = a} :: ContainerSettings)
{-# DEPRECATED csMpdSettings "Use generic-lens or generic-optics with 'mpdSettings' instead." #-}

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csContainer :: Lens.Lens' ContainerSettings (Lude.Maybe ContainerType)
csContainer = Lens.lens (container :: ContainerSettings -> Lude.Maybe ContainerType) (\s a -> s {container = a} :: ContainerSettings)
{-# DEPRECATED csContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | Settings for F4v container
--
-- /Note:/ Consider using 'f4vSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csF4vSettings :: Lens.Lens' ContainerSettings (Lude.Maybe F4vSettings)
csF4vSettings = Lens.lens (f4vSettings :: ContainerSettings -> Lude.Maybe F4vSettings) (\s a -> s {f4vSettings = a} :: ContainerSettings)
{-# DEPRECATED csF4vSettings "Use generic-lens or generic-optics with 'f4vSettings' instead." #-}

instance Lude.FromJSON ContainerSettings where
  parseJSON =
    Lude.withObject
      "ContainerSettings"
      ( \x ->
          ContainerSettings'
            Lude.<$> (x Lude..:? "m2tsSettings")
            Lude.<*> (x Lude..:? "mxfSettings")
            Lude.<*> (x Lude..:? "m3u8Settings")
            Lude.<*> (x Lude..:? "cmfcSettings")
            Lude.<*> (x Lude..:? "movSettings")
            Lude.<*> (x Lude..:? "mp4Settings")
            Lude.<*> (x Lude..:? "mpdSettings")
            Lude.<*> (x Lude..:? "container")
            Lude.<*> (x Lude..:? "f4vSettings")
      )

instance Lude.ToJSON ContainerSettings where
  toJSON ContainerSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("m2tsSettings" Lude..=) Lude.<$> m2tsSettings,
            ("mxfSettings" Lude..=) Lude.<$> mxfSettings,
            ("m3u8Settings" Lude..=) Lude.<$> m3u8Settings,
            ("cmfcSettings" Lude..=) Lude.<$> cmfcSettings,
            ("movSettings" Lude..=) Lude.<$> movSettings,
            ("mp4Settings" Lude..=) Lude.<$> mp4Settings,
            ("mpdSettings" Lude..=) Lude.<$> mpdSettings,
            ("container" Lude..=) Lude.<$> container,
            ("f4vSettings" Lude..=) Lude.<$> f4vSettings
          ]
      )
