{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ContainerSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ContainerSettings where

import qualified Network.AWS.Core as Core
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

-- | Container specific settings.
--
-- /See:/ 'newContainerSettings' smart constructor.
data ContainerSettings = ContainerSettings'
  { -- | Container for this output. Some containers require a container settings
    -- object. If not specified, the default object will be created.
    container :: Core.Maybe ContainerType,
    -- | Settings for MP4 segments in DASH
    mpdSettings :: Core.Maybe MpdSettings,
    -- | Settings for MP4 container. You can create audio-only AAC outputs with
    -- this container.
    mp4Settings :: Core.Maybe Mp4Settings,
    -- | Settings for F4v container
    f4vSettings :: Core.Maybe F4vSettings,
    -- | MXF settings
    mxfSettings :: Core.Maybe MxfSettings,
    -- | Settings for MOV Container.
    movSettings :: Core.Maybe MovSettings,
    -- | Settings for MP4 segments in CMAF
    cmfcSettings :: Core.Maybe CmfcSettings,
    -- | Settings for TS segments in HLS
    m3u8Settings :: Core.Maybe M3u8Settings,
    -- | MPEG-2 TS container settings. These apply to outputs in a File output
    -- group when the output\'s container (ContainerType) is MPEG-2 Transport
    -- Stream (M2TS). In these assets, data is organized by the program map
    -- table (PMT). Each transport stream program contains subsets of data,
    -- including audio, video, and metadata. Each of these subsets of data has
    -- a numerical label called a packet identifier (PID). Each transport
    -- stream program corresponds to one MediaConvert output. The PMT lists the
    -- types of data in a program along with their PID. Downstream systems and
    -- players use the program map table to look up the PID for each type of
    -- data it accesses and then uses the PIDs to locate specific data within
    -- the asset.
    m2tsSettings :: Core.Maybe M2tsSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContainerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'container', 'containerSettings_container' - Container for this output. Some containers require a container settings
-- object. If not specified, the default object will be created.
--
-- 'mpdSettings', 'containerSettings_mpdSettings' - Settings for MP4 segments in DASH
--
-- 'mp4Settings', 'containerSettings_mp4Settings' - Settings for MP4 container. You can create audio-only AAC outputs with
-- this container.
--
-- 'f4vSettings', 'containerSettings_f4vSettings' - Settings for F4v container
--
-- 'mxfSettings', 'containerSettings_mxfSettings' - MXF settings
--
-- 'movSettings', 'containerSettings_movSettings' - Settings for MOV Container.
--
-- 'cmfcSettings', 'containerSettings_cmfcSettings' - Settings for MP4 segments in CMAF
--
-- 'm3u8Settings', 'containerSettings_m3u8Settings' - Settings for TS segments in HLS
--
-- 'm2tsSettings', 'containerSettings_m2tsSettings' - MPEG-2 TS container settings. These apply to outputs in a File output
-- group when the output\'s container (ContainerType) is MPEG-2 Transport
-- Stream (M2TS). In these assets, data is organized by the program map
-- table (PMT). Each transport stream program contains subsets of data,
-- including audio, video, and metadata. Each of these subsets of data has
-- a numerical label called a packet identifier (PID). Each transport
-- stream program corresponds to one MediaConvert output. The PMT lists the
-- types of data in a program along with their PID. Downstream systems and
-- players use the program map table to look up the PID for each type of
-- data it accesses and then uses the PIDs to locate specific data within
-- the asset.
newContainerSettings ::
  ContainerSettings
newContainerSettings =
  ContainerSettings'
    { container = Core.Nothing,
      mpdSettings = Core.Nothing,
      mp4Settings = Core.Nothing,
      f4vSettings = Core.Nothing,
      mxfSettings = Core.Nothing,
      movSettings = Core.Nothing,
      cmfcSettings = Core.Nothing,
      m3u8Settings = Core.Nothing,
      m2tsSettings = Core.Nothing
    }

-- | Container for this output. Some containers require a container settings
-- object. If not specified, the default object will be created.
containerSettings_container :: Lens.Lens' ContainerSettings (Core.Maybe ContainerType)
containerSettings_container = Lens.lens (\ContainerSettings' {container} -> container) (\s@ContainerSettings' {} a -> s {container = a} :: ContainerSettings)

-- | Settings for MP4 segments in DASH
containerSettings_mpdSettings :: Lens.Lens' ContainerSettings (Core.Maybe MpdSettings)
containerSettings_mpdSettings = Lens.lens (\ContainerSettings' {mpdSettings} -> mpdSettings) (\s@ContainerSettings' {} a -> s {mpdSettings = a} :: ContainerSettings)

-- | Settings for MP4 container. You can create audio-only AAC outputs with
-- this container.
containerSettings_mp4Settings :: Lens.Lens' ContainerSettings (Core.Maybe Mp4Settings)
containerSettings_mp4Settings = Lens.lens (\ContainerSettings' {mp4Settings} -> mp4Settings) (\s@ContainerSettings' {} a -> s {mp4Settings = a} :: ContainerSettings)

-- | Settings for F4v container
containerSettings_f4vSettings :: Lens.Lens' ContainerSettings (Core.Maybe F4vSettings)
containerSettings_f4vSettings = Lens.lens (\ContainerSettings' {f4vSettings} -> f4vSettings) (\s@ContainerSettings' {} a -> s {f4vSettings = a} :: ContainerSettings)

-- | MXF settings
containerSettings_mxfSettings :: Lens.Lens' ContainerSettings (Core.Maybe MxfSettings)
containerSettings_mxfSettings = Lens.lens (\ContainerSettings' {mxfSettings} -> mxfSettings) (\s@ContainerSettings' {} a -> s {mxfSettings = a} :: ContainerSettings)

-- | Settings for MOV Container.
containerSettings_movSettings :: Lens.Lens' ContainerSettings (Core.Maybe MovSettings)
containerSettings_movSettings = Lens.lens (\ContainerSettings' {movSettings} -> movSettings) (\s@ContainerSettings' {} a -> s {movSettings = a} :: ContainerSettings)

-- | Settings for MP4 segments in CMAF
containerSettings_cmfcSettings :: Lens.Lens' ContainerSettings (Core.Maybe CmfcSettings)
containerSettings_cmfcSettings = Lens.lens (\ContainerSettings' {cmfcSettings} -> cmfcSettings) (\s@ContainerSettings' {} a -> s {cmfcSettings = a} :: ContainerSettings)

-- | Settings for TS segments in HLS
containerSettings_m3u8Settings :: Lens.Lens' ContainerSettings (Core.Maybe M3u8Settings)
containerSettings_m3u8Settings = Lens.lens (\ContainerSettings' {m3u8Settings} -> m3u8Settings) (\s@ContainerSettings' {} a -> s {m3u8Settings = a} :: ContainerSettings)

-- | MPEG-2 TS container settings. These apply to outputs in a File output
-- group when the output\'s container (ContainerType) is MPEG-2 Transport
-- Stream (M2TS). In these assets, data is organized by the program map
-- table (PMT). Each transport stream program contains subsets of data,
-- including audio, video, and metadata. Each of these subsets of data has
-- a numerical label called a packet identifier (PID). Each transport
-- stream program corresponds to one MediaConvert output. The PMT lists the
-- types of data in a program along with their PID. Downstream systems and
-- players use the program map table to look up the PID for each type of
-- data it accesses and then uses the PIDs to locate specific data within
-- the asset.
containerSettings_m2tsSettings :: Lens.Lens' ContainerSettings (Core.Maybe M2tsSettings)
containerSettings_m2tsSettings = Lens.lens (\ContainerSettings' {m2tsSettings} -> m2tsSettings) (\s@ContainerSettings' {} a -> s {m2tsSettings = a} :: ContainerSettings)

instance Core.FromJSON ContainerSettings where
  parseJSON =
    Core.withObject
      "ContainerSettings"
      ( \x ->
          ContainerSettings'
            Core.<$> (x Core..:? "container")
            Core.<*> (x Core..:? "mpdSettings")
            Core.<*> (x Core..:? "mp4Settings")
            Core.<*> (x Core..:? "f4vSettings")
            Core.<*> (x Core..:? "mxfSettings")
            Core.<*> (x Core..:? "movSettings")
            Core.<*> (x Core..:? "cmfcSettings")
            Core.<*> (x Core..:? "m3u8Settings")
            Core.<*> (x Core..:? "m2tsSettings")
      )

instance Core.Hashable ContainerSettings

instance Core.NFData ContainerSettings

instance Core.ToJSON ContainerSettings where
  toJSON ContainerSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("container" Core..=) Core.<$> container,
            ("mpdSettings" Core..=) Core.<$> mpdSettings,
            ("mp4Settings" Core..=) Core.<$> mp4Settings,
            ("f4vSettings" Core..=) Core.<$> f4vSettings,
            ("mxfSettings" Core..=) Core.<$> mxfSettings,
            ("movSettings" Core..=) Core.<$> movSettings,
            ("cmfcSettings" Core..=) Core.<$> cmfcSettings,
            ("m3u8Settings" Core..=) Core.<$> m3u8Settings,
            ("m2tsSettings" Core..=) Core.<$> m2tsSettings
          ]
      )
