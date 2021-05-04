{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Prelude as Prelude

-- | Container specific settings.
--
-- /See:/ 'newContainerSettings' smart constructor.
data ContainerSettings = ContainerSettings'
  { -- | Container for this output. Some containers require a container settings
    -- object. If not specified, the default object will be created.
    container :: Prelude.Maybe ContainerType,
    -- | Settings for MP4 segments in DASH
    mpdSettings :: Prelude.Maybe MpdSettings,
    -- | Settings for MP4 container. You can create audio-only AAC outputs with
    -- this container.
    mp4Settings :: Prelude.Maybe Mp4Settings,
    -- | Settings for F4v container
    f4vSettings :: Prelude.Maybe F4vSettings,
    -- | MXF settings
    mxfSettings :: Prelude.Maybe MxfSettings,
    -- | Settings for MOV Container.
    movSettings :: Prelude.Maybe MovSettings,
    -- | Settings for MP4 segments in CMAF
    cmfcSettings :: Prelude.Maybe CmfcSettings,
    -- | Settings for TS segments in HLS
    m3u8Settings :: Prelude.Maybe M3u8Settings,
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
    m2tsSettings :: Prelude.Maybe M2tsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { container = Prelude.Nothing,
      mpdSettings = Prelude.Nothing,
      mp4Settings = Prelude.Nothing,
      f4vSettings = Prelude.Nothing,
      mxfSettings = Prelude.Nothing,
      movSettings = Prelude.Nothing,
      cmfcSettings = Prelude.Nothing,
      m3u8Settings = Prelude.Nothing,
      m2tsSettings = Prelude.Nothing
    }

-- | Container for this output. Some containers require a container settings
-- object. If not specified, the default object will be created.
containerSettings_container :: Lens.Lens' ContainerSettings (Prelude.Maybe ContainerType)
containerSettings_container = Lens.lens (\ContainerSettings' {container} -> container) (\s@ContainerSettings' {} a -> s {container = a} :: ContainerSettings)

-- | Settings for MP4 segments in DASH
containerSettings_mpdSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe MpdSettings)
containerSettings_mpdSettings = Lens.lens (\ContainerSettings' {mpdSettings} -> mpdSettings) (\s@ContainerSettings' {} a -> s {mpdSettings = a} :: ContainerSettings)

-- | Settings for MP4 container. You can create audio-only AAC outputs with
-- this container.
containerSettings_mp4Settings :: Lens.Lens' ContainerSettings (Prelude.Maybe Mp4Settings)
containerSettings_mp4Settings = Lens.lens (\ContainerSettings' {mp4Settings} -> mp4Settings) (\s@ContainerSettings' {} a -> s {mp4Settings = a} :: ContainerSettings)

-- | Settings for F4v container
containerSettings_f4vSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe F4vSettings)
containerSettings_f4vSettings = Lens.lens (\ContainerSettings' {f4vSettings} -> f4vSettings) (\s@ContainerSettings' {} a -> s {f4vSettings = a} :: ContainerSettings)

-- | MXF settings
containerSettings_mxfSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe MxfSettings)
containerSettings_mxfSettings = Lens.lens (\ContainerSettings' {mxfSettings} -> mxfSettings) (\s@ContainerSettings' {} a -> s {mxfSettings = a} :: ContainerSettings)

-- | Settings for MOV Container.
containerSettings_movSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe MovSettings)
containerSettings_movSettings = Lens.lens (\ContainerSettings' {movSettings} -> movSettings) (\s@ContainerSettings' {} a -> s {movSettings = a} :: ContainerSettings)

-- | Settings for MP4 segments in CMAF
containerSettings_cmfcSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe CmfcSettings)
containerSettings_cmfcSettings = Lens.lens (\ContainerSettings' {cmfcSettings} -> cmfcSettings) (\s@ContainerSettings' {} a -> s {cmfcSettings = a} :: ContainerSettings)

-- | Settings for TS segments in HLS
containerSettings_m3u8Settings :: Lens.Lens' ContainerSettings (Prelude.Maybe M3u8Settings)
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
containerSettings_m2tsSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe M2tsSettings)
containerSettings_m2tsSettings = Lens.lens (\ContainerSettings' {m2tsSettings} -> m2tsSettings) (\s@ContainerSettings' {} a -> s {m2tsSettings = a} :: ContainerSettings)

instance Prelude.FromJSON ContainerSettings where
  parseJSON =
    Prelude.withObject
      "ContainerSettings"
      ( \x ->
          ContainerSettings'
            Prelude.<$> (x Prelude..:? "container")
            Prelude.<*> (x Prelude..:? "mpdSettings")
            Prelude.<*> (x Prelude..:? "mp4Settings")
            Prelude.<*> (x Prelude..:? "f4vSettings")
            Prelude.<*> (x Prelude..:? "mxfSettings")
            Prelude.<*> (x Prelude..:? "movSettings")
            Prelude.<*> (x Prelude..:? "cmfcSettings")
            Prelude.<*> (x Prelude..:? "m3u8Settings")
            Prelude.<*> (x Prelude..:? "m2tsSettings")
      )

instance Prelude.Hashable ContainerSettings

instance Prelude.NFData ContainerSettings

instance Prelude.ToJSON ContainerSettings where
  toJSON ContainerSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("container" Prelude..=) Prelude.<$> container,
            ("mpdSettings" Prelude..=) Prelude.<$> mpdSettings,
            ("mp4Settings" Prelude..=) Prelude.<$> mp4Settings,
            ("f4vSettings" Prelude..=) Prelude.<$> f4vSettings,
            ("mxfSettings" Prelude..=) Prelude.<$> mxfSettings,
            ("movSettings" Prelude..=) Prelude.<$> movSettings,
            ("cmfcSettings" Prelude..=) Prelude.<$> cmfcSettings,
            ("m3u8Settings" Prelude..=) Prelude.<$> m3u8Settings,
            ("m2tsSettings" Prelude..=)
              Prelude.<$> m2tsSettings
          ]
      )
