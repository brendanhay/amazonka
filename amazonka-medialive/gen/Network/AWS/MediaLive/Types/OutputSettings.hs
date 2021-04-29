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
-- Module      : Network.AWS.MediaLive.Types.OutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
import Network.AWS.MediaLive.Types.HlsOutputSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputSettings
import Network.AWS.MediaLive.Types.MsSmoothOutputSettings
import Network.AWS.MediaLive.Types.MultiplexOutputSettings
import Network.AWS.MediaLive.Types.RtmpOutputSettings
import Network.AWS.MediaLive.Types.UdpOutputSettings
import qualified Network.AWS.Prelude as Prelude

-- | Output Settings
--
-- /See:/ 'newOutputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { rtmpOutputSettings :: Prelude.Maybe RtmpOutputSettings,
    msSmoothOutputSettings :: Prelude.Maybe MsSmoothOutputSettings,
    udpOutputSettings :: Prelude.Maybe UdpOutputSettings,
    mediaPackageOutputSettings :: Prelude.Maybe MediaPackageOutputSettings,
    frameCaptureOutputSettings :: Prelude.Maybe FrameCaptureOutputSettings,
    archiveOutputSettings :: Prelude.Maybe ArchiveOutputSettings,
    hlsOutputSettings :: Prelude.Maybe HlsOutputSettings,
    multiplexOutputSettings :: Prelude.Maybe MultiplexOutputSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rtmpOutputSettings', 'outputSettings_rtmpOutputSettings' - Undocumented member.
--
-- 'msSmoothOutputSettings', 'outputSettings_msSmoothOutputSettings' - Undocumented member.
--
-- 'udpOutputSettings', 'outputSettings_udpOutputSettings' - Undocumented member.
--
-- 'mediaPackageOutputSettings', 'outputSettings_mediaPackageOutputSettings' - Undocumented member.
--
-- 'frameCaptureOutputSettings', 'outputSettings_frameCaptureOutputSettings' - Undocumented member.
--
-- 'archiveOutputSettings', 'outputSettings_archiveOutputSettings' - Undocumented member.
--
-- 'hlsOutputSettings', 'outputSettings_hlsOutputSettings' - Undocumented member.
--
-- 'multiplexOutputSettings', 'outputSettings_multiplexOutputSettings' - Undocumented member.
newOutputSettings ::
  OutputSettings
newOutputSettings =
  OutputSettings'
    { rtmpOutputSettings =
        Prelude.Nothing,
      msSmoothOutputSettings = Prelude.Nothing,
      udpOutputSettings = Prelude.Nothing,
      mediaPackageOutputSettings = Prelude.Nothing,
      frameCaptureOutputSettings = Prelude.Nothing,
      archiveOutputSettings = Prelude.Nothing,
      hlsOutputSettings = Prelude.Nothing,
      multiplexOutputSettings = Prelude.Nothing
    }

-- | Undocumented member.
outputSettings_rtmpOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe RtmpOutputSettings)
outputSettings_rtmpOutputSettings = Lens.lens (\OutputSettings' {rtmpOutputSettings} -> rtmpOutputSettings) (\s@OutputSettings' {} a -> s {rtmpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_msSmoothOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MsSmoothOutputSettings)
outputSettings_msSmoothOutputSettings = Lens.lens (\OutputSettings' {msSmoothOutputSettings} -> msSmoothOutputSettings) (\s@OutputSettings' {} a -> s {msSmoothOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_udpOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe UdpOutputSettings)
outputSettings_udpOutputSettings = Lens.lens (\OutputSettings' {udpOutputSettings} -> udpOutputSettings) (\s@OutputSettings' {} a -> s {udpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_mediaPackageOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MediaPackageOutputSettings)
outputSettings_mediaPackageOutputSettings = Lens.lens (\OutputSettings' {mediaPackageOutputSettings} -> mediaPackageOutputSettings) (\s@OutputSettings' {} a -> s {mediaPackageOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_frameCaptureOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe FrameCaptureOutputSettings)
outputSettings_frameCaptureOutputSettings = Lens.lens (\OutputSettings' {frameCaptureOutputSettings} -> frameCaptureOutputSettings) (\s@OutputSettings' {} a -> s {frameCaptureOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_archiveOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe ArchiveOutputSettings)
outputSettings_archiveOutputSettings = Lens.lens (\OutputSettings' {archiveOutputSettings} -> archiveOutputSettings) (\s@OutputSettings' {} a -> s {archiveOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_hlsOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe HlsOutputSettings)
outputSettings_hlsOutputSettings = Lens.lens (\OutputSettings' {hlsOutputSettings} -> hlsOutputSettings) (\s@OutputSettings' {} a -> s {hlsOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_multiplexOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MultiplexOutputSettings)
outputSettings_multiplexOutputSettings = Lens.lens (\OutputSettings' {multiplexOutputSettings} -> multiplexOutputSettings) (\s@OutputSettings' {} a -> s {multiplexOutputSettings = a} :: OutputSettings)

instance Prelude.FromJSON OutputSettings where
  parseJSON =
    Prelude.withObject
      "OutputSettings"
      ( \x ->
          OutputSettings'
            Prelude.<$> (x Prelude..:? "rtmpOutputSettings")
            Prelude.<*> (x Prelude..:? "msSmoothOutputSettings")
            Prelude.<*> (x Prelude..:? "udpOutputSettings")
            Prelude.<*> (x Prelude..:? "mediaPackageOutputSettings")
            Prelude.<*> (x Prelude..:? "frameCaptureOutputSettings")
            Prelude.<*> (x Prelude..:? "archiveOutputSettings")
            Prelude.<*> (x Prelude..:? "hlsOutputSettings")
            Prelude.<*> (x Prelude..:? "multiplexOutputSettings")
      )

instance Prelude.Hashable OutputSettings

instance Prelude.NFData OutputSettings

instance Prelude.ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("rtmpOutputSettings" Prelude..=)
              Prelude.<$> rtmpOutputSettings,
            ("msSmoothOutputSettings" Prelude..=)
              Prelude.<$> msSmoothOutputSettings,
            ("udpOutputSettings" Prelude..=)
              Prelude.<$> udpOutputSettings,
            ("mediaPackageOutputSettings" Prelude..=)
              Prelude.<$> mediaPackageOutputSettings,
            ("frameCaptureOutputSettings" Prelude..=)
              Prelude.<$> frameCaptureOutputSettings,
            ("archiveOutputSettings" Prelude..=)
              Prelude.<$> archiveOutputSettings,
            ("hlsOutputSettings" Prelude..=)
              Prelude.<$> hlsOutputSettings,
            ("multiplexOutputSettings" Prelude..=)
              Prelude.<$> multiplexOutputSettings
          ]
      )
