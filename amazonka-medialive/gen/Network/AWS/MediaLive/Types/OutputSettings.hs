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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
import Network.AWS.MediaLive.Types.HlsOutputSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputSettings
import Network.AWS.MediaLive.Types.MsSmoothOutputSettings
import Network.AWS.MediaLive.Types.MultiplexOutputSettings
import Network.AWS.MediaLive.Types.RtmpOutputSettings
import Network.AWS.MediaLive.Types.UdpOutputSettings

-- | Output Settings
--
-- /See:/ 'newOutputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { rtmpOutputSettings :: Core.Maybe RtmpOutputSettings,
    msSmoothOutputSettings :: Core.Maybe MsSmoothOutputSettings,
    udpOutputSettings :: Core.Maybe UdpOutputSettings,
    mediaPackageOutputSettings :: Core.Maybe MediaPackageOutputSettings,
    frameCaptureOutputSettings :: Core.Maybe FrameCaptureOutputSettings,
    archiveOutputSettings :: Core.Maybe ArchiveOutputSettings,
    hlsOutputSettings :: Core.Maybe HlsOutputSettings,
    multiplexOutputSettings :: Core.Maybe MultiplexOutputSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { rtmpOutputSettings = Core.Nothing,
      msSmoothOutputSettings = Core.Nothing,
      udpOutputSettings = Core.Nothing,
      mediaPackageOutputSettings = Core.Nothing,
      frameCaptureOutputSettings = Core.Nothing,
      archiveOutputSettings = Core.Nothing,
      hlsOutputSettings = Core.Nothing,
      multiplexOutputSettings = Core.Nothing
    }

-- | Undocumented member.
outputSettings_rtmpOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe RtmpOutputSettings)
outputSettings_rtmpOutputSettings = Lens.lens (\OutputSettings' {rtmpOutputSettings} -> rtmpOutputSettings) (\s@OutputSettings' {} a -> s {rtmpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_msSmoothOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe MsSmoothOutputSettings)
outputSettings_msSmoothOutputSettings = Lens.lens (\OutputSettings' {msSmoothOutputSettings} -> msSmoothOutputSettings) (\s@OutputSettings' {} a -> s {msSmoothOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_udpOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe UdpOutputSettings)
outputSettings_udpOutputSettings = Lens.lens (\OutputSettings' {udpOutputSettings} -> udpOutputSettings) (\s@OutputSettings' {} a -> s {udpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_mediaPackageOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe MediaPackageOutputSettings)
outputSettings_mediaPackageOutputSettings = Lens.lens (\OutputSettings' {mediaPackageOutputSettings} -> mediaPackageOutputSettings) (\s@OutputSettings' {} a -> s {mediaPackageOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_frameCaptureOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe FrameCaptureOutputSettings)
outputSettings_frameCaptureOutputSettings = Lens.lens (\OutputSettings' {frameCaptureOutputSettings} -> frameCaptureOutputSettings) (\s@OutputSettings' {} a -> s {frameCaptureOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_archiveOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe ArchiveOutputSettings)
outputSettings_archiveOutputSettings = Lens.lens (\OutputSettings' {archiveOutputSettings} -> archiveOutputSettings) (\s@OutputSettings' {} a -> s {archiveOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_hlsOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe HlsOutputSettings)
outputSettings_hlsOutputSettings = Lens.lens (\OutputSettings' {hlsOutputSettings} -> hlsOutputSettings) (\s@OutputSettings' {} a -> s {hlsOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_multiplexOutputSettings :: Lens.Lens' OutputSettings (Core.Maybe MultiplexOutputSettings)
outputSettings_multiplexOutputSettings = Lens.lens (\OutputSettings' {multiplexOutputSettings} -> multiplexOutputSettings) (\s@OutputSettings' {} a -> s {multiplexOutputSettings = a} :: OutputSettings)

instance Core.FromJSON OutputSettings where
  parseJSON =
    Core.withObject
      "OutputSettings"
      ( \x ->
          OutputSettings'
            Core.<$> (x Core..:? "rtmpOutputSettings")
            Core.<*> (x Core..:? "msSmoothOutputSettings")
            Core.<*> (x Core..:? "udpOutputSettings")
            Core.<*> (x Core..:? "mediaPackageOutputSettings")
            Core.<*> (x Core..:? "frameCaptureOutputSettings")
            Core.<*> (x Core..:? "archiveOutputSettings")
            Core.<*> (x Core..:? "hlsOutputSettings")
            Core.<*> (x Core..:? "multiplexOutputSettings")
      )

instance Core.Hashable OutputSettings

instance Core.NFData OutputSettings

instance Core.ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rtmpOutputSettings" Core..=)
              Core.<$> rtmpOutputSettings,
            ("msSmoothOutputSettings" Core..=)
              Core.<$> msSmoothOutputSettings,
            ("udpOutputSettings" Core..=)
              Core.<$> udpOutputSettings,
            ("mediaPackageOutputSettings" Core..=)
              Core.<$> mediaPackageOutputSettings,
            ("frameCaptureOutputSettings" Core..=)
              Core.<$> frameCaptureOutputSettings,
            ("archiveOutputSettings" Core..=)
              Core.<$> archiveOutputSettings,
            ("hlsOutputSettings" Core..=)
              Core.<$> hlsOutputSettings,
            ("multiplexOutputSettings" Core..=)
              Core.<$> multiplexOutputSettings
          ]
      )
