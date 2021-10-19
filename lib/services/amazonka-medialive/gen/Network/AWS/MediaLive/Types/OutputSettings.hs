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
import qualified Network.AWS.Prelude as Prelude

-- | Output Settings
--
-- /See:/ 'newOutputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { multiplexOutputSettings :: Prelude.Maybe MultiplexOutputSettings,
    archiveOutputSettings :: Prelude.Maybe ArchiveOutputSettings,
    rtmpOutputSettings :: Prelude.Maybe RtmpOutputSettings,
    mediaPackageOutputSettings :: Prelude.Maybe MediaPackageOutputSettings,
    hlsOutputSettings :: Prelude.Maybe HlsOutputSettings,
    frameCaptureOutputSettings :: Prelude.Maybe FrameCaptureOutputSettings,
    udpOutputSettings :: Prelude.Maybe UdpOutputSettings,
    msSmoothOutputSettings :: Prelude.Maybe MsSmoothOutputSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexOutputSettings', 'outputSettings_multiplexOutputSettings' - Undocumented member.
--
-- 'archiveOutputSettings', 'outputSettings_archiveOutputSettings' - Undocumented member.
--
-- 'rtmpOutputSettings', 'outputSettings_rtmpOutputSettings' - Undocumented member.
--
-- 'mediaPackageOutputSettings', 'outputSettings_mediaPackageOutputSettings' - Undocumented member.
--
-- 'hlsOutputSettings', 'outputSettings_hlsOutputSettings' - Undocumented member.
--
-- 'frameCaptureOutputSettings', 'outputSettings_frameCaptureOutputSettings' - Undocumented member.
--
-- 'udpOutputSettings', 'outputSettings_udpOutputSettings' - Undocumented member.
--
-- 'msSmoothOutputSettings', 'outputSettings_msSmoothOutputSettings' - Undocumented member.
newOutputSettings ::
  OutputSettings
newOutputSettings =
  OutputSettings'
    { multiplexOutputSettings =
        Prelude.Nothing,
      archiveOutputSettings = Prelude.Nothing,
      rtmpOutputSettings = Prelude.Nothing,
      mediaPackageOutputSettings = Prelude.Nothing,
      hlsOutputSettings = Prelude.Nothing,
      frameCaptureOutputSettings = Prelude.Nothing,
      udpOutputSettings = Prelude.Nothing,
      msSmoothOutputSettings = Prelude.Nothing
    }

-- | Undocumented member.
outputSettings_multiplexOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MultiplexOutputSettings)
outputSettings_multiplexOutputSettings = Lens.lens (\OutputSettings' {multiplexOutputSettings} -> multiplexOutputSettings) (\s@OutputSettings' {} a -> s {multiplexOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_archiveOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe ArchiveOutputSettings)
outputSettings_archiveOutputSettings = Lens.lens (\OutputSettings' {archiveOutputSettings} -> archiveOutputSettings) (\s@OutputSettings' {} a -> s {archiveOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_rtmpOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe RtmpOutputSettings)
outputSettings_rtmpOutputSettings = Lens.lens (\OutputSettings' {rtmpOutputSettings} -> rtmpOutputSettings) (\s@OutputSettings' {} a -> s {rtmpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_mediaPackageOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MediaPackageOutputSettings)
outputSettings_mediaPackageOutputSettings = Lens.lens (\OutputSettings' {mediaPackageOutputSettings} -> mediaPackageOutputSettings) (\s@OutputSettings' {} a -> s {mediaPackageOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_hlsOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe HlsOutputSettings)
outputSettings_hlsOutputSettings = Lens.lens (\OutputSettings' {hlsOutputSettings} -> hlsOutputSettings) (\s@OutputSettings' {} a -> s {hlsOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_frameCaptureOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe FrameCaptureOutputSettings)
outputSettings_frameCaptureOutputSettings = Lens.lens (\OutputSettings' {frameCaptureOutputSettings} -> frameCaptureOutputSettings) (\s@OutputSettings' {} a -> s {frameCaptureOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_udpOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe UdpOutputSettings)
outputSettings_udpOutputSettings = Lens.lens (\OutputSettings' {udpOutputSettings} -> udpOutputSettings) (\s@OutputSettings' {} a -> s {udpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_msSmoothOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MsSmoothOutputSettings)
outputSettings_msSmoothOutputSettings = Lens.lens (\OutputSettings' {msSmoothOutputSettings} -> msSmoothOutputSettings) (\s@OutputSettings' {} a -> s {msSmoothOutputSettings = a} :: OutputSettings)

instance Core.FromJSON OutputSettings where
  parseJSON =
    Core.withObject
      "OutputSettings"
      ( \x ->
          OutputSettings'
            Prelude.<$> (x Core..:? "multiplexOutputSettings")
            Prelude.<*> (x Core..:? "archiveOutputSettings")
            Prelude.<*> (x Core..:? "rtmpOutputSettings")
            Prelude.<*> (x Core..:? "mediaPackageOutputSettings")
            Prelude.<*> (x Core..:? "hlsOutputSettings")
            Prelude.<*> (x Core..:? "frameCaptureOutputSettings")
            Prelude.<*> (x Core..:? "udpOutputSettings")
            Prelude.<*> (x Core..:? "msSmoothOutputSettings")
      )

instance Prelude.Hashable OutputSettings

instance Prelude.NFData OutputSettings

instance Core.ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("multiplexOutputSettings" Core..=)
              Prelude.<$> multiplexOutputSettings,
            ("archiveOutputSettings" Core..=)
              Prelude.<$> archiveOutputSettings,
            ("rtmpOutputSettings" Core..=)
              Prelude.<$> rtmpOutputSettings,
            ("mediaPackageOutputSettings" Core..=)
              Prelude.<$> mediaPackageOutputSettings,
            ("hlsOutputSettings" Core..=)
              Prelude.<$> hlsOutputSettings,
            ("frameCaptureOutputSettings" Core..=)
              Prelude.<$> frameCaptureOutputSettings,
            ("udpOutputSettings" Core..=)
              Prelude.<$> udpOutputSettings,
            ("msSmoothOutputSettings" Core..=)
              Prelude.<$> msSmoothOutputSettings
          ]
      )
