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
-- Module      : Amazonka.MediaLive.Types.OutputSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.OutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.ArchiveOutputSettings
import Amazonka.MediaLive.Types.FrameCaptureOutputSettings
import Amazonka.MediaLive.Types.HlsOutputSettings
import Amazonka.MediaLive.Types.MediaPackageOutputSettings
import Amazonka.MediaLive.Types.MsSmoothOutputSettings
import Amazonka.MediaLive.Types.MultiplexOutputSettings
import Amazonka.MediaLive.Types.RtmpOutputSettings
import Amazonka.MediaLive.Types.UdpOutputSettings
import qualified Amazonka.Prelude as Prelude

-- | Output Settings
--
-- /See:/ 'newOutputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { multiplexOutputSettings :: Prelude.Maybe MultiplexOutputSettings,
    mediaPackageOutputSettings :: Prelude.Maybe MediaPackageOutputSettings,
    archiveOutputSettings :: Prelude.Maybe ArchiveOutputSettings,
    rtmpOutputSettings :: Prelude.Maybe RtmpOutputSettings,
    udpOutputSettings :: Prelude.Maybe UdpOutputSettings,
    frameCaptureOutputSettings :: Prelude.Maybe FrameCaptureOutputSettings,
    msSmoothOutputSettings :: Prelude.Maybe MsSmoothOutputSettings,
    hlsOutputSettings :: Prelude.Maybe HlsOutputSettings
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
-- 'mediaPackageOutputSettings', 'outputSettings_mediaPackageOutputSettings' - Undocumented member.
--
-- 'archiveOutputSettings', 'outputSettings_archiveOutputSettings' - Undocumented member.
--
-- 'rtmpOutputSettings', 'outputSettings_rtmpOutputSettings' - Undocumented member.
--
-- 'udpOutputSettings', 'outputSettings_udpOutputSettings' - Undocumented member.
--
-- 'frameCaptureOutputSettings', 'outputSettings_frameCaptureOutputSettings' - Undocumented member.
--
-- 'msSmoothOutputSettings', 'outputSettings_msSmoothOutputSettings' - Undocumented member.
--
-- 'hlsOutputSettings', 'outputSettings_hlsOutputSettings' - Undocumented member.
newOutputSettings ::
  OutputSettings
newOutputSettings =
  OutputSettings'
    { multiplexOutputSettings =
        Prelude.Nothing,
      mediaPackageOutputSettings = Prelude.Nothing,
      archiveOutputSettings = Prelude.Nothing,
      rtmpOutputSettings = Prelude.Nothing,
      udpOutputSettings = Prelude.Nothing,
      frameCaptureOutputSettings = Prelude.Nothing,
      msSmoothOutputSettings = Prelude.Nothing,
      hlsOutputSettings = Prelude.Nothing
    }

-- | Undocumented member.
outputSettings_multiplexOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MultiplexOutputSettings)
outputSettings_multiplexOutputSettings = Lens.lens (\OutputSettings' {multiplexOutputSettings} -> multiplexOutputSettings) (\s@OutputSettings' {} a -> s {multiplexOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_mediaPackageOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MediaPackageOutputSettings)
outputSettings_mediaPackageOutputSettings = Lens.lens (\OutputSettings' {mediaPackageOutputSettings} -> mediaPackageOutputSettings) (\s@OutputSettings' {} a -> s {mediaPackageOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_archiveOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe ArchiveOutputSettings)
outputSettings_archiveOutputSettings = Lens.lens (\OutputSettings' {archiveOutputSettings} -> archiveOutputSettings) (\s@OutputSettings' {} a -> s {archiveOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_rtmpOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe RtmpOutputSettings)
outputSettings_rtmpOutputSettings = Lens.lens (\OutputSettings' {rtmpOutputSettings} -> rtmpOutputSettings) (\s@OutputSettings' {} a -> s {rtmpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_udpOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe UdpOutputSettings)
outputSettings_udpOutputSettings = Lens.lens (\OutputSettings' {udpOutputSettings} -> udpOutputSettings) (\s@OutputSettings' {} a -> s {udpOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_frameCaptureOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe FrameCaptureOutputSettings)
outputSettings_frameCaptureOutputSettings = Lens.lens (\OutputSettings' {frameCaptureOutputSettings} -> frameCaptureOutputSettings) (\s@OutputSettings' {} a -> s {frameCaptureOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_msSmoothOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe MsSmoothOutputSettings)
outputSettings_msSmoothOutputSettings = Lens.lens (\OutputSettings' {msSmoothOutputSettings} -> msSmoothOutputSettings) (\s@OutputSettings' {} a -> s {msSmoothOutputSettings = a} :: OutputSettings)

-- | Undocumented member.
outputSettings_hlsOutputSettings :: Lens.Lens' OutputSettings (Prelude.Maybe HlsOutputSettings)
outputSettings_hlsOutputSettings = Lens.lens (\OutputSettings' {hlsOutputSettings} -> hlsOutputSettings) (\s@OutputSettings' {} a -> s {hlsOutputSettings = a} :: OutputSettings)

instance Core.FromJSON OutputSettings where
  parseJSON =
    Core.withObject
      "OutputSettings"
      ( \x ->
          OutputSettings'
            Prelude.<$> (x Core..:? "multiplexOutputSettings")
            Prelude.<*> (x Core..:? "mediaPackageOutputSettings")
            Prelude.<*> (x Core..:? "archiveOutputSettings")
            Prelude.<*> (x Core..:? "rtmpOutputSettings")
            Prelude.<*> (x Core..:? "udpOutputSettings")
            Prelude.<*> (x Core..:? "frameCaptureOutputSettings")
            Prelude.<*> (x Core..:? "msSmoothOutputSettings")
            Prelude.<*> (x Core..:? "hlsOutputSettings")
      )

instance Prelude.Hashable OutputSettings where
  hashWithSalt _salt OutputSettings' {..} =
    _salt
      `Prelude.hashWithSalt` multiplexOutputSettings
      `Prelude.hashWithSalt` mediaPackageOutputSettings
      `Prelude.hashWithSalt` archiveOutputSettings
      `Prelude.hashWithSalt` rtmpOutputSettings
      `Prelude.hashWithSalt` udpOutputSettings
      `Prelude.hashWithSalt` frameCaptureOutputSettings
      `Prelude.hashWithSalt` msSmoothOutputSettings
      `Prelude.hashWithSalt` hlsOutputSettings

instance Prelude.NFData OutputSettings where
  rnf OutputSettings' {..} =
    Prelude.rnf multiplexOutputSettings
      `Prelude.seq` Prelude.rnf mediaPackageOutputSettings
      `Prelude.seq` Prelude.rnf archiveOutputSettings
      `Prelude.seq` Prelude.rnf rtmpOutputSettings
      `Prelude.seq` Prelude.rnf udpOutputSettings
      `Prelude.seq` Prelude.rnf frameCaptureOutputSettings
      `Prelude.seq` Prelude.rnf msSmoothOutputSettings
      `Prelude.seq` Prelude.rnf hlsOutputSettings

instance Core.ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("multiplexOutputSettings" Core..=)
              Prelude.<$> multiplexOutputSettings,
            ("mediaPackageOutputSettings" Core..=)
              Prelude.<$> mediaPackageOutputSettings,
            ("archiveOutputSettings" Core..=)
              Prelude.<$> archiveOutputSettings,
            ("rtmpOutputSettings" Core..=)
              Prelude.<$> rtmpOutputSettings,
            ("udpOutputSettings" Core..=)
              Prelude.<$> udpOutputSettings,
            ("frameCaptureOutputSettings" Core..=)
              Prelude.<$> frameCaptureOutputSettings,
            ("msSmoothOutputSettings" Core..=)
              Prelude.<$> msSmoothOutputSettings,
            ("hlsOutputSettings" Core..=)
              Prelude.<$> hlsOutputSettings
          ]
      )
