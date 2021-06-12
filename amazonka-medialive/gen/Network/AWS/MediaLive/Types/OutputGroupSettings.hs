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
-- Module      : Network.AWS.MediaLive.Types.OutputGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
import Network.AWS.MediaLive.Types.HlsGroupSettings
import Network.AWS.MediaLive.Types.MediaPackageGroupSettings
import Network.AWS.MediaLive.Types.MsSmoothGroupSettings
import Network.AWS.MediaLive.Types.MultiplexGroupSettings
import Network.AWS.MediaLive.Types.RtmpGroupSettings
import Network.AWS.MediaLive.Types.UdpGroupSettings

-- | Output Group Settings
--
-- /See:/ 'newOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { msSmoothGroupSettings :: Core.Maybe MsSmoothGroupSettings,
    frameCaptureGroupSettings :: Core.Maybe FrameCaptureGroupSettings,
    hlsGroupSettings :: Core.Maybe HlsGroupSettings,
    mediaPackageGroupSettings :: Core.Maybe MediaPackageGroupSettings,
    rtmpGroupSettings :: Core.Maybe RtmpGroupSettings,
    udpGroupSettings :: Core.Maybe UdpGroupSettings,
    archiveGroupSettings :: Core.Maybe ArchiveGroupSettings,
    multiplexGroupSettings :: Core.Maybe MultiplexGroupSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'msSmoothGroupSettings', 'outputGroupSettings_msSmoothGroupSettings' - Undocumented member.
--
-- 'frameCaptureGroupSettings', 'outputGroupSettings_frameCaptureGroupSettings' - Undocumented member.
--
-- 'hlsGroupSettings', 'outputGroupSettings_hlsGroupSettings' - Undocumented member.
--
-- 'mediaPackageGroupSettings', 'outputGroupSettings_mediaPackageGroupSettings' - Undocumented member.
--
-- 'rtmpGroupSettings', 'outputGroupSettings_rtmpGroupSettings' - Undocumented member.
--
-- 'udpGroupSettings', 'outputGroupSettings_udpGroupSettings' - Undocumented member.
--
-- 'archiveGroupSettings', 'outputGroupSettings_archiveGroupSettings' - Undocumented member.
--
-- 'multiplexGroupSettings', 'outputGroupSettings_multiplexGroupSettings' - Undocumented member.
newOutputGroupSettings ::
  OutputGroupSettings
newOutputGroupSettings =
  OutputGroupSettings'
    { msSmoothGroupSettings =
        Core.Nothing,
      frameCaptureGroupSettings = Core.Nothing,
      hlsGroupSettings = Core.Nothing,
      mediaPackageGroupSettings = Core.Nothing,
      rtmpGroupSettings = Core.Nothing,
      udpGroupSettings = Core.Nothing,
      archiveGroupSettings = Core.Nothing,
      multiplexGroupSettings = Core.Nothing
    }

-- | Undocumented member.
outputGroupSettings_msSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe MsSmoothGroupSettings)
outputGroupSettings_msSmoothGroupSettings = Lens.lens (\OutputGroupSettings' {msSmoothGroupSettings} -> msSmoothGroupSettings) (\s@OutputGroupSettings' {} a -> s {msSmoothGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_frameCaptureGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe FrameCaptureGroupSettings)
outputGroupSettings_frameCaptureGroupSettings = Lens.lens (\OutputGroupSettings' {frameCaptureGroupSettings} -> frameCaptureGroupSettings) (\s@OutputGroupSettings' {} a -> s {frameCaptureGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_hlsGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe HlsGroupSettings)
outputGroupSettings_hlsGroupSettings = Lens.lens (\OutputGroupSettings' {hlsGroupSettings} -> hlsGroupSettings) (\s@OutputGroupSettings' {} a -> s {hlsGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_mediaPackageGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe MediaPackageGroupSettings)
outputGroupSettings_mediaPackageGroupSettings = Lens.lens (\OutputGroupSettings' {mediaPackageGroupSettings} -> mediaPackageGroupSettings) (\s@OutputGroupSettings' {} a -> s {mediaPackageGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_rtmpGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe RtmpGroupSettings)
outputGroupSettings_rtmpGroupSettings = Lens.lens (\OutputGroupSettings' {rtmpGroupSettings} -> rtmpGroupSettings) (\s@OutputGroupSettings' {} a -> s {rtmpGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_udpGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe UdpGroupSettings)
outputGroupSettings_udpGroupSettings = Lens.lens (\OutputGroupSettings' {udpGroupSettings} -> udpGroupSettings) (\s@OutputGroupSettings' {} a -> s {udpGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_archiveGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe ArchiveGroupSettings)
outputGroupSettings_archiveGroupSettings = Lens.lens (\OutputGroupSettings' {archiveGroupSettings} -> archiveGroupSettings) (\s@OutputGroupSettings' {} a -> s {archiveGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_multiplexGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe MultiplexGroupSettings)
outputGroupSettings_multiplexGroupSettings = Lens.lens (\OutputGroupSettings' {multiplexGroupSettings} -> multiplexGroupSettings) (\s@OutputGroupSettings' {} a -> s {multiplexGroupSettings = a} :: OutputGroupSettings)

instance Core.FromJSON OutputGroupSettings where
  parseJSON =
    Core.withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            Core.<$> (x Core..:? "msSmoothGroupSettings")
            Core.<*> (x Core..:? "frameCaptureGroupSettings")
            Core.<*> (x Core..:? "hlsGroupSettings")
            Core.<*> (x Core..:? "mediaPackageGroupSettings")
            Core.<*> (x Core..:? "rtmpGroupSettings")
            Core.<*> (x Core..:? "udpGroupSettings")
            Core.<*> (x Core..:? "archiveGroupSettings")
            Core.<*> (x Core..:? "multiplexGroupSettings")
      )

instance Core.Hashable OutputGroupSettings

instance Core.NFData OutputGroupSettings

instance Core.ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("msSmoothGroupSettings" Core..=)
              Core.<$> msSmoothGroupSettings,
            ("frameCaptureGroupSettings" Core..=)
              Core.<$> frameCaptureGroupSettings,
            ("hlsGroupSettings" Core..=)
              Core.<$> hlsGroupSettings,
            ("mediaPackageGroupSettings" Core..=)
              Core.<$> mediaPackageGroupSettings,
            ("rtmpGroupSettings" Core..=)
              Core.<$> rtmpGroupSettings,
            ("udpGroupSettings" Core..=)
              Core.<$> udpGroupSettings,
            ("archiveGroupSettings" Core..=)
              Core.<$> archiveGroupSettings,
            ("multiplexGroupSettings" Core..=)
              Core.<$> multiplexGroupSettings
          ]
      )
