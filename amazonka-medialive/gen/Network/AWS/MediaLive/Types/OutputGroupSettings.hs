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
-- Module      : Network.AWS.MediaLive.Types.OutputGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputGroupSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
import Network.AWS.MediaLive.Types.HlsGroupSettings
import Network.AWS.MediaLive.Types.MediaPackageGroupSettings
import Network.AWS.MediaLive.Types.MsSmoothGroupSettings
import Network.AWS.MediaLive.Types.MultiplexGroupSettings
import Network.AWS.MediaLive.Types.RtmpGroupSettings
import Network.AWS.MediaLive.Types.UdpGroupSettings
import qualified Network.AWS.Prelude as Prelude

-- | Output Group Settings
--
-- /See:/ 'newOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { msSmoothGroupSettings :: Prelude.Maybe MsSmoothGroupSettings,
    frameCaptureGroupSettings :: Prelude.Maybe FrameCaptureGroupSettings,
    hlsGroupSettings :: Prelude.Maybe HlsGroupSettings,
    mediaPackageGroupSettings :: Prelude.Maybe MediaPackageGroupSettings,
    rtmpGroupSettings :: Prelude.Maybe RtmpGroupSettings,
    udpGroupSettings :: Prelude.Maybe UdpGroupSettings,
    archiveGroupSettings :: Prelude.Maybe ArchiveGroupSettings,
    multiplexGroupSettings :: Prelude.Maybe MultiplexGroupSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      frameCaptureGroupSettings = Prelude.Nothing,
      hlsGroupSettings = Prelude.Nothing,
      mediaPackageGroupSettings = Prelude.Nothing,
      rtmpGroupSettings = Prelude.Nothing,
      udpGroupSettings = Prelude.Nothing,
      archiveGroupSettings = Prelude.Nothing,
      multiplexGroupSettings = Prelude.Nothing
    }

-- | Undocumented member.
outputGroupSettings_msSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe MsSmoothGroupSettings)
outputGroupSettings_msSmoothGroupSettings = Lens.lens (\OutputGroupSettings' {msSmoothGroupSettings} -> msSmoothGroupSettings) (\s@OutputGroupSettings' {} a -> s {msSmoothGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_frameCaptureGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe FrameCaptureGroupSettings)
outputGroupSettings_frameCaptureGroupSettings = Lens.lens (\OutputGroupSettings' {frameCaptureGroupSettings} -> frameCaptureGroupSettings) (\s@OutputGroupSettings' {} a -> s {frameCaptureGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_hlsGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe HlsGroupSettings)
outputGroupSettings_hlsGroupSettings = Lens.lens (\OutputGroupSettings' {hlsGroupSettings} -> hlsGroupSettings) (\s@OutputGroupSettings' {} a -> s {hlsGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_mediaPackageGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe MediaPackageGroupSettings)
outputGroupSettings_mediaPackageGroupSettings = Lens.lens (\OutputGroupSettings' {mediaPackageGroupSettings} -> mediaPackageGroupSettings) (\s@OutputGroupSettings' {} a -> s {mediaPackageGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_rtmpGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe RtmpGroupSettings)
outputGroupSettings_rtmpGroupSettings = Lens.lens (\OutputGroupSettings' {rtmpGroupSettings} -> rtmpGroupSettings) (\s@OutputGroupSettings' {} a -> s {rtmpGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_udpGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe UdpGroupSettings)
outputGroupSettings_udpGroupSettings = Lens.lens (\OutputGroupSettings' {udpGroupSettings} -> udpGroupSettings) (\s@OutputGroupSettings' {} a -> s {udpGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_archiveGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe ArchiveGroupSettings)
outputGroupSettings_archiveGroupSettings = Lens.lens (\OutputGroupSettings' {archiveGroupSettings} -> archiveGroupSettings) (\s@OutputGroupSettings' {} a -> s {archiveGroupSettings = a} :: OutputGroupSettings)

-- | Undocumented member.
outputGroupSettings_multiplexGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe MultiplexGroupSettings)
outputGroupSettings_multiplexGroupSettings = Lens.lens (\OutputGroupSettings' {multiplexGroupSettings} -> multiplexGroupSettings) (\s@OutputGroupSettings' {} a -> s {multiplexGroupSettings = a} :: OutputGroupSettings)

instance Prelude.FromJSON OutputGroupSettings where
  parseJSON =
    Prelude.withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            Prelude.<$> (x Prelude..:? "msSmoothGroupSettings")
            Prelude.<*> (x Prelude..:? "frameCaptureGroupSettings")
            Prelude.<*> (x Prelude..:? "hlsGroupSettings")
            Prelude.<*> (x Prelude..:? "mediaPackageGroupSettings")
            Prelude.<*> (x Prelude..:? "rtmpGroupSettings")
            Prelude.<*> (x Prelude..:? "udpGroupSettings")
            Prelude.<*> (x Prelude..:? "archiveGroupSettings")
            Prelude.<*> (x Prelude..:? "multiplexGroupSettings")
      )

instance Prelude.Hashable OutputGroupSettings

instance Prelude.NFData OutputGroupSettings

instance Prelude.ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("msSmoothGroupSettings" Prelude..=)
              Prelude.<$> msSmoothGroupSettings,
            ("frameCaptureGroupSettings" Prelude..=)
              Prelude.<$> frameCaptureGroupSettings,
            ("hlsGroupSettings" Prelude..=)
              Prelude.<$> hlsGroupSettings,
            ("mediaPackageGroupSettings" Prelude..=)
              Prelude.<$> mediaPackageGroupSettings,
            ("rtmpGroupSettings" Prelude..=)
              Prelude.<$> rtmpGroupSettings,
            ("udpGroupSettings" Prelude..=)
              Prelude.<$> udpGroupSettings,
            ("archiveGroupSettings" Prelude..=)
              Prelude.<$> archiveGroupSettings,
            ("multiplexGroupSettings" Prelude..=)
              Prelude.<$> multiplexGroupSettings
          ]
      )
