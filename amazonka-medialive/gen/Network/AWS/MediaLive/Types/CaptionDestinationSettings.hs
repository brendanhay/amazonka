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
-- Module      : Network.AWS.MediaLive.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AribDestinationSettings
import Network.AWS.MediaLive.Types.BurnInDestinationSettings
import Network.AWS.MediaLive.Types.DvbSubDestinationSettings
import Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.Scte27DestinationSettings
import Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
import Network.AWS.MediaLive.Types.TeletextDestinationSettings
import Network.AWS.MediaLive.Types.TtmlDestinationSettings
import Network.AWS.MediaLive.Types.WebvttDestinationSettings

-- | Caption Destination Settings
--
-- /See:/ 'newCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { webvttDestinationSettings :: Core.Maybe WebvttDestinationSettings,
    embeddedDestinationSettings :: Core.Maybe EmbeddedDestinationSettings,
    aribDestinationSettings :: Core.Maybe AribDestinationSettings,
    scte20PlusEmbeddedDestinationSettings :: Core.Maybe Scte20PlusEmbeddedDestinationSettings,
    embeddedPlusScte20DestinationSettings :: Core.Maybe EmbeddedPlusScte20DestinationSettings,
    dvbSubDestinationSettings :: Core.Maybe DvbSubDestinationSettings,
    scte27DestinationSettings :: Core.Maybe Scte27DestinationSettings,
    rtmpCaptionInfoDestinationSettings :: Core.Maybe RtmpCaptionInfoDestinationSettings,
    ebuTtDDestinationSettings :: Core.Maybe EbuTtDDestinationSettings,
    teletextDestinationSettings :: Core.Maybe TeletextDestinationSettings,
    smpteTtDestinationSettings :: Core.Maybe SmpteTtDestinationSettings,
    ttmlDestinationSettings :: Core.Maybe TtmlDestinationSettings,
    burnInDestinationSettings :: Core.Maybe BurnInDestinationSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CaptionDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webvttDestinationSettings', 'captionDestinationSettings_webvttDestinationSettings' - Undocumented member.
--
-- 'embeddedDestinationSettings', 'captionDestinationSettings_embeddedDestinationSettings' - Undocumented member.
--
-- 'aribDestinationSettings', 'captionDestinationSettings_aribDestinationSettings' - Undocumented member.
--
-- 'scte20PlusEmbeddedDestinationSettings', 'captionDestinationSettings_scte20PlusEmbeddedDestinationSettings' - Undocumented member.
--
-- 'embeddedPlusScte20DestinationSettings', 'captionDestinationSettings_embeddedPlusScte20DestinationSettings' - Undocumented member.
--
-- 'dvbSubDestinationSettings', 'captionDestinationSettings_dvbSubDestinationSettings' - Undocumented member.
--
-- 'scte27DestinationSettings', 'captionDestinationSettings_scte27DestinationSettings' - Undocumented member.
--
-- 'rtmpCaptionInfoDestinationSettings', 'captionDestinationSettings_rtmpCaptionInfoDestinationSettings' - Undocumented member.
--
-- 'ebuTtDDestinationSettings', 'captionDestinationSettings_ebuTtDDestinationSettings' - Undocumented member.
--
-- 'teletextDestinationSettings', 'captionDestinationSettings_teletextDestinationSettings' - Undocumented member.
--
-- 'smpteTtDestinationSettings', 'captionDestinationSettings_smpteTtDestinationSettings' - Undocumented member.
--
-- 'ttmlDestinationSettings', 'captionDestinationSettings_ttmlDestinationSettings' - Undocumented member.
--
-- 'burnInDestinationSettings', 'captionDestinationSettings_burnInDestinationSettings' - Undocumented member.
newCaptionDestinationSettings ::
  CaptionDestinationSettings
newCaptionDestinationSettings =
  CaptionDestinationSettings'
    { webvttDestinationSettings =
        Core.Nothing,
      embeddedDestinationSettings = Core.Nothing,
      aribDestinationSettings = Core.Nothing,
      scte20PlusEmbeddedDestinationSettings =
        Core.Nothing,
      embeddedPlusScte20DestinationSettings =
        Core.Nothing,
      dvbSubDestinationSettings = Core.Nothing,
      scte27DestinationSettings = Core.Nothing,
      rtmpCaptionInfoDestinationSettings =
        Core.Nothing,
      ebuTtDDestinationSettings = Core.Nothing,
      teletextDestinationSettings = Core.Nothing,
      smpteTtDestinationSettings = Core.Nothing,
      ttmlDestinationSettings = Core.Nothing,
      burnInDestinationSettings = Core.Nothing
    }

-- | Undocumented member.
captionDestinationSettings_webvttDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe WebvttDestinationSettings)
captionDestinationSettings_webvttDestinationSettings = Lens.lens (\CaptionDestinationSettings' {webvttDestinationSettings} -> webvttDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {webvttDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe EmbeddedDestinationSettings)
captionDestinationSettings_embeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedDestinationSettings} -> embeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_aribDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe AribDestinationSettings)
captionDestinationSettings_aribDestinationSettings = Lens.lens (\CaptionDestinationSettings' {aribDestinationSettings} -> aribDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {aribDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Scte20PlusEmbeddedDestinationSettings)
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte20PlusEmbeddedDestinationSettings} -> scte20PlusEmbeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte20PlusEmbeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedPlusScte20DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe EmbeddedPlusScte20DestinationSettings)
captionDestinationSettings_embeddedPlusScte20DestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedPlusScte20DestinationSettings} -> embeddedPlusScte20DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedPlusScte20DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_dvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe DvbSubDestinationSettings)
captionDestinationSettings_dvbSubDestinationSettings = Lens.lens (\CaptionDestinationSettings' {dvbSubDestinationSettings} -> dvbSubDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte27DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe Scte27DestinationSettings)
captionDestinationSettings_scte27DestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte27DestinationSettings} -> scte27DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte27DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_rtmpCaptionInfoDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe RtmpCaptionInfoDestinationSettings)
captionDestinationSettings_rtmpCaptionInfoDestinationSettings = Lens.lens (\CaptionDestinationSettings' {rtmpCaptionInfoDestinationSettings} -> rtmpCaptionInfoDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {rtmpCaptionInfoDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ebuTtDDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe EbuTtDDestinationSettings)
captionDestinationSettings_ebuTtDDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ebuTtDDestinationSettings} -> ebuTtDDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ebuTtDDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_teletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe TeletextDestinationSettings)
captionDestinationSettings_teletextDestinationSettings = Lens.lens (\CaptionDestinationSettings' {teletextDestinationSettings} -> teletextDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_smpteTtDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe SmpteTtDestinationSettings)
captionDestinationSettings_smpteTtDestinationSettings = Lens.lens (\CaptionDestinationSettings' {smpteTtDestinationSettings} -> smpteTtDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {smpteTtDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ttmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe TtmlDestinationSettings)
captionDestinationSettings_ttmlDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ttmlDestinationSettings} -> ttmlDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_burnInDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Core.Maybe BurnInDestinationSettings)
captionDestinationSettings_burnInDestinationSettings = Lens.lens (\CaptionDestinationSettings' {burnInDestinationSettings} -> burnInDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {burnInDestinationSettings = a} :: CaptionDestinationSettings)

instance Core.FromJSON CaptionDestinationSettings where
  parseJSON =
    Core.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Core.<$> (x Core..:? "webvttDestinationSettings")
            Core.<*> (x Core..:? "embeddedDestinationSettings")
            Core.<*> (x Core..:? "aribDestinationSettings")
            Core.<*> (x Core..:? "scte20PlusEmbeddedDestinationSettings")
            Core.<*> (x Core..:? "embeddedPlusScte20DestinationSettings")
            Core.<*> (x Core..:? "dvbSubDestinationSettings")
            Core.<*> (x Core..:? "scte27DestinationSettings")
            Core.<*> (x Core..:? "rtmpCaptionInfoDestinationSettings")
            Core.<*> (x Core..:? "ebuTtDDestinationSettings")
            Core.<*> (x Core..:? "teletextDestinationSettings")
            Core.<*> (x Core..:? "smpteTtDestinationSettings")
            Core.<*> (x Core..:? "ttmlDestinationSettings")
            Core.<*> (x Core..:? "burnInDestinationSettings")
      )

instance Core.Hashable CaptionDestinationSettings

instance Core.NFData CaptionDestinationSettings

instance Core.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("webvttDestinationSettings" Core..=)
              Core.<$> webvttDestinationSettings,
            ("embeddedDestinationSettings" Core..=)
              Core.<$> embeddedDestinationSettings,
            ("aribDestinationSettings" Core..=)
              Core.<$> aribDestinationSettings,
            ("scte20PlusEmbeddedDestinationSettings" Core..=)
              Core.<$> scte20PlusEmbeddedDestinationSettings,
            ("embeddedPlusScte20DestinationSettings" Core..=)
              Core.<$> embeddedPlusScte20DestinationSettings,
            ("dvbSubDestinationSettings" Core..=)
              Core.<$> dvbSubDestinationSettings,
            ("scte27DestinationSettings" Core..=)
              Core.<$> scte27DestinationSettings,
            ("rtmpCaptionInfoDestinationSettings" Core..=)
              Core.<$> rtmpCaptionInfoDestinationSettings,
            ("ebuTtDDestinationSettings" Core..=)
              Core.<$> ebuTtDDestinationSettings,
            ("teletextDestinationSettings" Core..=)
              Core.<$> teletextDestinationSettings,
            ("smpteTtDestinationSettings" Core..=)
              Core.<$> smpteTtDestinationSettings,
            ("ttmlDestinationSettings" Core..=)
              Core.<$> ttmlDestinationSettings,
            ("burnInDestinationSettings" Core..=)
              Core.<$> burnInDestinationSettings
          ]
      )
