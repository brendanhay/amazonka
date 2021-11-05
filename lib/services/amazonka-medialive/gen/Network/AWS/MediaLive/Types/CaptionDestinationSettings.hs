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
-- Module      : Amazonka.MediaLive.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CaptionDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.AribDestinationSettings
import Amazonka.MediaLive.Types.BurnInDestinationSettings
import Amazonka.MediaLive.Types.DvbSubDestinationSettings
import Amazonka.MediaLive.Types.EbuTtDDestinationSettings
import Amazonka.MediaLive.Types.EmbeddedDestinationSettings
import Amazonka.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Amazonka.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Amazonka.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Amazonka.MediaLive.Types.Scte27DestinationSettings
import Amazonka.MediaLive.Types.SmpteTtDestinationSettings
import Amazonka.MediaLive.Types.TeletextDestinationSettings
import Amazonka.MediaLive.Types.TtmlDestinationSettings
import Amazonka.MediaLive.Types.WebvttDestinationSettings
import qualified Amazonka.Prelude as Prelude

-- | Caption Destination Settings
--
-- /See:/ 'newCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { teletextDestinationSettings :: Prelude.Maybe TeletextDestinationSettings,
    ebuTtDDestinationSettings :: Prelude.Maybe EbuTtDDestinationSettings,
    rtmpCaptionInfoDestinationSettings :: Prelude.Maybe RtmpCaptionInfoDestinationSettings,
    dvbSubDestinationSettings :: Prelude.Maybe DvbSubDestinationSettings,
    scte27DestinationSettings :: Prelude.Maybe Scte27DestinationSettings,
    ttmlDestinationSettings :: Prelude.Maybe TtmlDestinationSettings,
    scte20PlusEmbeddedDestinationSettings :: Prelude.Maybe Scte20PlusEmbeddedDestinationSettings,
    embeddedPlusScte20DestinationSettings :: Prelude.Maybe EmbeddedPlusScte20DestinationSettings,
    smpteTtDestinationSettings :: Prelude.Maybe SmpteTtDestinationSettings,
    webvttDestinationSettings :: Prelude.Maybe WebvttDestinationSettings,
    embeddedDestinationSettings :: Prelude.Maybe EmbeddedDestinationSettings,
    burnInDestinationSettings :: Prelude.Maybe BurnInDestinationSettings,
    aribDestinationSettings :: Prelude.Maybe AribDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'teletextDestinationSettings', 'captionDestinationSettings_teletextDestinationSettings' - Undocumented member.
--
-- 'ebuTtDDestinationSettings', 'captionDestinationSettings_ebuTtDDestinationSettings' - Undocumented member.
--
-- 'rtmpCaptionInfoDestinationSettings', 'captionDestinationSettings_rtmpCaptionInfoDestinationSettings' - Undocumented member.
--
-- 'dvbSubDestinationSettings', 'captionDestinationSettings_dvbSubDestinationSettings' - Undocumented member.
--
-- 'scte27DestinationSettings', 'captionDestinationSettings_scte27DestinationSettings' - Undocumented member.
--
-- 'ttmlDestinationSettings', 'captionDestinationSettings_ttmlDestinationSettings' - Undocumented member.
--
-- 'scte20PlusEmbeddedDestinationSettings', 'captionDestinationSettings_scte20PlusEmbeddedDestinationSettings' - Undocumented member.
--
-- 'embeddedPlusScte20DestinationSettings', 'captionDestinationSettings_embeddedPlusScte20DestinationSettings' - Undocumented member.
--
-- 'smpteTtDestinationSettings', 'captionDestinationSettings_smpteTtDestinationSettings' - Undocumented member.
--
-- 'webvttDestinationSettings', 'captionDestinationSettings_webvttDestinationSettings' - Undocumented member.
--
-- 'embeddedDestinationSettings', 'captionDestinationSettings_embeddedDestinationSettings' - Undocumented member.
--
-- 'burnInDestinationSettings', 'captionDestinationSettings_burnInDestinationSettings' - Undocumented member.
--
-- 'aribDestinationSettings', 'captionDestinationSettings_aribDestinationSettings' - Undocumented member.
newCaptionDestinationSettings ::
  CaptionDestinationSettings
newCaptionDestinationSettings =
  CaptionDestinationSettings'
    { teletextDestinationSettings =
        Prelude.Nothing,
      ebuTtDDestinationSettings = Prelude.Nothing,
      rtmpCaptionInfoDestinationSettings =
        Prelude.Nothing,
      dvbSubDestinationSettings = Prelude.Nothing,
      scte27DestinationSettings = Prelude.Nothing,
      ttmlDestinationSettings = Prelude.Nothing,
      scte20PlusEmbeddedDestinationSettings =
        Prelude.Nothing,
      embeddedPlusScte20DestinationSettings =
        Prelude.Nothing,
      smpteTtDestinationSettings = Prelude.Nothing,
      webvttDestinationSettings = Prelude.Nothing,
      embeddedDestinationSettings = Prelude.Nothing,
      burnInDestinationSettings = Prelude.Nothing,
      aribDestinationSettings = Prelude.Nothing
    }

-- | Undocumented member.
captionDestinationSettings_teletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TeletextDestinationSettings)
captionDestinationSettings_teletextDestinationSettings = Lens.lens (\CaptionDestinationSettings' {teletextDestinationSettings} -> teletextDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ebuTtDDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EbuTtDDestinationSettings)
captionDestinationSettings_ebuTtDDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ebuTtDDestinationSettings} -> ebuTtDDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ebuTtDDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_rtmpCaptionInfoDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe RtmpCaptionInfoDestinationSettings)
captionDestinationSettings_rtmpCaptionInfoDestinationSettings = Lens.lens (\CaptionDestinationSettings' {rtmpCaptionInfoDestinationSettings} -> rtmpCaptionInfoDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {rtmpCaptionInfoDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_dvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe DvbSubDestinationSettings)
captionDestinationSettings_dvbSubDestinationSettings = Lens.lens (\CaptionDestinationSettings' {dvbSubDestinationSettings} -> dvbSubDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte27DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe Scte27DestinationSettings)
captionDestinationSettings_scte27DestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte27DestinationSettings} -> scte27DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte27DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ttmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TtmlDestinationSettings)
captionDestinationSettings_ttmlDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ttmlDestinationSettings} -> ttmlDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe Scte20PlusEmbeddedDestinationSettings)
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte20PlusEmbeddedDestinationSettings} -> scte20PlusEmbeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte20PlusEmbeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedPlusScte20DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedPlusScte20DestinationSettings)
captionDestinationSettings_embeddedPlusScte20DestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedPlusScte20DestinationSettings} -> embeddedPlusScte20DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedPlusScte20DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_smpteTtDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe SmpteTtDestinationSettings)
captionDestinationSettings_smpteTtDestinationSettings = Lens.lens (\CaptionDestinationSettings' {smpteTtDestinationSettings} -> smpteTtDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {smpteTtDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_webvttDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe WebvttDestinationSettings)
captionDestinationSettings_webvttDestinationSettings = Lens.lens (\CaptionDestinationSettings' {webvttDestinationSettings} -> webvttDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {webvttDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedDestinationSettings)
captionDestinationSettings_embeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedDestinationSettings} -> embeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_burnInDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe BurnInDestinationSettings)
captionDestinationSettings_burnInDestinationSettings = Lens.lens (\CaptionDestinationSettings' {burnInDestinationSettings} -> burnInDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {burnInDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_aribDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe AribDestinationSettings)
captionDestinationSettings_aribDestinationSettings = Lens.lens (\CaptionDestinationSettings' {aribDestinationSettings} -> aribDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {aribDestinationSettings = a} :: CaptionDestinationSettings)

instance Core.FromJSON CaptionDestinationSettings where
  parseJSON =
    Core.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Prelude.<$> (x Core..:? "teletextDestinationSettings")
            Prelude.<*> (x Core..:? "ebuTtDDestinationSettings")
            Prelude.<*> (x Core..:? "rtmpCaptionInfoDestinationSettings")
            Prelude.<*> (x Core..:? "dvbSubDestinationSettings")
            Prelude.<*> (x Core..:? "scte27DestinationSettings")
            Prelude.<*> (x Core..:? "ttmlDestinationSettings")
            Prelude.<*> (x Core..:? "scte20PlusEmbeddedDestinationSettings")
            Prelude.<*> (x Core..:? "embeddedPlusScte20DestinationSettings")
            Prelude.<*> (x Core..:? "smpteTtDestinationSettings")
            Prelude.<*> (x Core..:? "webvttDestinationSettings")
            Prelude.<*> (x Core..:? "embeddedDestinationSettings")
            Prelude.<*> (x Core..:? "burnInDestinationSettings")
            Prelude.<*> (x Core..:? "aribDestinationSettings")
      )

instance Prelude.Hashable CaptionDestinationSettings

instance Prelude.NFData CaptionDestinationSettings

instance Core.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("teletextDestinationSettings" Core..=)
              Prelude.<$> teletextDestinationSettings,
            ("ebuTtDDestinationSettings" Core..=)
              Prelude.<$> ebuTtDDestinationSettings,
            ("rtmpCaptionInfoDestinationSettings" Core..=)
              Prelude.<$> rtmpCaptionInfoDestinationSettings,
            ("dvbSubDestinationSettings" Core..=)
              Prelude.<$> dvbSubDestinationSettings,
            ("scte27DestinationSettings" Core..=)
              Prelude.<$> scte27DestinationSettings,
            ("ttmlDestinationSettings" Core..=)
              Prelude.<$> ttmlDestinationSettings,
            ("scte20PlusEmbeddedDestinationSettings" Core..=)
              Prelude.<$> scte20PlusEmbeddedDestinationSettings,
            ("embeddedPlusScte20DestinationSettings" Core..=)
              Prelude.<$> embeddedPlusScte20DestinationSettings,
            ("smpteTtDestinationSettings" Core..=)
              Prelude.<$> smpteTtDestinationSettings,
            ("webvttDestinationSettings" Core..=)
              Prelude.<$> webvttDestinationSettings,
            ("embeddedDestinationSettings" Core..=)
              Prelude.<$> embeddedDestinationSettings,
            ("burnInDestinationSettings" Core..=)
              Prelude.<$> burnInDestinationSettings,
            ("aribDestinationSettings" Core..=)
              Prelude.<$> aribDestinationSettings
          ]
      )
