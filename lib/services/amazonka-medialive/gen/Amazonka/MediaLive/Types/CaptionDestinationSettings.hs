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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CaptionDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { aribDestinationSettings :: Prelude.Maybe AribDestinationSettings,
    burnInDestinationSettings :: Prelude.Maybe BurnInDestinationSettings,
    dvbSubDestinationSettings :: Prelude.Maybe DvbSubDestinationSettings,
    ebuTtDDestinationSettings :: Prelude.Maybe EbuTtDDestinationSettings,
    embeddedDestinationSettings :: Prelude.Maybe EmbeddedDestinationSettings,
    embeddedPlusScte20DestinationSettings :: Prelude.Maybe EmbeddedPlusScte20DestinationSettings,
    rtmpCaptionInfoDestinationSettings :: Prelude.Maybe RtmpCaptionInfoDestinationSettings,
    scte20PlusEmbeddedDestinationSettings :: Prelude.Maybe Scte20PlusEmbeddedDestinationSettings,
    scte27DestinationSettings :: Prelude.Maybe Scte27DestinationSettings,
    smpteTtDestinationSettings :: Prelude.Maybe SmpteTtDestinationSettings,
    teletextDestinationSettings :: Prelude.Maybe TeletextDestinationSettings,
    ttmlDestinationSettings :: Prelude.Maybe TtmlDestinationSettings,
    webvttDestinationSettings :: Prelude.Maybe WebvttDestinationSettings
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
-- 'aribDestinationSettings', 'captionDestinationSettings_aribDestinationSettings' - Undocumented member.
--
-- 'burnInDestinationSettings', 'captionDestinationSettings_burnInDestinationSettings' - Undocumented member.
--
-- 'dvbSubDestinationSettings', 'captionDestinationSettings_dvbSubDestinationSettings' - Undocumented member.
--
-- 'ebuTtDDestinationSettings', 'captionDestinationSettings_ebuTtDDestinationSettings' - Undocumented member.
--
-- 'embeddedDestinationSettings', 'captionDestinationSettings_embeddedDestinationSettings' - Undocumented member.
--
-- 'embeddedPlusScte20DestinationSettings', 'captionDestinationSettings_embeddedPlusScte20DestinationSettings' - Undocumented member.
--
-- 'rtmpCaptionInfoDestinationSettings', 'captionDestinationSettings_rtmpCaptionInfoDestinationSettings' - Undocumented member.
--
-- 'scte20PlusEmbeddedDestinationSettings', 'captionDestinationSettings_scte20PlusEmbeddedDestinationSettings' - Undocumented member.
--
-- 'scte27DestinationSettings', 'captionDestinationSettings_scte27DestinationSettings' - Undocumented member.
--
-- 'smpteTtDestinationSettings', 'captionDestinationSettings_smpteTtDestinationSettings' - Undocumented member.
--
-- 'teletextDestinationSettings', 'captionDestinationSettings_teletextDestinationSettings' - Undocumented member.
--
-- 'ttmlDestinationSettings', 'captionDestinationSettings_ttmlDestinationSettings' - Undocumented member.
--
-- 'webvttDestinationSettings', 'captionDestinationSettings_webvttDestinationSettings' - Undocumented member.
newCaptionDestinationSettings ::
  CaptionDestinationSettings
newCaptionDestinationSettings =
  CaptionDestinationSettings'
    { aribDestinationSettings =
        Prelude.Nothing,
      burnInDestinationSettings = Prelude.Nothing,
      dvbSubDestinationSettings = Prelude.Nothing,
      ebuTtDDestinationSettings = Prelude.Nothing,
      embeddedDestinationSettings = Prelude.Nothing,
      embeddedPlusScte20DestinationSettings =
        Prelude.Nothing,
      rtmpCaptionInfoDestinationSettings =
        Prelude.Nothing,
      scte20PlusEmbeddedDestinationSettings =
        Prelude.Nothing,
      scte27DestinationSettings = Prelude.Nothing,
      smpteTtDestinationSettings = Prelude.Nothing,
      teletextDestinationSettings = Prelude.Nothing,
      ttmlDestinationSettings = Prelude.Nothing,
      webvttDestinationSettings = Prelude.Nothing
    }

-- | Undocumented member.
captionDestinationSettings_aribDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe AribDestinationSettings)
captionDestinationSettings_aribDestinationSettings = Lens.lens (\CaptionDestinationSettings' {aribDestinationSettings} -> aribDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {aribDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_burnInDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe BurnInDestinationSettings)
captionDestinationSettings_burnInDestinationSettings = Lens.lens (\CaptionDestinationSettings' {burnInDestinationSettings} -> burnInDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {burnInDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_dvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe DvbSubDestinationSettings)
captionDestinationSettings_dvbSubDestinationSettings = Lens.lens (\CaptionDestinationSettings' {dvbSubDestinationSettings} -> dvbSubDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ebuTtDDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EbuTtDDestinationSettings)
captionDestinationSettings_ebuTtDDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ebuTtDDestinationSettings} -> ebuTtDDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ebuTtDDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedDestinationSettings)
captionDestinationSettings_embeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedDestinationSettings} -> embeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_embeddedPlusScte20DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe EmbeddedPlusScte20DestinationSettings)
captionDestinationSettings_embeddedPlusScte20DestinationSettings = Lens.lens (\CaptionDestinationSettings' {embeddedPlusScte20DestinationSettings} -> embeddedPlusScte20DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {embeddedPlusScte20DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_rtmpCaptionInfoDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe RtmpCaptionInfoDestinationSettings)
captionDestinationSettings_rtmpCaptionInfoDestinationSettings = Lens.lens (\CaptionDestinationSettings' {rtmpCaptionInfoDestinationSettings} -> rtmpCaptionInfoDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {rtmpCaptionInfoDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe Scte20PlusEmbeddedDestinationSettings)
captionDestinationSettings_scte20PlusEmbeddedDestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte20PlusEmbeddedDestinationSettings} -> scte20PlusEmbeddedDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte20PlusEmbeddedDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_scte27DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe Scte27DestinationSettings)
captionDestinationSettings_scte27DestinationSettings = Lens.lens (\CaptionDestinationSettings' {scte27DestinationSettings} -> scte27DestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {scte27DestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_smpteTtDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe SmpteTtDestinationSettings)
captionDestinationSettings_smpteTtDestinationSettings = Lens.lens (\CaptionDestinationSettings' {smpteTtDestinationSettings} -> smpteTtDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {smpteTtDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_teletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TeletextDestinationSettings)
captionDestinationSettings_teletextDestinationSettings = Lens.lens (\CaptionDestinationSettings' {teletextDestinationSettings} -> teletextDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_ttmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe TtmlDestinationSettings)
captionDestinationSettings_ttmlDestinationSettings = Lens.lens (\CaptionDestinationSettings' {ttmlDestinationSettings} -> ttmlDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)

-- | Undocumented member.
captionDestinationSettings_webvttDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Prelude.Maybe WebvttDestinationSettings)
captionDestinationSettings_webvttDestinationSettings = Lens.lens (\CaptionDestinationSettings' {webvttDestinationSettings} -> webvttDestinationSettings) (\s@CaptionDestinationSettings' {} a -> s {webvttDestinationSettings = a} :: CaptionDestinationSettings)

instance Data.FromJSON CaptionDestinationSettings where
  parseJSON =
    Data.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Prelude.<$> (x Data..:? "aribDestinationSettings")
            Prelude.<*> (x Data..:? "burnInDestinationSettings")
            Prelude.<*> (x Data..:? "dvbSubDestinationSettings")
            Prelude.<*> (x Data..:? "ebuTtDDestinationSettings")
            Prelude.<*> (x Data..:? "embeddedDestinationSettings")
            Prelude.<*> (x Data..:? "embeddedPlusScte20DestinationSettings")
            Prelude.<*> (x Data..:? "rtmpCaptionInfoDestinationSettings")
            Prelude.<*> (x Data..:? "scte20PlusEmbeddedDestinationSettings")
            Prelude.<*> (x Data..:? "scte27DestinationSettings")
            Prelude.<*> (x Data..:? "smpteTtDestinationSettings")
            Prelude.<*> (x Data..:? "teletextDestinationSettings")
            Prelude.<*> (x Data..:? "ttmlDestinationSettings")
            Prelude.<*> (x Data..:? "webvttDestinationSettings")
      )

instance Prelude.Hashable CaptionDestinationSettings where
  hashWithSalt _salt CaptionDestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` aribDestinationSettings
      `Prelude.hashWithSalt` burnInDestinationSettings
      `Prelude.hashWithSalt` dvbSubDestinationSettings
      `Prelude.hashWithSalt` ebuTtDDestinationSettings
      `Prelude.hashWithSalt` embeddedDestinationSettings
      `Prelude.hashWithSalt` embeddedPlusScte20DestinationSettings
      `Prelude.hashWithSalt` rtmpCaptionInfoDestinationSettings
      `Prelude.hashWithSalt` scte20PlusEmbeddedDestinationSettings
      `Prelude.hashWithSalt` scte27DestinationSettings
      `Prelude.hashWithSalt` smpteTtDestinationSettings
      `Prelude.hashWithSalt` teletextDestinationSettings
      `Prelude.hashWithSalt` ttmlDestinationSettings
      `Prelude.hashWithSalt` webvttDestinationSettings

instance Prelude.NFData CaptionDestinationSettings where
  rnf CaptionDestinationSettings' {..} =
    Prelude.rnf aribDestinationSettings `Prelude.seq`
      Prelude.rnf burnInDestinationSettings `Prelude.seq`
        Prelude.rnf dvbSubDestinationSettings `Prelude.seq`
          Prelude.rnf ebuTtDDestinationSettings `Prelude.seq`
            Prelude.rnf embeddedDestinationSettings `Prelude.seq`
              Prelude.rnf embeddedPlusScte20DestinationSettings `Prelude.seq`
                Prelude.rnf rtmpCaptionInfoDestinationSettings `Prelude.seq`
                  Prelude.rnf scte20PlusEmbeddedDestinationSettings `Prelude.seq`
                    Prelude.rnf scte27DestinationSettings `Prelude.seq`
                      Prelude.rnf smpteTtDestinationSettings `Prelude.seq`
                        Prelude.rnf teletextDestinationSettings `Prelude.seq`
                          Prelude.rnf ttmlDestinationSettings `Prelude.seq`
                            Prelude.rnf webvttDestinationSettings

instance Data.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("aribDestinationSettings" Data..=)
              Prelude.<$> aribDestinationSettings,
            ("burnInDestinationSettings" Data..=)
              Prelude.<$> burnInDestinationSettings,
            ("dvbSubDestinationSettings" Data..=)
              Prelude.<$> dvbSubDestinationSettings,
            ("ebuTtDDestinationSettings" Data..=)
              Prelude.<$> ebuTtDDestinationSettings,
            ("embeddedDestinationSettings" Data..=)
              Prelude.<$> embeddedDestinationSettings,
            ("embeddedPlusScte20DestinationSettings" Data..=)
              Prelude.<$> embeddedPlusScte20DestinationSettings,
            ("rtmpCaptionInfoDestinationSettings" Data..=)
              Prelude.<$> rtmpCaptionInfoDestinationSettings,
            ("scte20PlusEmbeddedDestinationSettings" Data..=)
              Prelude.<$> scte20PlusEmbeddedDestinationSettings,
            ("scte27DestinationSettings" Data..=)
              Prelude.<$> scte27DestinationSettings,
            ("smpteTtDestinationSettings" Data..=)
              Prelude.<$> smpteTtDestinationSettings,
            ("teletextDestinationSettings" Data..=)
              Prelude.<$> teletextDestinationSettings,
            ("ttmlDestinationSettings" Data..=)
              Prelude.<$> ttmlDestinationSettings,
            ("webvttDestinationSettings" Data..=)
              Prelude.<$> webvttDestinationSettings
          ]
      )
