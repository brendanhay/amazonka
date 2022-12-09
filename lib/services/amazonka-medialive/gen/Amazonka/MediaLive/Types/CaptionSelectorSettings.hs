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
-- Module      : Amazonka.MediaLive.Types.CaptionSelectorSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CaptionSelectorSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AncillarySourceSettings
import Amazonka.MediaLive.Types.AribSourceSettings
import Amazonka.MediaLive.Types.DvbSubSourceSettings
import Amazonka.MediaLive.Types.EmbeddedSourceSettings
import Amazonka.MediaLive.Types.Scte20SourceSettings
import Amazonka.MediaLive.Types.Scte27SourceSettings
import Amazonka.MediaLive.Types.TeletextSourceSettings
import qualified Amazonka.Prelude as Prelude

-- | Caption Selector Settings
--
-- /See:/ 'newCaptionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'
  { ancillarySourceSettings :: Prelude.Maybe AncillarySourceSettings,
    aribSourceSettings :: Prelude.Maybe AribSourceSettings,
    dvbSubSourceSettings :: Prelude.Maybe DvbSubSourceSettings,
    embeddedSourceSettings :: Prelude.Maybe EmbeddedSourceSettings,
    scte20SourceSettings :: Prelude.Maybe Scte20SourceSettings,
    scte27SourceSettings :: Prelude.Maybe Scte27SourceSettings,
    teletextSourceSettings :: Prelude.Maybe TeletextSourceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionSelectorSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ancillarySourceSettings', 'captionSelectorSettings_ancillarySourceSettings' - Undocumented member.
--
-- 'aribSourceSettings', 'captionSelectorSettings_aribSourceSettings' - Undocumented member.
--
-- 'dvbSubSourceSettings', 'captionSelectorSettings_dvbSubSourceSettings' - Undocumented member.
--
-- 'embeddedSourceSettings', 'captionSelectorSettings_embeddedSourceSettings' - Undocumented member.
--
-- 'scte20SourceSettings', 'captionSelectorSettings_scte20SourceSettings' - Undocumented member.
--
-- 'scte27SourceSettings', 'captionSelectorSettings_scte27SourceSettings' - Undocumented member.
--
-- 'teletextSourceSettings', 'captionSelectorSettings_teletextSourceSettings' - Undocumented member.
newCaptionSelectorSettings ::
  CaptionSelectorSettings
newCaptionSelectorSettings =
  CaptionSelectorSettings'
    { ancillarySourceSettings =
        Prelude.Nothing,
      aribSourceSettings = Prelude.Nothing,
      dvbSubSourceSettings = Prelude.Nothing,
      embeddedSourceSettings = Prelude.Nothing,
      scte20SourceSettings = Prelude.Nothing,
      scte27SourceSettings = Prelude.Nothing,
      teletextSourceSettings = Prelude.Nothing
    }

-- | Undocumented member.
captionSelectorSettings_ancillarySourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe AncillarySourceSettings)
captionSelectorSettings_ancillarySourceSettings = Lens.lens (\CaptionSelectorSettings' {ancillarySourceSettings} -> ancillarySourceSettings) (\s@CaptionSelectorSettings' {} a -> s {ancillarySourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_aribSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe AribSourceSettings)
captionSelectorSettings_aribSourceSettings = Lens.lens (\CaptionSelectorSettings' {aribSourceSettings} -> aribSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {aribSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_dvbSubSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe DvbSubSourceSettings)
captionSelectorSettings_dvbSubSourceSettings = Lens.lens (\CaptionSelectorSettings' {dvbSubSourceSettings} -> dvbSubSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {dvbSubSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_embeddedSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe EmbeddedSourceSettings)
captionSelectorSettings_embeddedSourceSettings = Lens.lens (\CaptionSelectorSettings' {embeddedSourceSettings} -> embeddedSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {embeddedSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte20SourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe Scte20SourceSettings)
captionSelectorSettings_scte20SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte20SourceSettings} -> scte20SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte20SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte27SourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe Scte27SourceSettings)
captionSelectorSettings_scte27SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte27SourceSettings} -> scte27SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte27SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_teletextSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe TeletextSourceSettings)
captionSelectorSettings_teletextSourceSettings = Lens.lens (\CaptionSelectorSettings' {teletextSourceSettings} -> teletextSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {teletextSourceSettings = a} :: CaptionSelectorSettings)

instance Data.FromJSON CaptionSelectorSettings where
  parseJSON =
    Data.withObject
      "CaptionSelectorSettings"
      ( \x ->
          CaptionSelectorSettings'
            Prelude.<$> (x Data..:? "ancillarySourceSettings")
            Prelude.<*> (x Data..:? "aribSourceSettings")
            Prelude.<*> (x Data..:? "dvbSubSourceSettings")
            Prelude.<*> (x Data..:? "embeddedSourceSettings")
            Prelude.<*> (x Data..:? "scte20SourceSettings")
            Prelude.<*> (x Data..:? "scte27SourceSettings")
            Prelude.<*> (x Data..:? "teletextSourceSettings")
      )

instance Prelude.Hashable CaptionSelectorSettings where
  hashWithSalt _salt CaptionSelectorSettings' {..} =
    _salt
      `Prelude.hashWithSalt` ancillarySourceSettings
      `Prelude.hashWithSalt` aribSourceSettings
      `Prelude.hashWithSalt` dvbSubSourceSettings
      `Prelude.hashWithSalt` embeddedSourceSettings
      `Prelude.hashWithSalt` scte20SourceSettings
      `Prelude.hashWithSalt` scte27SourceSettings
      `Prelude.hashWithSalt` teletextSourceSettings

instance Prelude.NFData CaptionSelectorSettings where
  rnf CaptionSelectorSettings' {..} =
    Prelude.rnf ancillarySourceSettings
      `Prelude.seq` Prelude.rnf aribSourceSettings
      `Prelude.seq` Prelude.rnf dvbSubSourceSettings
      `Prelude.seq` Prelude.rnf embeddedSourceSettings
      `Prelude.seq` Prelude.rnf scte20SourceSettings
      `Prelude.seq` Prelude.rnf scte27SourceSettings
      `Prelude.seq` Prelude.rnf teletextSourceSettings

instance Data.ToJSON CaptionSelectorSettings where
  toJSON CaptionSelectorSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ancillarySourceSettings" Data..=)
              Prelude.<$> ancillarySourceSettings,
            ("aribSourceSettings" Data..=)
              Prelude.<$> aribSourceSettings,
            ("dvbSubSourceSettings" Data..=)
              Prelude.<$> dvbSubSourceSettings,
            ("embeddedSourceSettings" Data..=)
              Prelude.<$> embeddedSourceSettings,
            ("scte20SourceSettings" Data..=)
              Prelude.<$> scte20SourceSettings,
            ("scte27SourceSettings" Data..=)
              Prelude.<$> scte27SourceSettings,
            ("teletextSourceSettings" Data..=)
              Prelude.<$> teletextSourceSettings
          ]
      )
