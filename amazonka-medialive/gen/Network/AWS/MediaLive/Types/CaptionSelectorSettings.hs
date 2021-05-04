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
-- Module      : Network.AWS.MediaLive.Types.CaptionSelectorSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionSelectorSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AncillarySourceSettings
import Network.AWS.MediaLive.Types.AribSourceSettings
import Network.AWS.MediaLive.Types.DvbSubSourceSettings
import Network.AWS.MediaLive.Types.EmbeddedSourceSettings
import Network.AWS.MediaLive.Types.Scte20SourceSettings
import Network.AWS.MediaLive.Types.Scte27SourceSettings
import Network.AWS.MediaLive.Types.TeletextSourceSettings
import qualified Network.AWS.Prelude as Prelude

-- | Caption Selector Settings
--
-- /See:/ 'newCaptionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'
  { ancillarySourceSettings :: Prelude.Maybe AncillarySourceSettings,
    embeddedSourceSettings :: Prelude.Maybe EmbeddedSourceSettings,
    aribSourceSettings :: Prelude.Maybe AribSourceSettings,
    scte27SourceSettings :: Prelude.Maybe Scte27SourceSettings,
    dvbSubSourceSettings :: Prelude.Maybe DvbSubSourceSettings,
    scte20SourceSettings :: Prelude.Maybe Scte20SourceSettings,
    teletextSourceSettings :: Prelude.Maybe TeletextSourceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'embeddedSourceSettings', 'captionSelectorSettings_embeddedSourceSettings' - Undocumented member.
--
-- 'aribSourceSettings', 'captionSelectorSettings_aribSourceSettings' - Undocumented member.
--
-- 'scte27SourceSettings', 'captionSelectorSettings_scte27SourceSettings' - Undocumented member.
--
-- 'dvbSubSourceSettings', 'captionSelectorSettings_dvbSubSourceSettings' - Undocumented member.
--
-- 'scte20SourceSettings', 'captionSelectorSettings_scte20SourceSettings' - Undocumented member.
--
-- 'teletextSourceSettings', 'captionSelectorSettings_teletextSourceSettings' - Undocumented member.
newCaptionSelectorSettings ::
  CaptionSelectorSettings
newCaptionSelectorSettings =
  CaptionSelectorSettings'
    { ancillarySourceSettings =
        Prelude.Nothing,
      embeddedSourceSettings = Prelude.Nothing,
      aribSourceSettings = Prelude.Nothing,
      scte27SourceSettings = Prelude.Nothing,
      dvbSubSourceSettings = Prelude.Nothing,
      scte20SourceSettings = Prelude.Nothing,
      teletextSourceSettings = Prelude.Nothing
    }

-- | Undocumented member.
captionSelectorSettings_ancillarySourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe AncillarySourceSettings)
captionSelectorSettings_ancillarySourceSettings = Lens.lens (\CaptionSelectorSettings' {ancillarySourceSettings} -> ancillarySourceSettings) (\s@CaptionSelectorSettings' {} a -> s {ancillarySourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_embeddedSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe EmbeddedSourceSettings)
captionSelectorSettings_embeddedSourceSettings = Lens.lens (\CaptionSelectorSettings' {embeddedSourceSettings} -> embeddedSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {embeddedSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_aribSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe AribSourceSettings)
captionSelectorSettings_aribSourceSettings = Lens.lens (\CaptionSelectorSettings' {aribSourceSettings} -> aribSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {aribSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte27SourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe Scte27SourceSettings)
captionSelectorSettings_scte27SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte27SourceSettings} -> scte27SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte27SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_dvbSubSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe DvbSubSourceSettings)
captionSelectorSettings_dvbSubSourceSettings = Lens.lens (\CaptionSelectorSettings' {dvbSubSourceSettings} -> dvbSubSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {dvbSubSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte20SourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe Scte20SourceSettings)
captionSelectorSettings_scte20SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte20SourceSettings} -> scte20SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte20SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_teletextSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe TeletextSourceSettings)
captionSelectorSettings_teletextSourceSettings = Lens.lens (\CaptionSelectorSettings' {teletextSourceSettings} -> teletextSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {teletextSourceSettings = a} :: CaptionSelectorSettings)

instance Prelude.FromJSON CaptionSelectorSettings where
  parseJSON =
    Prelude.withObject
      "CaptionSelectorSettings"
      ( \x ->
          CaptionSelectorSettings'
            Prelude.<$> (x Prelude..:? "ancillarySourceSettings")
            Prelude.<*> (x Prelude..:? "embeddedSourceSettings")
            Prelude.<*> (x Prelude..:? "aribSourceSettings")
            Prelude.<*> (x Prelude..:? "scte27SourceSettings")
            Prelude.<*> (x Prelude..:? "dvbSubSourceSettings")
            Prelude.<*> (x Prelude..:? "scte20SourceSettings")
            Prelude.<*> (x Prelude..:? "teletextSourceSettings")
      )

instance Prelude.Hashable CaptionSelectorSettings

instance Prelude.NFData CaptionSelectorSettings

instance Prelude.ToJSON CaptionSelectorSettings where
  toJSON CaptionSelectorSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ancillarySourceSettings" Prelude..=)
              Prelude.<$> ancillarySourceSettings,
            ("embeddedSourceSettings" Prelude..=)
              Prelude.<$> embeddedSourceSettings,
            ("aribSourceSettings" Prelude..=)
              Prelude.<$> aribSourceSettings,
            ("scte27SourceSettings" Prelude..=)
              Prelude.<$> scte27SourceSettings,
            ("dvbSubSourceSettings" Prelude..=)
              Prelude.<$> dvbSubSourceSettings,
            ("scte20SourceSettings" Prelude..=)
              Prelude.<$> scte20SourceSettings,
            ("teletextSourceSettings" Prelude..=)
              Prelude.<$> teletextSourceSettings
          ]
      )
