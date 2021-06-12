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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AncillarySourceSettings
import Network.AWS.MediaLive.Types.AribSourceSettings
import Network.AWS.MediaLive.Types.DvbSubSourceSettings
import Network.AWS.MediaLive.Types.EmbeddedSourceSettings
import Network.AWS.MediaLive.Types.Scte20SourceSettings
import Network.AWS.MediaLive.Types.Scte27SourceSettings
import Network.AWS.MediaLive.Types.TeletextSourceSettings

-- | Caption Selector Settings
--
-- /See:/ 'newCaptionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'
  { ancillarySourceSettings :: Core.Maybe AncillarySourceSettings,
    embeddedSourceSettings :: Core.Maybe EmbeddedSourceSettings,
    aribSourceSettings :: Core.Maybe AribSourceSettings,
    scte27SourceSettings :: Core.Maybe Scte27SourceSettings,
    dvbSubSourceSettings :: Core.Maybe DvbSubSourceSettings,
    scte20SourceSettings :: Core.Maybe Scte20SourceSettings,
    teletextSourceSettings :: Core.Maybe TeletextSourceSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      embeddedSourceSettings = Core.Nothing,
      aribSourceSettings = Core.Nothing,
      scte27SourceSettings = Core.Nothing,
      dvbSubSourceSettings = Core.Nothing,
      scte20SourceSettings = Core.Nothing,
      teletextSourceSettings = Core.Nothing
    }

-- | Undocumented member.
captionSelectorSettings_ancillarySourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe AncillarySourceSettings)
captionSelectorSettings_ancillarySourceSettings = Lens.lens (\CaptionSelectorSettings' {ancillarySourceSettings} -> ancillarySourceSettings) (\s@CaptionSelectorSettings' {} a -> s {ancillarySourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_embeddedSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe EmbeddedSourceSettings)
captionSelectorSettings_embeddedSourceSettings = Lens.lens (\CaptionSelectorSettings' {embeddedSourceSettings} -> embeddedSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {embeddedSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_aribSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe AribSourceSettings)
captionSelectorSettings_aribSourceSettings = Lens.lens (\CaptionSelectorSettings' {aribSourceSettings} -> aribSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {aribSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte27SourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Scte27SourceSettings)
captionSelectorSettings_scte27SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte27SourceSettings} -> scte27SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte27SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_dvbSubSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe DvbSubSourceSettings)
captionSelectorSettings_dvbSubSourceSettings = Lens.lens (\CaptionSelectorSettings' {dvbSubSourceSettings} -> dvbSubSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {dvbSubSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte20SourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Scte20SourceSettings)
captionSelectorSettings_scte20SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte20SourceSettings} -> scte20SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte20SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_teletextSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe TeletextSourceSettings)
captionSelectorSettings_teletextSourceSettings = Lens.lens (\CaptionSelectorSettings' {teletextSourceSettings} -> teletextSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {teletextSourceSettings = a} :: CaptionSelectorSettings)

instance Core.FromJSON CaptionSelectorSettings where
  parseJSON =
    Core.withObject
      "CaptionSelectorSettings"
      ( \x ->
          CaptionSelectorSettings'
            Core.<$> (x Core..:? "ancillarySourceSettings")
            Core.<*> (x Core..:? "embeddedSourceSettings")
            Core.<*> (x Core..:? "aribSourceSettings")
            Core.<*> (x Core..:? "scte27SourceSettings")
            Core.<*> (x Core..:? "dvbSubSourceSettings")
            Core.<*> (x Core..:? "scte20SourceSettings")
            Core.<*> (x Core..:? "teletextSourceSettings")
      )

instance Core.Hashable CaptionSelectorSettings

instance Core.NFData CaptionSelectorSettings

instance Core.ToJSON CaptionSelectorSettings where
  toJSON CaptionSelectorSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ancillarySourceSettings" Core..=)
              Core.<$> ancillarySourceSettings,
            ("embeddedSourceSettings" Core..=)
              Core.<$> embeddedSourceSettings,
            ("aribSourceSettings" Core..=)
              Core.<$> aribSourceSettings,
            ("scte27SourceSettings" Core..=)
              Core.<$> scte27SourceSettings,
            ("dvbSubSourceSettings" Core..=)
              Core.<$> dvbSubSourceSettings,
            ("scte20SourceSettings" Core..=)
              Core.<$> scte20SourceSettings,
            ("teletextSourceSettings" Core..=)
              Core.<$> teletextSourceSettings
          ]
      )
