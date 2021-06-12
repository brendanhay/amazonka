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
-- Module      : Network.AWS.MediaLive.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedSourceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.EmbeddedConvert608To708
import Network.AWS.MediaLive.Types.EmbeddedScte20Detection

-- | Embedded Source Settings
--
-- /See:/ 'newEmbeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { -- | Set to \"auto\" to handle streams with intermittent and\/or non-aligned
    -- SCTE-20 and Embedded captions.
    scte20Detection :: Core.Maybe EmbeddedScte20Detection,
    -- | If upconvert, 608 data is both passed through via the \"608
    -- compatibility bytes\" fields of the 708 wrapper as well as translated
    -- into 708. 708 data present in the source content will be discarded.
    convert608To708 :: Core.Maybe EmbeddedConvert608To708,
    -- | This field is unused and deprecated.
    source608TrackNumber :: Core.Maybe Core.Natural,
    -- | Specifies the 608\/708 channel number within the video track from which
    -- to extract captions. Unused for passthrough.
    source608ChannelNumber :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EmbeddedSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scte20Detection', 'embeddedSourceSettings_scte20Detection' - Set to \"auto\" to handle streams with intermittent and\/or non-aligned
-- SCTE-20 and Embedded captions.
--
-- 'convert608To708', 'embeddedSourceSettings_convert608To708' - If upconvert, 608 data is both passed through via the \"608
-- compatibility bytes\" fields of the 708 wrapper as well as translated
-- into 708. 708 data present in the source content will be discarded.
--
-- 'source608TrackNumber', 'embeddedSourceSettings_source608TrackNumber' - This field is unused and deprecated.
--
-- 'source608ChannelNumber', 'embeddedSourceSettings_source608ChannelNumber' - Specifies the 608\/708 channel number within the video track from which
-- to extract captions. Unused for passthrough.
newEmbeddedSourceSettings ::
  EmbeddedSourceSettings
newEmbeddedSourceSettings =
  EmbeddedSourceSettings'
    { scte20Detection =
        Core.Nothing,
      convert608To708 = Core.Nothing,
      source608TrackNumber = Core.Nothing,
      source608ChannelNumber = Core.Nothing
    }

-- | Set to \"auto\" to handle streams with intermittent and\/or non-aligned
-- SCTE-20 and Embedded captions.
embeddedSourceSettings_scte20Detection :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe EmbeddedScte20Detection)
embeddedSourceSettings_scte20Detection = Lens.lens (\EmbeddedSourceSettings' {scte20Detection} -> scte20Detection) (\s@EmbeddedSourceSettings' {} a -> s {scte20Detection = a} :: EmbeddedSourceSettings)

-- | If upconvert, 608 data is both passed through via the \"608
-- compatibility bytes\" fields of the 708 wrapper as well as translated
-- into 708. 708 data present in the source content will be discarded.
embeddedSourceSettings_convert608To708 :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe EmbeddedConvert608To708)
embeddedSourceSettings_convert608To708 = Lens.lens (\EmbeddedSourceSettings' {convert608To708} -> convert608To708) (\s@EmbeddedSourceSettings' {} a -> s {convert608To708 = a} :: EmbeddedSourceSettings)

-- | This field is unused and deprecated.
embeddedSourceSettings_source608TrackNumber :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Core.Natural)
embeddedSourceSettings_source608TrackNumber = Lens.lens (\EmbeddedSourceSettings' {source608TrackNumber} -> source608TrackNumber) (\s@EmbeddedSourceSettings' {} a -> s {source608TrackNumber = a} :: EmbeddedSourceSettings)

-- | Specifies the 608\/708 channel number within the video track from which
-- to extract captions. Unused for passthrough.
embeddedSourceSettings_source608ChannelNumber :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Core.Natural)
embeddedSourceSettings_source608ChannelNumber = Lens.lens (\EmbeddedSourceSettings' {source608ChannelNumber} -> source608ChannelNumber) (\s@EmbeddedSourceSettings' {} a -> s {source608ChannelNumber = a} :: EmbeddedSourceSettings)

instance Core.FromJSON EmbeddedSourceSettings where
  parseJSON =
    Core.withObject
      "EmbeddedSourceSettings"
      ( \x ->
          EmbeddedSourceSettings'
            Core.<$> (x Core..:? "scte20Detection")
            Core.<*> (x Core..:? "convert608To708")
            Core.<*> (x Core..:? "source608TrackNumber")
            Core.<*> (x Core..:? "source608ChannelNumber")
      )

instance Core.Hashable EmbeddedSourceSettings

instance Core.NFData EmbeddedSourceSettings

instance Core.ToJSON EmbeddedSourceSettings where
  toJSON EmbeddedSourceSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("scte20Detection" Core..=)
              Core.<$> scte20Detection,
            ("convert608To708" Core..=) Core.<$> convert608To708,
            ("source608TrackNumber" Core..=)
              Core.<$> source608TrackNumber,
            ("source608ChannelNumber" Core..=)
              Core.<$> source608ChannelNumber
          ]
      )
