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
-- Module      : Amazonka.MediaLive.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.EmbeddedSourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.EmbeddedConvert608To708
import Amazonka.MediaLive.Types.EmbeddedScte20Detection
import qualified Amazonka.Prelude as Prelude

-- | Embedded Source Settings
--
-- /See:/ 'newEmbeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { -- | If upconvert, 608 data is both passed through via the \"608
    -- compatibility bytes\" fields of the 708 wrapper as well as translated
    -- into 708. 708 data present in the source content will be discarded.
    convert608To708 :: Prelude.Maybe EmbeddedConvert608To708,
    -- | Set to \"auto\" to handle streams with intermittent and\/or non-aligned
    -- SCTE-20 and Embedded captions.
    scte20Detection :: Prelude.Maybe EmbeddedScte20Detection,
    -- | Specifies the 608\/708 channel number within the video track from which
    -- to extract captions. Unused for passthrough.
    source608ChannelNumber :: Prelude.Maybe Prelude.Natural,
    -- | This field is unused and deprecated.
    source608TrackNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmbeddedSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'convert608To708', 'embeddedSourceSettings_convert608To708' - If upconvert, 608 data is both passed through via the \"608
-- compatibility bytes\" fields of the 708 wrapper as well as translated
-- into 708. 708 data present in the source content will be discarded.
--
-- 'scte20Detection', 'embeddedSourceSettings_scte20Detection' - Set to \"auto\" to handle streams with intermittent and\/or non-aligned
-- SCTE-20 and Embedded captions.
--
-- 'source608ChannelNumber', 'embeddedSourceSettings_source608ChannelNumber' - Specifies the 608\/708 channel number within the video track from which
-- to extract captions. Unused for passthrough.
--
-- 'source608TrackNumber', 'embeddedSourceSettings_source608TrackNumber' - This field is unused and deprecated.
newEmbeddedSourceSettings ::
  EmbeddedSourceSettings
newEmbeddedSourceSettings =
  EmbeddedSourceSettings'
    { convert608To708 =
        Prelude.Nothing,
      scte20Detection = Prelude.Nothing,
      source608ChannelNumber = Prelude.Nothing,
      source608TrackNumber = Prelude.Nothing
    }

-- | If upconvert, 608 data is both passed through via the \"608
-- compatibility bytes\" fields of the 708 wrapper as well as translated
-- into 708. 708 data present in the source content will be discarded.
embeddedSourceSettings_convert608To708 :: Lens.Lens' EmbeddedSourceSettings (Prelude.Maybe EmbeddedConvert608To708)
embeddedSourceSettings_convert608To708 = Lens.lens (\EmbeddedSourceSettings' {convert608To708} -> convert608To708) (\s@EmbeddedSourceSettings' {} a -> s {convert608To708 = a} :: EmbeddedSourceSettings)

-- | Set to \"auto\" to handle streams with intermittent and\/or non-aligned
-- SCTE-20 and Embedded captions.
embeddedSourceSettings_scte20Detection :: Lens.Lens' EmbeddedSourceSettings (Prelude.Maybe EmbeddedScte20Detection)
embeddedSourceSettings_scte20Detection = Lens.lens (\EmbeddedSourceSettings' {scte20Detection} -> scte20Detection) (\s@EmbeddedSourceSettings' {} a -> s {scte20Detection = a} :: EmbeddedSourceSettings)

-- | Specifies the 608\/708 channel number within the video track from which
-- to extract captions. Unused for passthrough.
embeddedSourceSettings_source608ChannelNumber :: Lens.Lens' EmbeddedSourceSettings (Prelude.Maybe Prelude.Natural)
embeddedSourceSettings_source608ChannelNumber = Lens.lens (\EmbeddedSourceSettings' {source608ChannelNumber} -> source608ChannelNumber) (\s@EmbeddedSourceSettings' {} a -> s {source608ChannelNumber = a} :: EmbeddedSourceSettings)

-- | This field is unused and deprecated.
embeddedSourceSettings_source608TrackNumber :: Lens.Lens' EmbeddedSourceSettings (Prelude.Maybe Prelude.Natural)
embeddedSourceSettings_source608TrackNumber = Lens.lens (\EmbeddedSourceSettings' {source608TrackNumber} -> source608TrackNumber) (\s@EmbeddedSourceSettings' {} a -> s {source608TrackNumber = a} :: EmbeddedSourceSettings)

instance Data.FromJSON EmbeddedSourceSettings where
  parseJSON =
    Data.withObject
      "EmbeddedSourceSettings"
      ( \x ->
          EmbeddedSourceSettings'
            Prelude.<$> (x Data..:? "convert608To708")
            Prelude.<*> (x Data..:? "scte20Detection")
            Prelude.<*> (x Data..:? "source608ChannelNumber")
            Prelude.<*> (x Data..:? "source608TrackNumber")
      )

instance Prelude.Hashable EmbeddedSourceSettings where
  hashWithSalt _salt EmbeddedSourceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` convert608To708
      `Prelude.hashWithSalt` scte20Detection
      `Prelude.hashWithSalt` source608ChannelNumber
      `Prelude.hashWithSalt` source608TrackNumber

instance Prelude.NFData EmbeddedSourceSettings where
  rnf EmbeddedSourceSettings' {..} =
    Prelude.rnf convert608To708
      `Prelude.seq` Prelude.rnf scte20Detection
      `Prelude.seq` Prelude.rnf source608ChannelNumber
      `Prelude.seq` Prelude.rnf source608TrackNumber

instance Data.ToJSON EmbeddedSourceSettings where
  toJSON EmbeddedSourceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("convert608To708" Data..=)
              Prelude.<$> convert608To708,
            ("scte20Detection" Data..=)
              Prelude.<$> scte20Detection,
            ("source608ChannelNumber" Data..=)
              Prelude.<$> source608ChannelNumber,
            ("source608TrackNumber" Data..=)
              Prelude.<$> source608TrackNumber
          ]
      )
