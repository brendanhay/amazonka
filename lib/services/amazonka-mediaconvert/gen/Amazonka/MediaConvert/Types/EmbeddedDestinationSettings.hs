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
-- Module      : Amazonka.MediaConvert.Types.EmbeddedDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.EmbeddedDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings related to CEA\/EIA-608 and CEA\/EIA-708 (also called embedded
-- or ancillary) captions. Set up embedded captions in the same output as
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/embedded-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to
-- EMBEDDED, EMBEDDED_PLUS_SCTE20, or SCTE20_PLUS_EMBEDDED.
--
-- /See:/ 'newEmbeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  { -- | Ignore this setting unless your input captions are SCC format and your
    -- output captions are embedded in the video stream. Specify a CC number
    -- for each captions channel in this output. If you have two channels,
    -- choose CC numbers that aren\'t in the same field. For example, choose 1
    -- and 3. For more information, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
    destination608ChannelNumber :: Prelude.Maybe Prelude.Natural,
    -- | Ignore this setting unless your input captions are SCC format and you
    -- want both 608 and 708 captions embedded in your output stream.
    -- Optionally, specify the 708 service number for each output captions
    -- channel. Choose a different number for each channel. To use this
    -- setting, also set Force 608 to 708 upconvert (Convert608To708) to
    -- Upconvert (UPCONVERT) in your input captions selector settings. If you
    -- choose to upconvert but don\'t specify a 708 service number,
    -- MediaConvert uses the number that you specify for CC channel number
    -- (destination608ChannelNumber) for the 708 service number. For more
    -- information, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
    destination708ServiceNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmbeddedDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination608ChannelNumber', 'embeddedDestinationSettings_destination608ChannelNumber' - Ignore this setting unless your input captions are SCC format and your
-- output captions are embedded in the video stream. Specify a CC number
-- for each captions channel in this output. If you have two channels,
-- choose CC numbers that aren\'t in the same field. For example, choose 1
-- and 3. For more information, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
--
-- 'destination708ServiceNumber', 'embeddedDestinationSettings_destination708ServiceNumber' - Ignore this setting unless your input captions are SCC format and you
-- want both 608 and 708 captions embedded in your output stream.
-- Optionally, specify the 708 service number for each output captions
-- channel. Choose a different number for each channel. To use this
-- setting, also set Force 608 to 708 upconvert (Convert608To708) to
-- Upconvert (UPCONVERT) in your input captions selector settings. If you
-- choose to upconvert but don\'t specify a 708 service number,
-- MediaConvert uses the number that you specify for CC channel number
-- (destination608ChannelNumber) for the 708 service number. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
newEmbeddedDestinationSettings ::
  EmbeddedDestinationSettings
newEmbeddedDestinationSettings =
  EmbeddedDestinationSettings'
    { destination608ChannelNumber =
        Prelude.Nothing,
      destination708ServiceNumber = Prelude.Nothing
    }

-- | Ignore this setting unless your input captions are SCC format and your
-- output captions are embedded in the video stream. Specify a CC number
-- for each captions channel in this output. If you have two channels,
-- choose CC numbers that aren\'t in the same field. For example, choose 1
-- and 3. For more information, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
embeddedDestinationSettings_destination608ChannelNumber :: Lens.Lens' EmbeddedDestinationSettings (Prelude.Maybe Prelude.Natural)
embeddedDestinationSettings_destination608ChannelNumber = Lens.lens (\EmbeddedDestinationSettings' {destination608ChannelNumber} -> destination608ChannelNumber) (\s@EmbeddedDestinationSettings' {} a -> s {destination608ChannelNumber = a} :: EmbeddedDestinationSettings)

-- | Ignore this setting unless your input captions are SCC format and you
-- want both 608 and 708 captions embedded in your output stream.
-- Optionally, specify the 708 service number for each output captions
-- channel. Choose a different number for each channel. To use this
-- setting, also set Force 608 to 708 upconvert (Convert608To708) to
-- Upconvert (UPCONVERT) in your input captions selector settings. If you
-- choose to upconvert but don\'t specify a 708 service number,
-- MediaConvert uses the number that you specify for CC channel number
-- (destination608ChannelNumber) for the 708 service number. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
embeddedDestinationSettings_destination708ServiceNumber :: Lens.Lens' EmbeddedDestinationSettings (Prelude.Maybe Prelude.Natural)
embeddedDestinationSettings_destination708ServiceNumber = Lens.lens (\EmbeddedDestinationSettings' {destination708ServiceNumber} -> destination708ServiceNumber) (\s@EmbeddedDestinationSettings' {} a -> s {destination708ServiceNumber = a} :: EmbeddedDestinationSettings)

instance Data.FromJSON EmbeddedDestinationSettings where
  parseJSON =
    Data.withObject
      "EmbeddedDestinationSettings"
      ( \x ->
          EmbeddedDestinationSettings'
            Prelude.<$> (x Data..:? "destination608ChannelNumber")
            Prelude.<*> (x Data..:? "destination708ServiceNumber")
      )

instance Prelude.Hashable EmbeddedDestinationSettings where
  hashWithSalt _salt EmbeddedDestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` destination608ChannelNumber
      `Prelude.hashWithSalt` destination708ServiceNumber

instance Prelude.NFData EmbeddedDestinationSettings where
  rnf EmbeddedDestinationSettings' {..} =
    Prelude.rnf destination608ChannelNumber
      `Prelude.seq` Prelude.rnf destination708ServiceNumber

instance Data.ToJSON EmbeddedDestinationSettings where
  toJSON EmbeddedDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destination608ChannelNumber" Data..=)
              Prelude.<$> destination608ChannelNumber,
            ("destination708ServiceNumber" Data..=)
              Prelude.<$> destination708ServiceNumber
          ]
      )
