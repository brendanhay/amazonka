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
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings specific to embedded\/ancillary caption outputs, including
-- 608\/708 Channel destination number.
--
-- /See:/ 'newEmbeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  { -- | Ignore this setting unless your input captions are SCC format and you
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
    destination708ServiceNumber :: Core.Maybe Core.Natural,
    -- | Ignore this setting unless your input captions are SCC format and your
    -- output captions are embedded in the video stream. Specify a CC number
    -- for each captions channel in this output. If you have two channels,
    -- choose CC numbers that aren\'t in the same field. For example, choose 1
    -- and 3. For more information, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
    destination608ChannelNumber :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EmbeddedDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'destination608ChannelNumber', 'embeddedDestinationSettings_destination608ChannelNumber' - Ignore this setting unless your input captions are SCC format and your
-- output captions are embedded in the video stream. Specify a CC number
-- for each captions channel in this output. If you have two channels,
-- choose CC numbers that aren\'t in the same field. For example, choose 1
-- and 3. For more information, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
newEmbeddedDestinationSettings ::
  EmbeddedDestinationSettings
newEmbeddedDestinationSettings =
  EmbeddedDestinationSettings'
    { destination708ServiceNumber =
        Core.Nothing,
      destination608ChannelNumber = Core.Nothing
    }

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
embeddedDestinationSettings_destination708ServiceNumber :: Lens.Lens' EmbeddedDestinationSettings (Core.Maybe Core.Natural)
embeddedDestinationSettings_destination708ServiceNumber = Lens.lens (\EmbeddedDestinationSettings' {destination708ServiceNumber} -> destination708ServiceNumber) (\s@EmbeddedDestinationSettings' {} a -> s {destination708ServiceNumber = a} :: EmbeddedDestinationSettings)

-- | Ignore this setting unless your input captions are SCC format and your
-- output captions are embedded in the video stream. Specify a CC number
-- for each captions channel in this output. If you have two channels,
-- choose CC numbers that aren\'t in the same field. For example, choose 1
-- and 3. For more information, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/dual-scc-to-embedded.
embeddedDestinationSettings_destination608ChannelNumber :: Lens.Lens' EmbeddedDestinationSettings (Core.Maybe Core.Natural)
embeddedDestinationSettings_destination608ChannelNumber = Lens.lens (\EmbeddedDestinationSettings' {destination608ChannelNumber} -> destination608ChannelNumber) (\s@EmbeddedDestinationSettings' {} a -> s {destination608ChannelNumber = a} :: EmbeddedDestinationSettings)

instance Core.FromJSON EmbeddedDestinationSettings where
  parseJSON =
    Core.withObject
      "EmbeddedDestinationSettings"
      ( \x ->
          EmbeddedDestinationSettings'
            Core.<$> (x Core..:? "destination708ServiceNumber")
            Core.<*> (x Core..:? "destination608ChannelNumber")
      )

instance Core.Hashable EmbeddedDestinationSettings

instance Core.NFData EmbeddedDestinationSettings

instance Core.ToJSON EmbeddedDestinationSettings where
  toJSON EmbeddedDestinationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destination708ServiceNumber" Core..=)
              Core.<$> destination708ServiceNumber,
            ("destination608ChannelNumber" Core..=)
              Core.<$> destination608ChannelNumber
          ]
      )
