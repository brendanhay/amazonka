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
-- Module      : Network.AWS.MediaLive.Types.Scte20SourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20SourceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte20Convert608To708

-- | Scte20 Source Settings
--
-- /See:/ 'newScte20SourceSettings' smart constructor.
data Scte20SourceSettings = Scte20SourceSettings'
  { -- | If upconvert, 608 data is both passed through via the \"608
    -- compatibility bytes\" fields of the 708 wrapper as well as translated
    -- into 708. 708 data present in the source content will be discarded.
    convert608To708 :: Core.Maybe Scte20Convert608To708,
    -- | Specifies the 608\/708 channel number within the video track from which
    -- to extract captions. Unused for passthrough.
    source608ChannelNumber :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Scte20SourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'convert608To708', 'scte20SourceSettings_convert608To708' - If upconvert, 608 data is both passed through via the \"608
-- compatibility bytes\" fields of the 708 wrapper as well as translated
-- into 708. 708 data present in the source content will be discarded.
--
-- 'source608ChannelNumber', 'scte20SourceSettings_source608ChannelNumber' - Specifies the 608\/708 channel number within the video track from which
-- to extract captions. Unused for passthrough.
newScte20SourceSettings ::
  Scte20SourceSettings
newScte20SourceSettings =
  Scte20SourceSettings'
    { convert608To708 =
        Core.Nothing,
      source608ChannelNumber = Core.Nothing
    }

-- | If upconvert, 608 data is both passed through via the \"608
-- compatibility bytes\" fields of the 708 wrapper as well as translated
-- into 708. 708 data present in the source content will be discarded.
scte20SourceSettings_convert608To708 :: Lens.Lens' Scte20SourceSettings (Core.Maybe Scte20Convert608To708)
scte20SourceSettings_convert608To708 = Lens.lens (\Scte20SourceSettings' {convert608To708} -> convert608To708) (\s@Scte20SourceSettings' {} a -> s {convert608To708 = a} :: Scte20SourceSettings)

-- | Specifies the 608\/708 channel number within the video track from which
-- to extract captions. Unused for passthrough.
scte20SourceSettings_source608ChannelNumber :: Lens.Lens' Scte20SourceSettings (Core.Maybe Core.Natural)
scte20SourceSettings_source608ChannelNumber = Lens.lens (\Scte20SourceSettings' {source608ChannelNumber} -> source608ChannelNumber) (\s@Scte20SourceSettings' {} a -> s {source608ChannelNumber = a} :: Scte20SourceSettings)

instance Core.FromJSON Scte20SourceSettings where
  parseJSON =
    Core.withObject
      "Scte20SourceSettings"
      ( \x ->
          Scte20SourceSettings'
            Core.<$> (x Core..:? "convert608To708")
            Core.<*> (x Core..:? "source608ChannelNumber")
      )

instance Core.Hashable Scte20SourceSettings

instance Core.NFData Scte20SourceSettings

instance Core.ToJSON Scte20SourceSettings where
  toJSON Scte20SourceSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("convert608To708" Core..=)
              Core.<$> convert608To708,
            ("source608ChannelNumber" Core..=)
              Core.<$> source608ChannelNumber
          ]
      )
