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
-- Module      : Amazonka.MediaConvert.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AncillarySourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.AncillaryConvert608To708
import Amazonka.MediaConvert.Types.AncillaryTerminateCaptions
import qualified Amazonka.Prelude as Prelude

-- | Settings for ancillary captions source.
--
-- /See:/ 'newAncillarySourceSettings' smart constructor.
data AncillarySourceSettings = AncillarySourceSettings'
  { -- | Specify whether this set of input captions appears in your outputs in
    -- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
    -- MediaConvert includes the captions data in two ways: it passes the 608
    -- data through using the 608 compatibility bytes fields of the 708
    -- wrapper, and it also translates the 608 data into 708.
    convert608To708 :: Prelude.Maybe AncillaryConvert608To708,
    -- | By default, the service terminates any unterminated captions at the end
    -- of each input. If you want the caption to continue onto your next input,
    -- disable this setting.
    terminateCaptions :: Prelude.Maybe AncillaryTerminateCaptions,
    -- | Specifies the 608 channel number in the ancillary data track from which
    -- to extract captions. Unused for passthrough.
    sourceAncillaryChannelNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AncillarySourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'convert608To708', 'ancillarySourceSettings_convert608To708' - Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
--
-- 'terminateCaptions', 'ancillarySourceSettings_terminateCaptions' - By default, the service terminates any unterminated captions at the end
-- of each input. If you want the caption to continue onto your next input,
-- disable this setting.
--
-- 'sourceAncillaryChannelNumber', 'ancillarySourceSettings_sourceAncillaryChannelNumber' - Specifies the 608 channel number in the ancillary data track from which
-- to extract captions. Unused for passthrough.
newAncillarySourceSettings ::
  AncillarySourceSettings
newAncillarySourceSettings =
  AncillarySourceSettings'
    { convert608To708 =
        Prelude.Nothing,
      terminateCaptions = Prelude.Nothing,
      sourceAncillaryChannelNumber = Prelude.Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
ancillarySourceSettings_convert608To708 :: Lens.Lens' AncillarySourceSettings (Prelude.Maybe AncillaryConvert608To708)
ancillarySourceSettings_convert608To708 = Lens.lens (\AncillarySourceSettings' {convert608To708} -> convert608To708) (\s@AncillarySourceSettings' {} a -> s {convert608To708 = a} :: AncillarySourceSettings)

-- | By default, the service terminates any unterminated captions at the end
-- of each input. If you want the caption to continue onto your next input,
-- disable this setting.
ancillarySourceSettings_terminateCaptions :: Lens.Lens' AncillarySourceSettings (Prelude.Maybe AncillaryTerminateCaptions)
ancillarySourceSettings_terminateCaptions = Lens.lens (\AncillarySourceSettings' {terminateCaptions} -> terminateCaptions) (\s@AncillarySourceSettings' {} a -> s {terminateCaptions = a} :: AncillarySourceSettings)

-- | Specifies the 608 channel number in the ancillary data track from which
-- to extract captions. Unused for passthrough.
ancillarySourceSettings_sourceAncillaryChannelNumber :: Lens.Lens' AncillarySourceSettings (Prelude.Maybe Prelude.Natural)
ancillarySourceSettings_sourceAncillaryChannelNumber = Lens.lens (\AncillarySourceSettings' {sourceAncillaryChannelNumber} -> sourceAncillaryChannelNumber) (\s@AncillarySourceSettings' {} a -> s {sourceAncillaryChannelNumber = a} :: AncillarySourceSettings)

instance Core.FromJSON AncillarySourceSettings where
  parseJSON =
    Core.withObject
      "AncillarySourceSettings"
      ( \x ->
          AncillarySourceSettings'
            Prelude.<$> (x Core..:? "convert608To708")
            Prelude.<*> (x Core..:? "terminateCaptions")
            Prelude.<*> (x Core..:? "sourceAncillaryChannelNumber")
      )

instance Prelude.Hashable AncillarySourceSettings where
  hashWithSalt _salt AncillarySourceSettings' {..} =
    _salt `Prelude.hashWithSalt` convert608To708
      `Prelude.hashWithSalt` terminateCaptions
      `Prelude.hashWithSalt` sourceAncillaryChannelNumber

instance Prelude.NFData AncillarySourceSettings where
  rnf AncillarySourceSettings' {..} =
    Prelude.rnf convert608To708
      `Prelude.seq` Prelude.rnf terminateCaptions
      `Prelude.seq` Prelude.rnf sourceAncillaryChannelNumber

instance Core.ToJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("convert608To708" Core..=)
              Prelude.<$> convert608To708,
            ("terminateCaptions" Core..=)
              Prelude.<$> terminateCaptions,
            ("sourceAncillaryChannelNumber" Core..=)
              Prelude.<$> sourceAncillaryChannelNumber
          ]
      )
