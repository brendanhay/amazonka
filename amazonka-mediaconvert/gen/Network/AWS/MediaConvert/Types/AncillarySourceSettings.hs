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
-- Module      : Network.AWS.MediaConvert.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillarySourceSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AncillaryConvert608To708
import Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
import qualified Network.AWS.Prelude as Prelude

-- | Settings for ancillary captions source.
--
-- /See:/ 'newAncillarySourceSettings' smart constructor.
data AncillarySourceSettings = AncillarySourceSettings'
  { -- | By default, the service terminates any unterminated captions at the end
    -- of each input. If you want the caption to continue onto your next input,
    -- disable this setting.
    terminateCaptions :: Prelude.Maybe AncillaryTerminateCaptions,
    -- | Specify whether this set of input captions appears in your outputs in
    -- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
    -- MediaConvert includes the captions data in two ways: it passes the 608
    -- data through using the 608 compatibility bytes fields of the 708
    -- wrapper, and it also translates the 608 data into 708.
    convert608To708 :: Prelude.Maybe AncillaryConvert608To708,
    -- | Specifies the 608 channel number in the ancillary data track from which
    -- to extract captions. Unused for passthrough.
    sourceAncillaryChannelNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AncillarySourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminateCaptions', 'ancillarySourceSettings_terminateCaptions' - By default, the service terminates any unterminated captions at the end
-- of each input. If you want the caption to continue onto your next input,
-- disable this setting.
--
-- 'convert608To708', 'ancillarySourceSettings_convert608To708' - Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
--
-- 'sourceAncillaryChannelNumber', 'ancillarySourceSettings_sourceAncillaryChannelNumber' - Specifies the 608 channel number in the ancillary data track from which
-- to extract captions. Unused for passthrough.
newAncillarySourceSettings ::
  AncillarySourceSettings
newAncillarySourceSettings =
  AncillarySourceSettings'
    { terminateCaptions =
        Prelude.Nothing,
      convert608To708 = Prelude.Nothing,
      sourceAncillaryChannelNumber = Prelude.Nothing
    }

-- | By default, the service terminates any unterminated captions at the end
-- of each input. If you want the caption to continue onto your next input,
-- disable this setting.
ancillarySourceSettings_terminateCaptions :: Lens.Lens' AncillarySourceSettings (Prelude.Maybe AncillaryTerminateCaptions)
ancillarySourceSettings_terminateCaptions = Lens.lens (\AncillarySourceSettings' {terminateCaptions} -> terminateCaptions) (\s@AncillarySourceSettings' {} a -> s {terminateCaptions = a} :: AncillarySourceSettings)

-- | Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
ancillarySourceSettings_convert608To708 :: Lens.Lens' AncillarySourceSettings (Prelude.Maybe AncillaryConvert608To708)
ancillarySourceSettings_convert608To708 = Lens.lens (\AncillarySourceSettings' {convert608To708} -> convert608To708) (\s@AncillarySourceSettings' {} a -> s {convert608To708 = a} :: AncillarySourceSettings)

-- | Specifies the 608 channel number in the ancillary data track from which
-- to extract captions. Unused for passthrough.
ancillarySourceSettings_sourceAncillaryChannelNumber :: Lens.Lens' AncillarySourceSettings (Prelude.Maybe Prelude.Natural)
ancillarySourceSettings_sourceAncillaryChannelNumber = Lens.lens (\AncillarySourceSettings' {sourceAncillaryChannelNumber} -> sourceAncillaryChannelNumber) (\s@AncillarySourceSettings' {} a -> s {sourceAncillaryChannelNumber = a} :: AncillarySourceSettings)

instance Prelude.FromJSON AncillarySourceSettings where
  parseJSON =
    Prelude.withObject
      "AncillarySourceSettings"
      ( \x ->
          AncillarySourceSettings'
            Prelude.<$> (x Prelude..:? "terminateCaptions")
            Prelude.<*> (x Prelude..:? "convert608To708")
            Prelude.<*> (x Prelude..:? "sourceAncillaryChannelNumber")
      )

instance Prelude.Hashable AncillarySourceSettings

instance Prelude.NFData AncillarySourceSettings

instance Prelude.ToJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("terminateCaptions" Prelude..=)
              Prelude.<$> terminateCaptions,
            ("convert608To708" Prelude..=)
              Prelude.<$> convert608To708,
            ("sourceAncillaryChannelNumber" Prelude..=)
              Prelude.<$> sourceAncillaryChannelNumber
          ]
      )
