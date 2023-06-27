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
-- Module      : Amazonka.MediaLive.Types.TeletextSourceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TeletextSourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.CaptionRectangle
import qualified Amazonka.Prelude as Prelude

-- | Teletext Source Settings
--
-- /See:/ 'newTeletextSourceSettings' smart constructor.
data TeletextSourceSettings = TeletextSourceSettings'
  { -- | Optionally defines a region where TTML style captions will be displayed
    outputRectangle :: Prelude.Maybe CaptionRectangle,
    -- | Specifies the teletext page number within the data stream from which to
    -- extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for
    -- passthrough. Should be specified as a hexadecimal string with no \"0x\"
    -- prefix.
    pageNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TeletextSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputRectangle', 'teletextSourceSettings_outputRectangle' - Optionally defines a region where TTML style captions will be displayed
--
-- 'pageNumber', 'teletextSourceSettings_pageNumber' - Specifies the teletext page number within the data stream from which to
-- extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for
-- passthrough. Should be specified as a hexadecimal string with no \"0x\"
-- prefix.
newTeletextSourceSettings ::
  TeletextSourceSettings
newTeletextSourceSettings =
  TeletextSourceSettings'
    { outputRectangle =
        Prelude.Nothing,
      pageNumber = Prelude.Nothing
    }

-- | Optionally defines a region where TTML style captions will be displayed
teletextSourceSettings_outputRectangle :: Lens.Lens' TeletextSourceSettings (Prelude.Maybe CaptionRectangle)
teletextSourceSettings_outputRectangle = Lens.lens (\TeletextSourceSettings' {outputRectangle} -> outputRectangle) (\s@TeletextSourceSettings' {} a -> s {outputRectangle = a} :: TeletextSourceSettings)

-- | Specifies the teletext page number within the data stream from which to
-- extract captions. Range of 0x100 (256) to 0x8FF (2303). Unused for
-- passthrough. Should be specified as a hexadecimal string with no \"0x\"
-- prefix.
teletextSourceSettings_pageNumber :: Lens.Lens' TeletextSourceSettings (Prelude.Maybe Prelude.Text)
teletextSourceSettings_pageNumber = Lens.lens (\TeletextSourceSettings' {pageNumber} -> pageNumber) (\s@TeletextSourceSettings' {} a -> s {pageNumber = a} :: TeletextSourceSettings)

instance Data.FromJSON TeletextSourceSettings where
  parseJSON =
    Data.withObject
      "TeletextSourceSettings"
      ( \x ->
          TeletextSourceSettings'
            Prelude.<$> (x Data..:? "outputRectangle")
            Prelude.<*> (x Data..:? "pageNumber")
      )

instance Prelude.Hashable TeletextSourceSettings where
  hashWithSalt _salt TeletextSourceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` outputRectangle
      `Prelude.hashWithSalt` pageNumber

instance Prelude.NFData TeletextSourceSettings where
  rnf TeletextSourceSettings' {..} =
    Prelude.rnf outputRectangle
      `Prelude.seq` Prelude.rnf pageNumber

instance Data.ToJSON TeletextSourceSettings where
  toJSON TeletextSourceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("outputRectangle" Data..=)
              Prelude.<$> outputRectangle,
            ("pageNumber" Data..=) Prelude.<$> pageNumber
          ]
      )
