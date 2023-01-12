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
-- Module      : Amazonka.Textract.Types.LendingDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.LendingDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Geometry
import Amazonka.Textract.Types.SelectionStatus

-- | The results extracted for a lending document.
--
-- /See:/ 'newLendingDetection' smart constructor.
data LendingDetection = LendingDetection'
  { -- | The confidence level for the text of a detected value in a lending
    -- document.
    confidence :: Prelude.Maybe Prelude.Double,
    geometry :: Prelude.Maybe Geometry,
    -- | The selection status of a selection element, such as an option button or
    -- check box.
    selectionStatus :: Prelude.Maybe SelectionStatus,
    -- | The text extracted for a detected value in a lending document.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LendingDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'lendingDetection_confidence' - The confidence level for the text of a detected value in a lending
-- document.
--
-- 'geometry', 'lendingDetection_geometry' - Undocumented member.
--
-- 'selectionStatus', 'lendingDetection_selectionStatus' - The selection status of a selection element, such as an option button or
-- check box.
--
-- 'text', 'lendingDetection_text' - The text extracted for a detected value in a lending document.
newLendingDetection ::
  LendingDetection
newLendingDetection =
  LendingDetection'
    { confidence = Prelude.Nothing,
      geometry = Prelude.Nothing,
      selectionStatus = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The confidence level for the text of a detected value in a lending
-- document.
lendingDetection_confidence :: Lens.Lens' LendingDetection (Prelude.Maybe Prelude.Double)
lendingDetection_confidence = Lens.lens (\LendingDetection' {confidence} -> confidence) (\s@LendingDetection' {} a -> s {confidence = a} :: LendingDetection)

-- | Undocumented member.
lendingDetection_geometry :: Lens.Lens' LendingDetection (Prelude.Maybe Geometry)
lendingDetection_geometry = Lens.lens (\LendingDetection' {geometry} -> geometry) (\s@LendingDetection' {} a -> s {geometry = a} :: LendingDetection)

-- | The selection status of a selection element, such as an option button or
-- check box.
lendingDetection_selectionStatus :: Lens.Lens' LendingDetection (Prelude.Maybe SelectionStatus)
lendingDetection_selectionStatus = Lens.lens (\LendingDetection' {selectionStatus} -> selectionStatus) (\s@LendingDetection' {} a -> s {selectionStatus = a} :: LendingDetection)

-- | The text extracted for a detected value in a lending document.
lendingDetection_text :: Lens.Lens' LendingDetection (Prelude.Maybe Prelude.Text)
lendingDetection_text = Lens.lens (\LendingDetection' {text} -> text) (\s@LendingDetection' {} a -> s {text = a} :: LendingDetection)

instance Data.FromJSON LendingDetection where
  parseJSON =
    Data.withObject
      "LendingDetection"
      ( \x ->
          LendingDetection'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Geometry")
            Prelude.<*> (x Data..:? "SelectionStatus")
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable LendingDetection where
  hashWithSalt _salt LendingDetection' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` geometry
      `Prelude.hashWithSalt` selectionStatus
      `Prelude.hashWithSalt` text

instance Prelude.NFData LendingDetection where
  rnf LendingDetection' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf geometry
      `Prelude.seq` Prelude.rnf selectionStatus
      `Prelude.seq` Prelude.rnf text
