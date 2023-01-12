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
-- Module      : Amazonka.Textract.Types.LendingField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.LendingField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.LendingDetection

-- | Holds the normalized key-value pairs returned by AnalyzeDocument,
-- including the document type, detected text, and geometry.
--
-- /See:/ 'newLendingField' smart constructor.
data LendingField = LendingField'
  { keyDetection :: Prelude.Maybe LendingDetection,
    -- | The type of the lending document.
    type' :: Prelude.Maybe Prelude.Text,
    -- | An array of LendingDetection objects.
    valueDetections :: Prelude.Maybe [LendingDetection]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LendingField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyDetection', 'lendingField_keyDetection' - Undocumented member.
--
-- 'type'', 'lendingField_type' - The type of the lending document.
--
-- 'valueDetections', 'lendingField_valueDetections' - An array of LendingDetection objects.
newLendingField ::
  LendingField
newLendingField =
  LendingField'
    { keyDetection = Prelude.Nothing,
      type' = Prelude.Nothing,
      valueDetections = Prelude.Nothing
    }

-- | Undocumented member.
lendingField_keyDetection :: Lens.Lens' LendingField (Prelude.Maybe LendingDetection)
lendingField_keyDetection = Lens.lens (\LendingField' {keyDetection} -> keyDetection) (\s@LendingField' {} a -> s {keyDetection = a} :: LendingField)

-- | The type of the lending document.
lendingField_type :: Lens.Lens' LendingField (Prelude.Maybe Prelude.Text)
lendingField_type = Lens.lens (\LendingField' {type'} -> type') (\s@LendingField' {} a -> s {type' = a} :: LendingField)

-- | An array of LendingDetection objects.
lendingField_valueDetections :: Lens.Lens' LendingField (Prelude.Maybe [LendingDetection])
lendingField_valueDetections = Lens.lens (\LendingField' {valueDetections} -> valueDetections) (\s@LendingField' {} a -> s {valueDetections = a} :: LendingField) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LendingField where
  parseJSON =
    Data.withObject
      "LendingField"
      ( \x ->
          LendingField'
            Prelude.<$> (x Data..:? "KeyDetection")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> ( x Data..:? "ValueDetections"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LendingField where
  hashWithSalt _salt LendingField' {..} =
    _salt `Prelude.hashWithSalt` keyDetection
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` valueDetections

instance Prelude.NFData LendingField where
  rnf LendingField' {..} =
    Prelude.rnf keyDetection
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf valueDetections
