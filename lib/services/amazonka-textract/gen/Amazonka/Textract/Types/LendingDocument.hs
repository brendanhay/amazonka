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
-- Module      : Amazonka.Textract.Types.LendingDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.LendingDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.LendingField
import Amazonka.Textract.Types.SignatureDetection

-- | Holds the structured data returned by AnalyzeDocument for lending
-- documents.
--
-- /See:/ 'newLendingDocument' smart constructor.
data LendingDocument = LendingDocument'
  { -- | An array of LendingField objects.
    lendingFields :: Prelude.Maybe [LendingField],
    -- | A list of signatures detected in a lending document.
    signatureDetections :: Prelude.Maybe [SignatureDetection]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LendingDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lendingFields', 'lendingDocument_lendingFields' - An array of LendingField objects.
--
-- 'signatureDetections', 'lendingDocument_signatureDetections' - A list of signatures detected in a lending document.
newLendingDocument ::
  LendingDocument
newLendingDocument =
  LendingDocument'
    { lendingFields = Prelude.Nothing,
      signatureDetections = Prelude.Nothing
    }

-- | An array of LendingField objects.
lendingDocument_lendingFields :: Lens.Lens' LendingDocument (Prelude.Maybe [LendingField])
lendingDocument_lendingFields = Lens.lens (\LendingDocument' {lendingFields} -> lendingFields) (\s@LendingDocument' {} a -> s {lendingFields = a} :: LendingDocument) Prelude.. Lens.mapping Lens.coerced

-- | A list of signatures detected in a lending document.
lendingDocument_signatureDetections :: Lens.Lens' LendingDocument (Prelude.Maybe [SignatureDetection])
lendingDocument_signatureDetections = Lens.lens (\LendingDocument' {signatureDetections} -> signatureDetections) (\s@LendingDocument' {} a -> s {signatureDetections = a} :: LendingDocument) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LendingDocument where
  parseJSON =
    Data.withObject
      "LendingDocument"
      ( \x ->
          LendingDocument'
            Prelude.<$> (x Data..:? "LendingFields" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "SignatureDetections"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LendingDocument where
  hashWithSalt _salt LendingDocument' {..} =
    _salt
      `Prelude.hashWithSalt` lendingFields
      `Prelude.hashWithSalt` signatureDetections

instance Prelude.NFData LendingDocument where
  rnf LendingDocument' {..} =
    Prelude.rnf lendingFields
      `Prelude.seq` Prelude.rnf signatureDetections
