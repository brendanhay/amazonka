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
-- Module      : Amazonka.Textract.Types.Extraction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Extraction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.ExpenseDocument
import Amazonka.Textract.Types.IdentityDocument
import Amazonka.Textract.Types.LendingDocument

-- | Contains information extracted by an analysis operation after using
-- StartLendingAnalysis.
--
-- /See:/ 'newExtraction' smart constructor.
data Extraction = Extraction'
  { expenseDocument :: Prelude.Maybe ExpenseDocument,
    identityDocument :: Prelude.Maybe IdentityDocument,
    -- | Holds the structured data returned by AnalyzeDocument for lending
    -- documents.
    lendingDocument :: Prelude.Maybe LendingDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Extraction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expenseDocument', 'extraction_expenseDocument' - Undocumented member.
--
-- 'identityDocument', 'extraction_identityDocument' - Undocumented member.
--
-- 'lendingDocument', 'extraction_lendingDocument' - Holds the structured data returned by AnalyzeDocument for lending
-- documents.
newExtraction ::
  Extraction
newExtraction =
  Extraction'
    { expenseDocument = Prelude.Nothing,
      identityDocument = Prelude.Nothing,
      lendingDocument = Prelude.Nothing
    }

-- | Undocumented member.
extraction_expenseDocument :: Lens.Lens' Extraction (Prelude.Maybe ExpenseDocument)
extraction_expenseDocument = Lens.lens (\Extraction' {expenseDocument} -> expenseDocument) (\s@Extraction' {} a -> s {expenseDocument = a} :: Extraction)

-- | Undocumented member.
extraction_identityDocument :: Lens.Lens' Extraction (Prelude.Maybe IdentityDocument)
extraction_identityDocument = Lens.lens (\Extraction' {identityDocument} -> identityDocument) (\s@Extraction' {} a -> s {identityDocument = a} :: Extraction)

-- | Holds the structured data returned by AnalyzeDocument for lending
-- documents.
extraction_lendingDocument :: Lens.Lens' Extraction (Prelude.Maybe LendingDocument)
extraction_lendingDocument = Lens.lens (\Extraction' {lendingDocument} -> lendingDocument) (\s@Extraction' {} a -> s {lendingDocument = a} :: Extraction)

instance Data.FromJSON Extraction where
  parseJSON =
    Data.withObject
      "Extraction"
      ( \x ->
          Extraction'
            Prelude.<$> (x Data..:? "ExpenseDocument")
            Prelude.<*> (x Data..:? "IdentityDocument")
            Prelude.<*> (x Data..:? "LendingDocument")
      )

instance Prelude.Hashable Extraction where
  hashWithSalt _salt Extraction' {..} =
    _salt
      `Prelude.hashWithSalt` expenseDocument
      `Prelude.hashWithSalt` identityDocument
      `Prelude.hashWithSalt` lendingDocument

instance Prelude.NFData Extraction where
  rnf Extraction' {..} =
    Prelude.rnf expenseDocument
      `Prelude.seq` Prelude.rnf identityDocument
      `Prelude.seq` Prelude.rnf lendingDocument
