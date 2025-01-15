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
-- Module      : Amazonka.Textract.Types.LendingResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.LendingResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Extraction
import Amazonka.Textract.Types.PageClassification

-- | Contains the detections for each page analyzed through the Analyze
-- Lending API.
--
-- /See:/ 'newLendingResult' smart constructor.
data LendingResult = LendingResult'
  { -- | An array of Extraction to hold structured data. e.g. normalized key
    -- value pairs instead of raw OCR detections .
    extractions :: Prelude.Maybe [Extraction],
    -- | The page number for a page, with regard to whole submission.
    page :: Prelude.Maybe Prelude.Natural,
    -- | The classifier result for a given page.
    pageClassification :: Prelude.Maybe PageClassification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LendingResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extractions', 'lendingResult_extractions' - An array of Extraction to hold structured data. e.g. normalized key
-- value pairs instead of raw OCR detections .
--
-- 'page', 'lendingResult_page' - The page number for a page, with regard to whole submission.
--
-- 'pageClassification', 'lendingResult_pageClassification' - The classifier result for a given page.
newLendingResult ::
  LendingResult
newLendingResult =
  LendingResult'
    { extractions = Prelude.Nothing,
      page = Prelude.Nothing,
      pageClassification = Prelude.Nothing
    }

-- | An array of Extraction to hold structured data. e.g. normalized key
-- value pairs instead of raw OCR detections .
lendingResult_extractions :: Lens.Lens' LendingResult (Prelude.Maybe [Extraction])
lendingResult_extractions = Lens.lens (\LendingResult' {extractions} -> extractions) (\s@LendingResult' {} a -> s {extractions = a} :: LendingResult) Prelude.. Lens.mapping Lens.coerced

-- | The page number for a page, with regard to whole submission.
lendingResult_page :: Lens.Lens' LendingResult (Prelude.Maybe Prelude.Natural)
lendingResult_page = Lens.lens (\LendingResult' {page} -> page) (\s@LendingResult' {} a -> s {page = a} :: LendingResult)

-- | The classifier result for a given page.
lendingResult_pageClassification :: Lens.Lens' LendingResult (Prelude.Maybe PageClassification)
lendingResult_pageClassification = Lens.lens (\LendingResult' {pageClassification} -> pageClassification) (\s@LendingResult' {} a -> s {pageClassification = a} :: LendingResult)

instance Data.FromJSON LendingResult where
  parseJSON =
    Data.withObject
      "LendingResult"
      ( \x ->
          LendingResult'
            Prelude.<$> (x Data..:? "Extractions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Page")
            Prelude.<*> (x Data..:? "PageClassification")
      )

instance Prelude.Hashable LendingResult where
  hashWithSalt _salt LendingResult' {..} =
    _salt
      `Prelude.hashWithSalt` extractions
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` pageClassification

instance Prelude.NFData LendingResult where
  rnf LendingResult' {..} =
    Prelude.rnf extractions `Prelude.seq`
      Prelude.rnf page `Prelude.seq`
        Prelude.rnf pageClassification
