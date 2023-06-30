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
-- Module      : Amazonka.Comprehend.Types.BatchDetectDominantLanguageItemResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BatchDetectDominantLanguageItemResult where

import Amazonka.Comprehend.Types.DominantLanguage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectDominantLanguageItemResult' smart constructor.
data BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int,
    -- | One or more DominantLanguage objects describing the dominant languages
    -- in the document.
    languages :: Prelude.Maybe [DominantLanguage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectDominantLanguageItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'batchDetectDominantLanguageItemResult_index' - The zero-based index of the document in the input list.
--
-- 'languages', 'batchDetectDominantLanguageItemResult_languages' - One or more DominantLanguage objects describing the dominant languages
-- in the document.
newBatchDetectDominantLanguageItemResult ::
  BatchDetectDominantLanguageItemResult
newBatchDetectDominantLanguageItemResult =
  BatchDetectDominantLanguageItemResult'
    { index =
        Prelude.Nothing,
      languages = Prelude.Nothing
    }

-- | The zero-based index of the document in the input list.
batchDetectDominantLanguageItemResult_index :: Lens.Lens' BatchDetectDominantLanguageItemResult (Prelude.Maybe Prelude.Int)
batchDetectDominantLanguageItemResult_index = Lens.lens (\BatchDetectDominantLanguageItemResult' {index} -> index) (\s@BatchDetectDominantLanguageItemResult' {} a -> s {index = a} :: BatchDetectDominantLanguageItemResult)

-- | One or more DominantLanguage objects describing the dominant languages
-- in the document.
batchDetectDominantLanguageItemResult_languages :: Lens.Lens' BatchDetectDominantLanguageItemResult (Prelude.Maybe [DominantLanguage])
batchDetectDominantLanguageItemResult_languages = Lens.lens (\BatchDetectDominantLanguageItemResult' {languages} -> languages) (\s@BatchDetectDominantLanguageItemResult' {} a -> s {languages = a} :: BatchDetectDominantLanguageItemResult) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    BatchDetectDominantLanguageItemResult
  where
  parseJSON =
    Data.withObject
      "BatchDetectDominantLanguageItemResult"
      ( \x ->
          BatchDetectDominantLanguageItemResult'
            Prelude.<$> (x Data..:? "Index")
            Prelude.<*> (x Data..:? "Languages" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchDetectDominantLanguageItemResult
  where
  hashWithSalt
    _salt
    BatchDetectDominantLanguageItemResult' {..} =
      _salt
        `Prelude.hashWithSalt` index
        `Prelude.hashWithSalt` languages

instance
  Prelude.NFData
    BatchDetectDominantLanguageItemResult
  where
  rnf BatchDetectDominantLanguageItemResult' {..} =
    Prelude.rnf index
      `Prelude.seq` Prelude.rnf languages
