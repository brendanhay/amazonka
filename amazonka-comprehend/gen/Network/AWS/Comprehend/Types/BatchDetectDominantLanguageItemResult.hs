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
-- Module      : Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult where

import Network.AWS.Comprehend.Types.DominantLanguage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectDominantLanguageItemResult' smart constructor.
data BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult'
  { -- | One or more DominantLanguage objects describing the dominant languages
    -- in the document.
    languages :: Prelude.Maybe [DominantLanguage],
    -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectDominantLanguageItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languages', 'batchDetectDominantLanguageItemResult_languages' - One or more DominantLanguage objects describing the dominant languages
-- in the document.
--
-- 'index', 'batchDetectDominantLanguageItemResult_index' - The zero-based index of the document in the input list.
newBatchDetectDominantLanguageItemResult ::
  BatchDetectDominantLanguageItemResult
newBatchDetectDominantLanguageItemResult =
  BatchDetectDominantLanguageItemResult'
    { languages =
        Prelude.Nothing,
      index = Prelude.Nothing
    }

-- | One or more DominantLanguage objects describing the dominant languages
-- in the document.
batchDetectDominantLanguageItemResult_languages :: Lens.Lens' BatchDetectDominantLanguageItemResult (Prelude.Maybe [DominantLanguage])
batchDetectDominantLanguageItemResult_languages = Lens.lens (\BatchDetectDominantLanguageItemResult' {languages} -> languages) (\s@BatchDetectDominantLanguageItemResult' {} a -> s {languages = a} :: BatchDetectDominantLanguageItemResult) Prelude.. Lens.mapping Prelude._Coerce

-- | The zero-based index of the document in the input list.
batchDetectDominantLanguageItemResult_index :: Lens.Lens' BatchDetectDominantLanguageItemResult (Prelude.Maybe Prelude.Int)
batchDetectDominantLanguageItemResult_index = Lens.lens (\BatchDetectDominantLanguageItemResult' {index} -> index) (\s@BatchDetectDominantLanguageItemResult' {} a -> s {index = a} :: BatchDetectDominantLanguageItemResult)

instance
  Prelude.FromJSON
    BatchDetectDominantLanguageItemResult
  where
  parseJSON =
    Prelude.withObject
      "BatchDetectDominantLanguageItemResult"
      ( \x ->
          BatchDetectDominantLanguageItemResult'
            Prelude.<$> ( x Prelude..:? "Languages"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Index")
      )

instance
  Prelude.Hashable
    BatchDetectDominantLanguageItemResult

instance
  Prelude.NFData
    BatchDetectDominantLanguageItemResult
