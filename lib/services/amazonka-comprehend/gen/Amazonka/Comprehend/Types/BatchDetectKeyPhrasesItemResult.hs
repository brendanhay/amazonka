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
-- Module      : Amazonka.Comprehend.Types.BatchDetectKeyPhrasesItemResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BatchDetectKeyPhrasesItemResult where

import Amazonka.Comprehend.Types.KeyPhrase
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectKeyPhrasesItemResult' smart constructor.
data BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int,
    -- | One or more KeyPhrase objects, one for each key phrase detected in the
    -- document.
    keyPhrases :: Prelude.Maybe [KeyPhrase]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectKeyPhrasesItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'batchDetectKeyPhrasesItemResult_index' - The zero-based index of the document in the input list.
--
-- 'keyPhrases', 'batchDetectKeyPhrasesItemResult_keyPhrases' - One or more KeyPhrase objects, one for each key phrase detected in the
-- document.
newBatchDetectKeyPhrasesItemResult ::
  BatchDetectKeyPhrasesItemResult
newBatchDetectKeyPhrasesItemResult =
  BatchDetectKeyPhrasesItemResult'
    { index =
        Prelude.Nothing,
      keyPhrases = Prelude.Nothing
    }

-- | The zero-based index of the document in the input list.
batchDetectKeyPhrasesItemResult_index :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Prelude.Maybe Prelude.Int)
batchDetectKeyPhrasesItemResult_index = Lens.lens (\BatchDetectKeyPhrasesItemResult' {index} -> index) (\s@BatchDetectKeyPhrasesItemResult' {} a -> s {index = a} :: BatchDetectKeyPhrasesItemResult)

-- | One or more KeyPhrase objects, one for each key phrase detected in the
-- document.
batchDetectKeyPhrasesItemResult_keyPhrases :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Prelude.Maybe [KeyPhrase])
batchDetectKeyPhrasesItemResult_keyPhrases = Lens.lens (\BatchDetectKeyPhrasesItemResult' {keyPhrases} -> keyPhrases) (\s@BatchDetectKeyPhrasesItemResult' {} a -> s {keyPhrases = a} :: BatchDetectKeyPhrasesItemResult) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    BatchDetectKeyPhrasesItemResult
  where
  parseJSON =
    Data.withObject
      "BatchDetectKeyPhrasesItemResult"
      ( \x ->
          BatchDetectKeyPhrasesItemResult'
            Prelude.<$> (x Data..:? "Index")
            Prelude.<*> (x Data..:? "KeyPhrases" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchDetectKeyPhrasesItemResult
  where
  hashWithSalt
    _salt
    BatchDetectKeyPhrasesItemResult' {..} =
      _salt
        `Prelude.hashWithSalt` index
        `Prelude.hashWithSalt` keyPhrases

instance
  Prelude.NFData
    BatchDetectKeyPhrasesItemResult
  where
  rnf BatchDetectKeyPhrasesItemResult' {..} =
    Prelude.rnf index `Prelude.seq`
      Prelude.rnf keyPhrases
