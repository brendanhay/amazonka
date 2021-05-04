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
-- Module      : Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult where

import Network.AWS.Comprehend.Types.KeyPhrase
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectKeyPhrasesItemResult' smart constructor.
data BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult'
  { -- | One or more KeyPhrase objects, one for each key phrase detected in the
    -- document.
    keyPhrases :: Prelude.Maybe [KeyPhrase],
    -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectKeyPhrasesItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPhrases', 'batchDetectKeyPhrasesItemResult_keyPhrases' - One or more KeyPhrase objects, one for each key phrase detected in the
-- document.
--
-- 'index', 'batchDetectKeyPhrasesItemResult_index' - The zero-based index of the document in the input list.
newBatchDetectKeyPhrasesItemResult ::
  BatchDetectKeyPhrasesItemResult
newBatchDetectKeyPhrasesItemResult =
  BatchDetectKeyPhrasesItemResult'
    { keyPhrases =
        Prelude.Nothing,
      index = Prelude.Nothing
    }

-- | One or more KeyPhrase objects, one for each key phrase detected in the
-- document.
batchDetectKeyPhrasesItemResult_keyPhrases :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Prelude.Maybe [KeyPhrase])
batchDetectKeyPhrasesItemResult_keyPhrases = Lens.lens (\BatchDetectKeyPhrasesItemResult' {keyPhrases} -> keyPhrases) (\s@BatchDetectKeyPhrasesItemResult' {} a -> s {keyPhrases = a} :: BatchDetectKeyPhrasesItemResult) Prelude.. Lens.mapping Prelude._Coerce

-- | The zero-based index of the document in the input list.
batchDetectKeyPhrasesItemResult_index :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Prelude.Maybe Prelude.Int)
batchDetectKeyPhrasesItemResult_index = Lens.lens (\BatchDetectKeyPhrasesItemResult' {index} -> index) (\s@BatchDetectKeyPhrasesItemResult' {} a -> s {index = a} :: BatchDetectKeyPhrasesItemResult)

instance
  Prelude.FromJSON
    BatchDetectKeyPhrasesItemResult
  where
  parseJSON =
    Prelude.withObject
      "BatchDetectKeyPhrasesItemResult"
      ( \x ->
          BatchDetectKeyPhrasesItemResult'
            Prelude.<$> ( x Prelude..:? "KeyPhrases"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Index")
      )

instance
  Prelude.Hashable
    BatchDetectKeyPhrasesItemResult

instance
  Prelude.NFData
    BatchDetectKeyPhrasesItemResult
