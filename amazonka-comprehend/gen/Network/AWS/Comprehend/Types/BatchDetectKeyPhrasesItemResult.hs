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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectKeyPhrasesItemResult' smart constructor.
data BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult'
  { -- | One or more KeyPhrase objects, one for each key phrase detected in the
    -- document.
    keyPhrases :: Core.Maybe [KeyPhrase],
    -- | The zero-based index of the document in the input list.
    index :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      index = Core.Nothing
    }

-- | One or more KeyPhrase objects, one for each key phrase detected in the
-- document.
batchDetectKeyPhrasesItemResult_keyPhrases :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Core.Maybe [KeyPhrase])
batchDetectKeyPhrasesItemResult_keyPhrases = Lens.lens (\BatchDetectKeyPhrasesItemResult' {keyPhrases} -> keyPhrases) (\s@BatchDetectKeyPhrasesItemResult' {} a -> s {keyPhrases = a} :: BatchDetectKeyPhrasesItemResult) Core.. Lens.mapping Lens._Coerce

-- | The zero-based index of the document in the input list.
batchDetectKeyPhrasesItemResult_index :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Core.Maybe Core.Int)
batchDetectKeyPhrasesItemResult_index = Lens.lens (\BatchDetectKeyPhrasesItemResult' {index} -> index) (\s@BatchDetectKeyPhrasesItemResult' {} a -> s {index = a} :: BatchDetectKeyPhrasesItemResult)

instance
  Core.FromJSON
    BatchDetectKeyPhrasesItemResult
  where
  parseJSON =
    Core.withObject
      "BatchDetectKeyPhrasesItemResult"
      ( \x ->
          BatchDetectKeyPhrasesItemResult'
            Core.<$> (x Core..:? "KeyPhrases" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Index")
      )

instance
  Core.Hashable
    BatchDetectKeyPhrasesItemResult

instance Core.NFData BatchDetectKeyPhrasesItemResult
