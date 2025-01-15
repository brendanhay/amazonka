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
-- Module      : Amazonka.Comprehend.Types.BatchDetectSyntaxItemResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BatchDetectSyntaxItemResult where

import Amazonka.Comprehend.Types.SyntaxToken
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectSyntaxItemResult' smart constructor.
data BatchDetectSyntaxItemResult = BatchDetectSyntaxItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int,
    -- | The syntax tokens for the words in the document, one token for each
    -- word.
    syntaxTokens :: Prelude.Maybe [SyntaxToken]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectSyntaxItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'batchDetectSyntaxItemResult_index' - The zero-based index of the document in the input list.
--
-- 'syntaxTokens', 'batchDetectSyntaxItemResult_syntaxTokens' - The syntax tokens for the words in the document, one token for each
-- word.
newBatchDetectSyntaxItemResult ::
  BatchDetectSyntaxItemResult
newBatchDetectSyntaxItemResult =
  BatchDetectSyntaxItemResult'
    { index =
        Prelude.Nothing,
      syntaxTokens = Prelude.Nothing
    }

-- | The zero-based index of the document in the input list.
batchDetectSyntaxItemResult_index :: Lens.Lens' BatchDetectSyntaxItemResult (Prelude.Maybe Prelude.Int)
batchDetectSyntaxItemResult_index = Lens.lens (\BatchDetectSyntaxItemResult' {index} -> index) (\s@BatchDetectSyntaxItemResult' {} a -> s {index = a} :: BatchDetectSyntaxItemResult)

-- | The syntax tokens for the words in the document, one token for each
-- word.
batchDetectSyntaxItemResult_syntaxTokens :: Lens.Lens' BatchDetectSyntaxItemResult (Prelude.Maybe [SyntaxToken])
batchDetectSyntaxItemResult_syntaxTokens = Lens.lens (\BatchDetectSyntaxItemResult' {syntaxTokens} -> syntaxTokens) (\s@BatchDetectSyntaxItemResult' {} a -> s {syntaxTokens = a} :: BatchDetectSyntaxItemResult) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BatchDetectSyntaxItemResult where
  parseJSON =
    Data.withObject
      "BatchDetectSyntaxItemResult"
      ( \x ->
          BatchDetectSyntaxItemResult'
            Prelude.<$> (x Data..:? "Index")
            Prelude.<*> (x Data..:? "SyntaxTokens" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BatchDetectSyntaxItemResult where
  hashWithSalt _salt BatchDetectSyntaxItemResult' {..} =
    _salt
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` syntaxTokens

instance Prelude.NFData BatchDetectSyntaxItemResult where
  rnf BatchDetectSyntaxItemResult' {..} =
    Prelude.rnf index `Prelude.seq`
      Prelude.rnf syntaxTokens
