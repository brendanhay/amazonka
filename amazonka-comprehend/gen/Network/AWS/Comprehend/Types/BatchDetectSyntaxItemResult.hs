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
-- Module      : Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult where

import Network.AWS.Comprehend.Types.SyntaxToken
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The result of calling the operation. The operation returns one object
-- that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectSyntaxItemResult' smart constructor.
data BatchDetectSyntaxItemResult = BatchDetectSyntaxItemResult'
  { -- | The syntax tokens for the words in the document, one token for each
    -- word.
    syntaxTokens :: Core.Maybe [SyntaxToken],
    -- | The zero-based index of the document in the input list.
    index :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectSyntaxItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syntaxTokens', 'batchDetectSyntaxItemResult_syntaxTokens' - The syntax tokens for the words in the document, one token for each
-- word.
--
-- 'index', 'batchDetectSyntaxItemResult_index' - The zero-based index of the document in the input list.
newBatchDetectSyntaxItemResult ::
  BatchDetectSyntaxItemResult
newBatchDetectSyntaxItemResult =
  BatchDetectSyntaxItemResult'
    { syntaxTokens =
        Core.Nothing,
      index = Core.Nothing
    }

-- | The syntax tokens for the words in the document, one token for each
-- word.
batchDetectSyntaxItemResult_syntaxTokens :: Lens.Lens' BatchDetectSyntaxItemResult (Core.Maybe [SyntaxToken])
batchDetectSyntaxItemResult_syntaxTokens = Lens.lens (\BatchDetectSyntaxItemResult' {syntaxTokens} -> syntaxTokens) (\s@BatchDetectSyntaxItemResult' {} a -> s {syntaxTokens = a} :: BatchDetectSyntaxItemResult) Core.. Lens.mapping Lens._Coerce

-- | The zero-based index of the document in the input list.
batchDetectSyntaxItemResult_index :: Lens.Lens' BatchDetectSyntaxItemResult (Core.Maybe Core.Int)
batchDetectSyntaxItemResult_index = Lens.lens (\BatchDetectSyntaxItemResult' {index} -> index) (\s@BatchDetectSyntaxItemResult' {} a -> s {index = a} :: BatchDetectSyntaxItemResult)

instance Core.FromJSON BatchDetectSyntaxItemResult where
  parseJSON =
    Core.withObject
      "BatchDetectSyntaxItemResult"
      ( \x ->
          BatchDetectSyntaxItemResult'
            Core.<$> (x Core..:? "SyntaxTokens" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Index")
      )

instance Core.Hashable BatchDetectSyntaxItemResult

instance Core.NFData BatchDetectSyntaxItemResult
