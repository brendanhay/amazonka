{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult
  ( BatchDetectSyntaxItemResult (..),

    -- * Smart constructor
    mkBatchDetectSyntaxItemResult,

    -- * Lenses
    bdsirIndex,
    bdsirSyntaxTokens,
  )
where

import qualified Network.AWS.Comprehend.Types.SyntaxToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result of calling the operation. The operation returns one object that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectSyntaxItemResult' smart constructor.
data BatchDetectSyntaxItemResult = BatchDetectSyntaxItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Core.Maybe Core.Int,
    -- | The syntax tokens for the words in the document, one token for each word.
    syntaxTokens :: Core.Maybe [Types.SyntaxToken]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectSyntaxItemResult' value with any optional fields omitted.
mkBatchDetectSyntaxItemResult ::
  BatchDetectSyntaxItemResult
mkBatchDetectSyntaxItemResult =
  BatchDetectSyntaxItemResult'
    { index = Core.Nothing,
      syntaxTokens = Core.Nothing
    }

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsirIndex :: Lens.Lens' BatchDetectSyntaxItemResult (Core.Maybe Core.Int)
bdsirIndex = Lens.field @"index"
{-# DEPRECATED bdsirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | The syntax tokens for the words in the document, one token for each word.
--
-- /Note:/ Consider using 'syntaxTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsirSyntaxTokens :: Lens.Lens' BatchDetectSyntaxItemResult (Core.Maybe [Types.SyntaxToken])
bdsirSyntaxTokens = Lens.field @"syntaxTokens"
{-# DEPRECATED bdsirSyntaxTokens "Use generic-lens or generic-optics with 'syntaxTokens' instead." #-}

instance Core.FromJSON BatchDetectSyntaxItemResult where
  parseJSON =
    Core.withObject "BatchDetectSyntaxItemResult" Core.$
      \x ->
        BatchDetectSyntaxItemResult'
          Core.<$> (x Core..:? "Index") Core.<*> (x Core..:? "SyntaxTokens")
