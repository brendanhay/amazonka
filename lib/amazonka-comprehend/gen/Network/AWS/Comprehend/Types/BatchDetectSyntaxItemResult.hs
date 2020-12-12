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

import Network.AWS.Comprehend.Types.SyntaxToken
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The result of calling the operation. The operation returns one object that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectSyntaxItemResult' smart constructor.
data BatchDetectSyntaxItemResult = BatchDetectSyntaxItemResult'
  { index ::
      Lude.Maybe Lude.Int,
    syntaxTokens ::
      Lude.Maybe [SyntaxToken]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectSyntaxItemResult' with the minimum fields required to make a request.
--
-- * 'index' - The zero-based index of the document in the input list.
-- * 'syntaxTokens' - The syntax tokens for the words in the document, one token for each word.
mkBatchDetectSyntaxItemResult ::
  BatchDetectSyntaxItemResult
mkBatchDetectSyntaxItemResult =
  BatchDetectSyntaxItemResult'
    { index = Lude.Nothing,
      syntaxTokens = Lude.Nothing
    }

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsirIndex :: Lens.Lens' BatchDetectSyntaxItemResult (Lude.Maybe Lude.Int)
bdsirIndex = Lens.lens (index :: BatchDetectSyntaxItemResult -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: BatchDetectSyntaxItemResult)
{-# DEPRECATED bdsirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | The syntax tokens for the words in the document, one token for each word.
--
-- /Note:/ Consider using 'syntaxTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsirSyntaxTokens :: Lens.Lens' BatchDetectSyntaxItemResult (Lude.Maybe [SyntaxToken])
bdsirSyntaxTokens = Lens.lens (syntaxTokens :: BatchDetectSyntaxItemResult -> Lude.Maybe [SyntaxToken]) (\s a -> s {syntaxTokens = a} :: BatchDetectSyntaxItemResult)
{-# DEPRECATED bdsirSyntaxTokens "Use generic-lens or generic-optics with 'syntaxTokens' instead." #-}

instance Lude.FromJSON BatchDetectSyntaxItemResult where
  parseJSON =
    Lude.withObject
      "BatchDetectSyntaxItemResult"
      ( \x ->
          BatchDetectSyntaxItemResult'
            Lude.<$> (x Lude..:? "Index")
            Lude.<*> (x Lude..:? "SyntaxTokens" Lude..!= Lude.mempty)
      )
