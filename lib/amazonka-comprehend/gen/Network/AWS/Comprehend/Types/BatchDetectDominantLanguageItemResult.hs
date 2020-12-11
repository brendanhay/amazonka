-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult
  ( BatchDetectDominantLanguageItemResult (..),

    -- * Smart constructor
    mkBatchDetectDominantLanguageItemResult,

    -- * Lenses
    bddlirLanguages,
    bddlirIndex,
  )
where

import Network.AWS.Comprehend.Types.DominantLanguage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectDominantLanguageItemResult' smart constructor.
data BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult'
  { languages ::
      Lude.Maybe
        [DominantLanguage],
    index ::
      Lude.Maybe
        Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectDominantLanguageItemResult' with the minimum fields required to make a request.
--
-- * 'index' - The zero-based index of the document in the input list.
-- * 'languages' - One or more 'DominantLanguage' objects describing the dominant languages in the document.
mkBatchDetectDominantLanguageItemResult ::
  BatchDetectDominantLanguageItemResult
mkBatchDetectDominantLanguageItemResult =
  BatchDetectDominantLanguageItemResult'
    { languages = Lude.Nothing,
      index = Lude.Nothing
    }

-- | One or more 'DominantLanguage' objects describing the dominant languages in the document.
--
-- /Note:/ Consider using 'languages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlirLanguages :: Lens.Lens' BatchDetectDominantLanguageItemResult (Lude.Maybe [DominantLanguage])
bddlirLanguages = Lens.lens (languages :: BatchDetectDominantLanguageItemResult -> Lude.Maybe [DominantLanguage]) (\s a -> s {languages = a} :: BatchDetectDominantLanguageItemResult)
{-# DEPRECATED bddlirLanguages "Use generic-lens or generic-optics with 'languages' instead." #-}

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlirIndex :: Lens.Lens' BatchDetectDominantLanguageItemResult (Lude.Maybe Lude.Int)
bddlirIndex = Lens.lens (index :: BatchDetectDominantLanguageItemResult -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: BatchDetectDominantLanguageItemResult)
{-# DEPRECATED bddlirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON BatchDetectDominantLanguageItemResult where
  parseJSON =
    Lude.withObject
      "BatchDetectDominantLanguageItemResult"
      ( \x ->
          BatchDetectDominantLanguageItemResult'
            Lude.<$> (x Lude..:? "Languages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Index")
      )
