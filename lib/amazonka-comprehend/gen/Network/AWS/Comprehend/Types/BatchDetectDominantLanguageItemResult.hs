{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    bddlirIndex,
    bddlirLanguages,
  )
where

import qualified Network.AWS.Comprehend.Types.DominantLanguage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectDominantLanguageItemResult' smart constructor.
data BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Core.Maybe Core.Int,
    -- | One or more 'DominantLanguage' objects describing the dominant languages in the document.
    languages :: Core.Maybe [Types.DominantLanguage]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectDominantLanguageItemResult' value with any optional fields omitted.
mkBatchDetectDominantLanguageItemResult ::
  BatchDetectDominantLanguageItemResult
mkBatchDetectDominantLanguageItemResult =
  BatchDetectDominantLanguageItemResult'
    { index = Core.Nothing,
      languages = Core.Nothing
    }

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlirIndex :: Lens.Lens' BatchDetectDominantLanguageItemResult (Core.Maybe Core.Int)
bddlirIndex = Lens.field @"index"
{-# DEPRECATED bddlirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | One or more 'DominantLanguage' objects describing the dominant languages in the document.
--
-- /Note:/ Consider using 'languages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlirLanguages :: Lens.Lens' BatchDetectDominantLanguageItemResult (Core.Maybe [Types.DominantLanguage])
bddlirLanguages = Lens.field @"languages"
{-# DEPRECATED bddlirLanguages "Use generic-lens or generic-optics with 'languages' instead." #-}

instance Core.FromJSON BatchDetectDominantLanguageItemResult where
  parseJSON =
    Core.withObject "BatchDetectDominantLanguageItemResult" Core.$
      \x ->
        BatchDetectDominantLanguageItemResult'
          Core.<$> (x Core..:? "Index") Core.<*> (x Core..:? "Languages")
