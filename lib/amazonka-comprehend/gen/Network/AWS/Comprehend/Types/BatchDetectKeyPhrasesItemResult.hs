{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult
  ( BatchDetectKeyPhrasesItemResult (..),

    -- * Smart constructor
    mkBatchDetectKeyPhrasesItemResult,

    -- * Lenses
    bdkpirIndex,
    bdkpirKeyPhrases,
  )
where

import qualified Network.AWS.Comprehend.Types.KeyPhrase as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectKeyPhrasesItemResult' smart constructor.
data BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Core.Maybe Core.Int,
    -- | One or more 'KeyPhrase' objects, one for each key phrase detected in the document.
    keyPhrases :: Core.Maybe [Types.KeyPhrase]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectKeyPhrasesItemResult' value with any optional fields omitted.
mkBatchDetectKeyPhrasesItemResult ::
  BatchDetectKeyPhrasesItemResult
mkBatchDetectKeyPhrasesItemResult =
  BatchDetectKeyPhrasesItemResult'
    { index = Core.Nothing,
      keyPhrases = Core.Nothing
    }

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpirIndex :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Core.Maybe Core.Int)
bdkpirIndex = Lens.field @"index"
{-# DEPRECATED bdkpirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | One or more 'KeyPhrase' objects, one for each key phrase detected in the document.
--
-- /Note:/ Consider using 'keyPhrases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpirKeyPhrases :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Core.Maybe [Types.KeyPhrase])
bdkpirKeyPhrases = Lens.field @"keyPhrases"
{-# DEPRECATED bdkpirKeyPhrases "Use generic-lens or generic-optics with 'keyPhrases' instead." #-}

instance Core.FromJSON BatchDetectKeyPhrasesItemResult where
  parseJSON =
    Core.withObject "BatchDetectKeyPhrasesItemResult" Core.$
      \x ->
        BatchDetectKeyPhrasesItemResult'
          Core.<$> (x Core..:? "Index") Core.<*> (x Core..:? "KeyPhrases")
