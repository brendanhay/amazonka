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

import Network.AWS.Comprehend.Types.KeyPhrase
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectKeyPhrasesItemResult' smart constructor.
data BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult'
  { index ::
      Lude.Maybe Lude.Int,
    keyPhrases ::
      Lude.Maybe [KeyPhrase]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectKeyPhrasesItemResult' with the minimum fields required to make a request.
--
-- * 'index' - The zero-based index of the document in the input list.
-- * 'keyPhrases' - One or more 'KeyPhrase' objects, one for each key phrase detected in the document.
mkBatchDetectKeyPhrasesItemResult ::
  BatchDetectKeyPhrasesItemResult
mkBatchDetectKeyPhrasesItemResult =
  BatchDetectKeyPhrasesItemResult'
    { index = Lude.Nothing,
      keyPhrases = Lude.Nothing
    }

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpirIndex :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Lude.Maybe Lude.Int)
bdkpirIndex = Lens.lens (index :: BatchDetectKeyPhrasesItemResult -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: BatchDetectKeyPhrasesItemResult)
{-# DEPRECATED bdkpirIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | One or more 'KeyPhrase' objects, one for each key phrase detected in the document.
--
-- /Note:/ Consider using 'keyPhrases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpirKeyPhrases :: Lens.Lens' BatchDetectKeyPhrasesItemResult (Lude.Maybe [KeyPhrase])
bdkpirKeyPhrases = Lens.lens (keyPhrases :: BatchDetectKeyPhrasesItemResult -> Lude.Maybe [KeyPhrase]) (\s a -> s {keyPhrases = a} :: BatchDetectKeyPhrasesItemResult)
{-# DEPRECATED bdkpirKeyPhrases "Use generic-lens or generic-optics with 'keyPhrases' instead." #-}

instance Lude.FromJSON BatchDetectKeyPhrasesItemResult where
  parseJSON =
    Lude.withObject
      "BatchDetectKeyPhrasesItemResult"
      ( \x ->
          BatchDetectKeyPhrasesItemResult'
            Lude.<$> (x Lude..:? "Index")
            Lude.<*> (x Lude..:? "KeyPhrases" Lude..!= Lude.mempty)
      )
