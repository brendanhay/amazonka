{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchItemError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchItemError
  ( BatchItemError (..),

    -- * Smart constructor
    mkBatchItemError,

    -- * Lenses
    bieErrorCode,
    bieErrorMessage,
    bieIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an error that occurred while processing a document in a batch. The operation returns on @BatchItemError@ object for each document that contained an error.
--
-- /See:/ 'mkBatchItemError' smart constructor.
data BatchItemError = BatchItemError'
  { -- | The numeric error code of the error.
    errorCode :: Lude.Maybe Lude.Text,
    -- | A text description of the error.
    errorMessage :: Lude.Maybe Lude.Text,
    -- | The zero-based index of the document in the input list.
    index :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchItemError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The numeric error code of the error.
-- * 'errorMessage' - A text description of the error.
-- * 'index' - The zero-based index of the document in the input list.
mkBatchItemError ::
  BatchItemError
mkBatchItemError =
  BatchItemError'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      index = Lude.Nothing
    }

-- | The numeric error code of the error.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bieErrorCode :: Lens.Lens' BatchItemError (Lude.Maybe Lude.Text)
bieErrorCode = Lens.lens (errorCode :: BatchItemError -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: BatchItemError)
{-# DEPRECATED bieErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A text description of the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bieErrorMessage :: Lens.Lens' BatchItemError (Lude.Maybe Lude.Text)
bieErrorMessage = Lens.lens (errorMessage :: BatchItemError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BatchItemError)
{-# DEPRECATED bieErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bieIndex :: Lens.Lens' BatchItemError (Lude.Maybe Lude.Int)
bieIndex = Lens.lens (index :: BatchItemError -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: BatchItemError)
{-# DEPRECATED bieIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON BatchItemError where
  parseJSON =
    Lude.withObject
      "BatchItemError"
      ( \x ->
          BatchItemError'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "Index")
      )
