-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
  ( BatchReadOperationResponse (..),

    -- * Smart constructor
    mkBatchReadOperationResponse,

    -- * Lenses
    broExceptionResponse,
    broSuccessfulResponse,
  )
where

import Network.AWS.CloudDirectory.Types.BatchReadException
import Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @BatchRead@ response operation.
--
-- /See:/ 'mkBatchReadOperationResponse' smart constructor.
data BatchReadOperationResponse = BatchReadOperationResponse'
  { exceptionResponse ::
      Lude.Maybe BatchReadException,
    successfulResponse ::
      Lude.Maybe
        BatchReadSuccessfulResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchReadOperationResponse' with the minimum fields required to make a request.
--
-- * 'exceptionResponse' - Identifies which operation in a batch has failed.
-- * 'successfulResponse' - Identifies which operation in a batch has succeeded.
mkBatchReadOperationResponse ::
  BatchReadOperationResponse
mkBatchReadOperationResponse =
  BatchReadOperationResponse'
    { exceptionResponse = Lude.Nothing,
      successfulResponse = Lude.Nothing
    }

-- | Identifies which operation in a batch has failed.
--
-- /Note:/ Consider using 'exceptionResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broExceptionResponse :: Lens.Lens' BatchReadOperationResponse (Lude.Maybe BatchReadException)
broExceptionResponse = Lens.lens (exceptionResponse :: BatchReadOperationResponse -> Lude.Maybe BatchReadException) (\s a -> s {exceptionResponse = a} :: BatchReadOperationResponse)
{-# DEPRECATED broExceptionResponse "Use generic-lens or generic-optics with 'exceptionResponse' instead." #-}

-- | Identifies which operation in a batch has succeeded.
--
-- /Note:/ Consider using 'successfulResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
broSuccessfulResponse :: Lens.Lens' BatchReadOperationResponse (Lude.Maybe BatchReadSuccessfulResponse)
broSuccessfulResponse = Lens.lens (successfulResponse :: BatchReadOperationResponse -> Lude.Maybe BatchReadSuccessfulResponse) (\s a -> s {successfulResponse = a} :: BatchReadOperationResponse)
{-# DEPRECATED broSuccessfulResponse "Use generic-lens or generic-optics with 'successfulResponse' instead." #-}

instance Lude.FromJSON BatchReadOperationResponse where
  parseJSON =
    Lude.withObject
      "BatchReadOperationResponse"
      ( \x ->
          BatchReadOperationResponse'
            Lude.<$> (x Lude..:? "ExceptionResponse")
            Lude.<*> (x Lude..:? "SuccessfulResponse")
      )
