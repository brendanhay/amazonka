{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementError
  ( BatchStatementError (..),

    -- * Smart constructor
    mkBatchStatementError,

    -- * Lenses
    bseCode,
    bseMessage,
  )
where

import Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An error associated with a statement in a PartiQL batch that was run.
--
-- /See:/ 'mkBatchStatementError' smart constructor.
data BatchStatementError = BatchStatementError'
  { code ::
      Lude.Maybe BatchStatementErrorCodeEnum,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchStatementError' with the minimum fields required to make a request.
--
-- * 'code' - The error code associated with the failed PartiQL batch statement.
-- * 'message' - The error message associated with the PartiQL batch resposne.
mkBatchStatementError ::
  BatchStatementError
mkBatchStatementError =
  BatchStatementError' {code = Lude.Nothing, message = Lude.Nothing}

-- | The error code associated with the failed PartiQL batch statement.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bseCode :: Lens.Lens' BatchStatementError (Lude.Maybe BatchStatementErrorCodeEnum)
bseCode = Lens.lens (code :: BatchStatementError -> Lude.Maybe BatchStatementErrorCodeEnum) (\s a -> s {code = a} :: BatchStatementError)
{-# DEPRECATED bseCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message associated with the PartiQL batch resposne.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bseMessage :: Lens.Lens' BatchStatementError (Lude.Maybe Lude.Text)
bseMessage = Lens.lens (message :: BatchStatementError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: BatchStatementError)
{-# DEPRECATED bseMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON BatchStatementError where
  parseJSON =
    Lude.withObject
      "BatchStatementError"
      ( \x ->
          BatchStatementError'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
