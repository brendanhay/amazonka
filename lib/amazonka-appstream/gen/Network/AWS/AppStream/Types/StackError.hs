-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StackError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackError
  ( StackError (..),

    -- * Smart constructor
    mkStackError,

    -- * Lenses
    seErrorCode,
    seErrorMessage,
  )
where

import Network.AWS.AppStream.Types.StackErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a stack error.
--
-- /See:/ 'mkStackError' smart constructor.
data StackError = StackError'
  { errorCode ::
      Lude.Maybe StackErrorCode,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code.
-- * 'errorMessage' - The error message.
mkStackError ::
  StackError
mkStackError =
  StackError'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seErrorCode :: Lens.Lens' StackError (Lude.Maybe StackErrorCode)
seErrorCode = Lens.lens (errorCode :: StackError -> Lude.Maybe StackErrorCode) (\s a -> s {errorCode = a} :: StackError)
{-# DEPRECATED seErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seErrorMessage :: Lens.Lens' StackError (Lude.Maybe Lude.Text)
seErrorMessage = Lens.lens (errorMessage :: StackError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: StackError)
{-# DEPRECATED seErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON StackError where
  parseJSON =
    Lude.withObject
      "StackError"
      ( \x ->
          StackError'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "ErrorMessage")
      )
