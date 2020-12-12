{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ErrorDetails
  ( ErrorDetails (..),

    -- * Smart constructor
    mkErrorDetails,

    -- * Lenses
    eErrorCode,
    eErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object containing error details.
--
-- /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { errorCode ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ErrorDetails' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code for an error.
-- * 'errorMessage' - The error message for an error.
mkErrorDetails ::
  ErrorDetails
mkErrorDetails =
  ErrorDetails'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The error code for an error.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eErrorCode :: Lens.Lens' ErrorDetails (Lude.Maybe Lude.Text)
eErrorCode = Lens.lens (errorCode :: ErrorDetails -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: ErrorDetails)
{-# DEPRECATED eErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for an error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eErrorMessage :: Lens.Lens' ErrorDetails (Lude.Maybe Lude.Text)
eErrorMessage = Lens.lens (errorMessage :: ErrorDetails -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: ErrorDetails)
{-# DEPRECATED eErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON ErrorDetails where
  parseJSON =
    Lude.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "ErrorMessage")
      )
