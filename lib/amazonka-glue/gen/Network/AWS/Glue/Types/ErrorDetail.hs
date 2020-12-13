{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ErrorDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ErrorDetail
  ( ErrorDetail (..),

    -- * Smart constructor
    mkErrorDetail,

    -- * Lenses
    edErrorCode,
    edErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an error.
--
-- /See:/ 'mkErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { -- | The code associated with this error.
    errorCode :: Lude.Maybe Lude.Text,
    -- | A message describing the error.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorDetail' with the minimum fields required to make a request.
--
-- * 'errorCode' - The code associated with this error.
-- * 'errorMessage' - A message describing the error.
mkErrorDetail ::
  ErrorDetail
mkErrorDetail =
  ErrorDetail'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The code associated with this error.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorCode :: Lens.Lens' ErrorDetail (Lude.Maybe Lude.Text)
edErrorCode = Lens.lens (errorCode :: ErrorDetail -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: ErrorDetail)
{-# DEPRECATED edErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A message describing the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorMessage :: Lens.Lens' ErrorDetail (Lude.Maybe Lude.Text)
edErrorMessage = Lens.lens (errorMessage :: ErrorDetail -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: ErrorDetail)
{-# DEPRECATED edErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON ErrorDetail where
  parseJSON =
    Lude.withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "ErrorMessage")
      )
