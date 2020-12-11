-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ErrorDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ErrorDetail
  ( ErrorDetail (..),

    -- * Smart constructor
    mkErrorDetail,

    -- * Lenses
    edDetailedErrorCode,
    edDetailedErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the error.
--
-- /See:/ 'mkErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { detailedErrorCode ::
      Lude.Maybe Lude.Text,
    detailedErrorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorDetail' with the minimum fields required to make a request.
--
-- * 'detailedErrorCode' - A detailed error code.
-- * 'detailedErrorMessage' - A detailed error message.
mkErrorDetail ::
  ErrorDetail
mkErrorDetail =
  ErrorDetail'
    { detailedErrorCode = Lude.Nothing,
      detailedErrorMessage = Lude.Nothing
    }

-- | A detailed error code.
--
-- /Note:/ Consider using 'detailedErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDetailedErrorCode :: Lens.Lens' ErrorDetail (Lude.Maybe Lude.Text)
edDetailedErrorCode = Lens.lens (detailedErrorCode :: ErrorDetail -> Lude.Maybe Lude.Text) (\s a -> s {detailedErrorCode = a} :: ErrorDetail)
{-# DEPRECATED edDetailedErrorCode "Use generic-lens or generic-optics with 'detailedErrorCode' instead." #-}

-- | A detailed error message.
--
-- /Note:/ Consider using 'detailedErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDetailedErrorMessage :: Lens.Lens' ErrorDetail (Lude.Maybe Lude.Text)
edDetailedErrorMessage = Lens.lens (detailedErrorMessage :: ErrorDetail -> Lude.Maybe Lude.Text) (\s a -> s {detailedErrorMessage = a} :: ErrorDetail)
{-# DEPRECATED edDetailedErrorMessage "Use generic-lens or generic-optics with 'detailedErrorMessage' instead." #-}

instance Lude.FromJSON ErrorDetail where
  parseJSON =
    Lude.withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail'
            Lude.<$> (x Lude..:? "DetailedErrorCode")
            Lude.<*> (x Lude..:? "DetailedErrorMessage")
      )
