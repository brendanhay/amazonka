{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ErrorDetails
  ( ErrorDetails (..),

    -- * Smart constructor
    mkErrorDetails,

    -- * Lenses
    edErrorType,
    edErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { errorType ::
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
-- * 'errorMessage' - Undocumented field.
-- * 'errorType' - Undocumented field.
mkErrorDetails ::
  ErrorDetails
mkErrorDetails =
  ErrorDetails'
    { errorType = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'errorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorType :: Lens.Lens' ErrorDetails (Lude.Maybe Lude.Text)
edErrorType = Lens.lens (errorType :: ErrorDetails -> Lude.Maybe Lude.Text) (\s a -> s {errorType = a} :: ErrorDetails)
{-# DEPRECATED edErrorType "Use generic-lens or generic-optics with 'errorType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorMessage :: Lens.Lens' ErrorDetails (Lude.Maybe Lude.Text)
edErrorMessage = Lens.lens (errorMessage :: ErrorDetails -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: ErrorDetails)
{-# DEPRECATED edErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON ErrorDetails where
  parseJSON =
    Lude.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Lude.<$> (x Lude..:? "ErrorType") Lude.<*> (x Lude..:? "ErrorMessage")
      )
