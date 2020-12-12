{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ValidationError
  ( ValidationError (..),

    -- * Smart constructor
    mkValidationError,

    -- * Lenses
    veErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an error found in a behavior specification.
--
-- /See:/ 'mkValidationError' smart constructor.
newtype ValidationError = ValidationError'
  { errorMessage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationError' with the minimum fields required to make a request.
--
-- * 'errorMessage' - The description of an error found in the behaviors.
mkValidationError ::
  ValidationError
mkValidationError = ValidationError' {errorMessage = Lude.Nothing}

-- | The description of an error found in the behaviors.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veErrorMessage :: Lens.Lens' ValidationError (Lude.Maybe Lude.Text)
veErrorMessage = Lens.lens (errorMessage :: ValidationError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: ValidationError)
{-# DEPRECATED veErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON ValidationError where
  parseJSON =
    Lude.withObject
      "ValidationError"
      (\x -> ValidationError' Lude.<$> (x Lude..:? "errorMessage"))
