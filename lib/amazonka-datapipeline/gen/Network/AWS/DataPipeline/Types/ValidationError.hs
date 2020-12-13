{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ValidationError
  ( ValidationError (..),

    -- * Smart constructor
    mkValidationError,

    -- * Lenses
    veId,
    veErrors,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a validation error. Validation errors prevent pipeline activation. The set of validation errors that can be returned are defined by AWS Data Pipeline.
--
-- /See:/ 'mkValidationError' smart constructor.
data ValidationError = ValidationError'
  { -- | The identifier of the object that contains the validation error.
    id :: Lude.Maybe Lude.Text,
    -- | A description of the validation error.
    errors :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidationError' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the object that contains the validation error.
-- * 'errors' - A description of the validation error.
mkValidationError ::
  ValidationError
mkValidationError =
  ValidationError' {id = Lude.Nothing, errors = Lude.Nothing}

-- | The identifier of the object that contains the validation error.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veId :: Lens.Lens' ValidationError (Lude.Maybe Lude.Text)
veId = Lens.lens (id :: ValidationError -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ValidationError)
{-# DEPRECATED veId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A description of the validation error.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veErrors :: Lens.Lens' ValidationError (Lude.Maybe [Lude.Text])
veErrors = Lens.lens (errors :: ValidationError -> Lude.Maybe [Lude.Text]) (\s a -> s {errors = a} :: ValidationError)
{-# DEPRECATED veErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

instance Lude.FromJSON ValidationError where
  parseJSON =
    Lude.withObject
      "ValidationError"
      ( \x ->
          ValidationError'
            Lude.<$> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "errors" Lude..!= Lude.mempty)
      )
