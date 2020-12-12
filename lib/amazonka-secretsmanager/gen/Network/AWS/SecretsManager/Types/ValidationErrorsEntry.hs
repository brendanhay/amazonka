{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.ValidationErrorsEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.ValidationErrorsEntry
  ( ValidationErrorsEntry (..),

    -- * Smart constructor
    mkValidationErrorsEntry,

    -- * Lenses
    veeCheckName,
    veeErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Displays errors that occurred during validation of the resource policy.
--
-- /See:/ 'mkValidationErrorsEntry' smart constructor.
data ValidationErrorsEntry = ValidationErrorsEntry'
  { checkName ::
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

-- | Creates a value of 'ValidationErrorsEntry' with the minimum fields required to make a request.
--
-- * 'checkName' - Checks the name of the policy.
-- * 'errorMessage' - Displays error messages if validation encounters problems during validation of the resource policy.
mkValidationErrorsEntry ::
  ValidationErrorsEntry
mkValidationErrorsEntry =
  ValidationErrorsEntry'
    { checkName = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | Checks the name of the policy.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veeCheckName :: Lens.Lens' ValidationErrorsEntry (Lude.Maybe Lude.Text)
veeCheckName = Lens.lens (checkName :: ValidationErrorsEntry -> Lude.Maybe Lude.Text) (\s a -> s {checkName = a} :: ValidationErrorsEntry)
{-# DEPRECATED veeCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Displays error messages if validation encounters problems during validation of the resource policy.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veeErrorMessage :: Lens.Lens' ValidationErrorsEntry (Lude.Maybe Lude.Text)
veeErrorMessage = Lens.lens (errorMessage :: ValidationErrorsEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: ValidationErrorsEntry)
{-# DEPRECATED veeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON ValidationErrorsEntry where
  parseJSON =
    Lude.withObject
      "ValidationErrorsEntry"
      ( \x ->
          ValidationErrorsEntry'
            Lude.<$> (x Lude..:? "CheckName") Lude.<*> (x Lude..:? "ErrorMessage")
      )
