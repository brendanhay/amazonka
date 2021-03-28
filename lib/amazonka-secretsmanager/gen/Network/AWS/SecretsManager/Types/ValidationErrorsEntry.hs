{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.ValidationErrorsEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SecretsManager.Types.ValidationErrorsEntry
  ( ValidationErrorsEntry (..)
  -- * Smart constructor
  , mkValidationErrorsEntry
  -- * Lenses
  , veeCheckName
  , veeErrorMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SecretsManager.Types.ErrorMessage as Types
import qualified Network.AWS.SecretsManager.Types.NameType as Types

-- | Displays errors that occurred during validation of the resource policy.
--
-- /See:/ 'mkValidationErrorsEntry' smart constructor.
data ValidationErrorsEntry = ValidationErrorsEntry'
  { checkName :: Core.Maybe Types.NameType
    -- ^ Checks the name of the policy.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ Displays error messages if validation encounters problems during validation of the resource policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationErrorsEntry' value with any optional fields omitted.
mkValidationErrorsEntry
    :: ValidationErrorsEntry
mkValidationErrorsEntry
  = ValidationErrorsEntry'{checkName = Core.Nothing,
                           errorMessage = Core.Nothing}

-- | Checks the name of the policy.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veeCheckName :: Lens.Lens' ValidationErrorsEntry (Core.Maybe Types.NameType)
veeCheckName = Lens.field @"checkName"
{-# INLINEABLE veeCheckName #-}
{-# DEPRECATED checkName "Use generic-lens or generic-optics with 'checkName' instead"  #-}

-- | Displays error messages if validation encounters problems during validation of the resource policy.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veeErrorMessage :: Lens.Lens' ValidationErrorsEntry (Core.Maybe Types.ErrorMessage)
veeErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE veeErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

instance Core.FromJSON ValidationErrorsEntry where
        parseJSON
          = Core.withObject "ValidationErrorsEntry" Core.$
              \ x ->
                ValidationErrorsEntry' Core.<$>
                  (x Core..:? "CheckName") Core.<*> x Core..:? "ErrorMessage"
