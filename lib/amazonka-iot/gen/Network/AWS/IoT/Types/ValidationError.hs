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

import qualified Network.AWS.IoT.Types.ErrorMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an error found in a behavior specification.
--
-- /See:/ 'mkValidationError' smart constructor.
newtype ValidationError = ValidationError'
  { -- | The description of an error found in the behaviors.
    errorMessage :: Core.Maybe Types.ErrorMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationError' value with any optional fields omitted.
mkValidationError ::
  ValidationError
mkValidationError = ValidationError' {errorMessage = Core.Nothing}

-- | The description of an error found in the behaviors.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veErrorMessage :: Lens.Lens' ValidationError (Core.Maybe Types.ErrorMessage)
veErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED veErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Core.FromJSON ValidationError where
  parseJSON =
    Core.withObject "ValidationError" Core.$
      \x -> ValidationError' Core.<$> (x Core..:? "errorMessage")
