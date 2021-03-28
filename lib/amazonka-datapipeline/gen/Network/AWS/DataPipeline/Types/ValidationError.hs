{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ValidationError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.ValidationError
  ( ValidationError (..)
  -- * Smart constructor
  , mkValidationError
  -- * Lenses
  , veErrors
  , veId
  ) where

import qualified Network.AWS.DataPipeline.Types.Id as Types
import qualified Network.AWS.DataPipeline.Types.ValidationMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines a validation error. Validation errors prevent pipeline activation. The set of validation errors that can be returned are defined by AWS Data Pipeline.
--
-- /See:/ 'mkValidationError' smart constructor.
data ValidationError = ValidationError'
  { errors :: Core.Maybe [Types.ValidationMessage]
    -- ^ A description of the validation error.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the object that contains the validation error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationError' value with any optional fields omitted.
mkValidationError
    :: ValidationError
mkValidationError
  = ValidationError'{errors = Core.Nothing, id = Core.Nothing}

-- | A description of the validation error.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veErrors :: Lens.Lens' ValidationError (Core.Maybe [Types.ValidationMessage])
veErrors = Lens.field @"errors"
{-# INLINEABLE veErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The identifier of the object that contains the validation error.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
veId :: Lens.Lens' ValidationError (Core.Maybe Types.Id)
veId = Lens.field @"id"
{-# INLINEABLE veId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON ValidationError where
        parseJSON
          = Core.withObject "ValidationError" Core.$
              \ x ->
                ValidationError' Core.<$>
                  (x Core..:? "errors") Core.<*> x Core..:? "id"
