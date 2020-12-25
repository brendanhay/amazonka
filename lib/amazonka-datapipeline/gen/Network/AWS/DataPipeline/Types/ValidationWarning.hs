{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ValidationWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ValidationWarning
  ( ValidationWarning (..),

    -- * Smart constructor
    mkValidationWarning,

    -- * Lenses
    vwId,
    vwWarnings,
  )
where

import qualified Network.AWS.DataPipeline.Types.Id as Types
import qualified Network.AWS.DataPipeline.Types.ValidationMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines a validation warning. Validation warnings do not prevent pipeline activation. The set of validation warnings that can be returned are defined by AWS Data Pipeline.
--
-- /See:/ 'mkValidationWarning' smart constructor.
data ValidationWarning = ValidationWarning'
  { -- | The identifier of the object that contains the validation warning.
    id :: Core.Maybe Types.Id,
    -- | A description of the validation warning.
    warnings :: Core.Maybe [Types.ValidationMessage]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidationWarning' value with any optional fields omitted.
mkValidationWarning ::
  ValidationWarning
mkValidationWarning =
  ValidationWarning' {id = Core.Nothing, warnings = Core.Nothing}

-- | The identifier of the object that contains the validation warning.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwId :: Lens.Lens' ValidationWarning (Core.Maybe Types.Id)
vwId = Lens.field @"id"
{-# DEPRECATED vwId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A description of the validation warning.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwWarnings :: Lens.Lens' ValidationWarning (Core.Maybe [Types.ValidationMessage])
vwWarnings = Lens.field @"warnings"
{-# DEPRECATED vwWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

instance Core.FromJSON ValidationWarning where
  parseJSON =
    Core.withObject "ValidationWarning" Core.$
      \x ->
        ValidationWarning'
          Core.<$> (x Core..:? "id") Core.<*> (x Core..:? "warnings")
