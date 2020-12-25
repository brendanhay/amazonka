{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
  ( MaintenanceWindowStepFunctionsParameters (..),

    -- * Smart constructor
    mkMaintenanceWindowStepFunctionsParameters,

    -- * Lenses
    mwsfpInput,
    mwsfpName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsInput as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsName as Types

-- | The parameters for a STEP_FUNCTIONS task.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /See:/ 'mkMaintenanceWindowStepFunctionsParameters' smart constructor.
data MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters'
  { -- | The inputs for the STEP_FUNCTIONS task.
    input :: Core.Maybe Types.MaintenanceWindowStepFunctionsInput,
    -- | The name of the STEP_FUNCTIONS task.
    name :: Core.Maybe Types.MaintenanceWindowStepFunctionsName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowStepFunctionsParameters' value with any optional fields omitted.
mkMaintenanceWindowStepFunctionsParameters ::
  MaintenanceWindowStepFunctionsParameters
mkMaintenanceWindowStepFunctionsParameters =
  MaintenanceWindowStepFunctionsParameters'
    { input = Core.Nothing,
      name = Core.Nothing
    }

-- | The inputs for the STEP_FUNCTIONS task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsfpInput :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Core.Maybe Types.MaintenanceWindowStepFunctionsInput)
mwsfpInput = Lens.field @"input"
{-# DEPRECATED mwsfpInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the STEP_FUNCTIONS task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsfpName :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Core.Maybe Types.MaintenanceWindowStepFunctionsName)
mwsfpName = Lens.field @"name"
{-# DEPRECATED mwsfpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON MaintenanceWindowStepFunctionsParameters where
  toJSON MaintenanceWindowStepFunctionsParameters {..} =
    Core.object
      ( Core.catMaybes
          [("Input" Core..=) Core.<$> input, ("Name" Core..=) Core.<$> name]
      )

instance Core.FromJSON MaintenanceWindowStepFunctionsParameters where
  parseJSON =
    Core.withObject "MaintenanceWindowStepFunctionsParameters" Core.$
      \x ->
        MaintenanceWindowStepFunctionsParameters'
          Core.<$> (x Core..:? "Input") Core.<*> (x Core..:? "Name")
