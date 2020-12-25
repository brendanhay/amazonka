{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
  ( MaintenanceWindowAutomationParameters (..),

    -- * Smart constructor
    mkMaintenanceWindowAutomationParameters,

    -- * Lenses
    mwapDocumentVersion,
    mwapParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AutomationParameterKey as Types
import qualified Network.AWS.SSM.Types.AutomationParameterValue as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types

-- | The parameters for an AUTOMATION task type.
--
-- /See:/ 'mkMaintenanceWindowAutomationParameters' smart constructor.
data MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters'
  { -- | The version of an Automation document to use during task execution.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The parameters for the AUTOMATION task.
    --
    -- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
    parameters :: Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue])
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowAutomationParameters' value with any optional fields omitted.
mkMaintenanceWindowAutomationParameters ::
  MaintenanceWindowAutomationParameters
mkMaintenanceWindowAutomationParameters =
  MaintenanceWindowAutomationParameters'
    { documentVersion =
        Core.Nothing,
      parameters = Core.Nothing
    }

-- | The version of an Automation document to use during task execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapDocumentVersion :: Lens.Lens' MaintenanceWindowAutomationParameters (Core.Maybe Types.DocumentVersion)
mwapDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED mwapDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The parameters for the AUTOMATION task.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapParameters :: Lens.Lens' MaintenanceWindowAutomationParameters (Core.Maybe (Core.HashMap Types.AutomationParameterKey [Types.AutomationParameterValue]))
mwapParameters = Lens.field @"parameters"
{-# DEPRECATED mwapParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON MaintenanceWindowAutomationParameters where
  toJSON MaintenanceWindowAutomationParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.FromJSON MaintenanceWindowAutomationParameters where
  parseJSON =
    Core.withObject "MaintenanceWindowAutomationParameters" Core.$
      \x ->
        MaintenanceWindowAutomationParameters'
          Core.<$> (x Core..:? "DocumentVersion") Core.<*> (x Core..:? "Parameters")
