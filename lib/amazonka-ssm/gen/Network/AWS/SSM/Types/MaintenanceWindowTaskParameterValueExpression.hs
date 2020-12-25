{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
  ( MaintenanceWindowTaskParameterValueExpression (..),

    -- * Smart constructor
    mkMaintenanceWindowTaskParameterValueExpression,

    -- * Lenses
    mwtpveValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValue as Types

-- | Defines the values for a task parameter.
--
-- /See:/ 'mkMaintenanceWindowTaskParameterValueExpression' smart constructor.
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression'
  { -- | This field contains an array of 0 or more strings, each 1 to 255 characters in length.
    values :: Core.Maybe [Types.MaintenanceWindowTaskParameterValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowTaskParameterValueExpression' value with any optional fields omitted.
mkMaintenanceWindowTaskParameterValueExpression ::
  MaintenanceWindowTaskParameterValueExpression
mkMaintenanceWindowTaskParameterValueExpression =
  MaintenanceWindowTaskParameterValueExpression'
    { values =
        Core.Nothing
    }

-- | This field contains an array of 0 or more strings, each 1 to 255 characters in length.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtpveValues :: Lens.Lens' MaintenanceWindowTaskParameterValueExpression (Core.Maybe [Types.MaintenanceWindowTaskParameterValue])
mwtpveValues = Lens.field @"values"
{-# DEPRECATED mwtpveValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance
  Core.FromJSON
    MaintenanceWindowTaskParameterValueExpression
  where
  toJSON MaintenanceWindowTaskParameterValueExpression {..} =
    Core.object (Core.catMaybes [("Values" Core..=) Core.<$> values])

instance
  Core.FromJSON
    MaintenanceWindowTaskParameterValueExpression
  where
  parseJSON =
    Core.withObject "MaintenanceWindowTaskParameterValueExpression" Core.$
      \x ->
        MaintenanceWindowTaskParameterValueExpression'
          Core.<$> (x Core..:? "Values")
