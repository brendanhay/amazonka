{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionFilter
  ( AutomationExecutionFilter (..),

    -- * Smart constructor
    mkAutomationExecutionFilter,

    -- * Lenses
    aKey,
    aValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AutomationExecutionFilterKey as Types
import qualified Network.AWS.SSM.Types.AutomationExecutionFilterValue as Types

-- | A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.
--
-- /See:/ 'mkAutomationExecutionFilter' smart constructor.
data AutomationExecutionFilter = AutomationExecutionFilter'
  { -- | One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
    key :: Types.AutomationExecutionFilterKey,
    -- | The values used to limit the execution information associated with the filter's key.
    values :: Core.NonEmpty Types.AutomationExecutionFilterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutomationExecutionFilter' value with any optional fields omitted.
mkAutomationExecutionFilter ::
  -- | 'key'
  Types.AutomationExecutionFilterKey ->
  -- | 'values'
  Core.NonEmpty Types.AutomationExecutionFilterValue ->
  AutomationExecutionFilter
mkAutomationExecutionFilter key values =
  AutomationExecutionFilter' {key, values}

-- | One or more keys to limit the results. Valid filter keys include the following: DocumentNamePrefix, ExecutionStatus, ExecutionId, ParentExecutionId, CurrentAction, StartTimeBefore, StartTimeAfter, TargetResourceGroup.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aKey :: Lens.Lens' AutomationExecutionFilter Types.AutomationExecutionFilterKey
aKey = Lens.field @"key"
{-# DEPRECATED aKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The values used to limit the execution information associated with the filter's key.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValues :: Lens.Lens' AutomationExecutionFilter (Core.NonEmpty Types.AutomationExecutionFilterValue)
aValues = Lens.field @"values"
{-# DEPRECATED aValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON AutomationExecutionFilter where
  toJSON AutomationExecutionFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )
