{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ScheduledWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ScheduledWindowExecution
  ( ScheduledWindowExecution (..)
  -- * Smart constructor
  , mkScheduledWindowExecution
  -- * Lenses
  , sweExecutionTime
  , sweName
  , sweWindowId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ExecutionTime as Types
import qualified Network.AWS.SSM.Types.Name as Types
import qualified Network.AWS.SSM.Types.WindowId as Types

-- | Information about a scheduled execution for a maintenance window.
--
-- /See:/ 'mkScheduledWindowExecution' smart constructor.
data ScheduledWindowExecution = ScheduledWindowExecution'
  { executionTime :: Core.Maybe Types.ExecutionTime
    -- ^ The time, in ISO-8601 Extended format, that the maintenance window is scheduled to be run.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the maintenance window to be run.
  , windowId :: Core.Maybe Types.WindowId
    -- ^ The ID of the maintenance window to be run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledWindowExecution' value with any optional fields omitted.
mkScheduledWindowExecution
    :: ScheduledWindowExecution
mkScheduledWindowExecution
  = ScheduledWindowExecution'{executionTime = Core.Nothing,
                              name = Core.Nothing, windowId = Core.Nothing}

-- | The time, in ISO-8601 Extended format, that the maintenance window is scheduled to be run.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweExecutionTime :: Lens.Lens' ScheduledWindowExecution (Core.Maybe Types.ExecutionTime)
sweExecutionTime = Lens.field @"executionTime"
{-# INLINEABLE sweExecutionTime #-}
{-# DEPRECATED executionTime "Use generic-lens or generic-optics with 'executionTime' instead"  #-}

-- | The name of the maintenance window to be run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweName :: Lens.Lens' ScheduledWindowExecution (Core.Maybe Types.Name)
sweName = Lens.field @"name"
{-# INLINEABLE sweName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the maintenance window to be run.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sweWindowId :: Lens.Lens' ScheduledWindowExecution (Core.Maybe Types.WindowId)
sweWindowId = Lens.field @"windowId"
{-# INLINEABLE sweWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

instance Core.FromJSON ScheduledWindowExecution where
        parseJSON
          = Core.withObject "ScheduledWindowExecution" Core.$
              \ x ->
                ScheduledWindowExecution' Core.<$>
                  (x Core..:? "ExecutionTime") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "WindowId"
