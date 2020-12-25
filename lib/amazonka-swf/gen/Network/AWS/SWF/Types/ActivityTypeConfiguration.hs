{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTypeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTypeConfiguration
  ( ActivityTypeConfiguration (..),

    -- * Smart constructor
    mkActivityTypeConfiguration,

    -- * Lenses
    atcDefaultTaskHeartbeatTimeout,
    atcDefaultTaskList,
    atcDefaultTaskPriority,
    atcDefaultTaskScheduleToCloseTimeout,
    atcDefaultTaskScheduleToStartTimeout,
    atcDefaultTaskStartToCloseTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.DefaultTaskHeartbeatTimeout as Types
import qualified Network.AWS.SWF.Types.DefaultTaskPriority as Types
import qualified Network.AWS.SWF.Types.DefaultTaskScheduleToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.DefaultTaskScheduleToStartTimeout as Types
import qualified Network.AWS.SWF.Types.DefaultTaskStartToCloseTimeout as Types
import qualified Network.AWS.SWF.Types.TaskList as Types

-- | Configuration settings registered with the activity type.
--
-- /See:/ 'mkActivityTypeConfiguration' smart constructor.
data ActivityTypeConfiguration = ActivityTypeConfiguration'
  { -- | The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' .
    --
    -- You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultTaskHeartbeatTimeout :: Core.Maybe Types.DefaultTaskHeartbeatTimeout,
    -- | The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
    defaultTaskList :: Core.Maybe Types.TaskList,
    -- | The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task.
    --
    -- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
    -- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
    defaultTaskPriority :: Core.Maybe Types.DefaultTaskPriority,
    -- | The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultTaskScheduleToCloseTimeout :: Core.Maybe Types.DefaultTaskScheduleToCloseTimeout,
    -- | The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultTaskScheduleToStartTimeout :: Core.Maybe Types.DefaultTaskScheduleToStartTimeout,
    -- | The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
    --
    -- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Core.Maybe Types.DefaultTaskStartToCloseTimeout
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityTypeConfiguration' value with any optional fields omitted.
mkActivityTypeConfiguration ::
  ActivityTypeConfiguration
mkActivityTypeConfiguration =
  ActivityTypeConfiguration'
    { defaultTaskHeartbeatTimeout =
        Core.Nothing,
      defaultTaskList = Core.Nothing,
      defaultTaskPriority = Core.Nothing,
      defaultTaskScheduleToCloseTimeout = Core.Nothing,
      defaultTaskScheduleToStartTimeout = Core.Nothing,
      defaultTaskStartToCloseTimeout = Core.Nothing
    }

-- | The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' .
--
-- You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskHeartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskHeartbeatTimeout :: Lens.Lens' ActivityTypeConfiguration (Core.Maybe Types.DefaultTaskHeartbeatTimeout)
atcDefaultTaskHeartbeatTimeout = Lens.field @"defaultTaskHeartbeatTimeout"
{-# DEPRECATED atcDefaultTaskHeartbeatTimeout "Use generic-lens or generic-optics with 'defaultTaskHeartbeatTimeout' instead." #-}

-- | The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskList :: Lens.Lens' ActivityTypeConfiguration (Core.Maybe Types.TaskList)
atcDefaultTaskList = Lens.field @"defaultTaskList"
{-# DEPRECATED atcDefaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead." #-}

-- | The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskPriority :: Lens.Lens' ActivityTypeConfiguration (Core.Maybe Types.DefaultTaskPriority)
atcDefaultTaskPriority = Lens.field @"defaultTaskPriority"
{-# DEPRECATED atcDefaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead." #-}

-- | The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskScheduleToCloseTimeout :: Lens.Lens' ActivityTypeConfiguration (Core.Maybe Types.DefaultTaskScheduleToCloseTimeout)
atcDefaultTaskScheduleToCloseTimeout = Lens.field @"defaultTaskScheduleToCloseTimeout"
{-# DEPRECATED atcDefaultTaskScheduleToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToCloseTimeout' instead." #-}

-- | The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskScheduleToStartTimeout :: Lens.Lens' ActivityTypeConfiguration (Core.Maybe Types.DefaultTaskScheduleToStartTimeout)
atcDefaultTaskScheduleToStartTimeout = Lens.field @"defaultTaskScheduleToStartTimeout"
{-# DEPRECATED atcDefaultTaskScheduleToStartTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToStartTimeout' instead." #-}

-- | The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskStartToCloseTimeout :: Lens.Lens' ActivityTypeConfiguration (Core.Maybe Types.DefaultTaskStartToCloseTimeout)
atcDefaultTaskStartToCloseTimeout = Lens.field @"defaultTaskStartToCloseTimeout"
{-# DEPRECATED atcDefaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead." #-}

instance Core.FromJSON ActivityTypeConfiguration where
  parseJSON =
    Core.withObject "ActivityTypeConfiguration" Core.$
      \x ->
        ActivityTypeConfiguration'
          Core.<$> (x Core..:? "defaultTaskHeartbeatTimeout")
          Core.<*> (x Core..:? "defaultTaskList")
          Core.<*> (x Core..:? "defaultTaskPriority")
          Core.<*> (x Core..:? "defaultTaskScheduleToCloseTimeout")
          Core.<*> (x Core..:? "defaultTaskScheduleToStartTimeout")
          Core.<*> (x Core..:? "defaultTaskStartToCloseTimeout")
