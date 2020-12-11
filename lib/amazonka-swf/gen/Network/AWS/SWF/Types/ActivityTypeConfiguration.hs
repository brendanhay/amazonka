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
    atcDefaultTaskScheduleToStartTimeout,
    atcDefaultTaskList,
    atcDefaultTaskPriority,
    atcDefaultTaskHeartbeatTimeout,
    atcDefaultTaskScheduleToCloseTimeout,
    atcDefaultTaskStartToCloseTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.TaskList

-- | Configuration settings registered with the activity type.
--
-- /See:/ 'mkActivityTypeConfiguration' smart constructor.
data ActivityTypeConfiguration = ActivityTypeConfiguration'
  { defaultTaskScheduleToStartTimeout ::
      Lude.Maybe Lude.Text,
    defaultTaskList :: Lude.Maybe TaskList,
    defaultTaskPriority ::
      Lude.Maybe Lude.Text,
    defaultTaskHeartbeatTimeout ::
      Lude.Maybe Lude.Text,
    defaultTaskScheduleToCloseTimeout ::
      Lude.Maybe Lude.Text,
    defaultTaskStartToCloseTimeout ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityTypeConfiguration' with the minimum fields required to make a request.
--
-- * 'defaultTaskHeartbeatTimeout' - The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' .
--
-- You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'defaultTaskList' - The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
-- * 'defaultTaskPriority' - The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
-- * 'defaultTaskScheduleToCloseTimeout' - The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'defaultTaskScheduleToStartTimeout' - The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'defaultTaskStartToCloseTimeout' - The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
mkActivityTypeConfiguration ::
  ActivityTypeConfiguration
mkActivityTypeConfiguration =
  ActivityTypeConfiguration'
    { defaultTaskScheduleToStartTimeout =
        Lude.Nothing,
      defaultTaskList = Lude.Nothing,
      defaultTaskPriority = Lude.Nothing,
      defaultTaskHeartbeatTimeout = Lude.Nothing,
      defaultTaskScheduleToCloseTimeout = Lude.Nothing,
      defaultTaskStartToCloseTimeout = Lude.Nothing
    }

-- | The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskScheduleToStartTimeout :: Lens.Lens' ActivityTypeConfiguration (Lude.Maybe Lude.Text)
atcDefaultTaskScheduleToStartTimeout = Lens.lens (defaultTaskScheduleToStartTimeout :: ActivityTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskScheduleToStartTimeout = a} :: ActivityTypeConfiguration)
{-# DEPRECATED atcDefaultTaskScheduleToStartTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToStartTimeout' instead." #-}

-- | The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskList :: Lens.Lens' ActivityTypeConfiguration (Lude.Maybe TaskList)
atcDefaultTaskList = Lens.lens (defaultTaskList :: ActivityTypeConfiguration -> Lude.Maybe TaskList) (\s a -> s {defaultTaskList = a} :: ActivityTypeConfiguration)
{-# DEPRECATED atcDefaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead." #-}

-- | The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task.
--
-- Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskPriority :: Lens.Lens' ActivityTypeConfiguration (Lude.Maybe Lude.Text)
atcDefaultTaskPriority = Lens.lens (defaultTaskPriority :: ActivityTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskPriority = a} :: ActivityTypeConfiguration)
{-# DEPRECATED atcDefaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead." #-}

-- | The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' .
--
-- You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskHeartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskHeartbeatTimeout :: Lens.Lens' ActivityTypeConfiguration (Lude.Maybe Lude.Text)
atcDefaultTaskHeartbeatTimeout = Lens.lens (defaultTaskHeartbeatTimeout :: ActivityTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskHeartbeatTimeout = a} :: ActivityTypeConfiguration)
{-# DEPRECATED atcDefaultTaskHeartbeatTimeout "Use generic-lens or generic-optics with 'defaultTaskHeartbeatTimeout' instead." #-}

-- | The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskScheduleToCloseTimeout :: Lens.Lens' ActivityTypeConfiguration (Lude.Maybe Lude.Text)
atcDefaultTaskScheduleToCloseTimeout = Lens.lens (defaultTaskScheduleToCloseTimeout :: ActivityTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskScheduleToCloseTimeout = a} :: ActivityTypeConfiguration)
{-# DEPRECATED atcDefaultTaskScheduleToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToCloseTimeout' instead." #-}

-- | The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcDefaultTaskStartToCloseTimeout :: Lens.Lens' ActivityTypeConfiguration (Lude.Maybe Lude.Text)
atcDefaultTaskStartToCloseTimeout = Lens.lens (defaultTaskStartToCloseTimeout :: ActivityTypeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskStartToCloseTimeout = a} :: ActivityTypeConfiguration)
{-# DEPRECATED atcDefaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead." #-}

instance Lude.FromJSON ActivityTypeConfiguration where
  parseJSON =
    Lude.withObject
      "ActivityTypeConfiguration"
      ( \x ->
          ActivityTypeConfiguration'
            Lude.<$> (x Lude..:? "defaultTaskScheduleToStartTimeout")
            Lude.<*> (x Lude..:? "defaultTaskList")
            Lude.<*> (x Lude..:? "defaultTaskPriority")
            Lude.<*> (x Lude..:? "defaultTaskHeartbeatTimeout")
            Lude.<*> (x Lude..:? "defaultTaskScheduleToCloseTimeout")
            Lude.<*> (x Lude..:? "defaultTaskStartToCloseTimeout")
      )
