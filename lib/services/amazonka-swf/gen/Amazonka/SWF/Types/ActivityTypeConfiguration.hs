{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SWF.Types.ActivityTypeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTypeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.TaskList

-- | Configuration settings registered with the activity type.
--
-- /See:/ 'newActivityTypeConfiguration' smart constructor.
data ActivityTypeConfiguration = ActivityTypeConfiguration'
  { -- | The default maximum time, in seconds, before which a worker processing a
    -- task must report progress by calling RecordActivityTaskHeartbeat.
    --
    -- You can specify this value only when /registering/ an activity type. The
    -- registered default value can be overridden when you schedule a task
    -- through the @ScheduleActivityTask@ Decision. If the activity worker
    -- subsequently attempts to record a heartbeat or returns a result, the
    -- activity worker receives an @UnknownResource@ fault. In this case,
    -- Amazon SWF no longer considers the activity task to be valid; the
    -- activity worker should clean up the activity task.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskHeartbeatTimeout :: Prelude.Maybe Prelude.Text,
    -- | The default task list specified for this activity type at registration.
    -- This default is used if a task list isn\'t provided when a task is
    -- scheduled through the @ScheduleActivityTask@ Decision. You can override
    -- the default registered task list when scheduling a task through the
    -- @ScheduleActivityTask@ Decision.
    defaultTaskList :: Prelude.Maybe TaskList,
    -- | The default task priority for tasks of this activity type, specified at
    -- registration. If not set, then @0@ is used as the default priority. This
    -- default can be overridden when scheduling an activity task.
    --
    -- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
    -- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
    -- indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /Amazon SWF Developer Guide/.
    defaultTaskPriority :: Prelude.Maybe Prelude.Text,
    -- | The default maximum duration, specified when registering the activity
    -- type, for tasks of this activity type. You can override this default
    -- when scheduling a task through the @ScheduleActivityTask@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskScheduleToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The default maximum duration, specified when registering the activity
    -- type, that a task of an activity type can wait before being assigned to
    -- a worker. You can override this default when scheduling a task through
    -- the @ScheduleActivityTask@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskScheduleToStartTimeout :: Prelude.Maybe Prelude.Text,
    -- | The default maximum duration for tasks of an activity type specified
    -- when registering the activity type. You can override this default when
    -- scheduling a task through the @ScheduleActivityTask@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultTaskHeartbeatTimeout', 'activityTypeConfiguration_defaultTaskHeartbeatTimeout' - The default maximum time, in seconds, before which a worker processing a
-- task must report progress by calling RecordActivityTaskHeartbeat.
--
-- You can specify this value only when /registering/ an activity type. The
-- registered default value can be overridden when you schedule a task
-- through the @ScheduleActivityTask@ Decision. If the activity worker
-- subsequently attempts to record a heartbeat or returns a result, the
-- activity worker receives an @UnknownResource@ fault. In this case,
-- Amazon SWF no longer considers the activity task to be valid; the
-- activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'defaultTaskList', 'activityTypeConfiguration_defaultTaskList' - The default task list specified for this activity type at registration.
-- This default is used if a task list isn\'t provided when a task is
-- scheduled through the @ScheduleActivityTask@ Decision. You can override
-- the default registered task list when scheduling a task through the
-- @ScheduleActivityTask@ Decision.
--
-- 'defaultTaskPriority', 'activityTypeConfiguration_defaultTaskPriority' - The default task priority for tasks of this activity type, specified at
-- registration. If not set, then @0@ is used as the default priority. This
-- default can be overridden when scheduling an activity task.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
--
-- 'defaultTaskScheduleToCloseTimeout', 'activityTypeConfiguration_defaultTaskScheduleToCloseTimeout' - The default maximum duration, specified when registering the activity
-- type, for tasks of this activity type. You can override this default
-- when scheduling a task through the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'defaultTaskScheduleToStartTimeout', 'activityTypeConfiguration_defaultTaskScheduleToStartTimeout' - The default maximum duration, specified when registering the activity
-- type, that a task of an activity type can wait before being assigned to
-- a worker. You can override this default when scheduling a task through
-- the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'defaultTaskStartToCloseTimeout', 'activityTypeConfiguration_defaultTaskStartToCloseTimeout' - The default maximum duration for tasks of an activity type specified
-- when registering the activity type. You can override this default when
-- scheduling a task through the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
newActivityTypeConfiguration ::
  ActivityTypeConfiguration
newActivityTypeConfiguration =
  ActivityTypeConfiguration'
    { defaultTaskHeartbeatTimeout =
        Prelude.Nothing,
      defaultTaskList = Prelude.Nothing,
      defaultTaskPriority = Prelude.Nothing,
      defaultTaskScheduleToCloseTimeout =
        Prelude.Nothing,
      defaultTaskScheduleToStartTimeout =
        Prelude.Nothing,
      defaultTaskStartToCloseTimeout = Prelude.Nothing
    }

-- | The default maximum time, in seconds, before which a worker processing a
-- task must report progress by calling RecordActivityTaskHeartbeat.
--
-- You can specify this value only when /registering/ an activity type. The
-- registered default value can be overridden when you schedule a task
-- through the @ScheduleActivityTask@ Decision. If the activity worker
-- subsequently attempts to record a heartbeat or returns a result, the
-- activity worker receives an @UnknownResource@ fault. In this case,
-- Amazon SWF no longer considers the activity task to be valid; the
-- activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
activityTypeConfiguration_defaultTaskHeartbeatTimeout :: Lens.Lens' ActivityTypeConfiguration (Prelude.Maybe Prelude.Text)
activityTypeConfiguration_defaultTaskHeartbeatTimeout = Lens.lens (\ActivityTypeConfiguration' {defaultTaskHeartbeatTimeout} -> defaultTaskHeartbeatTimeout) (\s@ActivityTypeConfiguration' {} a -> s {defaultTaskHeartbeatTimeout = a} :: ActivityTypeConfiguration)

-- | The default task list specified for this activity type at registration.
-- This default is used if a task list isn\'t provided when a task is
-- scheduled through the @ScheduleActivityTask@ Decision. You can override
-- the default registered task list when scheduling a task through the
-- @ScheduleActivityTask@ Decision.
activityTypeConfiguration_defaultTaskList :: Lens.Lens' ActivityTypeConfiguration (Prelude.Maybe TaskList)
activityTypeConfiguration_defaultTaskList = Lens.lens (\ActivityTypeConfiguration' {defaultTaskList} -> defaultTaskList) (\s@ActivityTypeConfiguration' {} a -> s {defaultTaskList = a} :: ActivityTypeConfiguration)

-- | The default task priority for tasks of this activity type, specified at
-- registration. If not set, then @0@ is used as the default priority. This
-- default can be overridden when scheduling an activity task.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon SWF Developer Guide/.
activityTypeConfiguration_defaultTaskPriority :: Lens.Lens' ActivityTypeConfiguration (Prelude.Maybe Prelude.Text)
activityTypeConfiguration_defaultTaskPriority = Lens.lens (\ActivityTypeConfiguration' {defaultTaskPriority} -> defaultTaskPriority) (\s@ActivityTypeConfiguration' {} a -> s {defaultTaskPriority = a} :: ActivityTypeConfiguration)

-- | The default maximum duration, specified when registering the activity
-- type, for tasks of this activity type. You can override this default
-- when scheduling a task through the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
activityTypeConfiguration_defaultTaskScheduleToCloseTimeout :: Lens.Lens' ActivityTypeConfiguration (Prelude.Maybe Prelude.Text)
activityTypeConfiguration_defaultTaskScheduleToCloseTimeout = Lens.lens (\ActivityTypeConfiguration' {defaultTaskScheduleToCloseTimeout} -> defaultTaskScheduleToCloseTimeout) (\s@ActivityTypeConfiguration' {} a -> s {defaultTaskScheduleToCloseTimeout = a} :: ActivityTypeConfiguration)

-- | The default maximum duration, specified when registering the activity
-- type, that a task of an activity type can wait before being assigned to
-- a worker. You can override this default when scheduling a task through
-- the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
activityTypeConfiguration_defaultTaskScheduleToStartTimeout :: Lens.Lens' ActivityTypeConfiguration (Prelude.Maybe Prelude.Text)
activityTypeConfiguration_defaultTaskScheduleToStartTimeout = Lens.lens (\ActivityTypeConfiguration' {defaultTaskScheduleToStartTimeout} -> defaultTaskScheduleToStartTimeout) (\s@ActivityTypeConfiguration' {} a -> s {defaultTaskScheduleToStartTimeout = a} :: ActivityTypeConfiguration)

-- | The default maximum duration for tasks of an activity type specified
-- when registering the activity type. You can override this default when
-- scheduling a task through the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
activityTypeConfiguration_defaultTaskStartToCloseTimeout :: Lens.Lens' ActivityTypeConfiguration (Prelude.Maybe Prelude.Text)
activityTypeConfiguration_defaultTaskStartToCloseTimeout = Lens.lens (\ActivityTypeConfiguration' {defaultTaskStartToCloseTimeout} -> defaultTaskStartToCloseTimeout) (\s@ActivityTypeConfiguration' {} a -> s {defaultTaskStartToCloseTimeout = a} :: ActivityTypeConfiguration)

instance Data.FromJSON ActivityTypeConfiguration where
  parseJSON =
    Data.withObject
      "ActivityTypeConfiguration"
      ( \x ->
          ActivityTypeConfiguration'
            Prelude.<$> (x Data..:? "defaultTaskHeartbeatTimeout")
            Prelude.<*> (x Data..:? "defaultTaskList")
            Prelude.<*> (x Data..:? "defaultTaskPriority")
            Prelude.<*> (x Data..:? "defaultTaskScheduleToCloseTimeout")
            Prelude.<*> (x Data..:? "defaultTaskScheduleToStartTimeout")
            Prelude.<*> (x Data..:? "defaultTaskStartToCloseTimeout")
      )

instance Prelude.Hashable ActivityTypeConfiguration where
  hashWithSalt _salt ActivityTypeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` defaultTaskHeartbeatTimeout
      `Prelude.hashWithSalt` defaultTaskList
      `Prelude.hashWithSalt` defaultTaskPriority
      `Prelude.hashWithSalt` defaultTaskScheduleToCloseTimeout
      `Prelude.hashWithSalt` defaultTaskScheduleToStartTimeout
      `Prelude.hashWithSalt` defaultTaskStartToCloseTimeout

instance Prelude.NFData ActivityTypeConfiguration where
  rnf ActivityTypeConfiguration' {..} =
    Prelude.rnf defaultTaskHeartbeatTimeout
      `Prelude.seq` Prelude.rnf defaultTaskList
      `Prelude.seq` Prelude.rnf defaultTaskPriority
      `Prelude.seq` Prelude.rnf defaultTaskScheduleToCloseTimeout
      `Prelude.seq` Prelude.rnf defaultTaskScheduleToStartTimeout
      `Prelude.seq` Prelude.rnf defaultTaskStartToCloseTimeout
