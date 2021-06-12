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
-- Module      : Network.AWS.CodeDeploy.Types.ECSTaskSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSTaskSet where

import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TargetLabel
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a set of Amazon ECS tasks in an AWS CodeDeploy
-- deployment. An Amazon ECS task set includes details such as the desired
-- number of tasks, how many tasks are running, and whether the task set
-- serves production traffic. An AWS CodeDeploy application that uses the
-- Amazon ECS compute platform deploys a containerized application in an
-- Amazon ECS service as a task set.
--
-- /See:/ 'newECSTaskSet' smart constructor.
data ECSTaskSet = ECSTaskSet'
  { -- | The status of the task set. There are three valid task set statuses:
    --
    -- -   @PRIMARY@: Indicates the task set is serving production traffic.
    --
    -- -   @ACTIVE@: Indicates the task set is not serving production traffic.
    --
    -- -   @DRAINING@: Indicates the tasks in the task set are being stopped
    --     and their corresponding targets are being deregistered from their
    --     target group.
    status :: Core.Maybe Core.Text,
    -- | The number of tasks in the task set that are in the @RUNNING@ status
    -- during an Amazon ECS deployment. A task in the @RUNNING@ state is
    -- running and ready for use.
    runningCount :: Core.Maybe Core.Integer,
    -- | The number of tasks in a task set. During a deployment that uses the
    -- Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new
    -- task set and uses this value to determine how many tasks to create.
    -- After the updated task set is created, CodeDeploy shifts traffic to the
    -- new task set.
    desiredCount :: Core.Maybe Core.Integer,
    -- | The number of tasks in the task set that are in the @PENDING@ status
    -- during an Amazon ECS deployment. A task in the @PENDING@ state is
    -- preparing to enter the @RUNNING@ state. A task set enters the @PENDING@
    -- status when it launches for the first time, or when it is restarted
    -- after being in the @STOPPED@ state.
    pendingCount :: Core.Maybe Core.Integer,
    -- | A label that identifies whether the ECS task set is an original target
    -- (@BLUE@) or a replacement target (@GREEN@).
    taskSetLabel :: Core.Maybe TargetLabel,
    -- | The target group associated with the task set. The target group is used
    -- by AWS CodeDeploy to manage traffic to a task set.
    targetGroup :: Core.Maybe TargetGroupInfo,
    -- | The percentage of traffic served by this task set.
    trafficWeight :: Core.Maybe Core.Double,
    -- | A unique ID of an @ECSTaskSet@.
    identifer :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ECSTaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'eCSTaskSet_status' - The status of the task set. There are three valid task set statuses:
--
-- -   @PRIMARY@: Indicates the task set is serving production traffic.
--
-- -   @ACTIVE@: Indicates the task set is not serving production traffic.
--
-- -   @DRAINING@: Indicates the tasks in the task set are being stopped
--     and their corresponding targets are being deregistered from their
--     target group.
--
-- 'runningCount', 'eCSTaskSet_runningCount' - The number of tasks in the task set that are in the @RUNNING@ status
-- during an Amazon ECS deployment. A task in the @RUNNING@ state is
-- running and ready for use.
--
-- 'desiredCount', 'eCSTaskSet_desiredCount' - The number of tasks in a task set. During a deployment that uses the
-- Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new
-- task set and uses this value to determine how many tasks to create.
-- After the updated task set is created, CodeDeploy shifts traffic to the
-- new task set.
--
-- 'pendingCount', 'eCSTaskSet_pendingCount' - The number of tasks in the task set that are in the @PENDING@ status
-- during an Amazon ECS deployment. A task in the @PENDING@ state is
-- preparing to enter the @RUNNING@ state. A task set enters the @PENDING@
-- status when it launches for the first time, or when it is restarted
-- after being in the @STOPPED@ state.
--
-- 'taskSetLabel', 'eCSTaskSet_taskSetLabel' - A label that identifies whether the ECS task set is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
--
-- 'targetGroup', 'eCSTaskSet_targetGroup' - The target group associated with the task set. The target group is used
-- by AWS CodeDeploy to manage traffic to a task set.
--
-- 'trafficWeight', 'eCSTaskSet_trafficWeight' - The percentage of traffic served by this task set.
--
-- 'identifer', 'eCSTaskSet_identifer' - A unique ID of an @ECSTaskSet@.
newECSTaskSet ::
  ECSTaskSet
newECSTaskSet =
  ECSTaskSet'
    { status = Core.Nothing,
      runningCount = Core.Nothing,
      desiredCount = Core.Nothing,
      pendingCount = Core.Nothing,
      taskSetLabel = Core.Nothing,
      targetGroup = Core.Nothing,
      trafficWeight = Core.Nothing,
      identifer = Core.Nothing
    }

-- | The status of the task set. There are three valid task set statuses:
--
-- -   @PRIMARY@: Indicates the task set is serving production traffic.
--
-- -   @ACTIVE@: Indicates the task set is not serving production traffic.
--
-- -   @DRAINING@: Indicates the tasks in the task set are being stopped
--     and their corresponding targets are being deregistered from their
--     target group.
eCSTaskSet_status :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Text)
eCSTaskSet_status = Lens.lens (\ECSTaskSet' {status} -> status) (\s@ECSTaskSet' {} a -> s {status = a} :: ECSTaskSet)

-- | The number of tasks in the task set that are in the @RUNNING@ status
-- during an Amazon ECS deployment. A task in the @RUNNING@ state is
-- running and ready for use.
eCSTaskSet_runningCount :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Integer)
eCSTaskSet_runningCount = Lens.lens (\ECSTaskSet' {runningCount} -> runningCount) (\s@ECSTaskSet' {} a -> s {runningCount = a} :: ECSTaskSet)

-- | The number of tasks in a task set. During a deployment that uses the
-- Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new
-- task set and uses this value to determine how many tasks to create.
-- After the updated task set is created, CodeDeploy shifts traffic to the
-- new task set.
eCSTaskSet_desiredCount :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Integer)
eCSTaskSet_desiredCount = Lens.lens (\ECSTaskSet' {desiredCount} -> desiredCount) (\s@ECSTaskSet' {} a -> s {desiredCount = a} :: ECSTaskSet)

-- | The number of tasks in the task set that are in the @PENDING@ status
-- during an Amazon ECS deployment. A task in the @PENDING@ state is
-- preparing to enter the @RUNNING@ state. A task set enters the @PENDING@
-- status when it launches for the first time, or when it is restarted
-- after being in the @STOPPED@ state.
eCSTaskSet_pendingCount :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Integer)
eCSTaskSet_pendingCount = Lens.lens (\ECSTaskSet' {pendingCount} -> pendingCount) (\s@ECSTaskSet' {} a -> s {pendingCount = a} :: ECSTaskSet)

-- | A label that identifies whether the ECS task set is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
eCSTaskSet_taskSetLabel :: Lens.Lens' ECSTaskSet (Core.Maybe TargetLabel)
eCSTaskSet_taskSetLabel = Lens.lens (\ECSTaskSet' {taskSetLabel} -> taskSetLabel) (\s@ECSTaskSet' {} a -> s {taskSetLabel = a} :: ECSTaskSet)

-- | The target group associated with the task set. The target group is used
-- by AWS CodeDeploy to manage traffic to a task set.
eCSTaskSet_targetGroup :: Lens.Lens' ECSTaskSet (Core.Maybe TargetGroupInfo)
eCSTaskSet_targetGroup = Lens.lens (\ECSTaskSet' {targetGroup} -> targetGroup) (\s@ECSTaskSet' {} a -> s {targetGroup = a} :: ECSTaskSet)

-- | The percentage of traffic served by this task set.
eCSTaskSet_trafficWeight :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Double)
eCSTaskSet_trafficWeight = Lens.lens (\ECSTaskSet' {trafficWeight} -> trafficWeight) (\s@ECSTaskSet' {} a -> s {trafficWeight = a} :: ECSTaskSet)

-- | A unique ID of an @ECSTaskSet@.
eCSTaskSet_identifer :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Text)
eCSTaskSet_identifer = Lens.lens (\ECSTaskSet' {identifer} -> identifer) (\s@ECSTaskSet' {} a -> s {identifer = a} :: ECSTaskSet)

instance Core.FromJSON ECSTaskSet where
  parseJSON =
    Core.withObject
      "ECSTaskSet"
      ( \x ->
          ECSTaskSet'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "runningCount")
            Core.<*> (x Core..:? "desiredCount")
            Core.<*> (x Core..:? "pendingCount")
            Core.<*> (x Core..:? "taskSetLabel")
            Core.<*> (x Core..:? "targetGroup")
            Core.<*> (x Core..:? "trafficWeight")
            Core.<*> (x Core..:? "identifer")
      )

instance Core.Hashable ECSTaskSet

instance Core.NFData ECSTaskSet
