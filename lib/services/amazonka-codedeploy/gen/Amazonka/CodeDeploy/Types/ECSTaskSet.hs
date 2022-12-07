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
-- Module      : Amazonka.CodeDeploy.Types.ECSTaskSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.ECSTaskSet where

import Amazonka.CodeDeploy.Types.TargetGroupInfo
import Amazonka.CodeDeploy.Types.TargetLabel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a set of Amazon ECS tasks in an CodeDeploy deployment.
-- An Amazon ECS task set includes details such as the desired number of
-- tasks, how many tasks are running, and whether the task set serves
-- production traffic. An CodeDeploy application that uses the Amazon ECS
-- compute platform deploys a containerized application in an Amazon ECS
-- service as a task set.
--
-- /See:/ 'newECSTaskSet' smart constructor.
data ECSTaskSet = ECSTaskSet'
  { -- | A unique ID of an @ECSTaskSet@.
    identifer :: Prelude.Maybe Prelude.Text,
    -- | The status of the task set. There are three valid task set statuses:
    --
    -- -   @PRIMARY@: Indicates the task set is serving production traffic.
    --
    -- -   @ACTIVE@: Indicates the task set is not serving production traffic.
    --
    -- -   @DRAINING@: Indicates the tasks in the task set are being stopped
    --     and their corresponding targets are being deregistered from their
    --     target group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The target group associated with the task set. The target group is used
    -- by CodeDeploy to manage traffic to a task set.
    targetGroup :: Prelude.Maybe TargetGroupInfo,
    -- | The number of tasks in a task set. During a deployment that uses the
    -- Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new
    -- task set and uses this value to determine how many tasks to create.
    -- After the updated task set is created, CodeDeploy shifts traffic to the
    -- new task set.
    desiredCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of tasks in the task set that are in the @PENDING@ status
    -- during an Amazon ECS deployment. A task in the @PENDING@ state is
    -- preparing to enter the @RUNNING@ state. A task set enters the @PENDING@
    -- status when it launches for the first time, or when it is restarted
    -- after being in the @STOPPED@ state.
    pendingCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of tasks in the task set that are in the @RUNNING@ status
    -- during an Amazon ECS deployment. A task in the @RUNNING@ state is
    -- running and ready for use.
    runningCount :: Prelude.Maybe Prelude.Integer,
    -- | A label that identifies whether the ECS task set is an original target
    -- (@BLUE@) or a replacement target (@GREEN@).
    taskSetLabel :: Prelude.Maybe TargetLabel,
    -- | The percentage of traffic served by this task set.
    trafficWeight :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSTaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifer', 'eCSTaskSet_identifer' - A unique ID of an @ECSTaskSet@.
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
-- 'targetGroup', 'eCSTaskSet_targetGroup' - The target group associated with the task set. The target group is used
-- by CodeDeploy to manage traffic to a task set.
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
-- 'runningCount', 'eCSTaskSet_runningCount' - The number of tasks in the task set that are in the @RUNNING@ status
-- during an Amazon ECS deployment. A task in the @RUNNING@ state is
-- running and ready for use.
--
-- 'taskSetLabel', 'eCSTaskSet_taskSetLabel' - A label that identifies whether the ECS task set is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
--
-- 'trafficWeight', 'eCSTaskSet_trafficWeight' - The percentage of traffic served by this task set.
newECSTaskSet ::
  ECSTaskSet
newECSTaskSet =
  ECSTaskSet'
    { identifer = Prelude.Nothing,
      status = Prelude.Nothing,
      targetGroup = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      pendingCount = Prelude.Nothing,
      runningCount = Prelude.Nothing,
      taskSetLabel = Prelude.Nothing,
      trafficWeight = Prelude.Nothing
    }

-- | A unique ID of an @ECSTaskSet@.
eCSTaskSet_identifer :: Lens.Lens' ECSTaskSet (Prelude.Maybe Prelude.Text)
eCSTaskSet_identifer = Lens.lens (\ECSTaskSet' {identifer} -> identifer) (\s@ECSTaskSet' {} a -> s {identifer = a} :: ECSTaskSet)

-- | The status of the task set. There are three valid task set statuses:
--
-- -   @PRIMARY@: Indicates the task set is serving production traffic.
--
-- -   @ACTIVE@: Indicates the task set is not serving production traffic.
--
-- -   @DRAINING@: Indicates the tasks in the task set are being stopped
--     and their corresponding targets are being deregistered from their
--     target group.
eCSTaskSet_status :: Lens.Lens' ECSTaskSet (Prelude.Maybe Prelude.Text)
eCSTaskSet_status = Lens.lens (\ECSTaskSet' {status} -> status) (\s@ECSTaskSet' {} a -> s {status = a} :: ECSTaskSet)

-- | The target group associated with the task set. The target group is used
-- by CodeDeploy to manage traffic to a task set.
eCSTaskSet_targetGroup :: Lens.Lens' ECSTaskSet (Prelude.Maybe TargetGroupInfo)
eCSTaskSet_targetGroup = Lens.lens (\ECSTaskSet' {targetGroup} -> targetGroup) (\s@ECSTaskSet' {} a -> s {targetGroup = a} :: ECSTaskSet)

-- | The number of tasks in a task set. During a deployment that uses the
-- Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new
-- task set and uses this value to determine how many tasks to create.
-- After the updated task set is created, CodeDeploy shifts traffic to the
-- new task set.
eCSTaskSet_desiredCount :: Lens.Lens' ECSTaskSet (Prelude.Maybe Prelude.Integer)
eCSTaskSet_desiredCount = Lens.lens (\ECSTaskSet' {desiredCount} -> desiredCount) (\s@ECSTaskSet' {} a -> s {desiredCount = a} :: ECSTaskSet)

-- | The number of tasks in the task set that are in the @PENDING@ status
-- during an Amazon ECS deployment. A task in the @PENDING@ state is
-- preparing to enter the @RUNNING@ state. A task set enters the @PENDING@
-- status when it launches for the first time, or when it is restarted
-- after being in the @STOPPED@ state.
eCSTaskSet_pendingCount :: Lens.Lens' ECSTaskSet (Prelude.Maybe Prelude.Integer)
eCSTaskSet_pendingCount = Lens.lens (\ECSTaskSet' {pendingCount} -> pendingCount) (\s@ECSTaskSet' {} a -> s {pendingCount = a} :: ECSTaskSet)

-- | The number of tasks in the task set that are in the @RUNNING@ status
-- during an Amazon ECS deployment. A task in the @RUNNING@ state is
-- running and ready for use.
eCSTaskSet_runningCount :: Lens.Lens' ECSTaskSet (Prelude.Maybe Prelude.Integer)
eCSTaskSet_runningCount = Lens.lens (\ECSTaskSet' {runningCount} -> runningCount) (\s@ECSTaskSet' {} a -> s {runningCount = a} :: ECSTaskSet)

-- | A label that identifies whether the ECS task set is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
eCSTaskSet_taskSetLabel :: Lens.Lens' ECSTaskSet (Prelude.Maybe TargetLabel)
eCSTaskSet_taskSetLabel = Lens.lens (\ECSTaskSet' {taskSetLabel} -> taskSetLabel) (\s@ECSTaskSet' {} a -> s {taskSetLabel = a} :: ECSTaskSet)

-- | The percentage of traffic served by this task set.
eCSTaskSet_trafficWeight :: Lens.Lens' ECSTaskSet (Prelude.Maybe Prelude.Double)
eCSTaskSet_trafficWeight = Lens.lens (\ECSTaskSet' {trafficWeight} -> trafficWeight) (\s@ECSTaskSet' {} a -> s {trafficWeight = a} :: ECSTaskSet)

instance Data.FromJSON ECSTaskSet where
  parseJSON =
    Data.withObject
      "ECSTaskSet"
      ( \x ->
          ECSTaskSet'
            Prelude.<$> (x Data..:? "identifer")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "targetGroup")
            Prelude.<*> (x Data..:? "desiredCount")
            Prelude.<*> (x Data..:? "pendingCount")
            Prelude.<*> (x Data..:? "runningCount")
            Prelude.<*> (x Data..:? "taskSetLabel")
            Prelude.<*> (x Data..:? "trafficWeight")
      )

instance Prelude.Hashable ECSTaskSet where
  hashWithSalt _salt ECSTaskSet' {..} =
    _salt `Prelude.hashWithSalt` identifer
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetGroup
      `Prelude.hashWithSalt` desiredCount
      `Prelude.hashWithSalt` pendingCount
      `Prelude.hashWithSalt` runningCount
      `Prelude.hashWithSalt` taskSetLabel
      `Prelude.hashWithSalt` trafficWeight

instance Prelude.NFData ECSTaskSet where
  rnf ECSTaskSet' {..} =
    Prelude.rnf identifer
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetGroup
      `Prelude.seq` Prelude.rnf desiredCount
      `Prelude.seq` Prelude.rnf pendingCount
      `Prelude.seq` Prelude.rnf runningCount
      `Prelude.seq` Prelude.rnf taskSetLabel
      `Prelude.seq` Prelude.rnf trafficWeight
