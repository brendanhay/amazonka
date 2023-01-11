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
-- Module      : Amazonka.CodeDeploy.Types.ECSTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.ECSTarget where

import Amazonka.CodeDeploy.Types.ECSTaskSet
import Amazonka.CodeDeploy.Types.LifecycleEvent
import Amazonka.CodeDeploy.Types.TargetStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the target of an Amazon ECS deployment.
--
-- /See:/ 'newECSTarget' smart constructor.
data ECSTarget = ECSTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the target Amazon ECS application was updated by
    -- a deployment.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The lifecycle events of the deployment to this target Amazon ECS
    -- application.
    lifecycleEvents :: Prelude.Maybe [LifecycleEvent],
    -- | The status an Amazon ECS deployment\'s target ECS application.
    status :: Prelude.Maybe TargetStatus,
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of a deployment target that has a type of @ecsTarget@.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The @ECSTaskSet@ objects associated with the ECS target.
    taskSetsInfo :: Prelude.Maybe [ECSTaskSet]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'eCSTarget_deploymentId' - The unique ID of a deployment.
--
-- 'lastUpdatedAt', 'eCSTarget_lastUpdatedAt' - The date and time when the target Amazon ECS application was updated by
-- a deployment.
--
-- 'lifecycleEvents', 'eCSTarget_lifecycleEvents' - The lifecycle events of the deployment to this target Amazon ECS
-- application.
--
-- 'status', 'eCSTarget_status' - The status an Amazon ECS deployment\'s target ECS application.
--
-- 'targetArn', 'eCSTarget_targetArn' - The Amazon Resource Name (ARN) of the target.
--
-- 'targetId', 'eCSTarget_targetId' - The unique ID of a deployment target that has a type of @ecsTarget@.
--
-- 'taskSetsInfo', 'eCSTarget_taskSetsInfo' - The @ECSTaskSet@ objects associated with the ECS target.
newECSTarget ::
  ECSTarget
newECSTarget =
  ECSTarget'
    { deploymentId = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      lifecycleEvents = Prelude.Nothing,
      status = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      targetId = Prelude.Nothing,
      taskSetsInfo = Prelude.Nothing
    }

-- | The unique ID of a deployment.
eCSTarget_deploymentId :: Lens.Lens' ECSTarget (Prelude.Maybe Prelude.Text)
eCSTarget_deploymentId = Lens.lens (\ECSTarget' {deploymentId} -> deploymentId) (\s@ECSTarget' {} a -> s {deploymentId = a} :: ECSTarget)

-- | The date and time when the target Amazon ECS application was updated by
-- a deployment.
eCSTarget_lastUpdatedAt :: Lens.Lens' ECSTarget (Prelude.Maybe Prelude.UTCTime)
eCSTarget_lastUpdatedAt = Lens.lens (\ECSTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@ECSTarget' {} a -> s {lastUpdatedAt = a} :: ECSTarget) Prelude.. Lens.mapping Data._Time

-- | The lifecycle events of the deployment to this target Amazon ECS
-- application.
eCSTarget_lifecycleEvents :: Lens.Lens' ECSTarget (Prelude.Maybe [LifecycleEvent])
eCSTarget_lifecycleEvents = Lens.lens (\ECSTarget' {lifecycleEvents} -> lifecycleEvents) (\s@ECSTarget' {} a -> s {lifecycleEvents = a} :: ECSTarget) Prelude.. Lens.mapping Lens.coerced

-- | The status an Amazon ECS deployment\'s target ECS application.
eCSTarget_status :: Lens.Lens' ECSTarget (Prelude.Maybe TargetStatus)
eCSTarget_status = Lens.lens (\ECSTarget' {status} -> status) (\s@ECSTarget' {} a -> s {status = a} :: ECSTarget)

-- | The Amazon Resource Name (ARN) of the target.
eCSTarget_targetArn :: Lens.Lens' ECSTarget (Prelude.Maybe Prelude.Text)
eCSTarget_targetArn = Lens.lens (\ECSTarget' {targetArn} -> targetArn) (\s@ECSTarget' {} a -> s {targetArn = a} :: ECSTarget)

-- | The unique ID of a deployment target that has a type of @ecsTarget@.
eCSTarget_targetId :: Lens.Lens' ECSTarget (Prelude.Maybe Prelude.Text)
eCSTarget_targetId = Lens.lens (\ECSTarget' {targetId} -> targetId) (\s@ECSTarget' {} a -> s {targetId = a} :: ECSTarget)

-- | The @ECSTaskSet@ objects associated with the ECS target.
eCSTarget_taskSetsInfo :: Lens.Lens' ECSTarget (Prelude.Maybe [ECSTaskSet])
eCSTarget_taskSetsInfo = Lens.lens (\ECSTarget' {taskSetsInfo} -> taskSetsInfo) (\s@ECSTarget' {} a -> s {taskSetsInfo = a} :: ECSTarget) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ECSTarget where
  parseJSON =
    Data.withObject
      "ECSTarget"
      ( \x ->
          ECSTarget'
            Prelude.<$> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> ( x Data..:? "lifecycleEvents"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "targetArn")
            Prelude.<*> (x Data..:? "targetId")
            Prelude.<*> (x Data..:? "taskSetsInfo" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ECSTarget where
  hashWithSalt _salt ECSTarget' {..} =
    _salt `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` lifecycleEvents
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` taskSetsInfo

instance Prelude.NFData ECSTarget where
  rnf ECSTarget' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf lifecycleEvents
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf taskSetsInfo
