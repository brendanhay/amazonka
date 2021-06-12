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
-- Module      : Network.AWS.CodeDeploy.Types.ECSTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSTarget where

import Network.AWS.CodeDeploy.Types.ECSTaskSet
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the target of an Amazon ECS deployment.
--
-- /See:/ 'newECSTarget' smart constructor.
data ECSTarget = ECSTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The status an Amazon ECS deployment\'s target ECS application.
    status :: Core.Maybe TargetStatus,
    -- | The unique ID of a deployment target that has a type of @ecsTarget@.
    targetId :: Core.Maybe Core.Text,
    -- | The @ECSTaskSet@ objects associated with the ECS target.
    taskSetsInfo :: Core.Maybe [ECSTaskSet],
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Core.Maybe Core.Text,
    -- | The lifecycle events of the deployment to this target Amazon ECS
    -- application.
    lifecycleEvents :: Core.Maybe [LifecycleEvent],
    -- | The date and time when the target Amazon ECS application was updated by
    -- a deployment.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'status', 'eCSTarget_status' - The status an Amazon ECS deployment\'s target ECS application.
--
-- 'targetId', 'eCSTarget_targetId' - The unique ID of a deployment target that has a type of @ecsTarget@.
--
-- 'taskSetsInfo', 'eCSTarget_taskSetsInfo' - The @ECSTaskSet@ objects associated with the ECS target.
--
-- 'targetArn', 'eCSTarget_targetArn' - The Amazon Resource Name (ARN) of the target.
--
-- 'lifecycleEvents', 'eCSTarget_lifecycleEvents' - The lifecycle events of the deployment to this target Amazon ECS
-- application.
--
-- 'lastUpdatedAt', 'eCSTarget_lastUpdatedAt' - The date and time when the target Amazon ECS application was updated by
-- a deployment.
newECSTarget ::
  ECSTarget
newECSTarget =
  ECSTarget'
    { deploymentId = Core.Nothing,
      status = Core.Nothing,
      targetId = Core.Nothing,
      taskSetsInfo = Core.Nothing,
      targetArn = Core.Nothing,
      lifecycleEvents = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The unique ID of a deployment.
eCSTarget_deploymentId :: Lens.Lens' ECSTarget (Core.Maybe Core.Text)
eCSTarget_deploymentId = Lens.lens (\ECSTarget' {deploymentId} -> deploymentId) (\s@ECSTarget' {} a -> s {deploymentId = a} :: ECSTarget)

-- | The status an Amazon ECS deployment\'s target ECS application.
eCSTarget_status :: Lens.Lens' ECSTarget (Core.Maybe TargetStatus)
eCSTarget_status = Lens.lens (\ECSTarget' {status} -> status) (\s@ECSTarget' {} a -> s {status = a} :: ECSTarget)

-- | The unique ID of a deployment target that has a type of @ecsTarget@.
eCSTarget_targetId :: Lens.Lens' ECSTarget (Core.Maybe Core.Text)
eCSTarget_targetId = Lens.lens (\ECSTarget' {targetId} -> targetId) (\s@ECSTarget' {} a -> s {targetId = a} :: ECSTarget)

-- | The @ECSTaskSet@ objects associated with the ECS target.
eCSTarget_taskSetsInfo :: Lens.Lens' ECSTarget (Core.Maybe [ECSTaskSet])
eCSTarget_taskSetsInfo = Lens.lens (\ECSTarget' {taskSetsInfo} -> taskSetsInfo) (\s@ECSTarget' {} a -> s {taskSetsInfo = a} :: ECSTarget) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the target.
eCSTarget_targetArn :: Lens.Lens' ECSTarget (Core.Maybe Core.Text)
eCSTarget_targetArn = Lens.lens (\ECSTarget' {targetArn} -> targetArn) (\s@ECSTarget' {} a -> s {targetArn = a} :: ECSTarget)

-- | The lifecycle events of the deployment to this target Amazon ECS
-- application.
eCSTarget_lifecycleEvents :: Lens.Lens' ECSTarget (Core.Maybe [LifecycleEvent])
eCSTarget_lifecycleEvents = Lens.lens (\ECSTarget' {lifecycleEvents} -> lifecycleEvents) (\s@ECSTarget' {} a -> s {lifecycleEvents = a} :: ECSTarget) Core.. Lens.mapping Lens._Coerce

-- | The date and time when the target Amazon ECS application was updated by
-- a deployment.
eCSTarget_lastUpdatedAt :: Lens.Lens' ECSTarget (Core.Maybe Core.UTCTime)
eCSTarget_lastUpdatedAt = Lens.lens (\ECSTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@ECSTarget' {} a -> s {lastUpdatedAt = a} :: ECSTarget) Core.. Lens.mapping Core._Time

instance Core.FromJSON ECSTarget where
  parseJSON =
    Core.withObject
      "ECSTarget"
      ( \x ->
          ECSTarget'
            Core.<$> (x Core..:? "deploymentId")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "targetId")
            Core.<*> (x Core..:? "taskSetsInfo" Core..!= Core.mempty)
            Core.<*> (x Core..:? "targetArn")
            Core.<*> (x Core..:? "lifecycleEvents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "lastUpdatedAt")
      )

instance Core.Hashable ECSTarget

instance Core.NFData ECSTarget
