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
-- Module      : Network.AWS.CodeDeploy.Types.InstanceTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceTarget where

import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetLabel
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A target Amazon EC2 or on-premises instance during a deployment that
-- uses the EC2\/On-premises compute platform.
--
-- /See:/ 'newInstanceTarget' smart constructor.
data InstanceTarget = InstanceTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The status an EC2\/On-premises deployment\'s target instance.
    status :: Core.Maybe TargetStatus,
    -- | The unique ID of a deployment target that has a type of
    -- @instanceTarget@.
    targetId :: Core.Maybe Core.Text,
    -- | A label that identifies whether the instance is an original target
    -- (@BLUE@) or a replacement target (@GREEN@).
    instanceLabel :: Core.Maybe TargetLabel,
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Core.Maybe Core.Text,
    -- | The lifecycle events of the deployment to this target instance.
    lifecycleEvents :: Core.Maybe [LifecycleEvent],
    -- | The date and time when the target instance was updated by a deployment.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'instanceTarget_deploymentId' - The unique ID of a deployment.
--
-- 'status', 'instanceTarget_status' - The status an EC2\/On-premises deployment\'s target instance.
--
-- 'targetId', 'instanceTarget_targetId' - The unique ID of a deployment target that has a type of
-- @instanceTarget@.
--
-- 'instanceLabel', 'instanceTarget_instanceLabel' - A label that identifies whether the instance is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
--
-- 'targetArn', 'instanceTarget_targetArn' - The Amazon Resource Name (ARN) of the target.
--
-- 'lifecycleEvents', 'instanceTarget_lifecycleEvents' - The lifecycle events of the deployment to this target instance.
--
-- 'lastUpdatedAt', 'instanceTarget_lastUpdatedAt' - The date and time when the target instance was updated by a deployment.
newInstanceTarget ::
  InstanceTarget
newInstanceTarget =
  InstanceTarget'
    { deploymentId = Core.Nothing,
      status = Core.Nothing,
      targetId = Core.Nothing,
      instanceLabel = Core.Nothing,
      targetArn = Core.Nothing,
      lifecycleEvents = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The unique ID of a deployment.
instanceTarget_deploymentId :: Lens.Lens' InstanceTarget (Core.Maybe Core.Text)
instanceTarget_deploymentId = Lens.lens (\InstanceTarget' {deploymentId} -> deploymentId) (\s@InstanceTarget' {} a -> s {deploymentId = a} :: InstanceTarget)

-- | The status an EC2\/On-premises deployment\'s target instance.
instanceTarget_status :: Lens.Lens' InstanceTarget (Core.Maybe TargetStatus)
instanceTarget_status = Lens.lens (\InstanceTarget' {status} -> status) (\s@InstanceTarget' {} a -> s {status = a} :: InstanceTarget)

-- | The unique ID of a deployment target that has a type of
-- @instanceTarget@.
instanceTarget_targetId :: Lens.Lens' InstanceTarget (Core.Maybe Core.Text)
instanceTarget_targetId = Lens.lens (\InstanceTarget' {targetId} -> targetId) (\s@InstanceTarget' {} a -> s {targetId = a} :: InstanceTarget)

-- | A label that identifies whether the instance is an original target
-- (@BLUE@) or a replacement target (@GREEN@).
instanceTarget_instanceLabel :: Lens.Lens' InstanceTarget (Core.Maybe TargetLabel)
instanceTarget_instanceLabel = Lens.lens (\InstanceTarget' {instanceLabel} -> instanceLabel) (\s@InstanceTarget' {} a -> s {instanceLabel = a} :: InstanceTarget)

-- | The Amazon Resource Name (ARN) of the target.
instanceTarget_targetArn :: Lens.Lens' InstanceTarget (Core.Maybe Core.Text)
instanceTarget_targetArn = Lens.lens (\InstanceTarget' {targetArn} -> targetArn) (\s@InstanceTarget' {} a -> s {targetArn = a} :: InstanceTarget)

-- | The lifecycle events of the deployment to this target instance.
instanceTarget_lifecycleEvents :: Lens.Lens' InstanceTarget (Core.Maybe [LifecycleEvent])
instanceTarget_lifecycleEvents = Lens.lens (\InstanceTarget' {lifecycleEvents} -> lifecycleEvents) (\s@InstanceTarget' {} a -> s {lifecycleEvents = a} :: InstanceTarget) Core.. Lens.mapping Lens._Coerce

-- | The date and time when the target instance was updated by a deployment.
instanceTarget_lastUpdatedAt :: Lens.Lens' InstanceTarget (Core.Maybe Core.UTCTime)
instanceTarget_lastUpdatedAt = Lens.lens (\InstanceTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@InstanceTarget' {} a -> s {lastUpdatedAt = a} :: InstanceTarget) Core.. Lens.mapping Core._Time

instance Core.FromJSON InstanceTarget where
  parseJSON =
    Core.withObject
      "InstanceTarget"
      ( \x ->
          InstanceTarget'
            Core.<$> (x Core..:? "deploymentId")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "targetId")
            Core.<*> (x Core..:? "instanceLabel")
            Core.<*> (x Core..:? "targetArn")
            Core.<*> (x Core..:? "lifecycleEvents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "lastUpdatedAt")
      )

instance Core.Hashable InstanceTarget

instance Core.NFData InstanceTarget
