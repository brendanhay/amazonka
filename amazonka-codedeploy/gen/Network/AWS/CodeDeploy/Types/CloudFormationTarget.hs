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
-- Module      : Network.AWS.CodeDeploy.Types.CloudFormationTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.CloudFormationTarget where

import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the target to be updated by an AWS CloudFormation
-- blue\/green deployment. This target type is used for all deployments
-- initiated by a CloudFormation stack update.
--
-- /See:/ 'newCloudFormationTarget' smart constructor.
data CloudFormationTarget = CloudFormationTarget'
  { -- | The unique ID of an AWS CloudFormation blue\/green deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The status of an AWS CloudFormation blue\/green deployment\'s target
    -- application.
    status :: Core.Maybe TargetStatus,
    -- | The unique ID of a deployment target that has a type
    -- of @CloudFormationTarget@.
    targetId :: Core.Maybe Core.Text,
    -- | The percentage of production traffic that the target version of an AWS
    -- CloudFormation blue\/green deployment receives.
    targetVersionWeight :: Core.Maybe Core.Double,
    -- | The resource type for the AWS CloudFormation blue\/green deployment.
    resourceType :: Core.Maybe Core.Text,
    -- | The lifecycle events of the AWS CloudFormation blue\/green deployment to
    -- this target application.
    lifecycleEvents :: Core.Maybe [LifecycleEvent],
    -- | The date and time when the target application was updated by an AWS
    -- CloudFormation blue\/green deployment.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudFormationTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'cloudFormationTarget_deploymentId' - The unique ID of an AWS CloudFormation blue\/green deployment.
--
-- 'status', 'cloudFormationTarget_status' - The status of an AWS CloudFormation blue\/green deployment\'s target
-- application.
--
-- 'targetId', 'cloudFormationTarget_targetId' - The unique ID of a deployment target that has a type
-- of @CloudFormationTarget@.
--
-- 'targetVersionWeight', 'cloudFormationTarget_targetVersionWeight' - The percentage of production traffic that the target version of an AWS
-- CloudFormation blue\/green deployment receives.
--
-- 'resourceType', 'cloudFormationTarget_resourceType' - The resource type for the AWS CloudFormation blue\/green deployment.
--
-- 'lifecycleEvents', 'cloudFormationTarget_lifecycleEvents' - The lifecycle events of the AWS CloudFormation blue\/green deployment to
-- this target application.
--
-- 'lastUpdatedAt', 'cloudFormationTarget_lastUpdatedAt' - The date and time when the target application was updated by an AWS
-- CloudFormation blue\/green deployment.
newCloudFormationTarget ::
  CloudFormationTarget
newCloudFormationTarget =
  CloudFormationTarget'
    { deploymentId = Core.Nothing,
      status = Core.Nothing,
      targetId = Core.Nothing,
      targetVersionWeight = Core.Nothing,
      resourceType = Core.Nothing,
      lifecycleEvents = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The unique ID of an AWS CloudFormation blue\/green deployment.
cloudFormationTarget_deploymentId :: Lens.Lens' CloudFormationTarget (Core.Maybe Core.Text)
cloudFormationTarget_deploymentId = Lens.lens (\CloudFormationTarget' {deploymentId} -> deploymentId) (\s@CloudFormationTarget' {} a -> s {deploymentId = a} :: CloudFormationTarget)

-- | The status of an AWS CloudFormation blue\/green deployment\'s target
-- application.
cloudFormationTarget_status :: Lens.Lens' CloudFormationTarget (Core.Maybe TargetStatus)
cloudFormationTarget_status = Lens.lens (\CloudFormationTarget' {status} -> status) (\s@CloudFormationTarget' {} a -> s {status = a} :: CloudFormationTarget)

-- | The unique ID of a deployment target that has a type
-- of @CloudFormationTarget@.
cloudFormationTarget_targetId :: Lens.Lens' CloudFormationTarget (Core.Maybe Core.Text)
cloudFormationTarget_targetId = Lens.lens (\CloudFormationTarget' {targetId} -> targetId) (\s@CloudFormationTarget' {} a -> s {targetId = a} :: CloudFormationTarget)

-- | The percentage of production traffic that the target version of an AWS
-- CloudFormation blue\/green deployment receives.
cloudFormationTarget_targetVersionWeight :: Lens.Lens' CloudFormationTarget (Core.Maybe Core.Double)
cloudFormationTarget_targetVersionWeight = Lens.lens (\CloudFormationTarget' {targetVersionWeight} -> targetVersionWeight) (\s@CloudFormationTarget' {} a -> s {targetVersionWeight = a} :: CloudFormationTarget)

-- | The resource type for the AWS CloudFormation blue\/green deployment.
cloudFormationTarget_resourceType :: Lens.Lens' CloudFormationTarget (Core.Maybe Core.Text)
cloudFormationTarget_resourceType = Lens.lens (\CloudFormationTarget' {resourceType} -> resourceType) (\s@CloudFormationTarget' {} a -> s {resourceType = a} :: CloudFormationTarget)

-- | The lifecycle events of the AWS CloudFormation blue\/green deployment to
-- this target application.
cloudFormationTarget_lifecycleEvents :: Lens.Lens' CloudFormationTarget (Core.Maybe [LifecycleEvent])
cloudFormationTarget_lifecycleEvents = Lens.lens (\CloudFormationTarget' {lifecycleEvents} -> lifecycleEvents) (\s@CloudFormationTarget' {} a -> s {lifecycleEvents = a} :: CloudFormationTarget) Core.. Lens.mapping Lens._Coerce

-- | The date and time when the target application was updated by an AWS
-- CloudFormation blue\/green deployment.
cloudFormationTarget_lastUpdatedAt :: Lens.Lens' CloudFormationTarget (Core.Maybe Core.UTCTime)
cloudFormationTarget_lastUpdatedAt = Lens.lens (\CloudFormationTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@CloudFormationTarget' {} a -> s {lastUpdatedAt = a} :: CloudFormationTarget) Core.. Lens.mapping Core._Time

instance Core.FromJSON CloudFormationTarget where
  parseJSON =
    Core.withObject
      "CloudFormationTarget"
      ( \x ->
          CloudFormationTarget'
            Core.<$> (x Core..:? "deploymentId")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "targetId")
            Core.<*> (x Core..:? "targetVersionWeight")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "lifecycleEvents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "lastUpdatedAt")
      )

instance Core.Hashable CloudFormationTarget

instance Core.NFData CloudFormationTarget
