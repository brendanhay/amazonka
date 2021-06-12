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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentTarget where

import Network.AWS.CodeDeploy.Types.CloudFormationTarget
import Network.AWS.CodeDeploy.Types.DeploymentTargetType
import Network.AWS.CodeDeploy.Types.ECSTarget
import Network.AWS.CodeDeploy.Types.InstanceTarget
import Network.AWS.CodeDeploy.Types.LambdaTarget
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the deployment target.
--
-- /See:/ 'newDeploymentTarget' smart constructor.
data DeploymentTarget = DeploymentTarget'
  { -- | Information about the target for a deployment that uses the Amazon ECS
    -- compute platform.
    ecsTarget :: Core.Maybe ECSTarget,
    -- | Information about the target for a deployment that uses the AWS Lambda
    -- compute platform.
    lambdaTarget :: Core.Maybe LambdaTarget,
    cloudFormationTarget :: Core.Maybe CloudFormationTarget,
    -- | Information about the target for a deployment that uses the
    -- EC2\/On-premises compute platform.
    instanceTarget :: Core.Maybe InstanceTarget,
    -- | The deployment type that is specific to the deployment\'s compute
    -- platform or deployments initiated by a CloudFormation stack update.
    deploymentTargetType :: Core.Maybe DeploymentTargetType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeploymentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecsTarget', 'deploymentTarget_ecsTarget' - Information about the target for a deployment that uses the Amazon ECS
-- compute platform.
--
-- 'lambdaTarget', 'deploymentTarget_lambdaTarget' - Information about the target for a deployment that uses the AWS Lambda
-- compute platform.
--
-- 'cloudFormationTarget', 'deploymentTarget_cloudFormationTarget' - Undocumented member.
--
-- 'instanceTarget', 'deploymentTarget_instanceTarget' - Information about the target for a deployment that uses the
-- EC2\/On-premises compute platform.
--
-- 'deploymentTargetType', 'deploymentTarget_deploymentTargetType' - The deployment type that is specific to the deployment\'s compute
-- platform or deployments initiated by a CloudFormation stack update.
newDeploymentTarget ::
  DeploymentTarget
newDeploymentTarget =
  DeploymentTarget'
    { ecsTarget = Core.Nothing,
      lambdaTarget = Core.Nothing,
      cloudFormationTarget = Core.Nothing,
      instanceTarget = Core.Nothing,
      deploymentTargetType = Core.Nothing
    }

-- | Information about the target for a deployment that uses the Amazon ECS
-- compute platform.
deploymentTarget_ecsTarget :: Lens.Lens' DeploymentTarget (Core.Maybe ECSTarget)
deploymentTarget_ecsTarget = Lens.lens (\DeploymentTarget' {ecsTarget} -> ecsTarget) (\s@DeploymentTarget' {} a -> s {ecsTarget = a} :: DeploymentTarget)

-- | Information about the target for a deployment that uses the AWS Lambda
-- compute platform.
deploymentTarget_lambdaTarget :: Lens.Lens' DeploymentTarget (Core.Maybe LambdaTarget)
deploymentTarget_lambdaTarget = Lens.lens (\DeploymentTarget' {lambdaTarget} -> lambdaTarget) (\s@DeploymentTarget' {} a -> s {lambdaTarget = a} :: DeploymentTarget)

-- | Undocumented member.
deploymentTarget_cloudFormationTarget :: Lens.Lens' DeploymentTarget (Core.Maybe CloudFormationTarget)
deploymentTarget_cloudFormationTarget = Lens.lens (\DeploymentTarget' {cloudFormationTarget} -> cloudFormationTarget) (\s@DeploymentTarget' {} a -> s {cloudFormationTarget = a} :: DeploymentTarget)

-- | Information about the target for a deployment that uses the
-- EC2\/On-premises compute platform.
deploymentTarget_instanceTarget :: Lens.Lens' DeploymentTarget (Core.Maybe InstanceTarget)
deploymentTarget_instanceTarget = Lens.lens (\DeploymentTarget' {instanceTarget} -> instanceTarget) (\s@DeploymentTarget' {} a -> s {instanceTarget = a} :: DeploymentTarget)

-- | The deployment type that is specific to the deployment\'s compute
-- platform or deployments initiated by a CloudFormation stack update.
deploymentTarget_deploymentTargetType :: Lens.Lens' DeploymentTarget (Core.Maybe DeploymentTargetType)
deploymentTarget_deploymentTargetType = Lens.lens (\DeploymentTarget' {deploymentTargetType} -> deploymentTargetType) (\s@DeploymentTarget' {} a -> s {deploymentTargetType = a} :: DeploymentTarget)

instance Core.FromJSON DeploymentTarget where
  parseJSON =
    Core.withObject
      "DeploymentTarget"
      ( \x ->
          DeploymentTarget'
            Core.<$> (x Core..:? "ecsTarget")
            Core.<*> (x Core..:? "lambdaTarget")
            Core.<*> (x Core..:? "cloudFormationTarget")
            Core.<*> (x Core..:? "instanceTarget")
            Core.<*> (x Core..:? "deploymentTargetType")
      )

instance Core.Hashable DeploymentTarget

instance Core.NFData DeploymentTarget
