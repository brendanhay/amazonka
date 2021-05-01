{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the deployment target.
--
-- /See:/ 'newDeploymentTarget' smart constructor.
data DeploymentTarget = DeploymentTarget'
  { -- | Information about the target for a deployment that uses the Amazon ECS
    -- compute platform.
    ecsTarget :: Prelude.Maybe ECSTarget,
    -- | Information about the target for a deployment that uses the AWS Lambda
    -- compute platform.
    lambdaTarget :: Prelude.Maybe LambdaTarget,
    cloudFormationTarget :: Prelude.Maybe CloudFormationTarget,
    -- | Information about the target for a deployment that uses the
    -- EC2\/On-premises compute platform.
    instanceTarget :: Prelude.Maybe InstanceTarget,
    -- | The deployment type that is specific to the deployment\'s compute
    -- platform or deployments initiated by a CloudFormation stack update.
    deploymentTargetType :: Prelude.Maybe DeploymentTargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ecsTarget = Prelude.Nothing,
      lambdaTarget = Prelude.Nothing,
      cloudFormationTarget = Prelude.Nothing,
      instanceTarget = Prelude.Nothing,
      deploymentTargetType = Prelude.Nothing
    }

-- | Information about the target for a deployment that uses the Amazon ECS
-- compute platform.
deploymentTarget_ecsTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe ECSTarget)
deploymentTarget_ecsTarget = Lens.lens (\DeploymentTarget' {ecsTarget} -> ecsTarget) (\s@DeploymentTarget' {} a -> s {ecsTarget = a} :: DeploymentTarget)

-- | Information about the target for a deployment that uses the AWS Lambda
-- compute platform.
deploymentTarget_lambdaTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe LambdaTarget)
deploymentTarget_lambdaTarget = Lens.lens (\DeploymentTarget' {lambdaTarget} -> lambdaTarget) (\s@DeploymentTarget' {} a -> s {lambdaTarget = a} :: DeploymentTarget)

-- | Undocumented member.
deploymentTarget_cloudFormationTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe CloudFormationTarget)
deploymentTarget_cloudFormationTarget = Lens.lens (\DeploymentTarget' {cloudFormationTarget} -> cloudFormationTarget) (\s@DeploymentTarget' {} a -> s {cloudFormationTarget = a} :: DeploymentTarget)

-- | Information about the target for a deployment that uses the
-- EC2\/On-premises compute platform.
deploymentTarget_instanceTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe InstanceTarget)
deploymentTarget_instanceTarget = Lens.lens (\DeploymentTarget' {instanceTarget} -> instanceTarget) (\s@DeploymentTarget' {} a -> s {instanceTarget = a} :: DeploymentTarget)

-- | The deployment type that is specific to the deployment\'s compute
-- platform or deployments initiated by a CloudFormation stack update.
deploymentTarget_deploymentTargetType :: Lens.Lens' DeploymentTarget (Prelude.Maybe DeploymentTargetType)
deploymentTarget_deploymentTargetType = Lens.lens (\DeploymentTarget' {deploymentTargetType} -> deploymentTargetType) (\s@DeploymentTarget' {} a -> s {deploymentTargetType = a} :: DeploymentTarget)

instance Prelude.FromJSON DeploymentTarget where
  parseJSON =
    Prelude.withObject
      "DeploymentTarget"
      ( \x ->
          DeploymentTarget'
            Prelude.<$> (x Prelude..:? "ecsTarget")
            Prelude.<*> (x Prelude..:? "lambdaTarget")
            Prelude.<*> (x Prelude..:? "cloudFormationTarget")
            Prelude.<*> (x Prelude..:? "instanceTarget")
            Prelude.<*> (x Prelude..:? "deploymentTargetType")
      )

instance Prelude.Hashable DeploymentTarget

instance Prelude.NFData DeploymentTarget
