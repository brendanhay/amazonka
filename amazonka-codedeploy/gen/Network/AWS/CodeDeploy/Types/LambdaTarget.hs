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
-- Module      : Network.AWS.CodeDeploy.Types.LambdaTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LambdaTarget where

import Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the target AWS Lambda function during an AWS Lambda
-- deployment.
--
-- /See:/ 'newLambdaTarget' smart constructor.
data LambdaTarget = LambdaTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The status an AWS Lambda deployment\'s target Lambda function.
    status :: Core.Maybe TargetStatus,
    -- | The unique ID of a deployment target that has a type of @lambdaTarget@.
    targetId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Core.Maybe Core.Text,
    -- | The lifecycle events of the deployment to this target Lambda function.
    lifecycleEvents :: Core.Maybe [LifecycleEvent],
    -- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
    lambdaFunctionInfo :: Core.Maybe LambdaFunctionInfo,
    -- | The date and time when the target Lambda function was updated by a
    -- deployment.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LambdaTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'lambdaTarget_deploymentId' - The unique ID of a deployment.
--
-- 'status', 'lambdaTarget_status' - The status an AWS Lambda deployment\'s target Lambda function.
--
-- 'targetId', 'lambdaTarget_targetId' - The unique ID of a deployment target that has a type of @lambdaTarget@.
--
-- 'targetArn', 'lambdaTarget_targetArn' - The Amazon Resource Name (ARN) of the target.
--
-- 'lifecycleEvents', 'lambdaTarget_lifecycleEvents' - The lifecycle events of the deployment to this target Lambda function.
--
-- 'lambdaFunctionInfo', 'lambdaTarget_lambdaFunctionInfo' - A @LambdaFunctionInfo@ object that describes a target Lambda function.
--
-- 'lastUpdatedAt', 'lambdaTarget_lastUpdatedAt' - The date and time when the target Lambda function was updated by a
-- deployment.
newLambdaTarget ::
  LambdaTarget
newLambdaTarget =
  LambdaTarget'
    { deploymentId = Core.Nothing,
      status = Core.Nothing,
      targetId = Core.Nothing,
      targetArn = Core.Nothing,
      lifecycleEvents = Core.Nothing,
      lambdaFunctionInfo = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The unique ID of a deployment.
lambdaTarget_deploymentId :: Lens.Lens' LambdaTarget (Core.Maybe Core.Text)
lambdaTarget_deploymentId = Lens.lens (\LambdaTarget' {deploymentId} -> deploymentId) (\s@LambdaTarget' {} a -> s {deploymentId = a} :: LambdaTarget)

-- | The status an AWS Lambda deployment\'s target Lambda function.
lambdaTarget_status :: Lens.Lens' LambdaTarget (Core.Maybe TargetStatus)
lambdaTarget_status = Lens.lens (\LambdaTarget' {status} -> status) (\s@LambdaTarget' {} a -> s {status = a} :: LambdaTarget)

-- | The unique ID of a deployment target that has a type of @lambdaTarget@.
lambdaTarget_targetId :: Lens.Lens' LambdaTarget (Core.Maybe Core.Text)
lambdaTarget_targetId = Lens.lens (\LambdaTarget' {targetId} -> targetId) (\s@LambdaTarget' {} a -> s {targetId = a} :: LambdaTarget)

-- | The Amazon Resource Name (ARN) of the target.
lambdaTarget_targetArn :: Lens.Lens' LambdaTarget (Core.Maybe Core.Text)
lambdaTarget_targetArn = Lens.lens (\LambdaTarget' {targetArn} -> targetArn) (\s@LambdaTarget' {} a -> s {targetArn = a} :: LambdaTarget)

-- | The lifecycle events of the deployment to this target Lambda function.
lambdaTarget_lifecycleEvents :: Lens.Lens' LambdaTarget (Core.Maybe [LifecycleEvent])
lambdaTarget_lifecycleEvents = Lens.lens (\LambdaTarget' {lifecycleEvents} -> lifecycleEvents) (\s@LambdaTarget' {} a -> s {lifecycleEvents = a} :: LambdaTarget) Core.. Lens.mapping Lens._Coerce

-- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
lambdaTarget_lambdaFunctionInfo :: Lens.Lens' LambdaTarget (Core.Maybe LambdaFunctionInfo)
lambdaTarget_lambdaFunctionInfo = Lens.lens (\LambdaTarget' {lambdaFunctionInfo} -> lambdaFunctionInfo) (\s@LambdaTarget' {} a -> s {lambdaFunctionInfo = a} :: LambdaTarget)

-- | The date and time when the target Lambda function was updated by a
-- deployment.
lambdaTarget_lastUpdatedAt :: Lens.Lens' LambdaTarget (Core.Maybe Core.UTCTime)
lambdaTarget_lastUpdatedAt = Lens.lens (\LambdaTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@LambdaTarget' {} a -> s {lastUpdatedAt = a} :: LambdaTarget) Core.. Lens.mapping Core._Time

instance Core.FromJSON LambdaTarget where
  parseJSON =
    Core.withObject
      "LambdaTarget"
      ( \x ->
          LambdaTarget'
            Core.<$> (x Core..:? "deploymentId")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "targetId")
            Core.<*> (x Core..:? "targetArn")
            Core.<*> (x Core..:? "lifecycleEvents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "lambdaFunctionInfo")
            Core.<*> (x Core..:? "lastUpdatedAt")
      )

instance Core.Hashable LambdaTarget

instance Core.NFData LambdaTarget
