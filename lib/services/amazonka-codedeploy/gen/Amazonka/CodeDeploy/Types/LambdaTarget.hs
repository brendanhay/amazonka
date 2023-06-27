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
-- Module      : Amazonka.CodeDeploy.Types.LambdaTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.LambdaTarget where

import Amazonka.CodeDeploy.Types.LambdaFunctionInfo
import Amazonka.CodeDeploy.Types.LifecycleEvent
import Amazonka.CodeDeploy.Types.TargetStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the target Lambda function during an Lambda
-- deployment.
--
-- /See:/ 'newLambdaTarget' smart constructor.
data LambdaTarget = LambdaTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
    lambdaFunctionInfo :: Prelude.Maybe LambdaFunctionInfo,
    -- | The date and time when the target Lambda function was updated by a
    -- deployment.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The lifecycle events of the deployment to this target Lambda function.
    lifecycleEvents :: Prelude.Maybe [LifecycleEvent],
    -- | The status an Lambda deployment\'s target Lambda function.
    status :: Prelude.Maybe TargetStatus,
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of a deployment target that has a type of @lambdaTarget@.
    targetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lambdaFunctionInfo', 'lambdaTarget_lambdaFunctionInfo' - A @LambdaFunctionInfo@ object that describes a target Lambda function.
--
-- 'lastUpdatedAt', 'lambdaTarget_lastUpdatedAt' - The date and time when the target Lambda function was updated by a
-- deployment.
--
-- 'lifecycleEvents', 'lambdaTarget_lifecycleEvents' - The lifecycle events of the deployment to this target Lambda function.
--
-- 'status', 'lambdaTarget_status' - The status an Lambda deployment\'s target Lambda function.
--
-- 'targetArn', 'lambdaTarget_targetArn' - The Amazon Resource Name (ARN) of the target.
--
-- 'targetId', 'lambdaTarget_targetId' - The unique ID of a deployment target that has a type of @lambdaTarget@.
newLambdaTarget ::
  LambdaTarget
newLambdaTarget =
  LambdaTarget'
    { deploymentId = Prelude.Nothing,
      lambdaFunctionInfo = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      lifecycleEvents = Prelude.Nothing,
      status = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      targetId = Prelude.Nothing
    }

-- | The unique ID of a deployment.
lambdaTarget_deploymentId :: Lens.Lens' LambdaTarget (Prelude.Maybe Prelude.Text)
lambdaTarget_deploymentId = Lens.lens (\LambdaTarget' {deploymentId} -> deploymentId) (\s@LambdaTarget' {} a -> s {deploymentId = a} :: LambdaTarget)

-- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
lambdaTarget_lambdaFunctionInfo :: Lens.Lens' LambdaTarget (Prelude.Maybe LambdaFunctionInfo)
lambdaTarget_lambdaFunctionInfo = Lens.lens (\LambdaTarget' {lambdaFunctionInfo} -> lambdaFunctionInfo) (\s@LambdaTarget' {} a -> s {lambdaFunctionInfo = a} :: LambdaTarget)

-- | The date and time when the target Lambda function was updated by a
-- deployment.
lambdaTarget_lastUpdatedAt :: Lens.Lens' LambdaTarget (Prelude.Maybe Prelude.UTCTime)
lambdaTarget_lastUpdatedAt = Lens.lens (\LambdaTarget' {lastUpdatedAt} -> lastUpdatedAt) (\s@LambdaTarget' {} a -> s {lastUpdatedAt = a} :: LambdaTarget) Prelude.. Lens.mapping Data._Time

-- | The lifecycle events of the deployment to this target Lambda function.
lambdaTarget_lifecycleEvents :: Lens.Lens' LambdaTarget (Prelude.Maybe [LifecycleEvent])
lambdaTarget_lifecycleEvents = Lens.lens (\LambdaTarget' {lifecycleEvents} -> lifecycleEvents) (\s@LambdaTarget' {} a -> s {lifecycleEvents = a} :: LambdaTarget) Prelude.. Lens.mapping Lens.coerced

-- | The status an Lambda deployment\'s target Lambda function.
lambdaTarget_status :: Lens.Lens' LambdaTarget (Prelude.Maybe TargetStatus)
lambdaTarget_status = Lens.lens (\LambdaTarget' {status} -> status) (\s@LambdaTarget' {} a -> s {status = a} :: LambdaTarget)

-- | The Amazon Resource Name (ARN) of the target.
lambdaTarget_targetArn :: Lens.Lens' LambdaTarget (Prelude.Maybe Prelude.Text)
lambdaTarget_targetArn = Lens.lens (\LambdaTarget' {targetArn} -> targetArn) (\s@LambdaTarget' {} a -> s {targetArn = a} :: LambdaTarget)

-- | The unique ID of a deployment target that has a type of @lambdaTarget@.
lambdaTarget_targetId :: Lens.Lens' LambdaTarget (Prelude.Maybe Prelude.Text)
lambdaTarget_targetId = Lens.lens (\LambdaTarget' {targetId} -> targetId) (\s@LambdaTarget' {} a -> s {targetId = a} :: LambdaTarget)

instance Data.FromJSON LambdaTarget where
  parseJSON =
    Data.withObject
      "LambdaTarget"
      ( \x ->
          LambdaTarget'
            Prelude.<$> (x Data..:? "deploymentId")
            Prelude.<*> (x Data..:? "lambdaFunctionInfo")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> ( x
                            Data..:? "lifecycleEvents"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "targetArn")
            Prelude.<*> (x Data..:? "targetId")
      )

instance Prelude.Hashable LambdaTarget where
  hashWithSalt _salt LambdaTarget' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` lambdaFunctionInfo
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` lifecycleEvents
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` targetId

instance Prelude.NFData LambdaTarget where
  rnf LambdaTarget' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf lambdaFunctionInfo
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf lifecycleEvents
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf targetId
