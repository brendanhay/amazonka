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
-- Module      : Amazonka.ECS.Types.DeploymentAlarms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DeploymentAlarms where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | One of the methods which provide a way for you to quickly identify when
-- a deployment has failed, and then to optionally roll back the failure to
-- the last working deployment.
--
-- When the alarms are generated, Amazon ECS sets the service deployment to
-- failed. Set the rollback parameter to have Amazon ECS to roll back your
-- service to the last completed deployment after a failure.
--
-- You can only use the @DeploymentAlarms@ method to detect failures when
-- the @DeploymentController@ is set to @ECS@ (rolling update).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update>
-- in the //Amazon Elastic Container Service Developer Guide// .
--
-- /See:/ 'newDeploymentAlarms' smart constructor.
data DeploymentAlarms = DeploymentAlarms'
  { -- | One or more CloudWatch alarm names. Use a \",\" to separate the alarms.
    alarmNames :: [Prelude.Text],
    -- | Determines whether to use the CloudWatch alarm option in the service
    -- deployment process.
    enable :: Prelude.Bool,
    -- | Determines whether to configure Amazon ECS to roll back the service if a
    -- service deployment fails. If rollback is used, when a service deployment
    -- fails, the service is rolled back to the last deployment that completed
    -- successfully.
    rollback :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentAlarms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmNames', 'deploymentAlarms_alarmNames' - One or more CloudWatch alarm names. Use a \",\" to separate the alarms.
--
-- 'enable', 'deploymentAlarms_enable' - Determines whether to use the CloudWatch alarm option in the service
-- deployment process.
--
-- 'rollback', 'deploymentAlarms_rollback' - Determines whether to configure Amazon ECS to roll back the service if a
-- service deployment fails. If rollback is used, when a service deployment
-- fails, the service is rolled back to the last deployment that completed
-- successfully.
newDeploymentAlarms ::
  -- | 'enable'
  Prelude.Bool ->
  -- | 'rollback'
  Prelude.Bool ->
  DeploymentAlarms
newDeploymentAlarms pEnable_ pRollback_ =
  DeploymentAlarms'
    { alarmNames = Prelude.mempty,
      enable = pEnable_,
      rollback = pRollback_
    }

-- | One or more CloudWatch alarm names. Use a \",\" to separate the alarms.
deploymentAlarms_alarmNames :: Lens.Lens' DeploymentAlarms [Prelude.Text]
deploymentAlarms_alarmNames = Lens.lens (\DeploymentAlarms' {alarmNames} -> alarmNames) (\s@DeploymentAlarms' {} a -> s {alarmNames = a} :: DeploymentAlarms) Prelude.. Lens.coerced

-- | Determines whether to use the CloudWatch alarm option in the service
-- deployment process.
deploymentAlarms_enable :: Lens.Lens' DeploymentAlarms Prelude.Bool
deploymentAlarms_enable = Lens.lens (\DeploymentAlarms' {enable} -> enable) (\s@DeploymentAlarms' {} a -> s {enable = a} :: DeploymentAlarms)

-- | Determines whether to configure Amazon ECS to roll back the service if a
-- service deployment fails. If rollback is used, when a service deployment
-- fails, the service is rolled back to the last deployment that completed
-- successfully.
deploymentAlarms_rollback :: Lens.Lens' DeploymentAlarms Prelude.Bool
deploymentAlarms_rollback = Lens.lens (\DeploymentAlarms' {rollback} -> rollback) (\s@DeploymentAlarms' {} a -> s {rollback = a} :: DeploymentAlarms)

instance Data.FromJSON DeploymentAlarms where
  parseJSON =
    Data.withObject
      "DeploymentAlarms"
      ( \x ->
          DeploymentAlarms'
            Prelude.<$> (x Data..:? "alarmNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "enable")
            Prelude.<*> (x Data..: "rollback")
      )

instance Prelude.Hashable DeploymentAlarms where
  hashWithSalt _salt DeploymentAlarms' {..} =
    _salt
      `Prelude.hashWithSalt` alarmNames
      `Prelude.hashWithSalt` enable
      `Prelude.hashWithSalt` rollback

instance Prelude.NFData DeploymentAlarms where
  rnf DeploymentAlarms' {..} =
    Prelude.rnf alarmNames `Prelude.seq`
      Prelude.rnf enable `Prelude.seq`
        Prelude.rnf rollback

instance Data.ToJSON DeploymentAlarms where
  toJSON DeploymentAlarms' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("alarmNames" Data..= alarmNames),
            Prelude.Just ("enable" Data..= enable),
            Prelude.Just ("rollback" Data..= rollback)
          ]
      )
