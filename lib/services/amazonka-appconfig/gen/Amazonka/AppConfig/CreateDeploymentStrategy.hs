{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.CreateDeploymentStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment strategy that defines important criteria for
-- rolling out your configuration to the designated targets. A deployment
-- strategy includes the overall duration required, a percentage of targets
-- to receive the deployment during each interval, an algorithm that
-- defines how percentage grows, and bake time.
module Amazonka.AppConfig.CreateDeploymentStrategy
  ( -- * Creating a Request
    CreateDeploymentStrategy (..),
    newCreateDeploymentStrategy,

    -- * Request Lenses
    createDeploymentStrategy_description,
    createDeploymentStrategy_finalBakeTimeInMinutes,
    createDeploymentStrategy_growthType,
    createDeploymentStrategy_replicateTo,
    createDeploymentStrategy_tags,
    createDeploymentStrategy_name,
    createDeploymentStrategy_deploymentDurationInMinutes,
    createDeploymentStrategy_growthFactor,

    -- * Destructuring the Response
    DeploymentStrategy (..),
    newDeploymentStrategy,

    -- * Response Lenses
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_growthFactor,
    deploymentStrategy_growthType,
    deploymentStrategy_id,
    deploymentStrategy_name,
    deploymentStrategy_replicateTo,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDeploymentStrategy' smart constructor.
data CreateDeploymentStrategy = CreateDeploymentStrategy'
  { -- | A description of the deployment strategy.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the amount of time AppConfig monitors for Amazon CloudWatch
    -- alarms after the configuration has been deployed to 100% of its targets,
    -- before considering the deployment to be complete. If an alarm is
    -- triggered during this time, AppConfig rolls back the deployment. You
    -- must configure permissions for AppConfig to roll back based on
    -- CloudWatch alarms. For more information, see
    -- <https://docs.aws.amazon.com/appconfig/latest/userguide/getting-started-with-appconfig-cloudwatch-alarms-permissions.html Configuring permissions for rollback based on Amazon CloudWatch alarms>
    -- in the /AppConfig User Guide/.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The algorithm used to define how percentage grows over time. AppConfig
    -- supports the following growth types:
    --
    -- __Linear__: For this type, AppConfig processes the deployment by
    -- dividing the total number of targets by the value specified for
    -- @Step percentage@. For example, a linear deployment that uses a
    -- @Step percentage@ of 10 deploys the configuration to 10 percent of the
    -- hosts. After those deployments are complete, the system deploys the
    -- configuration to the next 10 percent. This continues until 100% of the
    -- targets have successfully received the configuration.
    --
    -- __Exponential__: For this type, AppConfig processes the deployment
    -- exponentially using the following formula: @G*(2^N)@. In this formula,
    -- @G@ is the growth factor specified by the user and @N@ is the number of
    -- steps until the configuration is deployed to all targets. For example,
    -- if you specify a growth factor of 2, then the system rolls out the
    -- configuration as follows:
    --
    -- @2*(2^0)@
    --
    -- @2*(2^1)@
    --
    -- @2*(2^2)@
    --
    -- Expressed numerically, the deployment rolls out as follows: 2% of the
    -- targets, 4% of the targets, 8% of the targets, and continues until the
    -- configuration has been deployed to all targets.
    growthType :: Prelude.Maybe GrowthType,
    -- | Save the deployment strategy to a Systems Manager (SSM) document.
    replicateTo :: Prelude.Maybe ReplicateTo,
    -- | Metadata to assign to the deployment strategy. Tags help organize and
    -- categorize your AppConfig resources. Each tag consists of a key and an
    -- optional value, both of which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the deployment strategy.
    name :: Prelude.Text,
    -- | Total amount of time for a deployment to last.
    deploymentDurationInMinutes :: Prelude.Natural,
    -- | The percentage of targets to receive a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDeploymentStrategy_description' - A description of the deployment strategy.
--
-- 'finalBakeTimeInMinutes', 'createDeploymentStrategy_finalBakeTimeInMinutes' - Specifies the amount of time AppConfig monitors for Amazon CloudWatch
-- alarms after the configuration has been deployed to 100% of its targets,
-- before considering the deployment to be complete. If an alarm is
-- triggered during this time, AppConfig rolls back the deployment. You
-- must configure permissions for AppConfig to roll back based on
-- CloudWatch alarms. For more information, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/getting-started-with-appconfig-cloudwatch-alarms-permissions.html Configuring permissions for rollback based on Amazon CloudWatch alarms>
-- in the /AppConfig User Guide/.
--
-- 'growthType', 'createDeploymentStrategy_growthType' - The algorithm used to define how percentage grows over time. AppConfig
-- supports the following growth types:
--
-- __Linear__: For this type, AppConfig processes the deployment by
-- dividing the total number of targets by the value specified for
-- @Step percentage@. For example, a linear deployment that uses a
-- @Step percentage@ of 10 deploys the configuration to 10 percent of the
-- hosts. After those deployments are complete, the system deploys the
-- configuration to the next 10 percent. This continues until 100% of the
-- targets have successfully received the configuration.
--
-- __Exponential__: For this type, AppConfig processes the deployment
-- exponentially using the following formula: @G*(2^N)@. In this formula,
-- @G@ is the growth factor specified by the user and @N@ is the number of
-- steps until the configuration is deployed to all targets. For example,
-- if you specify a growth factor of 2, then the system rolls out the
-- configuration as follows:
--
-- @2*(2^0)@
--
-- @2*(2^1)@
--
-- @2*(2^2)@
--
-- Expressed numerically, the deployment rolls out as follows: 2% of the
-- targets, 4% of the targets, 8% of the targets, and continues until the
-- configuration has been deployed to all targets.
--
-- 'replicateTo', 'createDeploymentStrategy_replicateTo' - Save the deployment strategy to a Systems Manager (SSM) document.
--
-- 'tags', 'createDeploymentStrategy_tags' - Metadata to assign to the deployment strategy. Tags help organize and
-- categorize your AppConfig resources. Each tag consists of a key and an
-- optional value, both of which you define.
--
-- 'name', 'createDeploymentStrategy_name' - A name for the deployment strategy.
--
-- 'deploymentDurationInMinutes', 'createDeploymentStrategy_deploymentDurationInMinutes' - Total amount of time for a deployment to last.
--
-- 'growthFactor', 'createDeploymentStrategy_growthFactor' - The percentage of targets to receive a deployed configuration during
-- each interval.
newCreateDeploymentStrategy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'deploymentDurationInMinutes'
  Prelude.Natural ->
  -- | 'growthFactor'
  Prelude.Double ->
  CreateDeploymentStrategy
newCreateDeploymentStrategy
  pName_
  pDeploymentDurationInMinutes_
  pGrowthFactor_ =
    CreateDeploymentStrategy'
      { description =
          Prelude.Nothing,
        finalBakeTimeInMinutes = Prelude.Nothing,
        growthType = Prelude.Nothing,
        replicateTo = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        deploymentDurationInMinutes =
          pDeploymentDurationInMinutes_,
        growthFactor = pGrowthFactor_
      }

-- | A description of the deployment strategy.
createDeploymentStrategy_description :: Lens.Lens' CreateDeploymentStrategy (Prelude.Maybe Prelude.Text)
createDeploymentStrategy_description = Lens.lens (\CreateDeploymentStrategy' {description} -> description) (\s@CreateDeploymentStrategy' {} a -> s {description = a} :: CreateDeploymentStrategy)

-- | Specifies the amount of time AppConfig monitors for Amazon CloudWatch
-- alarms after the configuration has been deployed to 100% of its targets,
-- before considering the deployment to be complete. If an alarm is
-- triggered during this time, AppConfig rolls back the deployment. You
-- must configure permissions for AppConfig to roll back based on
-- CloudWatch alarms. For more information, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/getting-started-with-appconfig-cloudwatch-alarms-permissions.html Configuring permissions for rollback based on Amazon CloudWatch alarms>
-- in the /AppConfig User Guide/.
createDeploymentStrategy_finalBakeTimeInMinutes :: Lens.Lens' CreateDeploymentStrategy (Prelude.Maybe Prelude.Natural)
createDeploymentStrategy_finalBakeTimeInMinutes = Lens.lens (\CreateDeploymentStrategy' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@CreateDeploymentStrategy' {} a -> s {finalBakeTimeInMinutes = a} :: CreateDeploymentStrategy)

-- | The algorithm used to define how percentage grows over time. AppConfig
-- supports the following growth types:
--
-- __Linear__: For this type, AppConfig processes the deployment by
-- dividing the total number of targets by the value specified for
-- @Step percentage@. For example, a linear deployment that uses a
-- @Step percentage@ of 10 deploys the configuration to 10 percent of the
-- hosts. After those deployments are complete, the system deploys the
-- configuration to the next 10 percent. This continues until 100% of the
-- targets have successfully received the configuration.
--
-- __Exponential__: For this type, AppConfig processes the deployment
-- exponentially using the following formula: @G*(2^N)@. In this formula,
-- @G@ is the growth factor specified by the user and @N@ is the number of
-- steps until the configuration is deployed to all targets. For example,
-- if you specify a growth factor of 2, then the system rolls out the
-- configuration as follows:
--
-- @2*(2^0)@
--
-- @2*(2^1)@
--
-- @2*(2^2)@
--
-- Expressed numerically, the deployment rolls out as follows: 2% of the
-- targets, 4% of the targets, 8% of the targets, and continues until the
-- configuration has been deployed to all targets.
createDeploymentStrategy_growthType :: Lens.Lens' CreateDeploymentStrategy (Prelude.Maybe GrowthType)
createDeploymentStrategy_growthType = Lens.lens (\CreateDeploymentStrategy' {growthType} -> growthType) (\s@CreateDeploymentStrategy' {} a -> s {growthType = a} :: CreateDeploymentStrategy)

-- | Save the deployment strategy to a Systems Manager (SSM) document.
createDeploymentStrategy_replicateTo :: Lens.Lens' CreateDeploymentStrategy (Prelude.Maybe ReplicateTo)
createDeploymentStrategy_replicateTo = Lens.lens (\CreateDeploymentStrategy' {replicateTo} -> replicateTo) (\s@CreateDeploymentStrategy' {} a -> s {replicateTo = a} :: CreateDeploymentStrategy)

-- | Metadata to assign to the deployment strategy. Tags help organize and
-- categorize your AppConfig resources. Each tag consists of a key and an
-- optional value, both of which you define.
createDeploymentStrategy_tags :: Lens.Lens' CreateDeploymentStrategy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDeploymentStrategy_tags = Lens.lens (\CreateDeploymentStrategy' {tags} -> tags) (\s@CreateDeploymentStrategy' {} a -> s {tags = a} :: CreateDeploymentStrategy) Prelude.. Lens.mapping Lens.coerced

-- | A name for the deployment strategy.
createDeploymentStrategy_name :: Lens.Lens' CreateDeploymentStrategy Prelude.Text
createDeploymentStrategy_name = Lens.lens (\CreateDeploymentStrategy' {name} -> name) (\s@CreateDeploymentStrategy' {} a -> s {name = a} :: CreateDeploymentStrategy)

-- | Total amount of time for a deployment to last.
createDeploymentStrategy_deploymentDurationInMinutes :: Lens.Lens' CreateDeploymentStrategy Prelude.Natural
createDeploymentStrategy_deploymentDurationInMinutes = Lens.lens (\CreateDeploymentStrategy' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@CreateDeploymentStrategy' {} a -> s {deploymentDurationInMinutes = a} :: CreateDeploymentStrategy)

-- | The percentage of targets to receive a deployed configuration during
-- each interval.
createDeploymentStrategy_growthFactor :: Lens.Lens' CreateDeploymentStrategy Prelude.Double
createDeploymentStrategy_growthFactor = Lens.lens (\CreateDeploymentStrategy' {growthFactor} -> growthFactor) (\s@CreateDeploymentStrategy' {} a -> s {growthFactor = a} :: CreateDeploymentStrategy)

instance Core.AWSRequest CreateDeploymentStrategy where
  type
    AWSResponse CreateDeploymentStrategy =
      DeploymentStrategy
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateDeploymentStrategy where
  hashWithSalt _salt CreateDeploymentStrategy' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` finalBakeTimeInMinutes
      `Prelude.hashWithSalt` growthType
      `Prelude.hashWithSalt` replicateTo
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` deploymentDurationInMinutes
      `Prelude.hashWithSalt` growthFactor

instance Prelude.NFData CreateDeploymentStrategy where
  rnf CreateDeploymentStrategy' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf finalBakeTimeInMinutes `Prelude.seq`
        Prelude.rnf growthType `Prelude.seq`
          Prelude.rnf replicateTo `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf deploymentDurationInMinutes `Prelude.seq`
                  Prelude.rnf growthFactor

instance Data.ToHeaders CreateDeploymentStrategy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDeploymentStrategy where
  toJSON CreateDeploymentStrategy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("FinalBakeTimeInMinutes" Data..=)
              Prelude.<$> finalBakeTimeInMinutes,
            ("GrowthType" Data..=) Prelude.<$> growthType,
            ("ReplicateTo" Data..=) Prelude.<$> replicateTo,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "DeploymentDurationInMinutes"
                  Data..= deploymentDurationInMinutes
              ),
            Prelude.Just ("GrowthFactor" Data..= growthFactor)
          ]
      )

instance Data.ToPath CreateDeploymentStrategy where
  toPath = Prelude.const "/deploymentstrategies"

instance Data.ToQuery CreateDeploymentStrategy where
  toQuery = Prelude.const Prelude.mempty
