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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A deployment strategy defines important criteria for rolling out your
-- configuration to the designated targets. A deployment strategy includes:
-- the overall duration required, a percentage of targets to receive the
-- deployment during each interval, an algorithm that defines how
-- percentage grows, and bake time.
module Amazonka.AppConfig.CreateDeploymentStrategy
  ( -- * Creating a Request
    CreateDeploymentStrategy (..),
    newCreateDeploymentStrategy,

    -- * Request Lenses
    createDeploymentStrategy_finalBakeTimeInMinutes,
    createDeploymentStrategy_description,
    createDeploymentStrategy_growthType,
    createDeploymentStrategy_tags,
    createDeploymentStrategy_name,
    createDeploymentStrategy_deploymentDurationInMinutes,
    createDeploymentStrategy_growthFactor,
    createDeploymentStrategy_replicateTo,

    -- * Destructuring the Response
    DeploymentStrategy (..),
    newDeploymentStrategy,

    -- * Response Lenses
    deploymentStrategy_growthFactor,
    deploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_id,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_growthType,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDeploymentStrategy' smart constructor.
data CreateDeploymentStrategy = CreateDeploymentStrategy'
  { -- | The amount of time AppConfig monitors for alarms before considering the
    -- deployment to be complete and no longer eligible for automatic roll
    -- back.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | A description of the deployment strategy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The algorithm used to define how percentage grows over time. AWS
    -- AppConfig supports the following growth types:
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
    growthFactor :: Prelude.Double,
    -- | Save the deployment strategy to a Systems Manager (SSM) document.
    replicateTo :: ReplicateTo
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
-- 'finalBakeTimeInMinutes', 'createDeploymentStrategy_finalBakeTimeInMinutes' - The amount of time AppConfig monitors for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
--
-- 'description', 'createDeploymentStrategy_description' - A description of the deployment strategy.
--
-- 'growthType', 'createDeploymentStrategy_growthType' - The algorithm used to define how percentage grows over time. AWS
-- AppConfig supports the following growth types:
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
--
-- 'replicateTo', 'createDeploymentStrategy_replicateTo' - Save the deployment strategy to a Systems Manager (SSM) document.
newCreateDeploymentStrategy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'deploymentDurationInMinutes'
  Prelude.Natural ->
  -- | 'growthFactor'
  Prelude.Double ->
  -- | 'replicateTo'
  ReplicateTo ->
  CreateDeploymentStrategy
newCreateDeploymentStrategy
  pName_
  pDeploymentDurationInMinutes_
  pGrowthFactor_
  pReplicateTo_ =
    CreateDeploymentStrategy'
      { finalBakeTimeInMinutes =
          Prelude.Nothing,
        description = Prelude.Nothing,
        growthType = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        deploymentDurationInMinutes =
          pDeploymentDurationInMinutes_,
        growthFactor = pGrowthFactor_,
        replicateTo = pReplicateTo_
      }

-- | The amount of time AppConfig monitors for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
createDeploymentStrategy_finalBakeTimeInMinutes :: Lens.Lens' CreateDeploymentStrategy (Prelude.Maybe Prelude.Natural)
createDeploymentStrategy_finalBakeTimeInMinutes = Lens.lens (\CreateDeploymentStrategy' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@CreateDeploymentStrategy' {} a -> s {finalBakeTimeInMinutes = a} :: CreateDeploymentStrategy)

-- | A description of the deployment strategy.
createDeploymentStrategy_description :: Lens.Lens' CreateDeploymentStrategy (Prelude.Maybe Prelude.Text)
createDeploymentStrategy_description = Lens.lens (\CreateDeploymentStrategy' {description} -> description) (\s@CreateDeploymentStrategy' {} a -> s {description = a} :: CreateDeploymentStrategy)

-- | The algorithm used to define how percentage grows over time. AWS
-- AppConfig supports the following growth types:
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

-- | Save the deployment strategy to a Systems Manager (SSM) document.
createDeploymentStrategy_replicateTo :: Lens.Lens' CreateDeploymentStrategy ReplicateTo
createDeploymentStrategy_replicateTo = Lens.lens (\CreateDeploymentStrategy' {replicateTo} -> replicateTo) (\s@CreateDeploymentStrategy' {} a -> s {replicateTo = a} :: CreateDeploymentStrategy)

instance Core.AWSRequest CreateDeploymentStrategy where
  type
    AWSResponse CreateDeploymentStrategy =
      DeploymentStrategy
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateDeploymentStrategy

instance Prelude.NFData CreateDeploymentStrategy

instance Core.ToHeaders CreateDeploymentStrategy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDeploymentStrategy where
  toJSON CreateDeploymentStrategy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FinalBakeTimeInMinutes" Core..=)
              Prelude.<$> finalBakeTimeInMinutes,
            ("Description" Core..=) Prelude.<$> description,
            ("GrowthType" Core..=) Prelude.<$> growthType,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ( "DeploymentDurationInMinutes"
                  Core..= deploymentDurationInMinutes
              ),
            Prelude.Just ("GrowthFactor" Core..= growthFactor),
            Prelude.Just ("ReplicateTo" Core..= replicateTo)
          ]
      )

instance Core.ToPath CreateDeploymentStrategy where
  toPath = Prelude.const "/deploymentstrategies"

instance Core.ToQuery CreateDeploymentStrategy where
  toQuery = Prelude.const Prelude.mempty
