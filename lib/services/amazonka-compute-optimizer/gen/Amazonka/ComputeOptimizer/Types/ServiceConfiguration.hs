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
-- Module      : Amazonka.ComputeOptimizer.Types.ServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ServiceConfiguration where

import Amazonka.ComputeOptimizer.Types.AutoScalingConfiguration
import Amazonka.ComputeOptimizer.Types.ContainerConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon ECS service configurations used for recommendations.
--
-- /See:/ 'newServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { -- | Describes the Auto Scaling configuration methods for an Amazon ECS
    -- service. This affects the generated recommendations. For example, if
    -- Auto Scaling is configured on a ECS service’s CPU, then Compute
    -- Optimizer doesn’t generate CPU size recommendations.
    --
    -- The Auto Scaling configuration methods include:
    --
    -- -   @TARGET_TRACKING_SCALING_CPU@ — If the ECS service is configured to
    --     use target scaling on CPU, Compute Optimizer doesn\'t generate CPU
    --     recommendations.
    --
    -- -   @TARGET_TRACKING_SCALING_MEMORY@ — If the ECS service is configured
    --     to use target scaling on memory, Compute Optimizer doesn\'t generate
    --     memory recommendations.
    --
    -- For more information about step scaling and target scaling, see
    -- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step scaling policies for Application Auto Scaling>
    -- and
    -- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies for Application Auto Scaling>
    -- in the /Application Auto Scaling User Guide/.
    autoScalingConfiguration :: Prelude.Maybe AutoScalingConfiguration,
    -- | The container configurations within a task of an ECS service.
    containerConfigurations :: Prelude.Maybe [ContainerConfiguration],
    -- | The number of CPU units used by the tasks in the ECS service.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The amount of memory used by the tasks in the ECS service.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The task definition ARN used by the tasks in the ECS service.
    taskDefinitionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingConfiguration', 'serviceConfiguration_autoScalingConfiguration' - Describes the Auto Scaling configuration methods for an Amazon ECS
-- service. This affects the generated recommendations. For example, if
-- Auto Scaling is configured on a ECS service’s CPU, then Compute
-- Optimizer doesn’t generate CPU size recommendations.
--
-- The Auto Scaling configuration methods include:
--
-- -   @TARGET_TRACKING_SCALING_CPU@ — If the ECS service is configured to
--     use target scaling on CPU, Compute Optimizer doesn\'t generate CPU
--     recommendations.
--
-- -   @TARGET_TRACKING_SCALING_MEMORY@ — If the ECS service is configured
--     to use target scaling on memory, Compute Optimizer doesn\'t generate
--     memory recommendations.
--
-- For more information about step scaling and target scaling, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step scaling policies for Application Auto Scaling>
-- and
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies for Application Auto Scaling>
-- in the /Application Auto Scaling User Guide/.
--
-- 'containerConfigurations', 'serviceConfiguration_containerConfigurations' - The container configurations within a task of an ECS service.
--
-- 'cpu', 'serviceConfiguration_cpu' - The number of CPU units used by the tasks in the ECS service.
--
-- 'memory', 'serviceConfiguration_memory' - The amount of memory used by the tasks in the ECS service.
--
-- 'taskDefinitionArn', 'serviceConfiguration_taskDefinitionArn' - The task definition ARN used by the tasks in the ECS service.
newServiceConfiguration ::
  ServiceConfiguration
newServiceConfiguration =
  ServiceConfiguration'
    { autoScalingConfiguration =
        Prelude.Nothing,
      containerConfigurations = Prelude.Nothing,
      cpu = Prelude.Nothing,
      memory = Prelude.Nothing,
      taskDefinitionArn = Prelude.Nothing
    }

-- | Describes the Auto Scaling configuration methods for an Amazon ECS
-- service. This affects the generated recommendations. For example, if
-- Auto Scaling is configured on a ECS service’s CPU, then Compute
-- Optimizer doesn’t generate CPU size recommendations.
--
-- The Auto Scaling configuration methods include:
--
-- -   @TARGET_TRACKING_SCALING_CPU@ — If the ECS service is configured to
--     use target scaling on CPU, Compute Optimizer doesn\'t generate CPU
--     recommendations.
--
-- -   @TARGET_TRACKING_SCALING_MEMORY@ — If the ECS service is configured
--     to use target scaling on memory, Compute Optimizer doesn\'t generate
--     memory recommendations.
--
-- For more information about step scaling and target scaling, see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-step-scaling-policies.html Step scaling policies for Application Auto Scaling>
-- and
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-target-tracking.html Target tracking scaling policies for Application Auto Scaling>
-- in the /Application Auto Scaling User Guide/.
serviceConfiguration_autoScalingConfiguration :: Lens.Lens' ServiceConfiguration (Prelude.Maybe AutoScalingConfiguration)
serviceConfiguration_autoScalingConfiguration = Lens.lens (\ServiceConfiguration' {autoScalingConfiguration} -> autoScalingConfiguration) (\s@ServiceConfiguration' {} a -> s {autoScalingConfiguration = a} :: ServiceConfiguration)

-- | The container configurations within a task of an ECS service.
serviceConfiguration_containerConfigurations :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [ContainerConfiguration])
serviceConfiguration_containerConfigurations = Lens.lens (\ServiceConfiguration' {containerConfigurations} -> containerConfigurations) (\s@ServiceConfiguration' {} a -> s {containerConfigurations = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The number of CPU units used by the tasks in the ECS service.
serviceConfiguration_cpu :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Int)
serviceConfiguration_cpu = Lens.lens (\ServiceConfiguration' {cpu} -> cpu) (\s@ServiceConfiguration' {} a -> s {cpu = a} :: ServiceConfiguration)

-- | The amount of memory used by the tasks in the ECS service.
serviceConfiguration_memory :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Int)
serviceConfiguration_memory = Lens.lens (\ServiceConfiguration' {memory} -> memory) (\s@ServiceConfiguration' {} a -> s {memory = a} :: ServiceConfiguration)

-- | The task definition ARN used by the tasks in the ECS service.
serviceConfiguration_taskDefinitionArn :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_taskDefinitionArn = Lens.lens (\ServiceConfiguration' {taskDefinitionArn} -> taskDefinitionArn) (\s@ServiceConfiguration' {} a -> s {taskDefinitionArn = a} :: ServiceConfiguration)

instance Data.FromJSON ServiceConfiguration where
  parseJSON =
    Data.withObject
      "ServiceConfiguration"
      ( \x ->
          ServiceConfiguration'
            Prelude.<$> (x Data..:? "autoScalingConfiguration")
            Prelude.<*> ( x
                            Data..:? "containerConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "cpu")
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "taskDefinitionArn")
      )

instance Prelude.Hashable ServiceConfiguration where
  hashWithSalt _salt ServiceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingConfiguration
      `Prelude.hashWithSalt` containerConfigurations
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` taskDefinitionArn

instance Prelude.NFData ServiceConfiguration where
  rnf ServiceConfiguration' {..} =
    Prelude.rnf autoScalingConfiguration `Prelude.seq`
      Prelude.rnf containerConfigurations `Prelude.seq`
        Prelude.rnf cpu `Prelude.seq`
          Prelude.rnf memory `Prelude.seq`
            Prelude.rnf taskDefinitionArn
