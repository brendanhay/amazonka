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
-- Module      : Amazonka.AppConfig.Types.DeploymentStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.DeploymentStrategy where

import Amazonka.AppConfig.Types.GrowthType
import Amazonka.AppConfig.Types.ReplicateTo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDeploymentStrategy' smart constructor.
data DeploymentStrategy = DeploymentStrategy'
  { -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The description of the deployment strategy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that AppConfig monitored for alarms before
    -- considering the deployment to be complete and no longer eligible for
    -- automatic rollback.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of targets that received a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | The algorithm used to define how percentage grew over time.
    growthType :: Prelude.Maybe GrowthType,
    -- | The deployment strategy ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the deployment strategy.
    name :: Prelude.Maybe Prelude.Text,
    -- | Save the deployment strategy to a Systems Manager (SSM) document.
    replicateTo :: Prelude.Maybe ReplicateTo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentDurationInMinutes', 'deploymentStrategy_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'description', 'deploymentStrategy_description' - The description of the deployment strategy.
--
-- 'finalBakeTimeInMinutes', 'deploymentStrategy_finalBakeTimeInMinutes' - The amount of time that AppConfig monitored for alarms before
-- considering the deployment to be complete and no longer eligible for
-- automatic rollback.
--
-- 'growthFactor', 'deploymentStrategy_growthFactor' - The percentage of targets that received a deployed configuration during
-- each interval.
--
-- 'growthType', 'deploymentStrategy_growthType' - The algorithm used to define how percentage grew over time.
--
-- 'id', 'deploymentStrategy_id' - The deployment strategy ID.
--
-- 'name', 'deploymentStrategy_name' - The name of the deployment strategy.
--
-- 'replicateTo', 'deploymentStrategy_replicateTo' - Save the deployment strategy to a Systems Manager (SSM) document.
newDeploymentStrategy ::
  DeploymentStrategy
newDeploymentStrategy =
  DeploymentStrategy'
    { deploymentDurationInMinutes =
        Prelude.Nothing,
      description = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      growthFactor = Prelude.Nothing,
      growthType = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      replicateTo = Prelude.Nothing
    }

-- | Total amount of time the deployment lasted.
deploymentStrategy_deploymentDurationInMinutes :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Natural)
deploymentStrategy_deploymentDurationInMinutes = Lens.lens (\DeploymentStrategy' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@DeploymentStrategy' {} a -> s {deploymentDurationInMinutes = a} :: DeploymentStrategy)

-- | The description of the deployment strategy.
deploymentStrategy_description :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Text)
deploymentStrategy_description = Lens.lens (\DeploymentStrategy' {description} -> description) (\s@DeploymentStrategy' {} a -> s {description = a} :: DeploymentStrategy)

-- | The amount of time that AppConfig monitored for alarms before
-- considering the deployment to be complete and no longer eligible for
-- automatic rollback.
deploymentStrategy_finalBakeTimeInMinutes :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Natural)
deploymentStrategy_finalBakeTimeInMinutes = Lens.lens (\DeploymentStrategy' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@DeploymentStrategy' {} a -> s {finalBakeTimeInMinutes = a} :: DeploymentStrategy)

-- | The percentage of targets that received a deployed configuration during
-- each interval.
deploymentStrategy_growthFactor :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Double)
deploymentStrategy_growthFactor = Lens.lens (\DeploymentStrategy' {growthFactor} -> growthFactor) (\s@DeploymentStrategy' {} a -> s {growthFactor = a} :: DeploymentStrategy)

-- | The algorithm used to define how percentage grew over time.
deploymentStrategy_growthType :: Lens.Lens' DeploymentStrategy (Prelude.Maybe GrowthType)
deploymentStrategy_growthType = Lens.lens (\DeploymentStrategy' {growthType} -> growthType) (\s@DeploymentStrategy' {} a -> s {growthType = a} :: DeploymentStrategy)

-- | The deployment strategy ID.
deploymentStrategy_id :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Text)
deploymentStrategy_id = Lens.lens (\DeploymentStrategy' {id} -> id) (\s@DeploymentStrategy' {} a -> s {id = a} :: DeploymentStrategy)

-- | The name of the deployment strategy.
deploymentStrategy_name :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Text)
deploymentStrategy_name = Lens.lens (\DeploymentStrategy' {name} -> name) (\s@DeploymentStrategy' {} a -> s {name = a} :: DeploymentStrategy)

-- | Save the deployment strategy to a Systems Manager (SSM) document.
deploymentStrategy_replicateTo :: Lens.Lens' DeploymentStrategy (Prelude.Maybe ReplicateTo)
deploymentStrategy_replicateTo = Lens.lens (\DeploymentStrategy' {replicateTo} -> replicateTo) (\s@DeploymentStrategy' {} a -> s {replicateTo = a} :: DeploymentStrategy)

instance Data.FromJSON DeploymentStrategy where
  parseJSON =
    Data.withObject
      "DeploymentStrategy"
      ( \x ->
          DeploymentStrategy'
            Prelude.<$> (x Data..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Data..:? "GrowthFactor")
            Prelude.<*> (x Data..:? "GrowthType")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ReplicateTo")
      )

instance Prelude.Hashable DeploymentStrategy where
  hashWithSalt _salt DeploymentStrategy' {..} =
    _salt
      `Prelude.hashWithSalt` deploymentDurationInMinutes
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` finalBakeTimeInMinutes
      `Prelude.hashWithSalt` growthFactor
      `Prelude.hashWithSalt` growthType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` replicateTo

instance Prelude.NFData DeploymentStrategy where
  rnf DeploymentStrategy' {..} =
    Prelude.rnf deploymentDurationInMinutes `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf finalBakeTimeInMinutes `Prelude.seq`
          Prelude.rnf growthFactor `Prelude.seq`
            Prelude.rnf growthType `Prelude.seq`
              Prelude.rnf id `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf replicateTo
