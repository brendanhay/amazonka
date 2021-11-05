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
-- Module      : Amazonka.RobOMaker.Types.SimulationJobRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationJobRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.Compute
import Amazonka.RobOMaker.Types.DataSourceConfig
import Amazonka.RobOMaker.Types.FailureBehavior
import Amazonka.RobOMaker.Types.LoggingConfig
import Amazonka.RobOMaker.Types.OutputLocation
import Amazonka.RobOMaker.Types.RobotApplicationConfig
import Amazonka.RobOMaker.Types.SimulationApplicationConfig
import Amazonka.RobOMaker.Types.VPCConfig

-- | Information about a simulation job request.
--
-- /See:/ 'newSimulationJobRequest' smart constructor.
data SimulationJobRequest = SimulationJobRequest'
  { -- | The failure behavior the simulation job.
    --
    -- [Continue]
    --     Leaves the host running for its maximum timeout duration after a
    --     @4XX@ error code.
    --
    -- [Fail]
    --     Stop the simulation job and terminate the instance.
    failureBehavior :: Prelude.Maybe FailureBehavior,
    -- | The robot applications to use in the simulation job.
    robotApplications :: Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig),
    -- | Compute information for the simulation job
    compute :: Prelude.Maybe Compute,
    -- | Specify data sources to mount read-only files from S3 into your
    -- simulation. These files are available under
    -- @\/opt\/robomaker\/datasources\/data_source_name@.
    --
    -- There is a limit of 100 files and a combined size of 25GB for all
    -- @DataSourceConfig@ objects.
    dataSources :: Prelude.Maybe (Prelude.NonEmpty DataSourceConfig),
    -- | A Boolean indicating whether to use default applications in the
    -- simulation job. Default applications include Gazebo, rqt, rviz and
    -- terminal access.
    useDefaultApplications :: Prelude.Maybe Prelude.Bool,
    vpcConfig :: Prelude.Maybe VPCConfig,
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | The simulation applications to use in the simulation job.
    simulationApplications :: Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig),
    loggingConfig :: Prelude.Maybe LoggingConfig,
    -- | The IAM role name that allows the simulation instance to call the AWS
    -- APIs that are specified in its associated policies on your behalf. This
    -- is how credentials are passed in to your simulation job.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | A map that contains tag keys and tag values that are attached to the
    -- simulation job request.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The maximum simulation job duration in seconds. The value must be 8 days
    -- (691,200 seconds) or less.
    maxJobDurationInSeconds :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationJobRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureBehavior', 'simulationJobRequest_failureBehavior' - The failure behavior the simulation job.
--
-- [Continue]
--     Leaves the host running for its maximum timeout duration after a
--     @4XX@ error code.
--
-- [Fail]
--     Stop the simulation job and terminate the instance.
--
-- 'robotApplications', 'simulationJobRequest_robotApplications' - The robot applications to use in the simulation job.
--
-- 'compute', 'simulationJobRequest_compute' - Compute information for the simulation job
--
-- 'dataSources', 'simulationJobRequest_dataSources' - Specify data sources to mount read-only files from S3 into your
-- simulation. These files are available under
-- @\/opt\/robomaker\/datasources\/data_source_name@.
--
-- There is a limit of 100 files and a combined size of 25GB for all
-- @DataSourceConfig@ objects.
--
-- 'useDefaultApplications', 'simulationJobRequest_useDefaultApplications' - A Boolean indicating whether to use default applications in the
-- simulation job. Default applications include Gazebo, rqt, rviz and
-- terminal access.
--
-- 'vpcConfig', 'simulationJobRequest_vpcConfig' - Undocumented member.
--
-- 'outputLocation', 'simulationJobRequest_outputLocation' - Undocumented member.
--
-- 'simulationApplications', 'simulationJobRequest_simulationApplications' - The simulation applications to use in the simulation job.
--
-- 'loggingConfig', 'simulationJobRequest_loggingConfig' - Undocumented member.
--
-- 'iamRole', 'simulationJobRequest_iamRole' - The IAM role name that allows the simulation instance to call the AWS
-- APIs that are specified in its associated policies on your behalf. This
-- is how credentials are passed in to your simulation job.
--
-- 'tags', 'simulationJobRequest_tags' - A map that contains tag keys and tag values that are attached to the
-- simulation job request.
--
-- 'maxJobDurationInSeconds', 'simulationJobRequest_maxJobDurationInSeconds' - The maximum simulation job duration in seconds. The value must be 8 days
-- (691,200 seconds) or less.
newSimulationJobRequest ::
  -- | 'maxJobDurationInSeconds'
  Prelude.Integer ->
  SimulationJobRequest
newSimulationJobRequest pMaxJobDurationInSeconds_ =
  SimulationJobRequest'
    { failureBehavior =
        Prelude.Nothing,
      robotApplications = Prelude.Nothing,
      compute = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      useDefaultApplications = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      simulationApplications = Prelude.Nothing,
      loggingConfig = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      tags = Prelude.Nothing,
      maxJobDurationInSeconds = pMaxJobDurationInSeconds_
    }

-- | The failure behavior the simulation job.
--
-- [Continue]
--     Leaves the host running for its maximum timeout duration after a
--     @4XX@ error code.
--
-- [Fail]
--     Stop the simulation job and terminate the instance.
simulationJobRequest_failureBehavior :: Lens.Lens' SimulationJobRequest (Prelude.Maybe FailureBehavior)
simulationJobRequest_failureBehavior = Lens.lens (\SimulationJobRequest' {failureBehavior} -> failureBehavior) (\s@SimulationJobRequest' {} a -> s {failureBehavior = a} :: SimulationJobRequest)

-- | The robot applications to use in the simulation job.
simulationJobRequest_robotApplications :: Lens.Lens' SimulationJobRequest (Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig))
simulationJobRequest_robotApplications = Lens.lens (\SimulationJobRequest' {robotApplications} -> robotApplications) (\s@SimulationJobRequest' {} a -> s {robotApplications = a} :: SimulationJobRequest) Prelude.. Lens.mapping Lens.coerced

-- | Compute information for the simulation job
simulationJobRequest_compute :: Lens.Lens' SimulationJobRequest (Prelude.Maybe Compute)
simulationJobRequest_compute = Lens.lens (\SimulationJobRequest' {compute} -> compute) (\s@SimulationJobRequest' {} a -> s {compute = a} :: SimulationJobRequest)

-- | Specify data sources to mount read-only files from S3 into your
-- simulation. These files are available under
-- @\/opt\/robomaker\/datasources\/data_source_name@.
--
-- There is a limit of 100 files and a combined size of 25GB for all
-- @DataSourceConfig@ objects.
simulationJobRequest_dataSources :: Lens.Lens' SimulationJobRequest (Prelude.Maybe (Prelude.NonEmpty DataSourceConfig))
simulationJobRequest_dataSources = Lens.lens (\SimulationJobRequest' {dataSources} -> dataSources) (\s@SimulationJobRequest' {} a -> s {dataSources = a} :: SimulationJobRequest) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean indicating whether to use default applications in the
-- simulation job. Default applications include Gazebo, rqt, rviz and
-- terminal access.
simulationJobRequest_useDefaultApplications :: Lens.Lens' SimulationJobRequest (Prelude.Maybe Prelude.Bool)
simulationJobRequest_useDefaultApplications = Lens.lens (\SimulationJobRequest' {useDefaultApplications} -> useDefaultApplications) (\s@SimulationJobRequest' {} a -> s {useDefaultApplications = a} :: SimulationJobRequest)

-- | Undocumented member.
simulationJobRequest_vpcConfig :: Lens.Lens' SimulationJobRequest (Prelude.Maybe VPCConfig)
simulationJobRequest_vpcConfig = Lens.lens (\SimulationJobRequest' {vpcConfig} -> vpcConfig) (\s@SimulationJobRequest' {} a -> s {vpcConfig = a} :: SimulationJobRequest)

-- | Undocumented member.
simulationJobRequest_outputLocation :: Lens.Lens' SimulationJobRequest (Prelude.Maybe OutputLocation)
simulationJobRequest_outputLocation = Lens.lens (\SimulationJobRequest' {outputLocation} -> outputLocation) (\s@SimulationJobRequest' {} a -> s {outputLocation = a} :: SimulationJobRequest)

-- | The simulation applications to use in the simulation job.
simulationJobRequest_simulationApplications :: Lens.Lens' SimulationJobRequest (Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig))
simulationJobRequest_simulationApplications = Lens.lens (\SimulationJobRequest' {simulationApplications} -> simulationApplications) (\s@SimulationJobRequest' {} a -> s {simulationApplications = a} :: SimulationJobRequest) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
simulationJobRequest_loggingConfig :: Lens.Lens' SimulationJobRequest (Prelude.Maybe LoggingConfig)
simulationJobRequest_loggingConfig = Lens.lens (\SimulationJobRequest' {loggingConfig} -> loggingConfig) (\s@SimulationJobRequest' {} a -> s {loggingConfig = a} :: SimulationJobRequest)

-- | The IAM role name that allows the simulation instance to call the AWS
-- APIs that are specified in its associated policies on your behalf. This
-- is how credentials are passed in to your simulation job.
simulationJobRequest_iamRole :: Lens.Lens' SimulationJobRequest (Prelude.Maybe Prelude.Text)
simulationJobRequest_iamRole = Lens.lens (\SimulationJobRequest' {iamRole} -> iamRole) (\s@SimulationJobRequest' {} a -> s {iamRole = a} :: SimulationJobRequest)

-- | A map that contains tag keys and tag values that are attached to the
-- simulation job request.
simulationJobRequest_tags :: Lens.Lens' SimulationJobRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
simulationJobRequest_tags = Lens.lens (\SimulationJobRequest' {tags} -> tags) (\s@SimulationJobRequest' {} a -> s {tags = a} :: SimulationJobRequest) Prelude.. Lens.mapping Lens.coerced

-- | The maximum simulation job duration in seconds. The value must be 8 days
-- (691,200 seconds) or less.
simulationJobRequest_maxJobDurationInSeconds :: Lens.Lens' SimulationJobRequest Prelude.Integer
simulationJobRequest_maxJobDurationInSeconds = Lens.lens (\SimulationJobRequest' {maxJobDurationInSeconds} -> maxJobDurationInSeconds) (\s@SimulationJobRequest' {} a -> s {maxJobDurationInSeconds = a} :: SimulationJobRequest)

instance Core.FromJSON SimulationJobRequest where
  parseJSON =
    Core.withObject
      "SimulationJobRequest"
      ( \x ->
          SimulationJobRequest'
            Prelude.<$> (x Core..:? "failureBehavior")
            Prelude.<*> (x Core..:? "robotApplications")
            Prelude.<*> (x Core..:? "compute")
            Prelude.<*> (x Core..:? "dataSources")
            Prelude.<*> (x Core..:? "useDefaultApplications")
            Prelude.<*> (x Core..:? "vpcConfig")
            Prelude.<*> (x Core..:? "outputLocation")
            Prelude.<*> (x Core..:? "simulationApplications")
            Prelude.<*> (x Core..:? "loggingConfig")
            Prelude.<*> (x Core..:? "iamRole")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "maxJobDurationInSeconds")
      )

instance Prelude.Hashable SimulationJobRequest

instance Prelude.NFData SimulationJobRequest

instance Core.ToJSON SimulationJobRequest where
  toJSON SimulationJobRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("failureBehavior" Core..=)
              Prelude.<$> failureBehavior,
            ("robotApplications" Core..=)
              Prelude.<$> robotApplications,
            ("compute" Core..=) Prelude.<$> compute,
            ("dataSources" Core..=) Prelude.<$> dataSources,
            ("useDefaultApplications" Core..=)
              Prelude.<$> useDefaultApplications,
            ("vpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("outputLocation" Core..=)
              Prelude.<$> outputLocation,
            ("simulationApplications" Core..=)
              Prelude.<$> simulationApplications,
            ("loggingConfig" Core..=) Prelude.<$> loggingConfig,
            ("iamRole" Core..=) Prelude.<$> iamRole,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "maxJobDurationInSeconds"
                  Core..= maxJobDurationInSeconds
              )
          ]
      )
