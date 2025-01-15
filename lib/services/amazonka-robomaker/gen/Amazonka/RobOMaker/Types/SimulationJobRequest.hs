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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationJobRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Compute information for the simulation job
    compute :: Prelude.Maybe Compute,
    -- | Specify data sources to mount read-only files from S3 into your
    -- simulation. These files are available under
    -- @\/opt\/robomaker\/datasources\/data_source_name@.
    --
    -- There is a limit of 100 files and a combined size of 25GB for all
    -- @DataSourceConfig@ objects.
    dataSources :: Prelude.Maybe (Prelude.NonEmpty DataSourceConfig),
    -- | The failure behavior the simulation job.
    --
    -- [Continue]
    --     Leaves the host running for its maximum timeout duration after a
    --     @4XX@ error code.
    --
    -- [Fail]
    --     Stop the simulation job and terminate the instance.
    failureBehavior :: Prelude.Maybe FailureBehavior,
    -- | The IAM role name that allows the simulation instance to call the AWS
    -- APIs that are specified in its associated policies on your behalf. This
    -- is how credentials are passed in to your simulation job.
    iamRole :: Prelude.Maybe Prelude.Text,
    loggingConfig :: Prelude.Maybe LoggingConfig,
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | The robot applications to use in the simulation job.
    robotApplications :: Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig),
    -- | The simulation applications to use in the simulation job.
    simulationApplications :: Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig),
    -- | A map that contains tag keys and tag values that are attached to the
    -- simulation job request.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A Boolean indicating whether to use default applications in the
    -- simulation job. Default applications include Gazebo, rqt, rviz and
    -- terminal access.
    useDefaultApplications :: Prelude.Maybe Prelude.Bool,
    vpcConfig :: Prelude.Maybe VPCConfig,
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
-- 'compute', 'simulationJobRequest_compute' - Compute information for the simulation job
--
-- 'dataSources', 'simulationJobRequest_dataSources' - Specify data sources to mount read-only files from S3 into your
-- simulation. These files are available under
-- @\/opt\/robomaker\/datasources\/data_source_name@.
--
-- There is a limit of 100 files and a combined size of 25GB for all
-- @DataSourceConfig@ objects.
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
-- 'iamRole', 'simulationJobRequest_iamRole' - The IAM role name that allows the simulation instance to call the AWS
-- APIs that are specified in its associated policies on your behalf. This
-- is how credentials are passed in to your simulation job.
--
-- 'loggingConfig', 'simulationJobRequest_loggingConfig' - Undocumented member.
--
-- 'outputLocation', 'simulationJobRequest_outputLocation' - Undocumented member.
--
-- 'robotApplications', 'simulationJobRequest_robotApplications' - The robot applications to use in the simulation job.
--
-- 'simulationApplications', 'simulationJobRequest_simulationApplications' - The simulation applications to use in the simulation job.
--
-- 'tags', 'simulationJobRequest_tags' - A map that contains tag keys and tag values that are attached to the
-- simulation job request.
--
-- 'useDefaultApplications', 'simulationJobRequest_useDefaultApplications' - A Boolean indicating whether to use default applications in the
-- simulation job. Default applications include Gazebo, rqt, rviz and
-- terminal access.
--
-- 'vpcConfig', 'simulationJobRequest_vpcConfig' - Undocumented member.
--
-- 'maxJobDurationInSeconds', 'simulationJobRequest_maxJobDurationInSeconds' - The maximum simulation job duration in seconds. The value must be 8 days
-- (691,200 seconds) or less.
newSimulationJobRequest ::
  -- | 'maxJobDurationInSeconds'
  Prelude.Integer ->
  SimulationJobRequest
newSimulationJobRequest pMaxJobDurationInSeconds_ =
  SimulationJobRequest'
    { compute = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      failureBehavior = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      loggingConfig = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      robotApplications = Prelude.Nothing,
      simulationApplications = Prelude.Nothing,
      tags = Prelude.Nothing,
      useDefaultApplications = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      maxJobDurationInSeconds = pMaxJobDurationInSeconds_
    }

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

-- | The IAM role name that allows the simulation instance to call the AWS
-- APIs that are specified in its associated policies on your behalf. This
-- is how credentials are passed in to your simulation job.
simulationJobRequest_iamRole :: Lens.Lens' SimulationJobRequest (Prelude.Maybe Prelude.Text)
simulationJobRequest_iamRole = Lens.lens (\SimulationJobRequest' {iamRole} -> iamRole) (\s@SimulationJobRequest' {} a -> s {iamRole = a} :: SimulationJobRequest)

-- | Undocumented member.
simulationJobRequest_loggingConfig :: Lens.Lens' SimulationJobRequest (Prelude.Maybe LoggingConfig)
simulationJobRequest_loggingConfig = Lens.lens (\SimulationJobRequest' {loggingConfig} -> loggingConfig) (\s@SimulationJobRequest' {} a -> s {loggingConfig = a} :: SimulationJobRequest)

-- | Undocumented member.
simulationJobRequest_outputLocation :: Lens.Lens' SimulationJobRequest (Prelude.Maybe OutputLocation)
simulationJobRequest_outputLocation = Lens.lens (\SimulationJobRequest' {outputLocation} -> outputLocation) (\s@SimulationJobRequest' {} a -> s {outputLocation = a} :: SimulationJobRequest)

-- | The robot applications to use in the simulation job.
simulationJobRequest_robotApplications :: Lens.Lens' SimulationJobRequest (Prelude.Maybe (Prelude.NonEmpty RobotApplicationConfig))
simulationJobRequest_robotApplications = Lens.lens (\SimulationJobRequest' {robotApplications} -> robotApplications) (\s@SimulationJobRequest' {} a -> s {robotApplications = a} :: SimulationJobRequest) Prelude.. Lens.mapping Lens.coerced

-- | The simulation applications to use in the simulation job.
simulationJobRequest_simulationApplications :: Lens.Lens' SimulationJobRequest (Prelude.Maybe (Prelude.NonEmpty SimulationApplicationConfig))
simulationJobRequest_simulationApplications = Lens.lens (\SimulationJobRequest' {simulationApplications} -> simulationApplications) (\s@SimulationJobRequest' {} a -> s {simulationApplications = a} :: SimulationJobRequest) Prelude.. Lens.mapping Lens.coerced

-- | A map that contains tag keys and tag values that are attached to the
-- simulation job request.
simulationJobRequest_tags :: Lens.Lens' SimulationJobRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
simulationJobRequest_tags = Lens.lens (\SimulationJobRequest' {tags} -> tags) (\s@SimulationJobRequest' {} a -> s {tags = a} :: SimulationJobRequest) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean indicating whether to use default applications in the
-- simulation job. Default applications include Gazebo, rqt, rviz and
-- terminal access.
simulationJobRequest_useDefaultApplications :: Lens.Lens' SimulationJobRequest (Prelude.Maybe Prelude.Bool)
simulationJobRequest_useDefaultApplications = Lens.lens (\SimulationJobRequest' {useDefaultApplications} -> useDefaultApplications) (\s@SimulationJobRequest' {} a -> s {useDefaultApplications = a} :: SimulationJobRequest)

-- | Undocumented member.
simulationJobRequest_vpcConfig :: Lens.Lens' SimulationJobRequest (Prelude.Maybe VPCConfig)
simulationJobRequest_vpcConfig = Lens.lens (\SimulationJobRequest' {vpcConfig} -> vpcConfig) (\s@SimulationJobRequest' {} a -> s {vpcConfig = a} :: SimulationJobRequest)

-- | The maximum simulation job duration in seconds. The value must be 8 days
-- (691,200 seconds) or less.
simulationJobRequest_maxJobDurationInSeconds :: Lens.Lens' SimulationJobRequest Prelude.Integer
simulationJobRequest_maxJobDurationInSeconds = Lens.lens (\SimulationJobRequest' {maxJobDurationInSeconds} -> maxJobDurationInSeconds) (\s@SimulationJobRequest' {} a -> s {maxJobDurationInSeconds = a} :: SimulationJobRequest)

instance Data.FromJSON SimulationJobRequest where
  parseJSON =
    Data.withObject
      "SimulationJobRequest"
      ( \x ->
          SimulationJobRequest'
            Prelude.<$> (x Data..:? "compute")
            Prelude.<*> (x Data..:? "dataSources")
            Prelude.<*> (x Data..:? "failureBehavior")
            Prelude.<*> (x Data..:? "iamRole")
            Prelude.<*> (x Data..:? "loggingConfig")
            Prelude.<*> (x Data..:? "outputLocation")
            Prelude.<*> (x Data..:? "robotApplications")
            Prelude.<*> (x Data..:? "simulationApplications")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "useDefaultApplications")
            Prelude.<*> (x Data..:? "vpcConfig")
            Prelude.<*> (x Data..: "maxJobDurationInSeconds")
      )

instance Prelude.Hashable SimulationJobRequest where
  hashWithSalt _salt SimulationJobRequest' {..} =
    _salt
      `Prelude.hashWithSalt` compute
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` failureBehavior
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` loggingConfig
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` robotApplications
      `Prelude.hashWithSalt` simulationApplications
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` useDefaultApplications
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` maxJobDurationInSeconds

instance Prelude.NFData SimulationJobRequest where
  rnf SimulationJobRequest' {..} =
    Prelude.rnf compute `Prelude.seq`
      Prelude.rnf dataSources `Prelude.seq`
        Prelude.rnf failureBehavior `Prelude.seq`
          Prelude.rnf iamRole `Prelude.seq`
            Prelude.rnf loggingConfig `Prelude.seq`
              Prelude.rnf outputLocation `Prelude.seq`
                Prelude.rnf robotApplications `Prelude.seq`
                  Prelude.rnf simulationApplications `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf useDefaultApplications `Prelude.seq`
                        Prelude.rnf vpcConfig `Prelude.seq`
                          Prelude.rnf maxJobDurationInSeconds

instance Data.ToJSON SimulationJobRequest where
  toJSON SimulationJobRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("compute" Data..=) Prelude.<$> compute,
            ("dataSources" Data..=) Prelude.<$> dataSources,
            ("failureBehavior" Data..=)
              Prelude.<$> failureBehavior,
            ("iamRole" Data..=) Prelude.<$> iamRole,
            ("loggingConfig" Data..=) Prelude.<$> loggingConfig,
            ("outputLocation" Data..=)
              Prelude.<$> outputLocation,
            ("robotApplications" Data..=)
              Prelude.<$> robotApplications,
            ("simulationApplications" Data..=)
              Prelude.<$> simulationApplications,
            ("tags" Data..=) Prelude.<$> tags,
            ("useDefaultApplications" Data..=)
              Prelude.<$> useDefaultApplications,
            ("vpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just
              ( "maxJobDurationInSeconds"
                  Data..= maxJobDurationInSeconds
              )
          ]
      )
