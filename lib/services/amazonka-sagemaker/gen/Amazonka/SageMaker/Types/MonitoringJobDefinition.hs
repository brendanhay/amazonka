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
-- Module      : Amazonka.SageMaker.Types.MonitoringJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringJobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringAppSpecification
import Amazonka.SageMaker.Types.MonitoringBaselineConfig
import Amazonka.SageMaker.Types.MonitoringInput
import Amazonka.SageMaker.Types.MonitoringOutputConfig
import Amazonka.SageMaker.Types.MonitoringResources
import Amazonka.SageMaker.Types.MonitoringStoppingCondition
import Amazonka.SageMaker.Types.NetworkConfig

-- | Defines the monitoring job.
--
-- /See:/ 'newMonitoringJobDefinition' smart constructor.
data MonitoringJobDefinition = MonitoringJobDefinition'
  { -- | Baseline configuration used to validate that the data conforms to the
    -- specified constraints and statistics
    baselineConfig :: Prelude.Maybe MonitoringBaselineConfig,
    -- | Sets the environment variables in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies networking options for an monitoring job.
    networkConfig :: Prelude.Maybe NetworkConfig,
    -- | Specifies a time limit for how long the monitoring job is allowed to
    -- run.
    stoppingCondition :: Prelude.Maybe MonitoringStoppingCondition,
    -- | The array of inputs for the monitoring job. Currently we support
    -- monitoring an Amazon SageMaker Endpoint.
    monitoringInputs :: Prelude.NonEmpty MonitoringInput,
    -- | The array of outputs from the monitoring job to be uploaded to Amazon
    -- Simple Storage Service (Amazon S3).
    monitoringOutputConfig :: MonitoringOutputConfig,
    -- | Identifies the resources, ML compute instances, and ML storage volumes
    -- to deploy for a monitoring job. In distributed processing, you specify
    -- more than one instance.
    monitoringResources :: MonitoringResources,
    -- | Configures the monitoring job to run a specified Docker container image.
    monitoringAppSpecification :: MonitoringAppSpecification,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineConfig', 'monitoringJobDefinition_baselineConfig' - Baseline configuration used to validate that the data conforms to the
-- specified constraints and statistics
--
-- 'environment', 'monitoringJobDefinition_environment' - Sets the environment variables in the Docker container.
--
-- 'networkConfig', 'monitoringJobDefinition_networkConfig' - Specifies networking options for an monitoring job.
--
-- 'stoppingCondition', 'monitoringJobDefinition_stoppingCondition' - Specifies a time limit for how long the monitoring job is allowed to
-- run.
--
-- 'monitoringInputs', 'monitoringJobDefinition_monitoringInputs' - The array of inputs for the monitoring job. Currently we support
-- monitoring an Amazon SageMaker Endpoint.
--
-- 'monitoringOutputConfig', 'monitoringJobDefinition_monitoringOutputConfig' - The array of outputs from the monitoring job to be uploaded to Amazon
-- Simple Storage Service (Amazon S3).
--
-- 'monitoringResources', 'monitoringJobDefinition_monitoringResources' - Identifies the resources, ML compute instances, and ML storage volumes
-- to deploy for a monitoring job. In distributed processing, you specify
-- more than one instance.
--
-- 'monitoringAppSpecification', 'monitoringJobDefinition_monitoringAppSpecification' - Configures the monitoring job to run a specified Docker container image.
--
-- 'roleArn', 'monitoringJobDefinition_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
newMonitoringJobDefinition ::
  -- | 'monitoringInputs'
  Prelude.NonEmpty MonitoringInput ->
  -- | 'monitoringOutputConfig'
  MonitoringOutputConfig ->
  -- | 'monitoringResources'
  MonitoringResources ->
  -- | 'monitoringAppSpecification'
  MonitoringAppSpecification ->
  -- | 'roleArn'
  Prelude.Text ->
  MonitoringJobDefinition
newMonitoringJobDefinition
  pMonitoringInputs_
  pMonitoringOutputConfig_
  pMonitoringResources_
  pMonitoringAppSpecification_
  pRoleArn_ =
    MonitoringJobDefinition'
      { baselineConfig =
          Prelude.Nothing,
        environment = Prelude.Nothing,
        networkConfig = Prelude.Nothing,
        stoppingCondition = Prelude.Nothing,
        monitoringInputs =
          Lens.coerced Lens.# pMonitoringInputs_,
        monitoringOutputConfig = pMonitoringOutputConfig_,
        monitoringResources = pMonitoringResources_,
        monitoringAppSpecification =
          pMonitoringAppSpecification_,
        roleArn = pRoleArn_
      }

-- | Baseline configuration used to validate that the data conforms to the
-- specified constraints and statistics
monitoringJobDefinition_baselineConfig :: Lens.Lens' MonitoringJobDefinition (Prelude.Maybe MonitoringBaselineConfig)
monitoringJobDefinition_baselineConfig = Lens.lens (\MonitoringJobDefinition' {baselineConfig} -> baselineConfig) (\s@MonitoringJobDefinition' {} a -> s {baselineConfig = a} :: MonitoringJobDefinition)

-- | Sets the environment variables in the Docker container.
monitoringJobDefinition_environment :: Lens.Lens' MonitoringJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
monitoringJobDefinition_environment = Lens.lens (\MonitoringJobDefinition' {environment} -> environment) (\s@MonitoringJobDefinition' {} a -> s {environment = a} :: MonitoringJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Specifies networking options for an monitoring job.
monitoringJobDefinition_networkConfig :: Lens.Lens' MonitoringJobDefinition (Prelude.Maybe NetworkConfig)
monitoringJobDefinition_networkConfig = Lens.lens (\MonitoringJobDefinition' {networkConfig} -> networkConfig) (\s@MonitoringJobDefinition' {} a -> s {networkConfig = a} :: MonitoringJobDefinition)

-- | Specifies a time limit for how long the monitoring job is allowed to
-- run.
monitoringJobDefinition_stoppingCondition :: Lens.Lens' MonitoringJobDefinition (Prelude.Maybe MonitoringStoppingCondition)
monitoringJobDefinition_stoppingCondition = Lens.lens (\MonitoringJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@MonitoringJobDefinition' {} a -> s {stoppingCondition = a} :: MonitoringJobDefinition)

-- | The array of inputs for the monitoring job. Currently we support
-- monitoring an Amazon SageMaker Endpoint.
monitoringJobDefinition_monitoringInputs :: Lens.Lens' MonitoringJobDefinition (Prelude.NonEmpty MonitoringInput)
monitoringJobDefinition_monitoringInputs = Lens.lens (\MonitoringJobDefinition' {monitoringInputs} -> monitoringInputs) (\s@MonitoringJobDefinition' {} a -> s {monitoringInputs = a} :: MonitoringJobDefinition) Prelude.. Lens.coerced

-- | The array of outputs from the monitoring job to be uploaded to Amazon
-- Simple Storage Service (Amazon S3).
monitoringJobDefinition_monitoringOutputConfig :: Lens.Lens' MonitoringJobDefinition MonitoringOutputConfig
monitoringJobDefinition_monitoringOutputConfig = Lens.lens (\MonitoringJobDefinition' {monitoringOutputConfig} -> monitoringOutputConfig) (\s@MonitoringJobDefinition' {} a -> s {monitoringOutputConfig = a} :: MonitoringJobDefinition)

-- | Identifies the resources, ML compute instances, and ML storage volumes
-- to deploy for a monitoring job. In distributed processing, you specify
-- more than one instance.
monitoringJobDefinition_monitoringResources :: Lens.Lens' MonitoringJobDefinition MonitoringResources
monitoringJobDefinition_monitoringResources = Lens.lens (\MonitoringJobDefinition' {monitoringResources} -> monitoringResources) (\s@MonitoringJobDefinition' {} a -> s {monitoringResources = a} :: MonitoringJobDefinition)

-- | Configures the monitoring job to run a specified Docker container image.
monitoringJobDefinition_monitoringAppSpecification :: Lens.Lens' MonitoringJobDefinition MonitoringAppSpecification
monitoringJobDefinition_monitoringAppSpecification = Lens.lens (\MonitoringJobDefinition' {monitoringAppSpecification} -> monitoringAppSpecification) (\s@MonitoringJobDefinition' {} a -> s {monitoringAppSpecification = a} :: MonitoringJobDefinition)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
monitoringJobDefinition_roleArn :: Lens.Lens' MonitoringJobDefinition Prelude.Text
monitoringJobDefinition_roleArn = Lens.lens (\MonitoringJobDefinition' {roleArn} -> roleArn) (\s@MonitoringJobDefinition' {} a -> s {roleArn = a} :: MonitoringJobDefinition)

instance Data.FromJSON MonitoringJobDefinition where
  parseJSON =
    Data.withObject
      "MonitoringJobDefinition"
      ( \x ->
          MonitoringJobDefinition'
            Prelude.<$> (x Data..:? "BaselineConfig")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NetworkConfig")
            Prelude.<*> (x Data..:? "StoppingCondition")
            Prelude.<*> (x Data..: "MonitoringInputs")
            Prelude.<*> (x Data..: "MonitoringOutputConfig")
            Prelude.<*> (x Data..: "MonitoringResources")
            Prelude.<*> (x Data..: "MonitoringAppSpecification")
            Prelude.<*> (x Data..: "RoleArn")
      )

instance Prelude.Hashable MonitoringJobDefinition where
  hashWithSalt _salt MonitoringJobDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` baselineConfig
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` networkConfig
      `Prelude.hashWithSalt` stoppingCondition
      `Prelude.hashWithSalt` monitoringInputs
      `Prelude.hashWithSalt` monitoringOutputConfig
      `Prelude.hashWithSalt` monitoringResources
      `Prelude.hashWithSalt` monitoringAppSpecification
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData MonitoringJobDefinition where
  rnf MonitoringJobDefinition' {..} =
    Prelude.rnf baselineConfig
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf networkConfig
      `Prelude.seq` Prelude.rnf stoppingCondition
      `Prelude.seq` Prelude.rnf monitoringInputs
      `Prelude.seq` Prelude.rnf monitoringOutputConfig
      `Prelude.seq` Prelude.rnf monitoringResources
      `Prelude.seq` Prelude.rnf monitoringAppSpecification
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON MonitoringJobDefinition where
  toJSON MonitoringJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaselineConfig" Data..=)
              Prelude.<$> baselineConfig,
            ("Environment" Data..=) Prelude.<$> environment,
            ("NetworkConfig" Data..=) Prelude.<$> networkConfig,
            ("StoppingCondition" Data..=)
              Prelude.<$> stoppingCondition,
            Prelude.Just
              ("MonitoringInputs" Data..= monitoringInputs),
            Prelude.Just
              ( "MonitoringOutputConfig"
                  Data..= monitoringOutputConfig
              ),
            Prelude.Just
              ("MonitoringResources" Data..= monitoringResources),
            Prelude.Just
              ( "MonitoringAppSpecification"
                  Data..= monitoringAppSpecification
              ),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )
