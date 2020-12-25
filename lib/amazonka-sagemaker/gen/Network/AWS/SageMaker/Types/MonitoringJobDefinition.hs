{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringJobDefinition
  ( MonitoringJobDefinition (..),

    -- * Smart constructor
    mkMonitoringJobDefinition,

    -- * Lenses
    mjdMonitoringInputs,
    mjdMonitoringOutputConfig,
    mjdMonitoringResources,
    mjdMonitoringAppSpecification,
    mjdRoleArn,
    mjdBaselineConfig,
    mjdEnvironment,
    mjdNetworkConfig,
    mjdStoppingCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MonitoringAppSpecification as Types
import qualified Network.AWS.SageMaker.Types.MonitoringBaselineConfig as Types
import qualified Network.AWS.SageMaker.Types.MonitoringInput as Types
import qualified Network.AWS.SageMaker.Types.MonitoringOutputConfig as Types
import qualified Network.AWS.SageMaker.Types.MonitoringResources as Types
import qualified Network.AWS.SageMaker.Types.MonitoringStoppingCondition as Types
import qualified Network.AWS.SageMaker.Types.NetworkConfig as Types
import qualified Network.AWS.SageMaker.Types.ProcessingEnvironmentKey as Types
import qualified Network.AWS.SageMaker.Types.ProcessingEnvironmentValue as Types
import qualified Network.AWS.SageMaker.Types.RoleArn as Types

-- | Defines the monitoring job.
--
-- /See:/ 'mkMonitoringJobDefinition' smart constructor.
data MonitoringJobDefinition = MonitoringJobDefinition'
  { -- | The array of inputs for the monitoring job. Currently we support monitoring an Amazon SageMaker Endpoint.
    monitoringInputs :: Core.NonEmpty Types.MonitoringInput,
    -- | The array of outputs from the monitoring job to be uploaded to Amazon Simple Storage Service (Amazon S3).
    monitoringOutputConfig :: Types.MonitoringOutputConfig,
    -- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a monitoring job. In distributed processing, you specify more than one instance.
    monitoringResources :: Types.MonitoringResources,
    -- | Configures the monitoring job to run a specified Docker container image.
    monitoringAppSpecification :: Types.MonitoringAppSpecification,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
    roleArn :: Types.RoleArn,
    -- | Baseline configuration used to validate that the data conforms to the specified constraints and statistics
    baselineConfig :: Core.Maybe Types.MonitoringBaselineConfig,
    -- | Sets the environment variables in the Docker container.
    environment :: Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue),
    -- | Specifies networking options for an monitoring job.
    networkConfig :: Core.Maybe Types.NetworkConfig,
    -- | Specifies a time limit for how long the monitoring job is allowed to run.
    stoppingCondition :: Core.Maybe Types.MonitoringStoppingCondition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringJobDefinition' value with any optional fields omitted.
mkMonitoringJobDefinition ::
  -- | 'monitoringInputs'
  Core.NonEmpty Types.MonitoringInput ->
  -- | 'monitoringOutputConfig'
  Types.MonitoringOutputConfig ->
  -- | 'monitoringResources'
  Types.MonitoringResources ->
  -- | 'monitoringAppSpecification'
  Types.MonitoringAppSpecification ->
  -- | 'roleArn'
  Types.RoleArn ->
  MonitoringJobDefinition
mkMonitoringJobDefinition
  monitoringInputs
  monitoringOutputConfig
  monitoringResources
  monitoringAppSpecification
  roleArn =
    MonitoringJobDefinition'
      { monitoringInputs,
        monitoringOutputConfig,
        monitoringResources,
        monitoringAppSpecification,
        roleArn,
        baselineConfig = Core.Nothing,
        environment = Core.Nothing,
        networkConfig = Core.Nothing,
        stoppingCondition = Core.Nothing
      }

-- | The array of inputs for the monitoring job. Currently we support monitoring an Amazon SageMaker Endpoint.
--
-- /Note:/ Consider using 'monitoringInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringInputs :: Lens.Lens' MonitoringJobDefinition (Core.NonEmpty Types.MonitoringInput)
mjdMonitoringInputs = Lens.field @"monitoringInputs"
{-# DEPRECATED mjdMonitoringInputs "Use generic-lens or generic-optics with 'monitoringInputs' instead." #-}

-- | The array of outputs from the monitoring job to be uploaded to Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'monitoringOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringOutputConfig :: Lens.Lens' MonitoringJobDefinition Types.MonitoringOutputConfig
mjdMonitoringOutputConfig = Lens.field @"monitoringOutputConfig"
{-# DEPRECATED mjdMonitoringOutputConfig "Use generic-lens or generic-optics with 'monitoringOutputConfig' instead." #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a monitoring job. In distributed processing, you specify more than one instance.
--
-- /Note:/ Consider using 'monitoringResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringResources :: Lens.Lens' MonitoringJobDefinition Types.MonitoringResources
mjdMonitoringResources = Lens.field @"monitoringResources"
{-# DEPRECATED mjdMonitoringResources "Use generic-lens or generic-optics with 'monitoringResources' instead." #-}

-- | Configures the monitoring job to run a specified Docker container image.
--
-- /Note:/ Consider using 'monitoringAppSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringAppSpecification :: Lens.Lens' MonitoringJobDefinition Types.MonitoringAppSpecification
mjdMonitoringAppSpecification = Lens.field @"monitoringAppSpecification"
{-# DEPRECATED mjdMonitoringAppSpecification "Use generic-lens or generic-optics with 'monitoringAppSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdRoleArn :: Lens.Lens' MonitoringJobDefinition Types.RoleArn
mjdRoleArn = Lens.field @"roleArn"
{-# DEPRECATED mjdRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Baseline configuration used to validate that the data conforms to the specified constraints and statistics
--
-- /Note:/ Consider using 'baselineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdBaselineConfig :: Lens.Lens' MonitoringJobDefinition (Core.Maybe Types.MonitoringBaselineConfig)
mjdBaselineConfig = Lens.field @"baselineConfig"
{-# DEPRECATED mjdBaselineConfig "Use generic-lens or generic-optics with 'baselineConfig' instead." #-}

-- | Sets the environment variables in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdEnvironment :: Lens.Lens' MonitoringJobDefinition (Core.Maybe (Core.HashMap Types.ProcessingEnvironmentKey Types.ProcessingEnvironmentValue))
mjdEnvironment = Lens.field @"environment"
{-# DEPRECATED mjdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Specifies networking options for an monitoring job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdNetworkConfig :: Lens.Lens' MonitoringJobDefinition (Core.Maybe Types.NetworkConfig)
mjdNetworkConfig = Lens.field @"networkConfig"
{-# DEPRECATED mjdNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | Specifies a time limit for how long the monitoring job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdStoppingCondition :: Lens.Lens' MonitoringJobDefinition (Core.Maybe Types.MonitoringStoppingCondition)
mjdStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED mjdStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

instance Core.FromJSON MonitoringJobDefinition where
  toJSON MonitoringJobDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MonitoringInputs" Core..= monitoringInputs),
            Core.Just
              ("MonitoringOutputConfig" Core..= monitoringOutputConfig),
            Core.Just ("MonitoringResources" Core..= monitoringResources),
            Core.Just
              ("MonitoringAppSpecification" Core..= monitoringAppSpecification),
            Core.Just ("RoleArn" Core..= roleArn),
            ("BaselineConfig" Core..=) Core.<$> baselineConfig,
            ("Environment" Core..=) Core.<$> environment,
            ("NetworkConfig" Core..=) Core.<$> networkConfig,
            ("StoppingCondition" Core..=) Core.<$> stoppingCondition
          ]
      )

instance Core.FromJSON MonitoringJobDefinition where
  parseJSON =
    Core.withObject "MonitoringJobDefinition" Core.$
      \x ->
        MonitoringJobDefinition'
          Core.<$> (x Core..: "MonitoringInputs")
          Core.<*> (x Core..: "MonitoringOutputConfig")
          Core.<*> (x Core..: "MonitoringResources")
          Core.<*> (x Core..: "MonitoringAppSpecification")
          Core.<*> (x Core..: "RoleArn")
          Core.<*> (x Core..:? "BaselineConfig")
          Core.<*> (x Core..:? "Environment")
          Core.<*> (x Core..:? "NetworkConfig")
          Core.<*> (x Core..:? "StoppingCondition")
