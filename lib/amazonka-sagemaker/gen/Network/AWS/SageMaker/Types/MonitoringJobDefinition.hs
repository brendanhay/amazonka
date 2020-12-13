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
    mjdEnvironment,
    mjdStoppingCondition,
    mjdMonitoringOutputConfig,
    mjdNetworkConfig,
    mjdMonitoringResources,
    mjdMonitoringAppSpecification,
    mjdBaselineConfig,
    mjdMonitoringInputs,
    mjdRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.MonitoringAppSpecification
import Network.AWS.SageMaker.Types.MonitoringBaselineConfig
import Network.AWS.SageMaker.Types.MonitoringInput
import Network.AWS.SageMaker.Types.MonitoringOutputConfig
import Network.AWS.SageMaker.Types.MonitoringResources
import Network.AWS.SageMaker.Types.MonitoringStoppingCondition
import Network.AWS.SageMaker.Types.NetworkConfig

-- | Defines the monitoring job.
--
-- /See:/ 'mkMonitoringJobDefinition' smart constructor.
data MonitoringJobDefinition = MonitoringJobDefinition'
  { -- | Sets the environment variables in the Docker container.
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Specifies a time limit for how long the monitoring job is allowed to run.
    stoppingCondition :: Lude.Maybe MonitoringStoppingCondition,
    -- | The array of outputs from the monitoring job to be uploaded to Amazon Simple Storage Service (Amazon S3).
    monitoringOutputConfig :: MonitoringOutputConfig,
    -- | Specifies networking options for an monitoring job.
    networkConfig :: Lude.Maybe NetworkConfig,
    -- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a monitoring job. In distributed processing, you specify more than one instance.
    monitoringResources :: MonitoringResources,
    -- | Configures the monitoring job to run a specified Docker container image.
    monitoringAppSpecification :: MonitoringAppSpecification,
    -- | Baseline configuration used to validate that the data conforms to the specified constraints and statistics
    baselineConfig :: Lude.Maybe MonitoringBaselineConfig,
    -- | The array of inputs for the monitoring job. Currently we support monitoring an Amazon SageMaker Endpoint.
    monitoringInputs :: Lude.NonEmpty MonitoringInput,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringJobDefinition' with the minimum fields required to make a request.
--
-- * 'environment' - Sets the environment variables in the Docker container.
-- * 'stoppingCondition' - Specifies a time limit for how long the monitoring job is allowed to run.
-- * 'monitoringOutputConfig' - The array of outputs from the monitoring job to be uploaded to Amazon Simple Storage Service (Amazon S3).
-- * 'networkConfig' - Specifies networking options for an monitoring job.
-- * 'monitoringResources' - Identifies the resources, ML compute instances, and ML storage volumes to deploy for a monitoring job. In distributed processing, you specify more than one instance.
-- * 'monitoringAppSpecification' - Configures the monitoring job to run a specified Docker container image.
-- * 'baselineConfig' - Baseline configuration used to validate that the data conforms to the specified constraints and statistics
-- * 'monitoringInputs' - The array of inputs for the monitoring job. Currently we support monitoring an Amazon SageMaker Endpoint.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
mkMonitoringJobDefinition ::
  -- | 'monitoringOutputConfig'
  MonitoringOutputConfig ->
  -- | 'monitoringResources'
  MonitoringResources ->
  -- | 'monitoringAppSpecification'
  MonitoringAppSpecification ->
  -- | 'monitoringInputs'
  Lude.NonEmpty MonitoringInput ->
  -- | 'roleARN'
  Lude.Text ->
  MonitoringJobDefinition
mkMonitoringJobDefinition
  pMonitoringOutputConfig_
  pMonitoringResources_
  pMonitoringAppSpecification_
  pMonitoringInputs_
  pRoleARN_ =
    MonitoringJobDefinition'
      { environment = Lude.Nothing,
        stoppingCondition = Lude.Nothing,
        monitoringOutputConfig = pMonitoringOutputConfig_,
        networkConfig = Lude.Nothing,
        monitoringResources = pMonitoringResources_,
        monitoringAppSpecification = pMonitoringAppSpecification_,
        baselineConfig = Lude.Nothing,
        monitoringInputs = pMonitoringInputs_,
        roleARN = pRoleARN_
      }

-- | Sets the environment variables in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdEnvironment :: Lens.Lens' MonitoringJobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
mjdEnvironment = Lens.lens (environment :: MonitoringJobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Specifies a time limit for how long the monitoring job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdStoppingCondition :: Lens.Lens' MonitoringJobDefinition (Lude.Maybe MonitoringStoppingCondition)
mjdStoppingCondition = Lens.lens (stoppingCondition :: MonitoringJobDefinition -> Lude.Maybe MonitoringStoppingCondition) (\s a -> s {stoppingCondition = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | The array of outputs from the monitoring job to be uploaded to Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'monitoringOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringOutputConfig :: Lens.Lens' MonitoringJobDefinition MonitoringOutputConfig
mjdMonitoringOutputConfig = Lens.lens (monitoringOutputConfig :: MonitoringJobDefinition -> MonitoringOutputConfig) (\s a -> s {monitoringOutputConfig = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdMonitoringOutputConfig "Use generic-lens or generic-optics with 'monitoringOutputConfig' instead." #-}

-- | Specifies networking options for an monitoring job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdNetworkConfig :: Lens.Lens' MonitoringJobDefinition (Lude.Maybe NetworkConfig)
mjdNetworkConfig = Lens.lens (networkConfig :: MonitoringJobDefinition -> Lude.Maybe NetworkConfig) (\s a -> s {networkConfig = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a monitoring job. In distributed processing, you specify more than one instance.
--
-- /Note:/ Consider using 'monitoringResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringResources :: Lens.Lens' MonitoringJobDefinition MonitoringResources
mjdMonitoringResources = Lens.lens (monitoringResources :: MonitoringJobDefinition -> MonitoringResources) (\s a -> s {monitoringResources = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdMonitoringResources "Use generic-lens or generic-optics with 'monitoringResources' instead." #-}

-- | Configures the monitoring job to run a specified Docker container image.
--
-- /Note:/ Consider using 'monitoringAppSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringAppSpecification :: Lens.Lens' MonitoringJobDefinition MonitoringAppSpecification
mjdMonitoringAppSpecification = Lens.lens (monitoringAppSpecification :: MonitoringJobDefinition -> MonitoringAppSpecification) (\s a -> s {monitoringAppSpecification = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdMonitoringAppSpecification "Use generic-lens or generic-optics with 'monitoringAppSpecification' instead." #-}

-- | Baseline configuration used to validate that the data conforms to the specified constraints and statistics
--
-- /Note:/ Consider using 'baselineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdBaselineConfig :: Lens.Lens' MonitoringJobDefinition (Lude.Maybe MonitoringBaselineConfig)
mjdBaselineConfig = Lens.lens (baselineConfig :: MonitoringJobDefinition -> Lude.Maybe MonitoringBaselineConfig) (\s a -> s {baselineConfig = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdBaselineConfig "Use generic-lens or generic-optics with 'baselineConfig' instead." #-}

-- | The array of inputs for the monitoring job. Currently we support monitoring an Amazon SageMaker Endpoint.
--
-- /Note:/ Consider using 'monitoringInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdMonitoringInputs :: Lens.Lens' MonitoringJobDefinition (Lude.NonEmpty MonitoringInput)
mjdMonitoringInputs = Lens.lens (monitoringInputs :: MonitoringJobDefinition -> Lude.NonEmpty MonitoringInput) (\s a -> s {monitoringInputs = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdMonitoringInputs "Use generic-lens or generic-optics with 'monitoringInputs' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mjdRoleARN :: Lens.Lens' MonitoringJobDefinition Lude.Text
mjdRoleARN = Lens.lens (roleARN :: MonitoringJobDefinition -> Lude.Text) (\s a -> s {roleARN = a} :: MonitoringJobDefinition)
{-# DEPRECATED mjdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON MonitoringJobDefinition where
  parseJSON =
    Lude.withObject
      "MonitoringJobDefinition"
      ( \x ->
          MonitoringJobDefinition'
            Lude.<$> (x Lude..:? "Environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StoppingCondition")
            Lude.<*> (x Lude..: "MonitoringOutputConfig")
            Lude.<*> (x Lude..:? "NetworkConfig")
            Lude.<*> (x Lude..: "MonitoringResources")
            Lude.<*> (x Lude..: "MonitoringAppSpecification")
            Lude.<*> (x Lude..:? "BaselineConfig")
            Lude.<*> (x Lude..: "MonitoringInputs")
            Lude.<*> (x Lude..: "RoleArn")
      )

instance Lude.ToJSON MonitoringJobDefinition where
  toJSON MonitoringJobDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Environment" Lude..=) Lude.<$> environment,
            ("StoppingCondition" Lude..=) Lude.<$> stoppingCondition,
            Lude.Just
              ("MonitoringOutputConfig" Lude..= monitoringOutputConfig),
            ("NetworkConfig" Lude..=) Lude.<$> networkConfig,
            Lude.Just ("MonitoringResources" Lude..= monitoringResources),
            Lude.Just
              ("MonitoringAppSpecification" Lude..= monitoringAppSpecification),
            ("BaselineConfig" Lude..=) Lude.<$> baselineConfig,
            Lude.Just ("MonitoringInputs" Lude..= monitoringInputs),
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )
