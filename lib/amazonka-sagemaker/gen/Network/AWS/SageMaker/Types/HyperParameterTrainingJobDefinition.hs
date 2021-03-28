{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
  ( HyperParameterTrainingJobDefinition (..)
  -- * Smart constructor
  , mkHyperParameterTrainingJobDefinition
  -- * Lenses
  , hptjdAlgorithmSpecification
  , hptjdRoleArn
  , hptjdOutputDataConfig
  , hptjdResourceConfig
  , hptjdStoppingCondition
  , hptjdCheckpointConfig
  , hptjdDefinitionName
  , hptjdEnableInterContainerTrafficEncryption
  , hptjdEnableManagedSpotTraining
  , hptjdEnableNetworkIsolation
  , hptjdHyperParameterRanges
  , hptjdInputDataConfig
  , hptjdStaticHyperParameters
  , hptjdTuningObjective
  , hptjdVpcConfig
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Channel as Types
import qualified Network.AWS.SageMaker.Types.CheckpointConfig as Types
import qualified Network.AWS.SageMaker.Types.DefinitionName as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterKey as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterValue as Types
import qualified Network.AWS.SageMaker.Types.OutputDataConfig as Types
import qualified Network.AWS.SageMaker.Types.ParameterRanges as Types
import qualified Network.AWS.SageMaker.Types.ResourceConfig as Types
import qualified Network.AWS.SageMaker.Types.RoleArn as Types
import qualified Network.AWS.SageMaker.Types.StoppingCondition as Types
import qualified Network.AWS.SageMaker.Types.VpcConfig as Types

-- | Defines the training jobs launched by a hyperparameter tuning job.
--
-- /See:/ 'mkHyperParameterTrainingJobDefinition' smart constructor.
data HyperParameterTrainingJobDefinition = HyperParameterTrainingJobDefinition'
  { algorithmSpecification :: Types.HyperParameterAlgorithmSpecification
    -- ^ The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
  , outputDataConfig :: Types.OutputDataConfig
    -- ^ Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
  , resourceConfig :: Types.ResourceConfig
    -- ^ The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
  , stoppingCondition :: Types.StoppingCondition
    -- ^ Specifies a limit to how long a model hyperparameter training job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the a limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
  , checkpointConfig :: Core.Maybe Types.CheckpointConfig
  , definitionName :: Core.Maybe Types.DefinitionName
    -- ^ The job definition name.
  , enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool
    -- ^ To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
  , enableManagedSpotTraining :: Core.Maybe Core.Bool
    -- ^ A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
  , enableNetworkIsolation :: Core.Maybe Core.Bool
    -- ^ Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
  , hyperParameterRanges :: Core.Maybe Types.ParameterRanges
  , inputDataConfig :: Core.Maybe (Core.NonEmpty Types.Channel)
    -- ^ An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
  , staticHyperParameters :: Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue)
    -- ^ Specifies the values of hyperparameters that do not change for the tuning job.
  , tuningObjective :: Core.Maybe Types.HyperParameterTuningJobObjective
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HyperParameterTrainingJobDefinition' value with any optional fields omitted.
mkHyperParameterTrainingJobDefinition
    :: Types.HyperParameterAlgorithmSpecification -- ^ 'algorithmSpecification'
    -> Types.RoleArn -- ^ 'roleArn'
    -> Types.OutputDataConfig -- ^ 'outputDataConfig'
    -> Types.ResourceConfig -- ^ 'resourceConfig'
    -> Types.StoppingCondition -- ^ 'stoppingCondition'
    -> HyperParameterTrainingJobDefinition
mkHyperParameterTrainingJobDefinition algorithmSpecification
  roleArn outputDataConfig resourceConfig stoppingCondition
  = HyperParameterTrainingJobDefinition'{algorithmSpecification,
                                         roleArn, outputDataConfig, resourceConfig,
                                         stoppingCondition, checkpointConfig = Core.Nothing,
                                         definitionName = Core.Nothing,
                                         enableInterContainerTrafficEncryption = Core.Nothing,
                                         enableManagedSpotTraining = Core.Nothing,
                                         enableNetworkIsolation = Core.Nothing,
                                         hyperParameterRanges = Core.Nothing,
                                         inputDataConfig = Core.Nothing,
                                         staticHyperParameters = Core.Nothing,
                                         tuningObjective = Core.Nothing, vpcConfig = Core.Nothing}

-- | The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdAlgorithmSpecification :: Lens.Lens' HyperParameterTrainingJobDefinition Types.HyperParameterAlgorithmSpecification
hptjdAlgorithmSpecification = Lens.field @"algorithmSpecification"
{-# INLINEABLE hptjdAlgorithmSpecification #-}
{-# DEPRECATED algorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdRoleArn :: Lens.Lens' HyperParameterTrainingJobDefinition Types.RoleArn
hptjdRoleArn = Lens.field @"roleArn"
{-# INLINEABLE hptjdRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdOutputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition Types.OutputDataConfig
hptjdOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE hptjdOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdResourceConfig :: Lens.Lens' HyperParameterTrainingJobDefinition Types.ResourceConfig
hptjdResourceConfig = Lens.field @"resourceConfig"
{-# INLINEABLE hptjdResourceConfig #-}
{-# DEPRECATED resourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead"  #-}

-- | Specifies a limit to how long a model hyperparameter training job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the a limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdStoppingCondition :: Lens.Lens' HyperParameterTrainingJobDefinition Types.StoppingCondition
hptjdStoppingCondition = Lens.field @"stoppingCondition"
{-# INLINEABLE hptjdStoppingCondition #-}
{-# DEPRECATED stoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdCheckpointConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Types.CheckpointConfig)
hptjdCheckpointConfig = Lens.field @"checkpointConfig"
{-# INLINEABLE hptjdCheckpointConfig #-}
{-# DEPRECATED checkpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead"  #-}

-- | The job definition name.
--
-- /Note:/ Consider using 'definitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdDefinitionName :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Types.DefinitionName)
hptjdDefinitionName = Lens.field @"definitionName"
{-# INLINEABLE hptjdDefinitionName #-}
{-# DEPRECATED definitionName "Use generic-lens or generic-optics with 'definitionName' instead"  #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdEnableInterContainerTrafficEncryption :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Core.Bool)
hptjdEnableInterContainerTrafficEncryption = Lens.field @"enableInterContainerTrafficEncryption"
{-# INLINEABLE hptjdEnableInterContainerTrafficEncryption #-}
{-# DEPRECATED enableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead"  #-}

-- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdEnableManagedSpotTraining :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Core.Bool)
hptjdEnableManagedSpotTraining = Lens.field @"enableManagedSpotTraining"
{-# INLINEABLE hptjdEnableManagedSpotTraining #-}
{-# DEPRECATED enableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead"  #-}

-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdEnableNetworkIsolation :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Core.Bool)
hptjdEnableNetworkIsolation = Lens.field @"enableNetworkIsolation"
{-# INLINEABLE hptjdEnableNetworkIsolation #-}
{-# DEPRECATED enableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hyperParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdHyperParameterRanges :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Types.ParameterRanges)
hptjdHyperParameterRanges = Lens.field @"hyperParameterRanges"
{-# INLINEABLE hptjdHyperParameterRanges #-}
{-# DEPRECATED hyperParameterRanges "Use generic-lens or generic-optics with 'hyperParameterRanges' instead"  #-}

-- | An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdInputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe (Core.NonEmpty Types.Channel))
hptjdInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE hptjdInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Specifies the values of hyperparameters that do not change for the tuning job.
--
-- /Note:/ Consider using 'staticHyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdStaticHyperParameters :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue))
hptjdStaticHyperParameters = Lens.field @"staticHyperParameters"
{-# INLINEABLE hptjdStaticHyperParameters #-}
{-# DEPRECATED staticHyperParameters "Use generic-lens or generic-optics with 'staticHyperParameters' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tuningObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdTuningObjective :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Types.HyperParameterTuningJobObjective)
hptjdTuningObjective = Lens.field @"tuningObjective"
{-# INLINEABLE hptjdTuningObjective #-}
{-# DEPRECATED tuningObjective "Use generic-lens or generic-optics with 'tuningObjective' instead"  #-}

-- | The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdVpcConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Types.VpcConfig)
hptjdVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE hptjdVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.FromJSON HyperParameterTrainingJobDefinition where
        toJSON HyperParameterTrainingJobDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("AlgorithmSpecification" Core..= algorithmSpecification),
                  Core.Just ("RoleArn" Core..= roleArn),
                  Core.Just ("OutputDataConfig" Core..= outputDataConfig),
                  Core.Just ("ResourceConfig" Core..= resourceConfig),
                  Core.Just ("StoppingCondition" Core..= stoppingCondition),
                  ("CheckpointConfig" Core..=) Core.<$> checkpointConfig,
                  ("DefinitionName" Core..=) Core.<$> definitionName,
                  ("EnableInterContainerTrafficEncryption" Core..=) Core.<$>
                    enableInterContainerTrafficEncryption,
                  ("EnableManagedSpotTraining" Core..=) Core.<$>
                    enableManagedSpotTraining,
                  ("EnableNetworkIsolation" Core..=) Core.<$> enableNetworkIsolation,
                  ("HyperParameterRanges" Core..=) Core.<$> hyperParameterRanges,
                  ("InputDataConfig" Core..=) Core.<$> inputDataConfig,
                  ("StaticHyperParameters" Core..=) Core.<$> staticHyperParameters,
                  ("TuningObjective" Core..=) Core.<$> tuningObjective,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.FromJSON HyperParameterTrainingJobDefinition where
        parseJSON
          = Core.withObject "HyperParameterTrainingJobDefinition" Core.$
              \ x ->
                HyperParameterTrainingJobDefinition' Core.<$>
                  (x Core..: "AlgorithmSpecification") Core.<*> x Core..: "RoleArn"
                    Core.<*> x Core..: "OutputDataConfig"
                    Core.<*> x Core..: "ResourceConfig"
                    Core.<*> x Core..: "StoppingCondition"
                    Core.<*> x Core..:? "CheckpointConfig"
                    Core.<*> x Core..:? "DefinitionName"
                    Core.<*> x Core..:? "EnableInterContainerTrafficEncryption"
                    Core.<*> x Core..:? "EnableManagedSpotTraining"
                    Core.<*> x Core..:? "EnableNetworkIsolation"
                    Core.<*> x Core..:? "HyperParameterRanges"
                    Core.<*> x Core..:? "InputDataConfig"
                    Core.<*> x Core..:? "StaticHyperParameters"
                    Core.<*> x Core..:? "TuningObjective"
                    Core.<*> x Core..:? "VpcConfig"
