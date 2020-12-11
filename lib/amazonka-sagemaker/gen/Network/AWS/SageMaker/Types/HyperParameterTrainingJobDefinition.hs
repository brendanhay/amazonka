-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
  ( HyperParameterTrainingJobDefinition (..),

    -- * Smart constructor
    mkHyperParameterTrainingJobDefinition,

    -- * Lenses
    hptjdTuningObjective,
    hptjdCheckpointConfig,
    hptjdHyperParameterRanges,
    hptjdEnableNetworkIsolation,
    hptjdStaticHyperParameters,
    hptjdEnableManagedSpotTraining,
    hptjdInputDataConfig,
    hptjdVPCConfig,
    hptjdDefinitionName,
    hptjdEnableInterContainerTrafficEncryption,
    hptjdAlgorithmSpecification,
    hptjdRoleARN,
    hptjdOutputDataConfig,
    hptjdResourceConfig,
    hptjdStoppingCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.CheckpointConfig
import Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.ParameterRanges
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.VPCConfig

-- | Defines the training jobs launched by a hyperparameter tuning job.
--
-- /See:/ 'mkHyperParameterTrainingJobDefinition' smart constructor.
data HyperParameterTrainingJobDefinition = HyperParameterTrainingJobDefinition'
  { tuningObjective ::
      Lude.Maybe
        HyperParameterTuningJobObjective,
    checkpointConfig ::
      Lude.Maybe
        CheckpointConfig,
    hyperParameterRanges ::
      Lude.Maybe
        ParameterRanges,
    enableNetworkIsolation ::
      Lude.Maybe
        Lude.Bool,
    staticHyperParameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    enableManagedSpotTraining ::
      Lude.Maybe
        Lude.Bool,
    inputDataConfig ::
      Lude.Maybe
        ( Lude.NonEmpty
            Channel
        ),
    vpcConfig ::
      Lude.Maybe
        VPCConfig,
    definitionName ::
      Lude.Maybe
        Lude.Text,
    enableInterContainerTrafficEncryption ::
      Lude.Maybe
        Lude.Bool,
    algorithmSpecification ::
      HyperParameterAlgorithmSpecification,
    roleARN ::
      Lude.Text,
    outputDataConfig ::
      OutputDataConfig,
    resourceConfig ::
      ResourceConfig,
    stoppingCondition ::
      StoppingCondition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HyperParameterTrainingJobDefinition' with the minimum fields required to make a request.
--
-- * 'algorithmSpecification' - The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
-- * 'checkpointConfig' - Undocumented field.
-- * 'definitionName' - The job definition name.
-- * 'enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
-- * 'enableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
-- * 'enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
-- * 'hyperParameterRanges' - Undocumented field.
-- * 'inputDataConfig' - An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
-- * 'outputDataConfig' - Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
-- * 'resourceConfig' - The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
-- * 'staticHyperParameters' - Specifies the values of hyperparameters that do not change for the tuning job.
-- * 'stoppingCondition' - Specifies a limit to how long a model hyperparameter training job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the a limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
-- * 'tuningObjective' - Undocumented field.
-- * 'vpcConfig' - The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
mkHyperParameterTrainingJobDefinition ::
  -- | 'algorithmSpecification'
  HyperParameterAlgorithmSpecification ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  HyperParameterTrainingJobDefinition
mkHyperParameterTrainingJobDefinition
  pAlgorithmSpecification_
  pRoleARN_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    HyperParameterTrainingJobDefinition'
      { tuningObjective =
          Lude.Nothing,
        checkpointConfig = Lude.Nothing,
        hyperParameterRanges = Lude.Nothing,
        enableNetworkIsolation = Lude.Nothing,
        staticHyperParameters = Lude.Nothing,
        enableManagedSpotTraining = Lude.Nothing,
        inputDataConfig = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        definitionName = Lude.Nothing,
        enableInterContainerTrafficEncryption = Lude.Nothing,
        algorithmSpecification = pAlgorithmSpecification_,
        roleARN = pRoleARN_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'tuningObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdTuningObjective :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe HyperParameterTuningJobObjective)
hptjdTuningObjective = Lens.lens (tuningObjective :: HyperParameterTrainingJobDefinition -> Lude.Maybe HyperParameterTuningJobObjective) (\s a -> s {tuningObjective = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdTuningObjective "Use generic-lens or generic-optics with 'tuningObjective' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdCheckpointConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe CheckpointConfig)
hptjdCheckpointConfig = Lens.lens (checkpointConfig :: HyperParameterTrainingJobDefinition -> Lude.Maybe CheckpointConfig) (\s a -> s {checkpointConfig = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdCheckpointConfig "Use generic-lens or generic-optics with 'checkpointConfig' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hyperParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdHyperParameterRanges :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe ParameterRanges)
hptjdHyperParameterRanges = Lens.lens (hyperParameterRanges :: HyperParameterTrainingJobDefinition -> Lude.Maybe ParameterRanges) (\s a -> s {hyperParameterRanges = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdHyperParameterRanges "Use generic-lens or generic-optics with 'hyperParameterRanges' instead." #-}

-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdEnableNetworkIsolation :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe Lude.Bool)
hptjdEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: HyperParameterTrainingJobDefinition -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Specifies the values of hyperparameters that do not change for the tuning job.
--
-- /Note:/ Consider using 'staticHyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdStaticHyperParameters :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
hptjdStaticHyperParameters = Lens.lens (staticHyperParameters :: HyperParameterTrainingJobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {staticHyperParameters = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdStaticHyperParameters "Use generic-lens or generic-optics with 'staticHyperParameters' instead." #-}

-- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
--
-- /Note:/ Consider using 'enableManagedSpotTraining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdEnableManagedSpotTraining :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe Lude.Bool)
hptjdEnableManagedSpotTraining = Lens.lens (enableManagedSpotTraining :: HyperParameterTrainingJobDefinition -> Lude.Maybe Lude.Bool) (\s a -> s {enableManagedSpotTraining = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdEnableManagedSpotTraining "Use generic-lens or generic-optics with 'enableManagedSpotTraining' instead." #-}

-- | An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdInputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe (Lude.NonEmpty Channel))
hptjdInputDataConfig = Lens.lens (inputDataConfig :: HyperParameterTrainingJobDefinition -> Lude.Maybe (Lude.NonEmpty Channel)) (\s a -> s {inputDataConfig = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdVPCConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe VPCConfig)
hptjdVPCConfig = Lens.lens (vpcConfig :: HyperParameterTrainingJobDefinition -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The job definition name.
--
-- /Note:/ Consider using 'definitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdDefinitionName :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe Lude.Text)
hptjdDefinitionName = Lens.lens (definitionName :: HyperParameterTrainingJobDefinition -> Lude.Maybe Lude.Text) (\s a -> s {definitionName = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdDefinitionName "Use generic-lens or generic-optics with 'definitionName' instead." #-}

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdEnableInterContainerTrafficEncryption :: Lens.Lens' HyperParameterTrainingJobDefinition (Lude.Maybe Lude.Bool)
hptjdEnableInterContainerTrafficEncryption = Lens.lens (enableInterContainerTrafficEncryption :: HyperParameterTrainingJobDefinition -> Lude.Maybe Lude.Bool) (\s a -> s {enableInterContainerTrafficEncryption = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'algorithmSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdAlgorithmSpecification :: Lens.Lens' HyperParameterTrainingJobDefinition HyperParameterAlgorithmSpecification
hptjdAlgorithmSpecification = Lens.lens (algorithmSpecification :: HyperParameterTrainingJobDefinition -> HyperParameterAlgorithmSpecification) (\s a -> s {algorithmSpecification = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdAlgorithmSpecification "Use generic-lens or generic-optics with 'algorithmSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdRoleARN :: Lens.Lens' HyperParameterTrainingJobDefinition Lude.Text
hptjdRoleARN = Lens.lens (roleARN :: HyperParameterTrainingJobDefinition -> Lude.Text) (\s a -> s {roleARN = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdOutputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition OutputDataConfig
hptjdOutputDataConfig = Lens.lens (outputDataConfig :: HyperParameterTrainingJobDefinition -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdResourceConfig :: Lens.Lens' HyperParameterTrainingJobDefinition ResourceConfig
hptjdResourceConfig = Lens.lens (resourceConfig :: HyperParameterTrainingJobDefinition -> ResourceConfig) (\s a -> s {resourceConfig = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | Specifies a limit to how long a model hyperparameter training job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the a limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjdStoppingCondition :: Lens.Lens' HyperParameterTrainingJobDefinition StoppingCondition
hptjdStoppingCondition = Lens.lens (stoppingCondition :: HyperParameterTrainingJobDefinition -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: HyperParameterTrainingJobDefinition)
{-# DEPRECATED hptjdStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

instance Lude.FromJSON HyperParameterTrainingJobDefinition where
  parseJSON =
    Lude.withObject
      "HyperParameterTrainingJobDefinition"
      ( \x ->
          HyperParameterTrainingJobDefinition'
            Lude.<$> (x Lude..:? "TuningObjective")
            Lude.<*> (x Lude..:? "CheckpointConfig")
            Lude.<*> (x Lude..:? "HyperParameterRanges")
            Lude.<*> (x Lude..:? "EnableNetworkIsolation")
            Lude.<*> (x Lude..:? "StaticHyperParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EnableManagedSpotTraining")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "DefinitionName")
            Lude.<*> (x Lude..:? "EnableInterContainerTrafficEncryption")
            Lude.<*> (x Lude..: "AlgorithmSpecification")
            Lude.<*> (x Lude..: "RoleArn")
            Lude.<*> (x Lude..: "OutputDataConfig")
            Lude.<*> (x Lude..: "ResourceConfig")
            Lude.<*> (x Lude..: "StoppingCondition")
      )

instance Lude.ToJSON HyperParameterTrainingJobDefinition where
  toJSON HyperParameterTrainingJobDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TuningObjective" Lude..=) Lude.<$> tuningObjective,
            ("CheckpointConfig" Lude..=) Lude.<$> checkpointConfig,
            ("HyperParameterRanges" Lude..=) Lude.<$> hyperParameterRanges,
            ("EnableNetworkIsolation" Lude..=) Lude.<$> enableNetworkIsolation,
            ("StaticHyperParameters" Lude..=) Lude.<$> staticHyperParameters,
            ("EnableManagedSpotTraining" Lude..=)
              Lude.<$> enableManagedSpotTraining,
            ("InputDataConfig" Lude..=) Lude.<$> inputDataConfig,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("DefinitionName" Lude..=) Lude.<$> definitionName,
            ("EnableInterContainerTrafficEncryption" Lude..=)
              Lude.<$> enableInterContainerTrafficEncryption,
            Lude.Just
              ("AlgorithmSpecification" Lude..= algorithmSpecification),
            Lude.Just ("RoleArn" Lude..= roleARN),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("ResourceConfig" Lude..= resourceConfig),
            Lude.Just ("StoppingCondition" Lude..= stoppingCondition)
          ]
      )
