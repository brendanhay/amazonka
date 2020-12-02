{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude
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
--
--
-- /See:/ 'hyperParameterTrainingJobDefinition' smart constructor.
data HyperParameterTrainingJobDefinition = HyperParameterTrainingJobDefinition'
  { _hptjdTuningObjective ::
      !( Maybe
           HyperParameterTuningJobObjective
       ),
    _hptjdCheckpointConfig ::
      !( Maybe
           CheckpointConfig
       ),
    _hptjdHyperParameterRanges ::
      !( Maybe
           ParameterRanges
       ),
    _hptjdEnableNetworkIsolation ::
      !(Maybe Bool),
    _hptjdStaticHyperParameters ::
      !( Maybe
           ( Map
               Text
               (Text)
           )
       ),
    _hptjdEnableManagedSpotTraining ::
      !(Maybe Bool),
    _hptjdInputDataConfig ::
      !( Maybe
           ( List1
               Channel
           )
       ),
    _hptjdVPCConfig ::
      !(Maybe VPCConfig),
    _hptjdDefinitionName ::
      !(Maybe Text),
    _hptjdEnableInterContainerTrafficEncryption ::
      !(Maybe Bool),
    _hptjdAlgorithmSpecification ::
      !HyperParameterAlgorithmSpecification,
    _hptjdRoleARN ::
      !Text,
    _hptjdOutputDataConfig ::
      !OutputDataConfig,
    _hptjdResourceConfig ::
      !ResourceConfig,
    _hptjdStoppingCondition ::
      !StoppingCondition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterTrainingJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hptjdTuningObjective' - Undocumented member.
--
-- * 'hptjdCheckpointConfig' - Undocumented member.
--
-- * 'hptjdHyperParameterRanges' - Undocumented member.
--
-- * 'hptjdEnableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
--
-- * 'hptjdStaticHyperParameters' - Specifies the values of hyperparameters that do not change for the tuning job.
--
-- * 'hptjdEnableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
--
-- * 'hptjdInputDataConfig' - An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
--
-- * 'hptjdVPCConfig' - The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
--
-- * 'hptjdDefinitionName' - The job definition name.
--
-- * 'hptjdEnableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
--
-- * 'hptjdAlgorithmSpecification' - The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
--
-- * 'hptjdRoleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
--
-- * 'hptjdOutputDataConfig' - Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
--
-- * 'hptjdResourceConfig' - The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches. Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
--
-- * 'hptjdStoppingCondition' - Specifies a limit to how long a model hyperparameter training job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the a limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
hyperParameterTrainingJobDefinition ::
  -- | 'hptjdAlgorithmSpecification'
  HyperParameterAlgorithmSpecification ->
  -- | 'hptjdRoleARN'
  Text ->
  -- | 'hptjdOutputDataConfig'
  OutputDataConfig ->
  -- | 'hptjdResourceConfig'
  ResourceConfig ->
  -- | 'hptjdStoppingCondition'
  StoppingCondition ->
  HyperParameterTrainingJobDefinition
hyperParameterTrainingJobDefinition
  pAlgorithmSpecification_
  pRoleARN_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    HyperParameterTrainingJobDefinition'
      { _hptjdTuningObjective =
          Nothing,
        _hptjdCheckpointConfig = Nothing,
        _hptjdHyperParameterRanges = Nothing,
        _hptjdEnableNetworkIsolation = Nothing,
        _hptjdStaticHyperParameters = Nothing,
        _hptjdEnableManagedSpotTraining = Nothing,
        _hptjdInputDataConfig = Nothing,
        _hptjdVPCConfig = Nothing,
        _hptjdDefinitionName = Nothing,
        _hptjdEnableInterContainerTrafficEncryption = Nothing,
        _hptjdAlgorithmSpecification = pAlgorithmSpecification_,
        _hptjdRoleARN = pRoleARN_,
        _hptjdOutputDataConfig = pOutputDataConfig_,
        _hptjdResourceConfig = pResourceConfig_,
        _hptjdStoppingCondition = pStoppingCondition_
      }

-- | Undocumented member.
hptjdTuningObjective :: Lens' HyperParameterTrainingJobDefinition (Maybe HyperParameterTuningJobObjective)
hptjdTuningObjective = lens _hptjdTuningObjective (\s a -> s {_hptjdTuningObjective = a})

-- | Undocumented member.
hptjdCheckpointConfig :: Lens' HyperParameterTrainingJobDefinition (Maybe CheckpointConfig)
hptjdCheckpointConfig = lens _hptjdCheckpointConfig (\s a -> s {_hptjdCheckpointConfig = a})

-- | Undocumented member.
hptjdHyperParameterRanges :: Lens' HyperParameterTrainingJobDefinition (Maybe ParameterRanges)
hptjdHyperParameterRanges = lens _hptjdHyperParameterRanges (\s a -> s {_hptjdHyperParameterRanges = a})

-- | Isolates the training container. No inbound or outbound network calls can be made, except for calls between peers within a training cluster for distributed training. If network isolation is used for training jobs that are configured to use a VPC, Amazon SageMaker downloads and uploads customer data and model artifacts through the specified VPC, but the training container does not have network access.
hptjdEnableNetworkIsolation :: Lens' HyperParameterTrainingJobDefinition (Maybe Bool)
hptjdEnableNetworkIsolation = lens _hptjdEnableNetworkIsolation (\s a -> s {_hptjdEnableNetworkIsolation = a})

-- | Specifies the values of hyperparameters that do not change for the tuning job.
hptjdStaticHyperParameters :: Lens' HyperParameterTrainingJobDefinition (HashMap Text (Text))
hptjdStaticHyperParameters = lens _hptjdStaticHyperParameters (\s a -> s {_hptjdStaticHyperParameters = a}) . _Default . _Map

-- | A Boolean indicating whether managed spot training is enabled (@True@ ) or not (@False@ ).
hptjdEnableManagedSpotTraining :: Lens' HyperParameterTrainingJobDefinition (Maybe Bool)
hptjdEnableManagedSpotTraining = lens _hptjdEnableManagedSpotTraining (\s a -> s {_hptjdEnableManagedSpotTraining = a})

-- | An array of 'Channel' objects that specify the input for the training jobs that the tuning job launches.
hptjdInputDataConfig :: Lens' HyperParameterTrainingJobDefinition (Maybe (NonEmpty Channel))
hptjdInputDataConfig = lens _hptjdInputDataConfig (\s a -> s {_hptjdInputDataConfig = a}) . mapping _List1

-- | The 'VpcConfig' object that specifies the VPC that you want the training jobs that this hyperparameter tuning job launches to connect to. Control access to and from your training container by configuring the VPC. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud> .
hptjdVPCConfig :: Lens' HyperParameterTrainingJobDefinition (Maybe VPCConfig)
hptjdVPCConfig = lens _hptjdVPCConfig (\s a -> s {_hptjdVPCConfig = a})

-- | The job definition name.
hptjdDefinitionName :: Lens' HyperParameterTrainingJobDefinition (Maybe Text)
hptjdDefinitionName = lens _hptjdDefinitionName (\s a -> s {_hptjdDefinitionName = a})

-- | To encrypt all communications between ML compute instances in distributed training, choose @True@ . Encryption provides greater security for distributed training, but training might take longer. How long it takes depends on the amount of communication between compute instances, especially if you use a deep learning algorithm in distributed training.
hptjdEnableInterContainerTrafficEncryption :: Lens' HyperParameterTrainingJobDefinition (Maybe Bool)
hptjdEnableInterContainerTrafficEncryption = lens _hptjdEnableInterContainerTrafficEncryption (\s a -> s {_hptjdEnableInterContainerTrafficEncryption = a})

-- | The 'HyperParameterAlgorithmSpecification' object that specifies the resource algorithm to use for the training jobs that the tuning job launches.
hptjdAlgorithmSpecification :: Lens' HyperParameterTrainingJobDefinition HyperParameterAlgorithmSpecification
hptjdAlgorithmSpecification = lens _hptjdAlgorithmSpecification (\s a -> s {_hptjdAlgorithmSpecification = a})

-- | The Amazon Resource Name (ARN) of the IAM role associated with the training jobs that the tuning job launches.
hptjdRoleARN :: Lens' HyperParameterTrainingJobDefinition Text
hptjdRoleARN = lens _hptjdRoleARN (\s a -> s {_hptjdRoleARN = a})

-- | Specifies the path to the Amazon S3 bucket where you store model artifacts from the training jobs that the tuning job launches.
hptjdOutputDataConfig :: Lens' HyperParameterTrainingJobDefinition OutputDataConfig
hptjdOutputDataConfig = lens _hptjdOutputDataConfig (\s a -> s {_hptjdOutputDataConfig = a})

-- | The resources, including the compute instances and storage volumes, to use for the training jobs that the tuning job launches. Storage volumes store model artifacts and incremental states. Training algorithms might also use storage volumes for scratch space. If you want Amazon SageMaker to use the storage volume to store the training data, choose @File@ as the @TrainingInputMode@ in the algorithm specification. For distributed training algorithms, specify an instance count greater than 1.
hptjdResourceConfig :: Lens' HyperParameterTrainingJobDefinition ResourceConfig
hptjdResourceConfig = lens _hptjdResourceConfig (\s a -> s {_hptjdResourceConfig = a})

-- | Specifies a limit to how long a model hyperparameter training job can run. It also specifies how long you are willing to wait for a managed spot training job to complete. When the job reaches the a limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
hptjdStoppingCondition :: Lens' HyperParameterTrainingJobDefinition StoppingCondition
hptjdStoppingCondition = lens _hptjdStoppingCondition (\s a -> s {_hptjdStoppingCondition = a})

instance FromJSON HyperParameterTrainingJobDefinition where
  parseJSON =
    withObject
      "HyperParameterTrainingJobDefinition"
      ( \x ->
          HyperParameterTrainingJobDefinition'
            <$> (x .:? "TuningObjective")
            <*> (x .:? "CheckpointConfig")
            <*> (x .:? "HyperParameterRanges")
            <*> (x .:? "EnableNetworkIsolation")
            <*> (x .:? "StaticHyperParameters" .!= mempty)
            <*> (x .:? "EnableManagedSpotTraining")
            <*> (x .:? "InputDataConfig")
            <*> (x .:? "VpcConfig")
            <*> (x .:? "DefinitionName")
            <*> (x .:? "EnableInterContainerTrafficEncryption")
            <*> (x .: "AlgorithmSpecification")
            <*> (x .: "RoleArn")
            <*> (x .: "OutputDataConfig")
            <*> (x .: "ResourceConfig")
            <*> (x .: "StoppingCondition")
      )

instance Hashable HyperParameterTrainingJobDefinition

instance NFData HyperParameterTrainingJobDefinition

instance ToJSON HyperParameterTrainingJobDefinition where
  toJSON HyperParameterTrainingJobDefinition' {..} =
    object
      ( catMaybes
          [ ("TuningObjective" .=) <$> _hptjdTuningObjective,
            ("CheckpointConfig" .=) <$> _hptjdCheckpointConfig,
            ("HyperParameterRanges" .=) <$> _hptjdHyperParameterRanges,
            ("EnableNetworkIsolation" .=) <$> _hptjdEnableNetworkIsolation,
            ("StaticHyperParameters" .=) <$> _hptjdStaticHyperParameters,
            ("EnableManagedSpotTraining" .=)
              <$> _hptjdEnableManagedSpotTraining,
            ("InputDataConfig" .=) <$> _hptjdInputDataConfig,
            ("VpcConfig" .=) <$> _hptjdVPCConfig,
            ("DefinitionName" .=) <$> _hptjdDefinitionName,
            ("EnableInterContainerTrafficEncryption" .=)
              <$> _hptjdEnableInterContainerTrafficEncryption,
            Just ("AlgorithmSpecification" .= _hptjdAlgorithmSpecification),
            Just ("RoleArn" .= _hptjdRoleARN),
            Just ("OutputDataConfig" .= _hptjdOutputDataConfig),
            Just ("ResourceConfig" .= _hptjdResourceConfig),
            Just ("StoppingCondition" .= _hptjdStoppingCondition)
          ]
      )
