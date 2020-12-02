{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringJobDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MonitoringAppSpecification
import Network.AWS.SageMaker.Types.MonitoringBaselineConfig
import Network.AWS.SageMaker.Types.MonitoringInput
import Network.AWS.SageMaker.Types.MonitoringOutputConfig
import Network.AWS.SageMaker.Types.MonitoringResources
import Network.AWS.SageMaker.Types.MonitoringStoppingCondition
import Network.AWS.SageMaker.Types.NetworkConfig

-- | Defines the monitoring job.
--
--
--
-- /See:/ 'monitoringJobDefinition' smart constructor.
data MonitoringJobDefinition = MonitoringJobDefinition'
  { _mjdEnvironment ::
      !(Maybe (Map Text (Text))),
    _mjdStoppingCondition ::
      !(Maybe MonitoringStoppingCondition),
    _mjdNetworkConfig :: !(Maybe NetworkConfig),
    _mjdBaselineConfig ::
      !(Maybe MonitoringBaselineConfig),
    _mjdMonitoringInputs ::
      !(List1 MonitoringInput),
    _mjdMonitoringOutputConfig ::
      !MonitoringOutputConfig,
    _mjdMonitoringResources ::
      !MonitoringResources,
    _mjdMonitoringAppSpecification ::
      !MonitoringAppSpecification,
    _mjdRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mjdEnvironment' - Sets the environment variables in the Docker container.
--
-- * 'mjdStoppingCondition' - Specifies a time limit for how long the monitoring job is allowed to run.
--
-- * 'mjdNetworkConfig' - Specifies networking options for an monitoring job.
--
-- * 'mjdBaselineConfig' - Baseline configuration used to validate that the data conforms to the specified constraints and statistics
--
-- * 'mjdMonitoringInputs' - The array of inputs for the monitoring job. Currently we support monitoring an Amazon SageMaker Endpoint.
--
-- * 'mjdMonitoringOutputConfig' - The array of outputs from the monitoring job to be uploaded to Amazon Simple Storage Service (Amazon S3).
--
-- * 'mjdMonitoringResources' - Identifies the resources, ML compute instances, and ML storage volumes to deploy for a monitoring job. In distributed processing, you specify more than one instance.
--
-- * 'mjdMonitoringAppSpecification' - Configures the monitoring job to run a specified Docker container image.
--
-- * 'mjdRoleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
monitoringJobDefinition ::
  -- | 'mjdMonitoringInputs'
  NonEmpty MonitoringInput ->
  -- | 'mjdMonitoringOutputConfig'
  MonitoringOutputConfig ->
  -- | 'mjdMonitoringResources'
  MonitoringResources ->
  -- | 'mjdMonitoringAppSpecification'
  MonitoringAppSpecification ->
  -- | 'mjdRoleARN'
  Text ->
  MonitoringJobDefinition
monitoringJobDefinition
  pMonitoringInputs_
  pMonitoringOutputConfig_
  pMonitoringResources_
  pMonitoringAppSpecification_
  pRoleARN_ =
    MonitoringJobDefinition'
      { _mjdEnvironment = Nothing,
        _mjdStoppingCondition = Nothing,
        _mjdNetworkConfig = Nothing,
        _mjdBaselineConfig = Nothing,
        _mjdMonitoringInputs = _List1 # pMonitoringInputs_,
        _mjdMonitoringOutputConfig = pMonitoringOutputConfig_,
        _mjdMonitoringResources = pMonitoringResources_,
        _mjdMonitoringAppSpecification = pMonitoringAppSpecification_,
        _mjdRoleARN = pRoleARN_
      }

-- | Sets the environment variables in the Docker container.
mjdEnvironment :: Lens' MonitoringJobDefinition (HashMap Text (Text))
mjdEnvironment = lens _mjdEnvironment (\s a -> s {_mjdEnvironment = a}) . _Default . _Map

-- | Specifies a time limit for how long the monitoring job is allowed to run.
mjdStoppingCondition :: Lens' MonitoringJobDefinition (Maybe MonitoringStoppingCondition)
mjdStoppingCondition = lens _mjdStoppingCondition (\s a -> s {_mjdStoppingCondition = a})

-- | Specifies networking options for an monitoring job.
mjdNetworkConfig :: Lens' MonitoringJobDefinition (Maybe NetworkConfig)
mjdNetworkConfig = lens _mjdNetworkConfig (\s a -> s {_mjdNetworkConfig = a})

-- | Baseline configuration used to validate that the data conforms to the specified constraints and statistics
mjdBaselineConfig :: Lens' MonitoringJobDefinition (Maybe MonitoringBaselineConfig)
mjdBaselineConfig = lens _mjdBaselineConfig (\s a -> s {_mjdBaselineConfig = a})

-- | The array of inputs for the monitoring job. Currently we support monitoring an Amazon SageMaker Endpoint.
mjdMonitoringInputs :: Lens' MonitoringJobDefinition (NonEmpty MonitoringInput)
mjdMonitoringInputs = lens _mjdMonitoringInputs (\s a -> s {_mjdMonitoringInputs = a}) . _List1

-- | The array of outputs from the monitoring job to be uploaded to Amazon Simple Storage Service (Amazon S3).
mjdMonitoringOutputConfig :: Lens' MonitoringJobDefinition MonitoringOutputConfig
mjdMonitoringOutputConfig = lens _mjdMonitoringOutputConfig (\s a -> s {_mjdMonitoringOutputConfig = a})

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a monitoring job. In distributed processing, you specify more than one instance.
mjdMonitoringResources :: Lens' MonitoringJobDefinition MonitoringResources
mjdMonitoringResources = lens _mjdMonitoringResources (\s a -> s {_mjdMonitoringResources = a})

-- | Configures the monitoring job to run a specified Docker container image.
mjdMonitoringAppSpecification :: Lens' MonitoringJobDefinition MonitoringAppSpecification
mjdMonitoringAppSpecification = lens _mjdMonitoringAppSpecification (\s a -> s {_mjdMonitoringAppSpecification = a})

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
mjdRoleARN :: Lens' MonitoringJobDefinition Text
mjdRoleARN = lens _mjdRoleARN (\s a -> s {_mjdRoleARN = a})

instance FromJSON MonitoringJobDefinition where
  parseJSON =
    withObject
      "MonitoringJobDefinition"
      ( \x ->
          MonitoringJobDefinition'
            <$> (x .:? "Environment" .!= mempty)
            <*> (x .:? "StoppingCondition")
            <*> (x .:? "NetworkConfig")
            <*> (x .:? "BaselineConfig")
            <*> (x .: "MonitoringInputs")
            <*> (x .: "MonitoringOutputConfig")
            <*> (x .: "MonitoringResources")
            <*> (x .: "MonitoringAppSpecification")
            <*> (x .: "RoleArn")
      )

instance Hashable MonitoringJobDefinition

instance NFData MonitoringJobDefinition

instance ToJSON MonitoringJobDefinition where
  toJSON MonitoringJobDefinition' {..} =
    object
      ( catMaybes
          [ ("Environment" .=) <$> _mjdEnvironment,
            ("StoppingCondition" .=) <$> _mjdStoppingCondition,
            ("NetworkConfig" .=) <$> _mjdNetworkConfig,
            ("BaselineConfig" .=) <$> _mjdBaselineConfig,
            Just ("MonitoringInputs" .= _mjdMonitoringInputs),
            Just ("MonitoringOutputConfig" .= _mjdMonitoringOutputConfig),
            Just ("MonitoringResources" .= _mjdMonitoringResources),
            Just
              ("MonitoringAppSpecification" .= _mjdMonitoringAppSpecification),
            Just ("RoleArn" .= _mjdRoleARN)
          ]
      )
