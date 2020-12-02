{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StepExecution where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Detailed information about an the execution state of an Automation step.
--
--
--
-- /See:/ 'stepExecution' smart constructor.
data StepExecution = StepExecution'
  { _seFailureDetails ::
      !(Maybe FailureDetails),
    _seIsEnd :: !(Maybe Bool),
    _seInputs :: !(Maybe (Map Text (Text))),
    _seStepName :: !(Maybe Text),
    _seExecutionEndTime :: !(Maybe POSIX),
    _seFailureMessage :: !(Maybe Text),
    _seResponse :: !(Maybe Text),
    _seAction :: !(Maybe Text),
    _seResponseCode :: !(Maybe Text),
    _seStepStatus :: !(Maybe AutomationExecutionStatus),
    _seTargetLocation :: !(Maybe TargetLocation),
    _seOverriddenParameters :: !(Maybe (Map Text ([Text]))),
    _seOutputs :: !(Maybe (Map Text ([Text]))),
    _seExecutionStartTime :: !(Maybe POSIX),
    _seMaxAttempts :: !(Maybe Int),
    _seTargets :: !(Maybe [Target]),
    _seNextStep :: !(Maybe Text),
    _seStepExecutionId :: !(Maybe Text),
    _seValidNextSteps :: !(Maybe [Text]),
    _seTimeoutSeconds :: !(Maybe Integer),
    _seOnFailure :: !(Maybe Text),
    _seIsCritical :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seFailureDetails' - Information about the Automation failure.
--
-- * 'seIsEnd' - The flag which can be used to end automation no matter whether the step succeeds or fails.
--
-- * 'seInputs' - Fully-resolved values passed into the step before execution.
--
-- * 'seStepName' - The name of this execution step.
--
-- * 'seExecutionEndTime' - If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
--
-- * 'seFailureMessage' - If a step failed, this message explains why the execution failed.
--
-- * 'seResponse' - A message associated with the response code for an execution.
--
-- * 'seAction' - The action this step performs. The action determines the behavior of the step.
--
-- * 'seResponseCode' - The response code returned by the execution of the step.
--
-- * 'seStepStatus' - The execution status for this step.
--
-- * 'seTargetLocation' - The combination of AWS Regions and accounts targeted by the current Automation execution.
--
-- * 'seOverriddenParameters' - A user-specified list of parameters to override when running a step.
--
-- * 'seOutputs' - Returned values from the execution of the step.
--
-- * 'seExecutionStartTime' - If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
--
-- * 'seMaxAttempts' - The maximum number of tries to run the action of the step. The default value is 1.
--
-- * 'seTargets' - The targets for the step execution.
--
-- * 'seNextStep' - The next step after the step succeeds.
--
-- * 'seStepExecutionId' - The unique ID of a step execution.
--
-- * 'seValidNextSteps' - Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
--
-- * 'seTimeoutSeconds' - The timeout seconds of the step.
--
-- * 'seOnFailure' - The action to take if the step fails. The default value is Abort.
--
-- * 'seIsCritical' - The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
stepExecution ::
  StepExecution
stepExecution =
  StepExecution'
    { _seFailureDetails = Nothing,
      _seIsEnd = Nothing,
      _seInputs = Nothing,
      _seStepName = Nothing,
      _seExecutionEndTime = Nothing,
      _seFailureMessage = Nothing,
      _seResponse = Nothing,
      _seAction = Nothing,
      _seResponseCode = Nothing,
      _seStepStatus = Nothing,
      _seTargetLocation = Nothing,
      _seOverriddenParameters = Nothing,
      _seOutputs = Nothing,
      _seExecutionStartTime = Nothing,
      _seMaxAttempts = Nothing,
      _seTargets = Nothing,
      _seNextStep = Nothing,
      _seStepExecutionId = Nothing,
      _seValidNextSteps = Nothing,
      _seTimeoutSeconds = Nothing,
      _seOnFailure = Nothing,
      _seIsCritical = Nothing
    }

-- | Information about the Automation failure.
seFailureDetails :: Lens' StepExecution (Maybe FailureDetails)
seFailureDetails = lens _seFailureDetails (\s a -> s {_seFailureDetails = a})

-- | The flag which can be used to end automation no matter whether the step succeeds or fails.
seIsEnd :: Lens' StepExecution (Maybe Bool)
seIsEnd = lens _seIsEnd (\s a -> s {_seIsEnd = a})

-- | Fully-resolved values passed into the step before execution.
seInputs :: Lens' StepExecution (HashMap Text (Text))
seInputs = lens _seInputs (\s a -> s {_seInputs = a}) . _Default . _Map

-- | The name of this execution step.
seStepName :: Lens' StepExecution (Maybe Text)
seStepName = lens _seStepName (\s a -> s {_seStepName = a})

-- | If a step has finished execution, this contains the time the execution ended. If the step has not yet concluded, this field is not populated.
seExecutionEndTime :: Lens' StepExecution (Maybe UTCTime)
seExecutionEndTime = lens _seExecutionEndTime (\s a -> s {_seExecutionEndTime = a}) . mapping _Time

-- | If a step failed, this message explains why the execution failed.
seFailureMessage :: Lens' StepExecution (Maybe Text)
seFailureMessage = lens _seFailureMessage (\s a -> s {_seFailureMessage = a})

-- | A message associated with the response code for an execution.
seResponse :: Lens' StepExecution (Maybe Text)
seResponse = lens _seResponse (\s a -> s {_seResponse = a})

-- | The action this step performs. The action determines the behavior of the step.
seAction :: Lens' StepExecution (Maybe Text)
seAction = lens _seAction (\s a -> s {_seAction = a})

-- | The response code returned by the execution of the step.
seResponseCode :: Lens' StepExecution (Maybe Text)
seResponseCode = lens _seResponseCode (\s a -> s {_seResponseCode = a})

-- | The execution status for this step.
seStepStatus :: Lens' StepExecution (Maybe AutomationExecutionStatus)
seStepStatus = lens _seStepStatus (\s a -> s {_seStepStatus = a})

-- | The combination of AWS Regions and accounts targeted by the current Automation execution.
seTargetLocation :: Lens' StepExecution (Maybe TargetLocation)
seTargetLocation = lens _seTargetLocation (\s a -> s {_seTargetLocation = a})

-- | A user-specified list of parameters to override when running a step.
seOverriddenParameters :: Lens' StepExecution (HashMap Text ([Text]))
seOverriddenParameters = lens _seOverriddenParameters (\s a -> s {_seOverriddenParameters = a}) . _Default . _Map

-- | Returned values from the execution of the step.
seOutputs :: Lens' StepExecution (HashMap Text ([Text]))
seOutputs = lens _seOutputs (\s a -> s {_seOutputs = a}) . _Default . _Map

-- | If a step has begun execution, this contains the time the step started. If the step is in Pending status, this field is not populated.
seExecutionStartTime :: Lens' StepExecution (Maybe UTCTime)
seExecutionStartTime = lens _seExecutionStartTime (\s a -> s {_seExecutionStartTime = a}) . mapping _Time

-- | The maximum number of tries to run the action of the step. The default value is 1.
seMaxAttempts :: Lens' StepExecution (Maybe Int)
seMaxAttempts = lens _seMaxAttempts (\s a -> s {_seMaxAttempts = a})

-- | The targets for the step execution.
seTargets :: Lens' StepExecution [Target]
seTargets = lens _seTargets (\s a -> s {_seTargets = a}) . _Default . _Coerce

-- | The next step after the step succeeds.
seNextStep :: Lens' StepExecution (Maybe Text)
seNextStep = lens _seNextStep (\s a -> s {_seNextStep = a})

-- | The unique ID of a step execution.
seStepExecutionId :: Lens' StepExecution (Maybe Text)
seStepExecutionId = lens _seStepExecutionId (\s a -> s {_seStepExecutionId = a})

-- | Strategies used when step fails, we support Continue and Abort. Abort will fail the automation when the step fails. Continue will ignore the failure of current step and allow automation to run the next step. With conditional branching, we add step:stepName to support the automation to go to another specific step.
seValidNextSteps :: Lens' StepExecution [Text]
seValidNextSteps = lens _seValidNextSteps (\s a -> s {_seValidNextSteps = a}) . _Default . _Coerce

-- | The timeout seconds of the step.
seTimeoutSeconds :: Lens' StepExecution (Maybe Integer)
seTimeoutSeconds = lens _seTimeoutSeconds (\s a -> s {_seTimeoutSeconds = a})

-- | The action to take if the step fails. The default value is Abort.
seOnFailure :: Lens' StepExecution (Maybe Text)
seOnFailure = lens _seOnFailure (\s a -> s {_seOnFailure = a})

-- | The flag which can be used to help decide whether the failure of current step leads to the Automation failure.
seIsCritical :: Lens' StepExecution (Maybe Bool)
seIsCritical = lens _seIsCritical (\s a -> s {_seIsCritical = a})

instance FromJSON StepExecution where
  parseJSON =
    withObject
      "StepExecution"
      ( \x ->
          StepExecution'
            <$> (x .:? "FailureDetails")
            <*> (x .:? "IsEnd")
            <*> (x .:? "Inputs" .!= mempty)
            <*> (x .:? "StepName")
            <*> (x .:? "ExecutionEndTime")
            <*> (x .:? "FailureMessage")
            <*> (x .:? "Response")
            <*> (x .:? "Action")
            <*> (x .:? "ResponseCode")
            <*> (x .:? "StepStatus")
            <*> (x .:? "TargetLocation")
            <*> (x .:? "OverriddenParameters" .!= mempty)
            <*> (x .:? "Outputs" .!= mempty)
            <*> (x .:? "ExecutionStartTime")
            <*> (x .:? "MaxAttempts")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "NextStep")
            <*> (x .:? "StepExecutionId")
            <*> (x .:? "ValidNextSteps" .!= mempty)
            <*> (x .:? "TimeoutSeconds")
            <*> (x .:? "OnFailure")
            <*> (x .:? "IsCritical")
      )

instance Hashable StepExecution

instance NFData StepExecution
