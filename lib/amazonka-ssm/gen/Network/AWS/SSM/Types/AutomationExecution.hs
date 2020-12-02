{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecution where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.ProgressCounters
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.StepExecution
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation

-- | Detailed information about the current state of an individual Automation execution.
--
--
--
-- /See:/ 'automationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
  { _aeCurrentStepName ::
      !(Maybe Text),
    _aeTargetParameterName :: !(Maybe Text),
    _aeTargetLocations ::
      !(Maybe (List1 TargetLocation)),
    _aeProgressCounters :: !(Maybe ProgressCounters),
    _aeExecutedBy :: !(Maybe Text),
    _aeDocumentName :: !(Maybe Text),
    _aeExecutionEndTime :: !(Maybe POSIX),
    _aeFailureMessage :: !(Maybe Text),
    _aeMode :: !(Maybe ExecutionMode),
    _aeTargetMaps :: !(Maybe [Map Text ([Text])]),
    _aeStepExecutionsTruncated :: !(Maybe Bool),
    _aeAutomationExecutionStatus ::
      !(Maybe AutomationExecutionStatus),
    _aeParentAutomationExecutionId :: !(Maybe Text),
    _aeOutputs :: !(Maybe (Map Text ([Text]))),
    _aeMaxErrors :: !(Maybe Text),
    _aeExecutionStartTime :: !(Maybe POSIX),
    _aeCurrentAction :: !(Maybe Text),
    _aeTargets :: !(Maybe [Target]),
    _aeResolvedTargets :: !(Maybe ResolvedTargets),
    _aeParameters :: !(Maybe (Map Text ([Text]))),
    _aeDocumentVersion :: !(Maybe Text),
    _aeAutomationExecutionId :: !(Maybe Text),
    _aeStepExecutions :: !(Maybe [StepExecution]),
    _aeMaxConcurrency :: !(Maybe Text),
    _aeTarget :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomationExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeCurrentStepName' - The name of the step that is currently running.
--
-- * 'aeTargetParameterName' - The parameter name.
--
-- * 'aeTargetLocations' - The combination of AWS Regions and/or AWS accounts where you want to run the Automation.
--
-- * 'aeProgressCounters' - An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
--
-- * 'aeExecutedBy' - The Amazon Resource Name (ARN) of the user who ran the automation.
--
-- * 'aeDocumentName' - The name of the Automation document used during the execution.
--
-- * 'aeExecutionEndTime' - The time the execution finished.
--
-- * 'aeFailureMessage' - A message describing why an execution has failed, if the status is set to Failed.
--
-- * 'aeMode' - The automation execution mode.
--
-- * 'aeTargetMaps' - The specified key-value mapping of document parameters to target resources.
--
-- * 'aeStepExecutionsTruncated' - A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
--
-- * 'aeAutomationExecutionStatus' - The execution status of the Automation.
--
-- * 'aeParentAutomationExecutionId' - The AutomationExecutionId of the parent automation.
--
-- * 'aeOutputs' - The list of execution outputs as defined in the automation document.
--
-- * 'aeMaxErrors' - The MaxErrors value specified by the user when the execution started.
--
-- * 'aeExecutionStartTime' - The time the execution started.
--
-- * 'aeCurrentAction' - The action of the step that is currently running.
--
-- * 'aeTargets' - The specified targets.
--
-- * 'aeResolvedTargets' - A list of resolved targets in the rate control execution.
--
-- * 'aeParameters' - The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
--
-- * 'aeDocumentVersion' - The version of the document to use during execution.
--
-- * 'aeAutomationExecutionId' - The execution ID.
--
-- * 'aeStepExecutions' - A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are run in order.
--
-- * 'aeMaxConcurrency' - The MaxConcurrency value specified by the user when the execution started.
--
-- * 'aeTarget' - The target of the execution.
automationExecution ::
  AutomationExecution
automationExecution =
  AutomationExecution'
    { _aeCurrentStepName = Nothing,
      _aeTargetParameterName = Nothing,
      _aeTargetLocations = Nothing,
      _aeProgressCounters = Nothing,
      _aeExecutedBy = Nothing,
      _aeDocumentName = Nothing,
      _aeExecutionEndTime = Nothing,
      _aeFailureMessage = Nothing,
      _aeMode = Nothing,
      _aeTargetMaps = Nothing,
      _aeStepExecutionsTruncated = Nothing,
      _aeAutomationExecutionStatus = Nothing,
      _aeParentAutomationExecutionId = Nothing,
      _aeOutputs = Nothing,
      _aeMaxErrors = Nothing,
      _aeExecutionStartTime = Nothing,
      _aeCurrentAction = Nothing,
      _aeTargets = Nothing,
      _aeResolvedTargets = Nothing,
      _aeParameters = Nothing,
      _aeDocumentVersion = Nothing,
      _aeAutomationExecutionId = Nothing,
      _aeStepExecutions = Nothing,
      _aeMaxConcurrency = Nothing,
      _aeTarget = Nothing
    }

-- | The name of the step that is currently running.
aeCurrentStepName :: Lens' AutomationExecution (Maybe Text)
aeCurrentStepName = lens _aeCurrentStepName (\s a -> s {_aeCurrentStepName = a})

-- | The parameter name.
aeTargetParameterName :: Lens' AutomationExecution (Maybe Text)
aeTargetParameterName = lens _aeTargetParameterName (\s a -> s {_aeTargetParameterName = a})

-- | The combination of AWS Regions and/or AWS accounts where you want to run the Automation.
aeTargetLocations :: Lens' AutomationExecution (Maybe (NonEmpty TargetLocation))
aeTargetLocations = lens _aeTargetLocations (\s a -> s {_aeTargetLocations = a}) . mapping _List1

-- | An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
aeProgressCounters :: Lens' AutomationExecution (Maybe ProgressCounters)
aeProgressCounters = lens _aeProgressCounters (\s a -> s {_aeProgressCounters = a})

-- | The Amazon Resource Name (ARN) of the user who ran the automation.
aeExecutedBy :: Lens' AutomationExecution (Maybe Text)
aeExecutedBy = lens _aeExecutedBy (\s a -> s {_aeExecutedBy = a})

-- | The name of the Automation document used during the execution.
aeDocumentName :: Lens' AutomationExecution (Maybe Text)
aeDocumentName = lens _aeDocumentName (\s a -> s {_aeDocumentName = a})

-- | The time the execution finished.
aeExecutionEndTime :: Lens' AutomationExecution (Maybe UTCTime)
aeExecutionEndTime = lens _aeExecutionEndTime (\s a -> s {_aeExecutionEndTime = a}) . mapping _Time

-- | A message describing why an execution has failed, if the status is set to Failed.
aeFailureMessage :: Lens' AutomationExecution (Maybe Text)
aeFailureMessage = lens _aeFailureMessage (\s a -> s {_aeFailureMessage = a})

-- | The automation execution mode.
aeMode :: Lens' AutomationExecution (Maybe ExecutionMode)
aeMode = lens _aeMode (\s a -> s {_aeMode = a})

-- | The specified key-value mapping of document parameters to target resources.
aeTargetMaps :: Lens' AutomationExecution [HashMap Text ([Text])]
aeTargetMaps = lens _aeTargetMaps (\s a -> s {_aeTargetMaps = a}) . _Default . _Coerce

-- | A boolean value that indicates if the response contains the full list of the Automation step executions. If true, use the DescribeAutomationStepExecutions API action to get the full list of step executions.
aeStepExecutionsTruncated :: Lens' AutomationExecution (Maybe Bool)
aeStepExecutionsTruncated = lens _aeStepExecutionsTruncated (\s a -> s {_aeStepExecutionsTruncated = a})

-- | The execution status of the Automation.
aeAutomationExecutionStatus :: Lens' AutomationExecution (Maybe AutomationExecutionStatus)
aeAutomationExecutionStatus = lens _aeAutomationExecutionStatus (\s a -> s {_aeAutomationExecutionStatus = a})

-- | The AutomationExecutionId of the parent automation.
aeParentAutomationExecutionId :: Lens' AutomationExecution (Maybe Text)
aeParentAutomationExecutionId = lens _aeParentAutomationExecutionId (\s a -> s {_aeParentAutomationExecutionId = a})

-- | The list of execution outputs as defined in the automation document.
aeOutputs :: Lens' AutomationExecution (HashMap Text ([Text]))
aeOutputs = lens _aeOutputs (\s a -> s {_aeOutputs = a}) . _Default . _Map

-- | The MaxErrors value specified by the user when the execution started.
aeMaxErrors :: Lens' AutomationExecution (Maybe Text)
aeMaxErrors = lens _aeMaxErrors (\s a -> s {_aeMaxErrors = a})

-- | The time the execution started.
aeExecutionStartTime :: Lens' AutomationExecution (Maybe UTCTime)
aeExecutionStartTime = lens _aeExecutionStartTime (\s a -> s {_aeExecutionStartTime = a}) . mapping _Time

-- | The action of the step that is currently running.
aeCurrentAction :: Lens' AutomationExecution (Maybe Text)
aeCurrentAction = lens _aeCurrentAction (\s a -> s {_aeCurrentAction = a})

-- | The specified targets.
aeTargets :: Lens' AutomationExecution [Target]
aeTargets = lens _aeTargets (\s a -> s {_aeTargets = a}) . _Default . _Coerce

-- | A list of resolved targets in the rate control execution.
aeResolvedTargets :: Lens' AutomationExecution (Maybe ResolvedTargets)
aeResolvedTargets = lens _aeResolvedTargets (\s a -> s {_aeResolvedTargets = a})

-- | The key-value map of execution parameters, which were supplied when calling StartAutomationExecution.
aeParameters :: Lens' AutomationExecution (HashMap Text ([Text]))
aeParameters = lens _aeParameters (\s a -> s {_aeParameters = a}) . _Default . _Map

-- | The version of the document to use during execution.
aeDocumentVersion :: Lens' AutomationExecution (Maybe Text)
aeDocumentVersion = lens _aeDocumentVersion (\s a -> s {_aeDocumentVersion = a})

-- | The execution ID.
aeAutomationExecutionId :: Lens' AutomationExecution (Maybe Text)
aeAutomationExecutionId = lens _aeAutomationExecutionId (\s a -> s {_aeAutomationExecutionId = a})

-- | A list of details about the current state of all steps that comprise an execution. An Automation document contains a list of steps that are run in order.
aeStepExecutions :: Lens' AutomationExecution [StepExecution]
aeStepExecutions = lens _aeStepExecutions (\s a -> s {_aeStepExecutions = a}) . _Default . _Coerce

-- | The MaxConcurrency value specified by the user when the execution started.
aeMaxConcurrency :: Lens' AutomationExecution (Maybe Text)
aeMaxConcurrency = lens _aeMaxConcurrency (\s a -> s {_aeMaxConcurrency = a})

-- | The target of the execution.
aeTarget :: Lens' AutomationExecution (Maybe Text)
aeTarget = lens _aeTarget (\s a -> s {_aeTarget = a})

instance FromJSON AutomationExecution where
  parseJSON =
    withObject
      "AutomationExecution"
      ( \x ->
          AutomationExecution'
            <$> (x .:? "CurrentStepName")
            <*> (x .:? "TargetParameterName")
            <*> (x .:? "TargetLocations")
            <*> (x .:? "ProgressCounters")
            <*> (x .:? "ExecutedBy")
            <*> (x .:? "DocumentName")
            <*> (x .:? "ExecutionEndTime")
            <*> (x .:? "FailureMessage")
            <*> (x .:? "Mode")
            <*> (x .:? "TargetMaps" .!= mempty)
            <*> (x .:? "StepExecutionsTruncated")
            <*> (x .:? "AutomationExecutionStatus")
            <*> (x .:? "ParentAutomationExecutionId")
            <*> (x .:? "Outputs" .!= mempty)
            <*> (x .:? "MaxErrors")
            <*> (x .:? "ExecutionStartTime")
            <*> (x .:? "CurrentAction")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "ResolvedTargets")
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "AutomationExecutionId")
            <*> (x .:? "StepExecutions" .!= mempty)
            <*> (x .:? "MaxConcurrency")
            <*> (x .:? "Target")
      )

instance Hashable AutomationExecution

instance NFData AutomationExecution
