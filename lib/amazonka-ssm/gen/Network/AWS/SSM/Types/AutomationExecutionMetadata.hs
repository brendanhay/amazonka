{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AutomationExecutionStatus
import Network.AWS.SSM.Types.AutomationType
import Network.AWS.SSM.Types.ExecutionMode
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.Target

-- | Details about a specific Automation execution.
--
--
--
-- /See:/ 'automationExecutionMetadata' smart constructor.
data AutomationExecutionMetadata = AutomationExecutionMetadata'
  { _aemCurrentStepName ::
      !(Maybe Text),
    _aemTargetParameterName ::
      !(Maybe Text),
    _aemLogFile :: !(Maybe Text),
    _aemExecutedBy :: !(Maybe Text),
    _aemDocumentName :: !(Maybe Text),
    _aemExecutionEndTime ::
      !(Maybe POSIX),
    _aemFailureMessage :: !(Maybe Text),
    _aemMode :: !(Maybe ExecutionMode),
    _aemTargetMaps ::
      !(Maybe [Map Text ([Text])]),
    _aemAutomationExecutionStatus ::
      !(Maybe AutomationExecutionStatus),
    _aemParentAutomationExecutionId ::
      !(Maybe Text),
    _aemOutputs ::
      !(Maybe (Map Text ([Text]))),
    _aemMaxErrors :: !(Maybe Text),
    _aemExecutionStartTime ::
      !(Maybe POSIX),
    _aemAutomationType ::
      !(Maybe AutomationType),
    _aemCurrentAction :: !(Maybe Text),
    _aemTargets :: !(Maybe [Target]),
    _aemResolvedTargets ::
      !(Maybe ResolvedTargets),
    _aemDocumentVersion ::
      !(Maybe Text),
    _aemAutomationExecutionId ::
      !(Maybe Text),
    _aemMaxConcurrency :: !(Maybe Text),
    _aemTarget :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomationExecutionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aemCurrentStepName' - The name of the step that is currently running.
--
-- * 'aemTargetParameterName' - The list of execution outputs as defined in the Automation document.
--
-- * 'aemLogFile' - An S3 bucket where execution information is stored.
--
-- * 'aemExecutedBy' - The IAM role ARN of the user who ran the Automation.
--
-- * 'aemDocumentName' - The name of the Automation document used during execution.
--
-- * 'aemExecutionEndTime' - The time the execution finished. This is not populated if the execution is still in progress.
--
-- * 'aemFailureMessage' - The list of execution outputs as defined in the Automation document.
--
-- * 'aemMode' - The Automation execution mode.
--
-- * 'aemTargetMaps' - The specified key-value mapping of document parameters to target resources.
--
-- * 'aemAutomationExecutionStatus' - The status of the execution.
--
-- * 'aemParentAutomationExecutionId' - The ExecutionId of the parent Automation.
--
-- * 'aemOutputs' - The list of execution outputs as defined in the Automation document.
--
-- * 'aemMaxErrors' - The MaxErrors value specified by the user when starting the Automation.
--
-- * 'aemExecutionStartTime' - The time the execution started.
--
-- * 'aemAutomationType' - Use this filter with 'DescribeAutomationExecutions' . Specify either Local or CrossAccount. CrossAccount is an Automation that runs in multiple AWS Regions and accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
--
-- * 'aemCurrentAction' - The action of the step that is currently running.
--
-- * 'aemTargets' - The targets defined by the user when starting the Automation.
--
-- * 'aemResolvedTargets' - A list of targets that resolved during the execution.
--
-- * 'aemDocumentVersion' - The document version used during the execution.
--
-- * 'aemAutomationExecutionId' - The execution ID.
--
-- * 'aemMaxConcurrency' - The MaxConcurrency value specified by the user when starting the Automation.
--
-- * 'aemTarget' - The list of execution outputs as defined in the Automation document.
automationExecutionMetadata ::
  AutomationExecutionMetadata
automationExecutionMetadata =
  AutomationExecutionMetadata'
    { _aemCurrentStepName = Nothing,
      _aemTargetParameterName = Nothing,
      _aemLogFile = Nothing,
      _aemExecutedBy = Nothing,
      _aemDocumentName = Nothing,
      _aemExecutionEndTime = Nothing,
      _aemFailureMessage = Nothing,
      _aemMode = Nothing,
      _aemTargetMaps = Nothing,
      _aemAutomationExecutionStatus = Nothing,
      _aemParentAutomationExecutionId = Nothing,
      _aemOutputs = Nothing,
      _aemMaxErrors = Nothing,
      _aemExecutionStartTime = Nothing,
      _aemAutomationType = Nothing,
      _aemCurrentAction = Nothing,
      _aemTargets = Nothing,
      _aemResolvedTargets = Nothing,
      _aemDocumentVersion = Nothing,
      _aemAutomationExecutionId = Nothing,
      _aemMaxConcurrency = Nothing,
      _aemTarget = Nothing
    }

-- | The name of the step that is currently running.
aemCurrentStepName :: Lens' AutomationExecutionMetadata (Maybe Text)
aemCurrentStepName = lens _aemCurrentStepName (\s a -> s {_aemCurrentStepName = a})

-- | The list of execution outputs as defined in the Automation document.
aemTargetParameterName :: Lens' AutomationExecutionMetadata (Maybe Text)
aemTargetParameterName = lens _aemTargetParameterName (\s a -> s {_aemTargetParameterName = a})

-- | An S3 bucket where execution information is stored.
aemLogFile :: Lens' AutomationExecutionMetadata (Maybe Text)
aemLogFile = lens _aemLogFile (\s a -> s {_aemLogFile = a})

-- | The IAM role ARN of the user who ran the Automation.
aemExecutedBy :: Lens' AutomationExecutionMetadata (Maybe Text)
aemExecutedBy = lens _aemExecutedBy (\s a -> s {_aemExecutedBy = a})

-- | The name of the Automation document used during execution.
aemDocumentName :: Lens' AutomationExecutionMetadata (Maybe Text)
aemDocumentName = lens _aemDocumentName (\s a -> s {_aemDocumentName = a})

-- | The time the execution finished. This is not populated if the execution is still in progress.
aemExecutionEndTime :: Lens' AutomationExecutionMetadata (Maybe UTCTime)
aemExecutionEndTime = lens _aemExecutionEndTime (\s a -> s {_aemExecutionEndTime = a}) . mapping _Time

-- | The list of execution outputs as defined in the Automation document.
aemFailureMessage :: Lens' AutomationExecutionMetadata (Maybe Text)
aemFailureMessage = lens _aemFailureMessage (\s a -> s {_aemFailureMessage = a})

-- | The Automation execution mode.
aemMode :: Lens' AutomationExecutionMetadata (Maybe ExecutionMode)
aemMode = lens _aemMode (\s a -> s {_aemMode = a})

-- | The specified key-value mapping of document parameters to target resources.
aemTargetMaps :: Lens' AutomationExecutionMetadata [HashMap Text ([Text])]
aemTargetMaps = lens _aemTargetMaps (\s a -> s {_aemTargetMaps = a}) . _Default . _Coerce

-- | The status of the execution.
aemAutomationExecutionStatus :: Lens' AutomationExecutionMetadata (Maybe AutomationExecutionStatus)
aemAutomationExecutionStatus = lens _aemAutomationExecutionStatus (\s a -> s {_aemAutomationExecutionStatus = a})

-- | The ExecutionId of the parent Automation.
aemParentAutomationExecutionId :: Lens' AutomationExecutionMetadata (Maybe Text)
aemParentAutomationExecutionId = lens _aemParentAutomationExecutionId (\s a -> s {_aemParentAutomationExecutionId = a})

-- | The list of execution outputs as defined in the Automation document.
aemOutputs :: Lens' AutomationExecutionMetadata (HashMap Text ([Text]))
aemOutputs = lens _aemOutputs (\s a -> s {_aemOutputs = a}) . _Default . _Map

-- | The MaxErrors value specified by the user when starting the Automation.
aemMaxErrors :: Lens' AutomationExecutionMetadata (Maybe Text)
aemMaxErrors = lens _aemMaxErrors (\s a -> s {_aemMaxErrors = a})

-- | The time the execution started.
aemExecutionStartTime :: Lens' AutomationExecutionMetadata (Maybe UTCTime)
aemExecutionStartTime = lens _aemExecutionStartTime (\s a -> s {_aemExecutionStartTime = a}) . mapping _Time

-- | Use this filter with 'DescribeAutomationExecutions' . Specify either Local or CrossAccount. CrossAccount is an Automation that runs in multiple AWS Regions and accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
aemAutomationType :: Lens' AutomationExecutionMetadata (Maybe AutomationType)
aemAutomationType = lens _aemAutomationType (\s a -> s {_aemAutomationType = a})

-- | The action of the step that is currently running.
aemCurrentAction :: Lens' AutomationExecutionMetadata (Maybe Text)
aemCurrentAction = lens _aemCurrentAction (\s a -> s {_aemCurrentAction = a})

-- | The targets defined by the user when starting the Automation.
aemTargets :: Lens' AutomationExecutionMetadata [Target]
aemTargets = lens _aemTargets (\s a -> s {_aemTargets = a}) . _Default . _Coerce

-- | A list of targets that resolved during the execution.
aemResolvedTargets :: Lens' AutomationExecutionMetadata (Maybe ResolvedTargets)
aemResolvedTargets = lens _aemResolvedTargets (\s a -> s {_aemResolvedTargets = a})

-- | The document version used during the execution.
aemDocumentVersion :: Lens' AutomationExecutionMetadata (Maybe Text)
aemDocumentVersion = lens _aemDocumentVersion (\s a -> s {_aemDocumentVersion = a})

-- | The execution ID.
aemAutomationExecutionId :: Lens' AutomationExecutionMetadata (Maybe Text)
aemAutomationExecutionId = lens _aemAutomationExecutionId (\s a -> s {_aemAutomationExecutionId = a})

-- | The MaxConcurrency value specified by the user when starting the Automation.
aemMaxConcurrency :: Lens' AutomationExecutionMetadata (Maybe Text)
aemMaxConcurrency = lens _aemMaxConcurrency (\s a -> s {_aemMaxConcurrency = a})

-- | The list of execution outputs as defined in the Automation document.
aemTarget :: Lens' AutomationExecutionMetadata (Maybe Text)
aemTarget = lens _aemTarget (\s a -> s {_aemTarget = a})

instance FromJSON AutomationExecutionMetadata where
  parseJSON =
    withObject
      "AutomationExecutionMetadata"
      ( \x ->
          AutomationExecutionMetadata'
            <$> (x .:? "CurrentStepName")
            <*> (x .:? "TargetParameterName")
            <*> (x .:? "LogFile")
            <*> (x .:? "ExecutedBy")
            <*> (x .:? "DocumentName")
            <*> (x .:? "ExecutionEndTime")
            <*> (x .:? "FailureMessage")
            <*> (x .:? "Mode")
            <*> (x .:? "TargetMaps" .!= mempty)
            <*> (x .:? "AutomationExecutionStatus")
            <*> (x .:? "ParentAutomationExecutionId")
            <*> (x .:? "Outputs" .!= mempty)
            <*> (x .:? "MaxErrors")
            <*> (x .:? "ExecutionStartTime")
            <*> (x .:? "AutomationType")
            <*> (x .:? "CurrentAction")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "ResolvedTargets")
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "AutomationExecutionId")
            <*> (x .:? "MaxConcurrency")
            <*> (x .:? "Target")
      )

instance Hashable AutomationExecutionMetadata

instance NFData AutomationExecutionMetadata
