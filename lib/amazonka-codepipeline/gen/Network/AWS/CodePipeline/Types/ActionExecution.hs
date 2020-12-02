{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecution where

import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import Network.AWS.CodePipeline.Types.ErrorDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the run of an action.
--
--
--
-- /See:/ 'actionExecution' smart constructor.
data ActionExecution = ActionExecution'
  { _aeLastUpdatedBy ::
      !(Maybe Text),
    _aeSummary :: !(Maybe Text),
    _aeStatus :: !(Maybe ActionExecutionStatus),
    _aeLastStatusChange :: !(Maybe POSIX),
    _aeToken :: !(Maybe Text),
    _aeExternalExecutionURL :: !(Maybe Text),
    _aeExternalExecutionId :: !(Maybe Text),
    _aeErrorDetails :: !(Maybe ErrorDetails),
    _aePercentComplete :: !(Maybe Nat),
    _aeActionExecutionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeLastUpdatedBy' - The ARN of the user who last changed the pipeline.
--
-- * 'aeSummary' - A summary of the run of the action.
--
-- * 'aeStatus' - The status of the action, or for a completed action, the last status of the action.
--
-- * 'aeLastStatusChange' - The last status change of the action.
--
-- * 'aeToken' - The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the @GetPipelineState@ command. It is used to validate that the approval request corresponding to this token is still valid.
--
-- * 'aeExternalExecutionURL' - The URL of a resource external to AWS that is used when running the action (for example, an external repository URL).
--
-- * 'aeExternalExecutionId' - The external ID of the run of the action.
--
-- * 'aeErrorDetails' - The details of an error returned by a URL external to AWS.
--
-- * 'aePercentComplete' - A percentage of completeness of the action as it runs.
--
-- * 'aeActionExecutionId' - ID of the workflow action execution in the current stage. Use the 'GetPipelineState' action to retrieve the current action execution details of the current stage.
actionExecution ::
  ActionExecution
actionExecution =
  ActionExecution'
    { _aeLastUpdatedBy = Nothing,
      _aeSummary = Nothing,
      _aeStatus = Nothing,
      _aeLastStatusChange = Nothing,
      _aeToken = Nothing,
      _aeExternalExecutionURL = Nothing,
      _aeExternalExecutionId = Nothing,
      _aeErrorDetails = Nothing,
      _aePercentComplete = Nothing,
      _aeActionExecutionId = Nothing
    }

-- | The ARN of the user who last changed the pipeline.
aeLastUpdatedBy :: Lens' ActionExecution (Maybe Text)
aeLastUpdatedBy = lens _aeLastUpdatedBy (\s a -> s {_aeLastUpdatedBy = a})

-- | A summary of the run of the action.
aeSummary :: Lens' ActionExecution (Maybe Text)
aeSummary = lens _aeSummary (\s a -> s {_aeSummary = a})

-- | The status of the action, or for a completed action, the last status of the action.
aeStatus :: Lens' ActionExecution (Maybe ActionExecutionStatus)
aeStatus = lens _aeStatus (\s a -> s {_aeStatus = a})

-- | The last status change of the action.
aeLastStatusChange :: Lens' ActionExecution (Maybe UTCTime)
aeLastStatusChange = lens _aeLastStatusChange (\s a -> s {_aeLastStatusChange = a}) . mapping _Time

-- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the @GetPipelineState@ command. It is used to validate that the approval request corresponding to this token is still valid.
aeToken :: Lens' ActionExecution (Maybe Text)
aeToken = lens _aeToken (\s a -> s {_aeToken = a})

-- | The URL of a resource external to AWS that is used when running the action (for example, an external repository URL).
aeExternalExecutionURL :: Lens' ActionExecution (Maybe Text)
aeExternalExecutionURL = lens _aeExternalExecutionURL (\s a -> s {_aeExternalExecutionURL = a})

-- | The external ID of the run of the action.
aeExternalExecutionId :: Lens' ActionExecution (Maybe Text)
aeExternalExecutionId = lens _aeExternalExecutionId (\s a -> s {_aeExternalExecutionId = a})

-- | The details of an error returned by a URL external to AWS.
aeErrorDetails :: Lens' ActionExecution (Maybe ErrorDetails)
aeErrorDetails = lens _aeErrorDetails (\s a -> s {_aeErrorDetails = a})

-- | A percentage of completeness of the action as it runs.
aePercentComplete :: Lens' ActionExecution (Maybe Natural)
aePercentComplete = lens _aePercentComplete (\s a -> s {_aePercentComplete = a}) . mapping _Nat

-- | ID of the workflow action execution in the current stage. Use the 'GetPipelineState' action to retrieve the current action execution details of the current stage.
aeActionExecutionId :: Lens' ActionExecution (Maybe Text)
aeActionExecutionId = lens _aeActionExecutionId (\s a -> s {_aeActionExecutionId = a})

instance FromJSON ActionExecution where
  parseJSON =
    withObject
      "ActionExecution"
      ( \x ->
          ActionExecution'
            <$> (x .:? "lastUpdatedBy")
            <*> (x .:? "summary")
            <*> (x .:? "status")
            <*> (x .:? "lastStatusChange")
            <*> (x .:? "token")
            <*> (x .:? "externalExecutionUrl")
            <*> (x .:? "externalExecutionId")
            <*> (x .:? "errorDetails")
            <*> (x .:? "percentComplete")
            <*> (x .:? "actionExecutionId")
      )

instance Hashable ActionExecution

instance NFData ActionExecution
