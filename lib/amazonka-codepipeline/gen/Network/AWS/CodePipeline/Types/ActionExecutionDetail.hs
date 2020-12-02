{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionDetail where

import Network.AWS.CodePipeline.Types.ActionExecutionInput
import Network.AWS.CodePipeline.Types.ActionExecutionOutput
import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about an execution of an action, including the action execution ID, and the name, version, and timing of the action.
--
--
--
-- /See:/ 'actionExecutionDetail' smart constructor.
data ActionExecutionDetail = ActionExecutionDetail'
  { _aedStatus ::
      !(Maybe ActionExecutionStatus),
    _aedStartTime :: !(Maybe POSIX),
    _aedPipelineVersion :: !(Maybe Nat),
    _aedInput :: !(Maybe ActionExecutionInput),
    _aedActionName :: !(Maybe Text),
    _aedOutput :: !(Maybe ActionExecutionOutput),
    _aedPipelineExecutionId :: !(Maybe Text),
    _aedStageName :: !(Maybe Text),
    _aedLastUpdateTime :: !(Maybe POSIX),
    _aedActionExecutionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionExecutionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aedStatus' - The status of the action execution. Status categories are @InProgress@ , @Succeeded@ , and @Failed@ .
--
-- * 'aedStartTime' - The start time of the action execution.
--
-- * 'aedPipelineVersion' - The version of the pipeline where the action was run.
--
-- * 'aedInput' - Input details for the action execution, such as role ARN, Region, and input artifacts.
--
-- * 'aedActionName' - The name of the action.
--
-- * 'aedOutput' - Output details for the action execution, such as the action execution result.
--
-- * 'aedPipelineExecutionId' - The pipeline execution ID for the action execution.
--
-- * 'aedStageName' - The name of the stage that contains the action.
--
-- * 'aedLastUpdateTime' - The last update time of the action execution.
--
-- * 'aedActionExecutionId' - The action execution ID.
actionExecutionDetail ::
  ActionExecutionDetail
actionExecutionDetail =
  ActionExecutionDetail'
    { _aedStatus = Nothing,
      _aedStartTime = Nothing,
      _aedPipelineVersion = Nothing,
      _aedInput = Nothing,
      _aedActionName = Nothing,
      _aedOutput = Nothing,
      _aedPipelineExecutionId = Nothing,
      _aedStageName = Nothing,
      _aedLastUpdateTime = Nothing,
      _aedActionExecutionId = Nothing
    }

-- | The status of the action execution. Status categories are @InProgress@ , @Succeeded@ , and @Failed@ .
aedStatus :: Lens' ActionExecutionDetail (Maybe ActionExecutionStatus)
aedStatus = lens _aedStatus (\s a -> s {_aedStatus = a})

-- | The start time of the action execution.
aedStartTime :: Lens' ActionExecutionDetail (Maybe UTCTime)
aedStartTime = lens _aedStartTime (\s a -> s {_aedStartTime = a}) . mapping _Time

-- | The version of the pipeline where the action was run.
aedPipelineVersion :: Lens' ActionExecutionDetail (Maybe Natural)
aedPipelineVersion = lens _aedPipelineVersion (\s a -> s {_aedPipelineVersion = a}) . mapping _Nat

-- | Input details for the action execution, such as role ARN, Region, and input artifacts.
aedInput :: Lens' ActionExecutionDetail (Maybe ActionExecutionInput)
aedInput = lens _aedInput (\s a -> s {_aedInput = a})

-- | The name of the action.
aedActionName :: Lens' ActionExecutionDetail (Maybe Text)
aedActionName = lens _aedActionName (\s a -> s {_aedActionName = a})

-- | Output details for the action execution, such as the action execution result.
aedOutput :: Lens' ActionExecutionDetail (Maybe ActionExecutionOutput)
aedOutput = lens _aedOutput (\s a -> s {_aedOutput = a})

-- | The pipeline execution ID for the action execution.
aedPipelineExecutionId :: Lens' ActionExecutionDetail (Maybe Text)
aedPipelineExecutionId = lens _aedPipelineExecutionId (\s a -> s {_aedPipelineExecutionId = a})

-- | The name of the stage that contains the action.
aedStageName :: Lens' ActionExecutionDetail (Maybe Text)
aedStageName = lens _aedStageName (\s a -> s {_aedStageName = a})

-- | The last update time of the action execution.
aedLastUpdateTime :: Lens' ActionExecutionDetail (Maybe UTCTime)
aedLastUpdateTime = lens _aedLastUpdateTime (\s a -> s {_aedLastUpdateTime = a}) . mapping _Time

-- | The action execution ID.
aedActionExecutionId :: Lens' ActionExecutionDetail (Maybe Text)
aedActionExecutionId = lens _aedActionExecutionId (\s a -> s {_aedActionExecutionId = a})

instance FromJSON ActionExecutionDetail where
  parseJSON =
    withObject
      "ActionExecutionDetail"
      ( \x ->
          ActionExecutionDetail'
            <$> (x .:? "status")
            <*> (x .:? "startTime")
            <*> (x .:? "pipelineVersion")
            <*> (x .:? "input")
            <*> (x .:? "actionName")
            <*> (x .:? "output")
            <*> (x .:? "pipelineExecutionId")
            <*> (x .:? "stageName")
            <*> (x .:? "lastUpdateTime")
            <*> (x .:? "actionExecutionId")
      )

instance Hashable ActionExecutionDetail

instance NFData ActionExecutionDetail
