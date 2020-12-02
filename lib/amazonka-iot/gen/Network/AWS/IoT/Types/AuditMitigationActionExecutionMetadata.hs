{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata where

import Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returned by ListAuditMitigationActionsTask, this object contains information that describes a mitigation action that has been started.
--
--
--
-- /See:/ 'auditMitigationActionExecutionMetadata' smart constructor.
data AuditMitigationActionExecutionMetadata = AuditMitigationActionExecutionMetadata'
  { _amaemStatus ::
      !( Maybe
           AuditMitigationActionsExecutionStatus
       ),
    _amaemStartTime ::
      !( Maybe
           POSIX
       ),
    _amaemTaskId ::
      !(Maybe Text),
    _amaemActionId ::
      !(Maybe Text),
    _amaemActionName ::
      !(Maybe Text),
    _amaemEndTime ::
      !( Maybe
           POSIX
       ),
    _amaemErrorCode ::
      !(Maybe Text),
    _amaemFindingId ::
      !(Maybe Text),
    _amaemMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuditMitigationActionExecutionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amaemStatus' - The current status of the task being executed.
--
-- * 'amaemStartTime' - The date and time when the task was started.
--
-- * 'amaemTaskId' - The unique identifier for the task that applies the mitigation action.
--
-- * 'amaemActionId' - The unique identifier for the mitigation action being applied by the task.
--
-- * 'amaemActionName' - The friendly name of the mitigation action being applied by the task.
--
-- * 'amaemEndTime' - The date and time when the task was completed or canceled. Blank if the task is still running.
--
-- * 'amaemErrorCode' - If an error occurred, the code that indicates which type of error occurred.
--
-- * 'amaemFindingId' - The unique identifier for the findings to which the task and associated mitigation action are applied.
--
-- * 'amaemMessage' - If an error occurred, a message that describes the error.
auditMitigationActionExecutionMetadata ::
  AuditMitigationActionExecutionMetadata
auditMitigationActionExecutionMetadata =
  AuditMitigationActionExecutionMetadata'
    { _amaemStatus = Nothing,
      _amaemStartTime = Nothing,
      _amaemTaskId = Nothing,
      _amaemActionId = Nothing,
      _amaemActionName = Nothing,
      _amaemEndTime = Nothing,
      _amaemErrorCode = Nothing,
      _amaemFindingId = Nothing,
      _amaemMessage = Nothing
    }

-- | The current status of the task being executed.
amaemStatus :: Lens' AuditMitigationActionExecutionMetadata (Maybe AuditMitigationActionsExecutionStatus)
amaemStatus = lens _amaemStatus (\s a -> s {_amaemStatus = a})

-- | The date and time when the task was started.
amaemStartTime :: Lens' AuditMitigationActionExecutionMetadata (Maybe UTCTime)
amaemStartTime = lens _amaemStartTime (\s a -> s {_amaemStartTime = a}) . mapping _Time

-- | The unique identifier for the task that applies the mitigation action.
amaemTaskId :: Lens' AuditMitigationActionExecutionMetadata (Maybe Text)
amaemTaskId = lens _amaemTaskId (\s a -> s {_amaemTaskId = a})

-- | The unique identifier for the mitigation action being applied by the task.
amaemActionId :: Lens' AuditMitigationActionExecutionMetadata (Maybe Text)
amaemActionId = lens _amaemActionId (\s a -> s {_amaemActionId = a})

-- | The friendly name of the mitigation action being applied by the task.
amaemActionName :: Lens' AuditMitigationActionExecutionMetadata (Maybe Text)
amaemActionName = lens _amaemActionName (\s a -> s {_amaemActionName = a})

-- | The date and time when the task was completed or canceled. Blank if the task is still running.
amaemEndTime :: Lens' AuditMitigationActionExecutionMetadata (Maybe UTCTime)
amaemEndTime = lens _amaemEndTime (\s a -> s {_amaemEndTime = a}) . mapping _Time

-- | If an error occurred, the code that indicates which type of error occurred.
amaemErrorCode :: Lens' AuditMitigationActionExecutionMetadata (Maybe Text)
amaemErrorCode = lens _amaemErrorCode (\s a -> s {_amaemErrorCode = a})

-- | The unique identifier for the findings to which the task and associated mitigation action are applied.
amaemFindingId :: Lens' AuditMitigationActionExecutionMetadata (Maybe Text)
amaemFindingId = lens _amaemFindingId (\s a -> s {_amaemFindingId = a})

-- | If an error occurred, a message that describes the error.
amaemMessage :: Lens' AuditMitigationActionExecutionMetadata (Maybe Text)
amaemMessage = lens _amaemMessage (\s a -> s {_amaemMessage = a})

instance FromJSON AuditMitigationActionExecutionMetadata where
  parseJSON =
    withObject
      "AuditMitigationActionExecutionMetadata"
      ( \x ->
          AuditMitigationActionExecutionMetadata'
            <$> (x .:? "status")
            <*> (x .:? "startTime")
            <*> (x .:? "taskId")
            <*> (x .:? "actionId")
            <*> (x .:? "actionName")
            <*> (x .:? "endTime")
            <*> (x .:? "errorCode")
            <*> (x .:? "findingId")
            <*> (x .:? "message")
      )

instance Hashable AuditMitigationActionExecutionMetadata

instance NFData AuditMitigationActionExecutionMetadata
