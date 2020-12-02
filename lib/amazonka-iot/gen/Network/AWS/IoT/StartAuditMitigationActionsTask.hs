{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StartAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task that applies a set of mitigation actions to the specified target.
module Network.AWS.IoT.StartAuditMitigationActionsTask
  ( -- * Creating a Request
    startAuditMitigationActionsTask,
    StartAuditMitigationActionsTask,

    -- * Request Lenses
    samatTaskId,
    samatTarget,
    samatAuditCheckToActionsMapping,
    samatClientRequestToken,

    -- * Destructuring the Response
    startAuditMitigationActionsTaskResponse,
    StartAuditMitigationActionsTaskResponse,

    -- * Response Lenses
    samatrsTaskId,
    samatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startAuditMitigationActionsTask' smart constructor.
data StartAuditMitigationActionsTask = StartAuditMitigationActionsTask'
  { _samatTaskId ::
      !Text,
    _samatTarget ::
      !AuditMitigationActionsTaskTarget,
    _samatAuditCheckToActionsMapping ::
      !(Map Text (List1 Text)),
    _samatClientRequestToken ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartAuditMitigationActionsTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samatTaskId' - A unique identifier for the task. You can use this identifier to check the status of the task or to cancel it.
--
-- * 'samatTarget' - Specifies the audit findings to which the mitigation actions are applied. You can apply them to a type of audit check, to all findings from an audit, or to a speecific set of findings.
--
-- * 'samatAuditCheckToActionsMapping' - For an audit check, specifies which mitigation actions to apply. Those actions must be defined in your AWS account.
--
-- * 'samatClientRequestToken' - Each audit mitigation task must have a unique client request token. If you try to start a new task with the same token as a task that already exists, an exception occurs. If you omit this value, a unique client request token is generated automatically.
startAuditMitigationActionsTask ::
  -- | 'samatTaskId'
  Text ->
  -- | 'samatTarget'
  AuditMitigationActionsTaskTarget ->
  -- | 'samatClientRequestToken'
  Text ->
  StartAuditMitigationActionsTask
startAuditMitigationActionsTask
  pTaskId_
  pTarget_
  pClientRequestToken_ =
    StartAuditMitigationActionsTask'
      { _samatTaskId = pTaskId_,
        _samatTarget = pTarget_,
        _samatAuditCheckToActionsMapping = mempty,
        _samatClientRequestToken = pClientRequestToken_
      }

-- | A unique identifier for the task. You can use this identifier to check the status of the task or to cancel it.
samatTaskId :: Lens' StartAuditMitigationActionsTask Text
samatTaskId = lens _samatTaskId (\s a -> s {_samatTaskId = a})

-- | Specifies the audit findings to which the mitigation actions are applied. You can apply them to a type of audit check, to all findings from an audit, or to a speecific set of findings.
samatTarget :: Lens' StartAuditMitigationActionsTask AuditMitigationActionsTaskTarget
samatTarget = lens _samatTarget (\s a -> s {_samatTarget = a})

-- | For an audit check, specifies which mitigation actions to apply. Those actions must be defined in your AWS account.
samatAuditCheckToActionsMapping :: Lens' StartAuditMitigationActionsTask (HashMap Text (NonEmpty Text))
samatAuditCheckToActionsMapping = lens _samatAuditCheckToActionsMapping (\s a -> s {_samatAuditCheckToActionsMapping = a}) . _Map

-- | Each audit mitigation task must have a unique client request token. If you try to start a new task with the same token as a task that already exists, an exception occurs. If you omit this value, a unique client request token is generated automatically.
samatClientRequestToken :: Lens' StartAuditMitigationActionsTask Text
samatClientRequestToken = lens _samatClientRequestToken (\s a -> s {_samatClientRequestToken = a})

instance AWSRequest StartAuditMitigationActionsTask where
  type
    Rs StartAuditMitigationActionsTask =
      StartAuditMitigationActionsTaskResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          StartAuditMitigationActionsTaskResponse'
            <$> (x .?> "taskId") <*> (pure (fromEnum s))
      )

instance Hashable StartAuditMitigationActionsTask

instance NFData StartAuditMitigationActionsTask

instance ToHeaders StartAuditMitigationActionsTask where
  toHeaders = const mempty

instance ToJSON StartAuditMitigationActionsTask where
  toJSON StartAuditMitigationActionsTask' {..} =
    object
      ( catMaybes
          [ Just ("target" .= _samatTarget),
            Just
              ("auditCheckToActionsMapping" .= _samatAuditCheckToActionsMapping),
            Just ("clientRequestToken" .= _samatClientRequestToken)
          ]
      )

instance ToPath StartAuditMitigationActionsTask where
  toPath StartAuditMitigationActionsTask' {..} =
    mconcat ["/audit/mitigationactions/tasks/", toBS _samatTaskId]

instance ToQuery StartAuditMitigationActionsTask where
  toQuery = const mempty

-- | /See:/ 'startAuditMitigationActionsTaskResponse' smart constructor.
data StartAuditMitigationActionsTaskResponse = StartAuditMitigationActionsTaskResponse'
  { _samatrsTaskId ::
      !( Maybe
           Text
       ),
    _samatrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartAuditMitigationActionsTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'samatrsTaskId' - The unique identifier for the audit mitigation task. This matches the @taskId@ that you specified in the request.
--
-- * 'samatrsResponseStatus' - -- | The response status code.
startAuditMitigationActionsTaskResponse ::
  -- | 'samatrsResponseStatus'
  Int ->
  StartAuditMitigationActionsTaskResponse
startAuditMitigationActionsTaskResponse pResponseStatus_ =
  StartAuditMitigationActionsTaskResponse'
    { _samatrsTaskId =
        Nothing,
      _samatrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier for the audit mitigation task. This matches the @taskId@ that you specified in the request.
samatrsTaskId :: Lens' StartAuditMitigationActionsTaskResponse (Maybe Text)
samatrsTaskId = lens _samatrsTaskId (\s a -> s {_samatrsTaskId = a})

-- | -- | The response status code.
samatrsResponseStatus :: Lens' StartAuditMitigationActionsTaskResponse Int
samatrsResponseStatus = lens _samatrsResponseStatus (\s a -> s {_samatrsResponseStatus = a})

instance NFData StartAuditMitigationActionsTaskResponse
