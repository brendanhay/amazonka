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
-- Module      : Network.AWS.Glue.GetMLTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for a specific task run on a machine learning transform. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can check the stats of any task run by calling @GetMLTaskRun@ with the @TaskRunID@ and its parent transform's @TransformID@ .
module Network.AWS.Glue.GetMLTaskRun
  ( -- * Creating a Request
    getMLTaskRun,
    GetMLTaskRun,

    -- * Request Lenses
    gTransformId,
    gTaskRunId,

    -- * Destructuring the Response
    getMLTaskRunResponse,
    GetMLTaskRunResponse,

    -- * Response Lenses
    gmltrrsCompletedOn,
    gmltrrsStatus,
    gmltrrsLastModifiedOn,
    gmltrrsErrorString,
    gmltrrsStartedOn,
    gmltrrsLogGroupName,
    gmltrrsExecutionTime,
    gmltrrsProperties,
    gmltrrsTransformId,
    gmltrrsTaskRunId,
    gmltrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMLTaskRun' smart constructor.
data GetMLTaskRun = GetMLTaskRun'
  { _gTransformId :: !Text,
    _gTaskRunId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTaskRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gTransformId' - The unique identifier of the machine learning transform.
--
-- * 'gTaskRunId' - The unique identifier of the task run.
getMLTaskRun ::
  -- | 'gTransformId'
  Text ->
  -- | 'gTaskRunId'
  Text ->
  GetMLTaskRun
getMLTaskRun pTransformId_ pTaskRunId_ =
  GetMLTaskRun'
    { _gTransformId = pTransformId_,
      _gTaskRunId = pTaskRunId_
    }

-- | The unique identifier of the machine learning transform.
gTransformId :: Lens' GetMLTaskRun Text
gTransformId = lens _gTransformId (\s a -> s {_gTransformId = a})

-- | The unique identifier of the task run.
gTaskRunId :: Lens' GetMLTaskRun Text
gTaskRunId = lens _gTaskRunId (\s a -> s {_gTaskRunId = a})

instance AWSRequest GetMLTaskRun where
  type Rs GetMLTaskRun = GetMLTaskRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetMLTaskRunResponse'
            <$> (x .?> "CompletedOn")
            <*> (x .?> "Status")
            <*> (x .?> "LastModifiedOn")
            <*> (x .?> "ErrorString")
            <*> (x .?> "StartedOn")
            <*> (x .?> "LogGroupName")
            <*> (x .?> "ExecutionTime")
            <*> (x .?> "Properties")
            <*> (x .?> "TransformId")
            <*> (x .?> "TaskRunId")
            <*> (pure (fromEnum s))
      )

instance Hashable GetMLTaskRun

instance NFData GetMLTaskRun

instance ToHeaders GetMLTaskRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetMLTaskRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMLTaskRun where
  toJSON GetMLTaskRun' {..} =
    object
      ( catMaybes
          [ Just ("TransformId" .= _gTransformId),
            Just ("TaskRunId" .= _gTaskRunId)
          ]
      )

instance ToPath GetMLTaskRun where
  toPath = const "/"

instance ToQuery GetMLTaskRun where
  toQuery = const mempty

-- | /See:/ 'getMLTaskRunResponse' smart constructor.
data GetMLTaskRunResponse = GetMLTaskRunResponse'
  { _gmltrrsCompletedOn ::
      !(Maybe POSIX),
    _gmltrrsStatus :: !(Maybe TaskStatusType),
    _gmltrrsLastModifiedOn :: !(Maybe POSIX),
    _gmltrrsErrorString :: !(Maybe Text),
    _gmltrrsStartedOn :: !(Maybe POSIX),
    _gmltrrsLogGroupName :: !(Maybe Text),
    _gmltrrsExecutionTime :: !(Maybe Int),
    _gmltrrsProperties :: !(Maybe TaskRunProperties),
    _gmltrrsTransformId :: !(Maybe Text),
    _gmltrrsTaskRunId :: !(Maybe Text),
    _gmltrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTaskRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmltrrsCompletedOn' - The date and time when this task run was completed.
--
-- * 'gmltrrsStatus' - The status for this task run.
--
-- * 'gmltrrsLastModifiedOn' - The date and time when this task run was last modified.
--
-- * 'gmltrrsErrorString' - The error strings that are associated with the task run.
--
-- * 'gmltrrsStartedOn' - The date and time when this task run started.
--
-- * 'gmltrrsLogGroupName' - The names of the log groups that are associated with the task run.
--
-- * 'gmltrrsExecutionTime' - The amount of time (in seconds) that the task run consumed resources.
--
-- * 'gmltrrsProperties' - The list of properties that are associated with the task run.
--
-- * 'gmltrrsTransformId' - The unique identifier of the task run.
--
-- * 'gmltrrsTaskRunId' - The unique run identifier associated with this run.
--
-- * 'gmltrrsResponseStatus' - -- | The response status code.
getMLTaskRunResponse ::
  -- | 'gmltrrsResponseStatus'
  Int ->
  GetMLTaskRunResponse
getMLTaskRunResponse pResponseStatus_ =
  GetMLTaskRunResponse'
    { _gmltrrsCompletedOn = Nothing,
      _gmltrrsStatus = Nothing,
      _gmltrrsLastModifiedOn = Nothing,
      _gmltrrsErrorString = Nothing,
      _gmltrrsStartedOn = Nothing,
      _gmltrrsLogGroupName = Nothing,
      _gmltrrsExecutionTime = Nothing,
      _gmltrrsProperties = Nothing,
      _gmltrrsTransformId = Nothing,
      _gmltrrsTaskRunId = Nothing,
      _gmltrrsResponseStatus = pResponseStatus_
    }

-- | The date and time when this task run was completed.
gmltrrsCompletedOn :: Lens' GetMLTaskRunResponse (Maybe UTCTime)
gmltrrsCompletedOn = lens _gmltrrsCompletedOn (\s a -> s {_gmltrrsCompletedOn = a}) . mapping _Time

-- | The status for this task run.
gmltrrsStatus :: Lens' GetMLTaskRunResponse (Maybe TaskStatusType)
gmltrrsStatus = lens _gmltrrsStatus (\s a -> s {_gmltrrsStatus = a})

-- | The date and time when this task run was last modified.
gmltrrsLastModifiedOn :: Lens' GetMLTaskRunResponse (Maybe UTCTime)
gmltrrsLastModifiedOn = lens _gmltrrsLastModifiedOn (\s a -> s {_gmltrrsLastModifiedOn = a}) . mapping _Time

-- | The error strings that are associated with the task run.
gmltrrsErrorString :: Lens' GetMLTaskRunResponse (Maybe Text)
gmltrrsErrorString = lens _gmltrrsErrorString (\s a -> s {_gmltrrsErrorString = a})

-- | The date and time when this task run started.
gmltrrsStartedOn :: Lens' GetMLTaskRunResponse (Maybe UTCTime)
gmltrrsStartedOn = lens _gmltrrsStartedOn (\s a -> s {_gmltrrsStartedOn = a}) . mapping _Time

-- | The names of the log groups that are associated with the task run.
gmltrrsLogGroupName :: Lens' GetMLTaskRunResponse (Maybe Text)
gmltrrsLogGroupName = lens _gmltrrsLogGroupName (\s a -> s {_gmltrrsLogGroupName = a})

-- | The amount of time (in seconds) that the task run consumed resources.
gmltrrsExecutionTime :: Lens' GetMLTaskRunResponse (Maybe Int)
gmltrrsExecutionTime = lens _gmltrrsExecutionTime (\s a -> s {_gmltrrsExecutionTime = a})

-- | The list of properties that are associated with the task run.
gmltrrsProperties :: Lens' GetMLTaskRunResponse (Maybe TaskRunProperties)
gmltrrsProperties = lens _gmltrrsProperties (\s a -> s {_gmltrrsProperties = a})

-- | The unique identifier of the task run.
gmltrrsTransformId :: Lens' GetMLTaskRunResponse (Maybe Text)
gmltrrsTransformId = lens _gmltrrsTransformId (\s a -> s {_gmltrrsTransformId = a})

-- | The unique run identifier associated with this run.
gmltrrsTaskRunId :: Lens' GetMLTaskRunResponse (Maybe Text)
gmltrrsTaskRunId = lens _gmltrrsTaskRunId (\s a -> s {_gmltrrsTaskRunId = a})

-- | -- | The response status code.
gmltrrsResponseStatus :: Lens' GetMLTaskRunResponse Int
gmltrrsResponseStatus = lens _gmltrrsResponseStatus (\s a -> s {_gmltrrsResponseStatus = a})

instance NFData GetMLTaskRunResponse
