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
-- Module      : Network.AWS.Glue.ResumeWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts selected nodes of a previous partially completed workflow run and resumes the workflow run. The selected nodes and all nodes that are downstream from the selected nodes are run.
module Network.AWS.Glue.ResumeWorkflowRun
  ( -- * Creating a Request
    resumeWorkflowRun,
    ResumeWorkflowRun,

    -- * Request Lenses
    rwrName,
    rwrRunId,
    rwrNodeIds,

    -- * Destructuring the Response
    resumeWorkflowRunResponse,
    ResumeWorkflowRunResponse,

    -- * Response Lenses
    rwrrsNodeIds,
    rwrrsRunId,
    rwrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resumeWorkflowRun' smart constructor.
data ResumeWorkflowRun = ResumeWorkflowRun'
  { _rwrName :: !Text,
    _rwrRunId :: !Text,
    _rwrNodeIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResumeWorkflowRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwrName' - The name of the workflow to resume.
--
-- * 'rwrRunId' - The ID of the workflow run to resume.
--
-- * 'rwrNodeIds' - A list of the node IDs for the nodes you want to restart. The nodes that are to be restarted must have a run attempt in the original run.
resumeWorkflowRun ::
  -- | 'rwrName'
  Text ->
  -- | 'rwrRunId'
  Text ->
  ResumeWorkflowRun
resumeWorkflowRun pName_ pRunId_ =
  ResumeWorkflowRun'
    { _rwrName = pName_,
      _rwrRunId = pRunId_,
      _rwrNodeIds = mempty
    }

-- | The name of the workflow to resume.
rwrName :: Lens' ResumeWorkflowRun Text
rwrName = lens _rwrName (\s a -> s {_rwrName = a})

-- | The ID of the workflow run to resume.
rwrRunId :: Lens' ResumeWorkflowRun Text
rwrRunId = lens _rwrRunId (\s a -> s {_rwrRunId = a})

-- | A list of the node IDs for the nodes you want to restart. The nodes that are to be restarted must have a run attempt in the original run.
rwrNodeIds :: Lens' ResumeWorkflowRun [Text]
rwrNodeIds = lens _rwrNodeIds (\s a -> s {_rwrNodeIds = a}) . _Coerce

instance AWSRequest ResumeWorkflowRun where
  type Rs ResumeWorkflowRun = ResumeWorkflowRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ResumeWorkflowRunResponse'
            <$> (x .?> "NodeIds" .!@ mempty)
            <*> (x .?> "RunId")
            <*> (pure (fromEnum s))
      )

instance Hashable ResumeWorkflowRun

instance NFData ResumeWorkflowRun

instance ToHeaders ResumeWorkflowRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ResumeWorkflowRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ResumeWorkflowRun where
  toJSON ResumeWorkflowRun' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _rwrName),
            Just ("RunId" .= _rwrRunId),
            Just ("NodeIds" .= _rwrNodeIds)
          ]
      )

instance ToPath ResumeWorkflowRun where
  toPath = const "/"

instance ToQuery ResumeWorkflowRun where
  toQuery = const mempty

-- | /See:/ 'resumeWorkflowRunResponse' smart constructor.
data ResumeWorkflowRunResponse = ResumeWorkflowRunResponse'
  { _rwrrsNodeIds ::
      !(Maybe [Text]),
    _rwrrsRunId :: !(Maybe Text),
    _rwrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResumeWorkflowRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwrrsNodeIds' - A list of the node IDs for the nodes that were actually restarted.
--
-- * 'rwrrsRunId' - The new ID assigned to the resumed workflow run. Each resume of a workflow run will have a new run ID.
--
-- * 'rwrrsResponseStatus' - -- | The response status code.
resumeWorkflowRunResponse ::
  -- | 'rwrrsResponseStatus'
  Int ->
  ResumeWorkflowRunResponse
resumeWorkflowRunResponse pResponseStatus_ =
  ResumeWorkflowRunResponse'
    { _rwrrsNodeIds = Nothing,
      _rwrrsRunId = Nothing,
      _rwrrsResponseStatus = pResponseStatus_
    }

-- | A list of the node IDs for the nodes that were actually restarted.
rwrrsNodeIds :: Lens' ResumeWorkflowRunResponse [Text]
rwrrsNodeIds = lens _rwrrsNodeIds (\s a -> s {_rwrrsNodeIds = a}) . _Default . _Coerce

-- | The new ID assigned to the resumed workflow run. Each resume of a workflow run will have a new run ID.
rwrrsRunId :: Lens' ResumeWorkflowRunResponse (Maybe Text)
rwrrsRunId = lens _rwrrsRunId (\s a -> s {_rwrrsRunId = a})

-- | -- | The response status code.
rwrrsResponseStatus :: Lens' ResumeWorkflowRunResponse Int
rwrrsResponseStatus = lens _rwrrsResponseStatus (\s a -> s {_rwrrsResponseStatus = a})

instance NFData ResumeWorkflowRunResponse
