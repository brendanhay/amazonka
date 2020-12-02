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
-- Module      : Network.AWS.EMR.ListNotebookExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summaries of all notebook executions. You can filter the list based on multiple criteria such as status, time range, and editor id. Returns a maximum of 50 notebook executions and a marker to track the paging of a longer notebook execution list across multiple @ListNotebookExecution@ calls.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListNotebookExecutions
  ( -- * Creating a Request
    listNotebookExecutions,
    ListNotebookExecutions,

    -- * Request Lenses
    lneStatus,
    lneEditorId,
    lneTo,
    lneFrom,
    lneMarker,

    -- * Destructuring the Response
    listNotebookExecutionsResponse,
    ListNotebookExecutionsResponse,

    -- * Response Lenses
    lnersNotebookExecutions,
    lnersMarker,
    lnersResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listNotebookExecutions' smart constructor.
data ListNotebookExecutions = ListNotebookExecutions'
  { _lneStatus ::
      !(Maybe NotebookExecutionStatus),
    _lneEditorId :: !(Maybe Text),
    _lneTo :: !(Maybe POSIX),
    _lneFrom :: !(Maybe POSIX),
    _lneMarker :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListNotebookExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lneStatus' - The status filter for listing notebook executions.     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.     * @STARTING@ indicates that the execution is starting on the cluster.     * @RUNNING@ indicates that the execution is being processed by the cluster.     * @FINISHING@ indicates that execution processing is in the final stages.     * @FINISHED@ indicates that the execution has completed without error.     * @FAILING@ indicates that the execution is failing and will not finish successfully.     * @FAILED@ indicates that the execution failed.     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
--
-- * 'lneEditorId' - The unique ID of the editor associated with the notebook execution.
--
-- * 'lneTo' - The end of time range filter for listing notebook executions. The default is the current timestamp.
--
-- * 'lneFrom' - The beginning of time range filter for listing notebook executions. The default is the timestamp of 30 days ago.
--
-- * 'lneMarker' - The pagination token, returned by a previous @ListNotebookExecutions@ call, that indicates the start of the list for this @ListNotebookExecutions@ call.
listNotebookExecutions ::
  ListNotebookExecutions
listNotebookExecutions =
  ListNotebookExecutions'
    { _lneStatus = Nothing,
      _lneEditorId = Nothing,
      _lneTo = Nothing,
      _lneFrom = Nothing,
      _lneMarker = Nothing
    }

-- | The status filter for listing notebook executions.     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.     * @STARTING@ indicates that the execution is starting on the cluster.     * @RUNNING@ indicates that the execution is being processed by the cluster.     * @FINISHING@ indicates that execution processing is in the final stages.     * @FINISHED@ indicates that the execution has completed without error.     * @FAILING@ indicates that the execution is failing and will not finish successfully.     * @FAILED@ indicates that the execution failed.     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
lneStatus :: Lens' ListNotebookExecutions (Maybe NotebookExecutionStatus)
lneStatus = lens _lneStatus (\s a -> s {_lneStatus = a})

-- | The unique ID of the editor associated with the notebook execution.
lneEditorId :: Lens' ListNotebookExecutions (Maybe Text)
lneEditorId = lens _lneEditorId (\s a -> s {_lneEditorId = a})

-- | The end of time range filter for listing notebook executions. The default is the current timestamp.
lneTo :: Lens' ListNotebookExecutions (Maybe UTCTime)
lneTo = lens _lneTo (\s a -> s {_lneTo = a}) . mapping _Time

-- | The beginning of time range filter for listing notebook executions. The default is the timestamp of 30 days ago.
lneFrom :: Lens' ListNotebookExecutions (Maybe UTCTime)
lneFrom = lens _lneFrom (\s a -> s {_lneFrom = a}) . mapping _Time

-- | The pagination token, returned by a previous @ListNotebookExecutions@ call, that indicates the start of the list for this @ListNotebookExecutions@ call.
lneMarker :: Lens' ListNotebookExecutions (Maybe Text)
lneMarker = lens _lneMarker (\s a -> s {_lneMarker = a})

instance AWSPager ListNotebookExecutions where
  page rq rs
    | stop (rs ^. lnersMarker) = Nothing
    | stop (rs ^. lnersNotebookExecutions) = Nothing
    | otherwise = Just $ rq & lneMarker .~ rs ^. lnersMarker

instance AWSRequest ListNotebookExecutions where
  type Rs ListNotebookExecutions = ListNotebookExecutionsResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          ListNotebookExecutionsResponse'
            <$> (x .?> "NotebookExecutions" .!@ mempty)
            <*> (x .?> "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable ListNotebookExecutions

instance NFData ListNotebookExecutions

instance ToHeaders ListNotebookExecutions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.ListNotebookExecutions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListNotebookExecutions where
  toJSON ListNotebookExecutions' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _lneStatus,
            ("EditorId" .=) <$> _lneEditorId,
            ("To" .=) <$> _lneTo,
            ("From" .=) <$> _lneFrom,
            ("Marker" .=) <$> _lneMarker
          ]
      )

instance ToPath ListNotebookExecutions where
  toPath = const "/"

instance ToQuery ListNotebookExecutions where
  toQuery = const mempty

-- | /See:/ 'listNotebookExecutionsResponse' smart constructor.
data ListNotebookExecutionsResponse = ListNotebookExecutionsResponse'
  { _lnersNotebookExecutions ::
      !( Maybe
           [NotebookExecutionSummary]
       ),
    _lnersMarker :: !(Maybe Text),
    _lnersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListNotebookExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnersNotebookExecutions' - A list of notebook executions.
--
-- * 'lnersMarker' - A pagination token that a subsequent @ListNotebookExecutions@ can use to determine the next set of results to retrieve.
--
-- * 'lnersResponseStatus' - -- | The response status code.
listNotebookExecutionsResponse ::
  -- | 'lnersResponseStatus'
  Int ->
  ListNotebookExecutionsResponse
listNotebookExecutionsResponse pResponseStatus_ =
  ListNotebookExecutionsResponse'
    { _lnersNotebookExecutions =
        Nothing,
      _lnersMarker = Nothing,
      _lnersResponseStatus = pResponseStatus_
    }

-- | A list of notebook executions.
lnersNotebookExecutions :: Lens' ListNotebookExecutionsResponse [NotebookExecutionSummary]
lnersNotebookExecutions = lens _lnersNotebookExecutions (\s a -> s {_lnersNotebookExecutions = a}) . _Default . _Coerce

-- | A pagination token that a subsequent @ListNotebookExecutions@ can use to determine the next set of results to retrieve.
lnersMarker :: Lens' ListNotebookExecutionsResponse (Maybe Text)
lnersMarker = lens _lnersMarker (\s a -> s {_lnersMarker = a})

-- | -- | The response status code.
lnersResponseStatus :: Lens' ListNotebookExecutionsResponse Int
lnersResponseStatus = lens _lnersResponseStatus (\s a -> s {_lnersResponseStatus = a})

instance NFData ListNotebookExecutionsResponse
