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
-- Module      : Network.AWS.Glue.GetMLTaskRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of runs for a machine learning transform. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can get a sortable, filterable list of machine learning task runs by calling @GetMLTaskRuns@ with their parent transform's @TransformID@ and other optional parameters as documented in this section.
--
--
-- This operation returns a list of historic runs and must be paginated.
module Network.AWS.Glue.GetMLTaskRuns
  ( -- * Creating a Request
    getMLTaskRuns,
    GetMLTaskRuns,

    -- * Request Lenses
    gmltrNextToken,
    gmltrSort,
    gmltrFilter,
    gmltrMaxResults,
    gmltrTransformId,

    -- * Destructuring the Response
    getMLTaskRunsResponse,
    GetMLTaskRunsResponse,

    -- * Response Lenses
    gmltrsrsNextToken,
    gmltrsrsTaskRuns,
    gmltrsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMLTaskRuns' smart constructor.
data GetMLTaskRuns = GetMLTaskRuns'
  { _gmltrNextToken ::
      !(Maybe Text),
    _gmltrSort :: !(Maybe TaskRunSortCriteria),
    _gmltrFilter :: !(Maybe TaskRunFilterCriteria),
    _gmltrMaxResults :: !(Maybe Nat),
    _gmltrTransformId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTaskRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmltrNextToken' - A token for pagination of the results. The default is empty.
--
-- * 'gmltrSort' - The sorting criteria, in the @TaskRunSortCriteria@ structure, for the task run.
--
-- * 'gmltrFilter' - The filter criteria, in the @TaskRunFilterCriteria@ structure, for the task run.
--
-- * 'gmltrMaxResults' - The maximum number of results to return.
--
-- * 'gmltrTransformId' - The unique identifier of the machine learning transform.
getMLTaskRuns ::
  -- | 'gmltrTransformId'
  Text ->
  GetMLTaskRuns
getMLTaskRuns pTransformId_ =
  GetMLTaskRuns'
    { _gmltrNextToken = Nothing,
      _gmltrSort = Nothing,
      _gmltrFilter = Nothing,
      _gmltrMaxResults = Nothing,
      _gmltrTransformId = pTransformId_
    }

-- | A token for pagination of the results. The default is empty.
gmltrNextToken :: Lens' GetMLTaskRuns (Maybe Text)
gmltrNextToken = lens _gmltrNextToken (\s a -> s {_gmltrNextToken = a})

-- | The sorting criteria, in the @TaskRunSortCriteria@ structure, for the task run.
gmltrSort :: Lens' GetMLTaskRuns (Maybe TaskRunSortCriteria)
gmltrSort = lens _gmltrSort (\s a -> s {_gmltrSort = a})

-- | The filter criteria, in the @TaskRunFilterCriteria@ structure, for the task run.
gmltrFilter :: Lens' GetMLTaskRuns (Maybe TaskRunFilterCriteria)
gmltrFilter = lens _gmltrFilter (\s a -> s {_gmltrFilter = a})

-- | The maximum number of results to return.
gmltrMaxResults :: Lens' GetMLTaskRuns (Maybe Natural)
gmltrMaxResults = lens _gmltrMaxResults (\s a -> s {_gmltrMaxResults = a}) . mapping _Nat

-- | The unique identifier of the machine learning transform.
gmltrTransformId :: Lens' GetMLTaskRuns Text
gmltrTransformId = lens _gmltrTransformId (\s a -> s {_gmltrTransformId = a})

instance AWSRequest GetMLTaskRuns where
  type Rs GetMLTaskRuns = GetMLTaskRunsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetMLTaskRunsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "TaskRuns" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetMLTaskRuns

instance NFData GetMLTaskRuns

instance ToHeaders GetMLTaskRuns where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetMLTaskRuns" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMLTaskRuns where
  toJSON GetMLTaskRuns' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gmltrNextToken,
            ("Sort" .=) <$> _gmltrSort,
            ("Filter" .=) <$> _gmltrFilter,
            ("MaxResults" .=) <$> _gmltrMaxResults,
            Just ("TransformId" .= _gmltrTransformId)
          ]
      )

instance ToPath GetMLTaskRuns where
  toPath = const "/"

instance ToQuery GetMLTaskRuns where
  toQuery = const mempty

-- | /See:/ 'getMLTaskRunsResponse' smart constructor.
data GetMLTaskRunsResponse = GetMLTaskRunsResponse'
  { _gmltrsrsNextToken ::
      !(Maybe Text),
    _gmltrsrsTaskRuns :: !(Maybe [TaskRun]),
    _gmltrsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTaskRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmltrsrsNextToken' - A pagination token, if more results are available.
--
-- * 'gmltrsrsTaskRuns' - A list of task runs that are associated with the transform.
--
-- * 'gmltrsrsResponseStatus' - -- | The response status code.
getMLTaskRunsResponse ::
  -- | 'gmltrsrsResponseStatus'
  Int ->
  GetMLTaskRunsResponse
getMLTaskRunsResponse pResponseStatus_ =
  GetMLTaskRunsResponse'
    { _gmltrsrsNextToken = Nothing,
      _gmltrsrsTaskRuns = Nothing,
      _gmltrsrsResponseStatus = pResponseStatus_
    }

-- | A pagination token, if more results are available.
gmltrsrsNextToken :: Lens' GetMLTaskRunsResponse (Maybe Text)
gmltrsrsNextToken = lens _gmltrsrsNextToken (\s a -> s {_gmltrsrsNextToken = a})

-- | A list of task runs that are associated with the transform.
gmltrsrsTaskRuns :: Lens' GetMLTaskRunsResponse [TaskRun]
gmltrsrsTaskRuns = lens _gmltrsrsTaskRuns (\s a -> s {_gmltrsrsTaskRuns = a}) . _Default . _Coerce

-- | -- | The response status code.
gmltrsrsResponseStatus :: Lens' GetMLTaskRunsResponse Int
gmltrsrsResponseStatus = lens _gmltrsrsResponseStatus (\s a -> s {_gmltrsrsResponseStatus = a})

instance NFData GetMLTaskRunsResponse
