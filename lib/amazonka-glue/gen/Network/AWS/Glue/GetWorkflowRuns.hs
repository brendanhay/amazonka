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
-- Module      : Network.AWS.Glue.GetWorkflowRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given workflow.
module Network.AWS.Glue.GetWorkflowRuns
  ( -- * Creating a Request
    getWorkflowRuns,
    GetWorkflowRuns,

    -- * Request Lenses
    gwrIncludeGraph,
    gwrNextToken,
    gwrMaxResults,
    gwrName,

    -- * Destructuring the Response
    getWorkflowRunsResponse,
    GetWorkflowRunsResponse,

    -- * Response Lenses
    gwrrsRuns,
    gwrrsNextToken,
    gwrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getWorkflowRuns' smart constructor.
data GetWorkflowRuns = GetWorkflowRuns'
  { _gwrIncludeGraph ::
      !(Maybe Bool),
    _gwrNextToken :: !(Maybe Text),
    _gwrMaxResults :: !(Maybe Nat),
    _gwrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetWorkflowRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwrIncludeGraph' - Specifies whether to include the workflow graph in response or not.
--
-- * 'gwrNextToken' - The maximum size of the response.
--
-- * 'gwrMaxResults' - The maximum number of workflow runs to be included in the response.
--
-- * 'gwrName' - Name of the workflow whose metadata of runs should be returned.
getWorkflowRuns ::
  -- | 'gwrName'
  Text ->
  GetWorkflowRuns
getWorkflowRuns pName_ =
  GetWorkflowRuns'
    { _gwrIncludeGraph = Nothing,
      _gwrNextToken = Nothing,
      _gwrMaxResults = Nothing,
      _gwrName = pName_
    }

-- | Specifies whether to include the workflow graph in response or not.
gwrIncludeGraph :: Lens' GetWorkflowRuns (Maybe Bool)
gwrIncludeGraph = lens _gwrIncludeGraph (\s a -> s {_gwrIncludeGraph = a})

-- | The maximum size of the response.
gwrNextToken :: Lens' GetWorkflowRuns (Maybe Text)
gwrNextToken = lens _gwrNextToken (\s a -> s {_gwrNextToken = a})

-- | The maximum number of workflow runs to be included in the response.
gwrMaxResults :: Lens' GetWorkflowRuns (Maybe Natural)
gwrMaxResults = lens _gwrMaxResults (\s a -> s {_gwrMaxResults = a}) . mapping _Nat

-- | Name of the workflow whose metadata of runs should be returned.
gwrName :: Lens' GetWorkflowRuns Text
gwrName = lens _gwrName (\s a -> s {_gwrName = a})

instance AWSRequest GetWorkflowRuns where
  type Rs GetWorkflowRuns = GetWorkflowRunsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetWorkflowRunsResponse'
            <$> (x .?> "Runs") <*> (x .?> "NextToken") <*> (pure (fromEnum s))
      )

instance Hashable GetWorkflowRuns

instance NFData GetWorkflowRuns

instance ToHeaders GetWorkflowRuns where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetWorkflowRuns" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetWorkflowRuns where
  toJSON GetWorkflowRuns' {..} =
    object
      ( catMaybes
          [ ("IncludeGraph" .=) <$> _gwrIncludeGraph,
            ("NextToken" .=) <$> _gwrNextToken,
            ("MaxResults" .=) <$> _gwrMaxResults,
            Just ("Name" .= _gwrName)
          ]
      )

instance ToPath GetWorkflowRuns where
  toPath = const "/"

instance ToQuery GetWorkflowRuns where
  toQuery = const mempty

-- | /See:/ 'getWorkflowRunsResponse' smart constructor.
data GetWorkflowRunsResponse = GetWorkflowRunsResponse'
  { _gwrrsRuns ::
      !(Maybe (List1 WorkflowRun)),
    _gwrrsNextToken :: !(Maybe Text),
    _gwrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetWorkflowRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwrrsRuns' - A list of workflow run metadata objects.
--
-- * 'gwrrsNextToken' - A continuation token, if not all requested workflow runs have been returned.
--
-- * 'gwrrsResponseStatus' - -- | The response status code.
getWorkflowRunsResponse ::
  -- | 'gwrrsResponseStatus'
  Int ->
  GetWorkflowRunsResponse
getWorkflowRunsResponse pResponseStatus_ =
  GetWorkflowRunsResponse'
    { _gwrrsRuns = Nothing,
      _gwrrsNextToken = Nothing,
      _gwrrsResponseStatus = pResponseStatus_
    }

-- | A list of workflow run metadata objects.
gwrrsRuns :: Lens' GetWorkflowRunsResponse (Maybe (NonEmpty WorkflowRun))
gwrrsRuns = lens _gwrrsRuns (\s a -> s {_gwrrsRuns = a}) . mapping _List1

-- | A continuation token, if not all requested workflow runs have been returned.
gwrrsNextToken :: Lens' GetWorkflowRunsResponse (Maybe Text)
gwrrsNextToken = lens _gwrrsNextToken (\s a -> s {_gwrrsNextToken = a})

-- | -- | The response status code.
gwrrsResponseStatus :: Lens' GetWorkflowRunsResponse Int
gwrrsResponseStatus = lens _gwrrsResponseStatus (\s a -> s {_gwrrsResponseStatus = a})

instance NFData GetWorkflowRunsResponse
