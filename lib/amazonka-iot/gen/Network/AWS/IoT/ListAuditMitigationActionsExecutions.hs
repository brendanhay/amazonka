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
-- Module      : Network.AWS.IoT.ListAuditMitigationActionsExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of audit mitigation action tasks that were executed.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditMitigationActionsExecutions
  ( -- * Creating a Request
    listAuditMitigationActionsExecutions,
    ListAuditMitigationActionsExecutions,

    -- * Request Lenses
    lamaeNextToken,
    lamaeActionStatus,
    lamaeMaxResults,
    lamaeTaskId,
    lamaeFindingId,

    -- * Destructuring the Response
    listAuditMitigationActionsExecutionsResponse,
    ListAuditMitigationActionsExecutionsResponse,

    -- * Response Lenses
    lamaersActionsExecutions,
    lamaersNextToken,
    lamaersResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAuditMitigationActionsExecutions' smart constructor.
data ListAuditMitigationActionsExecutions = ListAuditMitigationActionsExecutions'
  { _lamaeNextToken ::
      !(Maybe Text),
    _lamaeActionStatus ::
      !( Maybe
           AuditMitigationActionsExecutionStatus
       ),
    _lamaeMaxResults ::
      !(Maybe Nat),
    _lamaeTaskId ::
      !Text,
    _lamaeFindingId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditMitigationActionsExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lamaeNextToken' - The token for the next set of results.
--
-- * 'lamaeActionStatus' - Specify this filter to limit results to those with a specific status.
--
-- * 'lamaeMaxResults' - The maximum number of results to return at one time. The default is 25.
--
-- * 'lamaeTaskId' - Specify this filter to limit results to actions for a specific audit mitigation actions task.
--
-- * 'lamaeFindingId' - Specify this filter to limit results to those that were applied to a specific audit finding.
listAuditMitigationActionsExecutions ::
  -- | 'lamaeTaskId'
  Text ->
  -- | 'lamaeFindingId'
  Text ->
  ListAuditMitigationActionsExecutions
listAuditMitigationActionsExecutions pTaskId_ pFindingId_ =
  ListAuditMitigationActionsExecutions'
    { _lamaeNextToken = Nothing,
      _lamaeActionStatus = Nothing,
      _lamaeMaxResults = Nothing,
      _lamaeTaskId = pTaskId_,
      _lamaeFindingId = pFindingId_
    }

-- | The token for the next set of results.
lamaeNextToken :: Lens' ListAuditMitigationActionsExecutions (Maybe Text)
lamaeNextToken = lens _lamaeNextToken (\s a -> s {_lamaeNextToken = a})

-- | Specify this filter to limit results to those with a specific status.
lamaeActionStatus :: Lens' ListAuditMitigationActionsExecutions (Maybe AuditMitigationActionsExecutionStatus)
lamaeActionStatus = lens _lamaeActionStatus (\s a -> s {_lamaeActionStatus = a})

-- | The maximum number of results to return at one time. The default is 25.
lamaeMaxResults :: Lens' ListAuditMitigationActionsExecutions (Maybe Natural)
lamaeMaxResults = lens _lamaeMaxResults (\s a -> s {_lamaeMaxResults = a}) . mapping _Nat

-- | Specify this filter to limit results to actions for a specific audit mitigation actions task.
lamaeTaskId :: Lens' ListAuditMitigationActionsExecutions Text
lamaeTaskId = lens _lamaeTaskId (\s a -> s {_lamaeTaskId = a})

-- | Specify this filter to limit results to those that were applied to a specific audit finding.
lamaeFindingId :: Lens' ListAuditMitigationActionsExecutions Text
lamaeFindingId = lens _lamaeFindingId (\s a -> s {_lamaeFindingId = a})

instance AWSPager ListAuditMitigationActionsExecutions where
  page rq rs
    | stop (rs ^. lamaersNextToken) = Nothing
    | stop (rs ^. lamaersActionsExecutions) = Nothing
    | otherwise = Just $ rq & lamaeNextToken .~ rs ^. lamaersNextToken

instance AWSRequest ListAuditMitigationActionsExecutions where
  type
    Rs ListAuditMitigationActionsExecutions =
      ListAuditMitigationActionsExecutionsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListAuditMitigationActionsExecutionsResponse'
            <$> (x .?> "actionsExecutions" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListAuditMitigationActionsExecutions

instance NFData ListAuditMitigationActionsExecutions

instance ToHeaders ListAuditMitigationActionsExecutions where
  toHeaders = const mempty

instance ToPath ListAuditMitigationActionsExecutions where
  toPath = const "/audit/mitigationactions/executions"

instance ToQuery ListAuditMitigationActionsExecutions where
  toQuery ListAuditMitigationActionsExecutions' {..} =
    mconcat
      [ "nextToken" =: _lamaeNextToken,
        "actionStatus" =: _lamaeActionStatus,
        "maxResults" =: _lamaeMaxResults,
        "taskId" =: _lamaeTaskId,
        "findingId" =: _lamaeFindingId
      ]

-- | /See:/ 'listAuditMitigationActionsExecutionsResponse' smart constructor.
data ListAuditMitigationActionsExecutionsResponse = ListAuditMitigationActionsExecutionsResponse'
  { _lamaersActionsExecutions ::
      !( Maybe
           [AuditMitigationActionExecutionMetadata]
       ),
    _lamaersNextToken ::
      !( Maybe
           Text
       ),
    _lamaersResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListAuditMitigationActionsExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lamaersActionsExecutions' - A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
--
-- * 'lamaersNextToken' - The token for the next set of results.
--
-- * 'lamaersResponseStatus' - -- | The response status code.
listAuditMitigationActionsExecutionsResponse ::
  -- | 'lamaersResponseStatus'
  Int ->
  ListAuditMitigationActionsExecutionsResponse
listAuditMitigationActionsExecutionsResponse pResponseStatus_ =
  ListAuditMitigationActionsExecutionsResponse'
    { _lamaersActionsExecutions =
        Nothing,
      _lamaersNextToken = Nothing,
      _lamaersResponseStatus = pResponseStatus_
    }

-- | A set of task execution results based on the input parameters. Details include the mitigation action applied, start time, and task status.
lamaersActionsExecutions :: Lens' ListAuditMitigationActionsExecutionsResponse [AuditMitigationActionExecutionMetadata]
lamaersActionsExecutions = lens _lamaersActionsExecutions (\s a -> s {_lamaersActionsExecutions = a}) . _Default . _Coerce

-- | The token for the next set of results.
lamaersNextToken :: Lens' ListAuditMitigationActionsExecutionsResponse (Maybe Text)
lamaersNextToken = lens _lamaersNextToken (\s a -> s {_lamaersNextToken = a})

-- | -- | The response status code.
lamaersResponseStatus :: Lens' ListAuditMitigationActionsExecutionsResponse Int
lamaersResponseStatus = lens _lamaersResponseStatus (\s a -> s {_lamaersResponseStatus = a})

instance NFData ListAuditMitigationActionsExecutionsResponse
