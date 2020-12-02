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
-- Module      : Network.AWS.IoT.ListAuditFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the findings (results) of a Device Defender audit or of the audits performed during a specified time period. (Findings are retained for 90 days.)
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditFindings
  ( -- * Creating a Request
    listAuditFindings,
    ListAuditFindings,

    -- * Request Lenses
    lafStartTime,
    lafTaskId,
    lafCheckName,
    lafListSuppressedFindings,
    lafNextToken,
    lafEndTime,
    lafMaxResults,
    lafResourceIdentifier,

    -- * Destructuring the Response
    listAuditFindingsResponse,
    ListAuditFindingsResponse,

    -- * Response Lenses
    lafrsNextToken,
    lafrsFindings,
    lafrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAuditFindings' smart constructor.
data ListAuditFindings = ListAuditFindings'
  { _lafStartTime ::
      !(Maybe POSIX),
    _lafTaskId :: !(Maybe Text),
    _lafCheckName :: !(Maybe Text),
    _lafListSuppressedFindings :: !(Maybe Bool),
    _lafNextToken :: !(Maybe Text),
    _lafEndTime :: !(Maybe POSIX),
    _lafMaxResults :: !(Maybe Nat),
    _lafResourceIdentifier :: !(Maybe ResourceIdentifier)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lafStartTime' - A filter to limit results to those found after the specified time. You must specify either the startTime and endTime or the taskId, but not both.
--
-- * 'lafTaskId' - A filter to limit results to the audit with the specified ID. You must specify either the taskId or the startTime and endTime, but not both.
--
-- * 'lafCheckName' - A filter to limit results to the findings for the specified audit check.
--
-- * 'lafListSuppressedFindings' - Boolean flag indicating whether only the suppressed findings or the unsuppressed findings should be listed. If this parameter isn't provided, the response will list both suppressed and unsuppressed findings.
--
-- * 'lafNextToken' - The token for the next set of results.
--
-- * 'lafEndTime' - A filter to limit results to those found before the specified time. You must specify either the startTime and endTime or the taskId, but not both.
--
-- * 'lafMaxResults' - The maximum number of results to return at one time. The default is 25.
--
-- * 'lafResourceIdentifier' - Information identifying the noncompliant resource.
listAuditFindings ::
  ListAuditFindings
listAuditFindings =
  ListAuditFindings'
    { _lafStartTime = Nothing,
      _lafTaskId = Nothing,
      _lafCheckName = Nothing,
      _lafListSuppressedFindings = Nothing,
      _lafNextToken = Nothing,
      _lafEndTime = Nothing,
      _lafMaxResults = Nothing,
      _lafResourceIdentifier = Nothing
    }

-- | A filter to limit results to those found after the specified time. You must specify either the startTime and endTime or the taskId, but not both.
lafStartTime :: Lens' ListAuditFindings (Maybe UTCTime)
lafStartTime = lens _lafStartTime (\s a -> s {_lafStartTime = a}) . mapping _Time

-- | A filter to limit results to the audit with the specified ID. You must specify either the taskId or the startTime and endTime, but not both.
lafTaskId :: Lens' ListAuditFindings (Maybe Text)
lafTaskId = lens _lafTaskId (\s a -> s {_lafTaskId = a})

-- | A filter to limit results to the findings for the specified audit check.
lafCheckName :: Lens' ListAuditFindings (Maybe Text)
lafCheckName = lens _lafCheckName (\s a -> s {_lafCheckName = a})

-- | Boolean flag indicating whether only the suppressed findings or the unsuppressed findings should be listed. If this parameter isn't provided, the response will list both suppressed and unsuppressed findings.
lafListSuppressedFindings :: Lens' ListAuditFindings (Maybe Bool)
lafListSuppressedFindings = lens _lafListSuppressedFindings (\s a -> s {_lafListSuppressedFindings = a})

-- | The token for the next set of results.
lafNextToken :: Lens' ListAuditFindings (Maybe Text)
lafNextToken = lens _lafNextToken (\s a -> s {_lafNextToken = a})

-- | A filter to limit results to those found before the specified time. You must specify either the startTime and endTime or the taskId, but not both.
lafEndTime :: Lens' ListAuditFindings (Maybe UTCTime)
lafEndTime = lens _lafEndTime (\s a -> s {_lafEndTime = a}) . mapping _Time

-- | The maximum number of results to return at one time. The default is 25.
lafMaxResults :: Lens' ListAuditFindings (Maybe Natural)
lafMaxResults = lens _lafMaxResults (\s a -> s {_lafMaxResults = a}) . mapping _Nat

-- | Information identifying the noncompliant resource.
lafResourceIdentifier :: Lens' ListAuditFindings (Maybe ResourceIdentifier)
lafResourceIdentifier = lens _lafResourceIdentifier (\s a -> s {_lafResourceIdentifier = a})

instance AWSPager ListAuditFindings where
  page rq rs
    | stop (rs ^. lafrsNextToken) = Nothing
    | stop (rs ^. lafrsFindings) = Nothing
    | otherwise = Just $ rq & lafNextToken .~ rs ^. lafrsNextToken

instance AWSRequest ListAuditFindings where
  type Rs ListAuditFindings = ListAuditFindingsResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          ListAuditFindingsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "findings" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListAuditFindings

instance NFData ListAuditFindings

instance ToHeaders ListAuditFindings where
  toHeaders = const mempty

instance ToJSON ListAuditFindings where
  toJSON ListAuditFindings' {..} =
    object
      ( catMaybes
          [ ("startTime" .=) <$> _lafStartTime,
            ("taskId" .=) <$> _lafTaskId,
            ("checkName" .=) <$> _lafCheckName,
            ("listSuppressedFindings" .=) <$> _lafListSuppressedFindings,
            ("nextToken" .=) <$> _lafNextToken,
            ("endTime" .=) <$> _lafEndTime,
            ("maxResults" .=) <$> _lafMaxResults,
            ("resourceIdentifier" .=) <$> _lafResourceIdentifier
          ]
      )

instance ToPath ListAuditFindings where
  toPath = const "/audit/findings"

instance ToQuery ListAuditFindings where
  toQuery = const mempty

-- | /See:/ 'listAuditFindingsResponse' smart constructor.
data ListAuditFindingsResponse = ListAuditFindingsResponse'
  { _lafrsNextToken ::
      !(Maybe Text),
    _lafrsFindings ::
      !(Maybe [AuditFinding]),
    _lafrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lafrsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'lafrsFindings' - The findings (results) of the audit.
--
-- * 'lafrsResponseStatus' - -- | The response status code.
listAuditFindingsResponse ::
  -- | 'lafrsResponseStatus'
  Int ->
  ListAuditFindingsResponse
listAuditFindingsResponse pResponseStatus_ =
  ListAuditFindingsResponse'
    { _lafrsNextToken = Nothing,
      _lafrsFindings = Nothing,
      _lafrsResponseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
lafrsNextToken :: Lens' ListAuditFindingsResponse (Maybe Text)
lafrsNextToken = lens _lafrsNextToken (\s a -> s {_lafrsNextToken = a})

-- | The findings (results) of the audit.
lafrsFindings :: Lens' ListAuditFindingsResponse [AuditFinding]
lafrsFindings = lens _lafrsFindings (\s a -> s {_lafrsFindings = a}) . _Default . _Coerce

-- | -- | The response status code.
lafrsResponseStatus :: Lens' ListAuditFindingsResponse Int
lafrsResponseStatus = lens _lafrsResponseStatus (\s a -> s {_lafrsResponseStatus = a})

instance NFData ListAuditFindingsResponse
