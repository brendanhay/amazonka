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
-- Module      : Network.AWS.IoT.ListThingRegistrationTaskReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the thing registration tasks.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingRegistrationTaskReports
  ( -- * Creating a Request
    listThingRegistrationTaskReports,
    ListThingRegistrationTaskReports,

    -- * Request Lenses
    ltrtrNextToken,
    ltrtrMaxResults,
    ltrtrTaskId,
    ltrtrReportType,

    -- * Destructuring the Response
    listThingRegistrationTaskReportsResponse,
    ListThingRegistrationTaskReportsResponse,

    -- * Response Lenses
    ltrtrrsResourceLinks,
    ltrtrrsNextToken,
    ltrtrrsReportType,
    ltrtrrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThingRegistrationTaskReports' smart constructor.
data ListThingRegistrationTaskReports = ListThingRegistrationTaskReports'
  { _ltrtrNextToken ::
      !(Maybe Text),
    _ltrtrMaxResults ::
      !(Maybe Nat),
    _ltrtrTaskId :: !Text,
    _ltrtrReportType ::
      !ReportType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThingRegistrationTaskReports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrtrNextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- * 'ltrtrMaxResults' - The maximum number of results to return per request.
--
-- * 'ltrtrTaskId' - The id of the task.
--
-- * 'ltrtrReportType' - The type of task report.
listThingRegistrationTaskReports ::
  -- | 'ltrtrTaskId'
  Text ->
  -- | 'ltrtrReportType'
  ReportType ->
  ListThingRegistrationTaskReports
listThingRegistrationTaskReports pTaskId_ pReportType_ =
  ListThingRegistrationTaskReports'
    { _ltrtrNextToken = Nothing,
      _ltrtrMaxResults = Nothing,
      _ltrtrTaskId = pTaskId_,
      _ltrtrReportType = pReportType_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
ltrtrNextToken :: Lens' ListThingRegistrationTaskReports (Maybe Text)
ltrtrNextToken = lens _ltrtrNextToken (\s a -> s {_ltrtrNextToken = a})

-- | The maximum number of results to return per request.
ltrtrMaxResults :: Lens' ListThingRegistrationTaskReports (Maybe Natural)
ltrtrMaxResults = lens _ltrtrMaxResults (\s a -> s {_ltrtrMaxResults = a}) . mapping _Nat

-- | The id of the task.
ltrtrTaskId :: Lens' ListThingRegistrationTaskReports Text
ltrtrTaskId = lens _ltrtrTaskId (\s a -> s {_ltrtrTaskId = a})

-- | The type of task report.
ltrtrReportType :: Lens' ListThingRegistrationTaskReports ReportType
ltrtrReportType = lens _ltrtrReportType (\s a -> s {_ltrtrReportType = a})

instance AWSPager ListThingRegistrationTaskReports where
  page rq rs
    | stop (rs ^. ltrtrrsNextToken) = Nothing
    | stop (rs ^. ltrtrrsResourceLinks) = Nothing
    | otherwise = Just $ rq & ltrtrNextToken .~ rs ^. ltrtrrsNextToken

instance AWSRequest ListThingRegistrationTaskReports where
  type
    Rs ListThingRegistrationTaskReports =
      ListThingRegistrationTaskReportsResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListThingRegistrationTaskReportsResponse'
            <$> (x .?> "resourceLinks" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (x .?> "reportType")
            <*> (pure (fromEnum s))
      )

instance Hashable ListThingRegistrationTaskReports

instance NFData ListThingRegistrationTaskReports

instance ToHeaders ListThingRegistrationTaskReports where
  toHeaders = const mempty

instance ToPath ListThingRegistrationTaskReports where
  toPath ListThingRegistrationTaskReports' {..} =
    mconcat
      ["/thing-registration-tasks/", toBS _ltrtrTaskId, "/reports"]

instance ToQuery ListThingRegistrationTaskReports where
  toQuery ListThingRegistrationTaskReports' {..} =
    mconcat
      [ "nextToken" =: _ltrtrNextToken,
        "maxResults" =: _ltrtrMaxResults,
        "reportType" =: _ltrtrReportType
      ]

-- | /See:/ 'listThingRegistrationTaskReportsResponse' smart constructor.
data ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse'
  { _ltrtrrsResourceLinks ::
      !( Maybe
           [Text]
       ),
    _ltrtrrsNextToken ::
      !( Maybe
           Text
       ),
    _ltrtrrsReportType ::
      !( Maybe
           ReportType
       ),
    _ltrtrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThingRegistrationTaskReportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrtrrsResourceLinks' - Links to the task resources.
--
-- * 'ltrtrrsNextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- * 'ltrtrrsReportType' - The type of task report.
--
-- * 'ltrtrrsResponseStatus' - -- | The response status code.
listThingRegistrationTaskReportsResponse ::
  -- | 'ltrtrrsResponseStatus'
  Int ->
  ListThingRegistrationTaskReportsResponse
listThingRegistrationTaskReportsResponse pResponseStatus_ =
  ListThingRegistrationTaskReportsResponse'
    { _ltrtrrsResourceLinks =
        Nothing,
      _ltrtrrsNextToken = Nothing,
      _ltrtrrsReportType = Nothing,
      _ltrtrrsResponseStatus = pResponseStatus_
    }

-- | Links to the task resources.
ltrtrrsResourceLinks :: Lens' ListThingRegistrationTaskReportsResponse [Text]
ltrtrrsResourceLinks = lens _ltrtrrsResourceLinks (\s a -> s {_ltrtrrsResourceLinks = a}) . _Default . _Coerce

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
ltrtrrsNextToken :: Lens' ListThingRegistrationTaskReportsResponse (Maybe Text)
ltrtrrsNextToken = lens _ltrtrrsNextToken (\s a -> s {_ltrtrrsNextToken = a})

-- | The type of task report.
ltrtrrsReportType :: Lens' ListThingRegistrationTaskReportsResponse (Maybe ReportType)
ltrtrrsReportType = lens _ltrtrrsReportType (\s a -> s {_ltrtrrsReportType = a})

-- | -- | The response status code.
ltrtrrsResponseStatus :: Lens' ListThingRegistrationTaskReportsResponse Int
ltrtrrsResponseStatus = lens _ltrtrrsResponseStatus (\s a -> s {_ltrtrrsResponseStatus = a})

instance NFData ListThingRegistrationTaskReportsResponse
