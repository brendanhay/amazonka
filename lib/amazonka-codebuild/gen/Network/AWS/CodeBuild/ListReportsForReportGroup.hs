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
-- Module      : Network.AWS.CodeBuild.ListReportsForReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs for the reports that belong to a @ReportGroup@ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReportsForReportGroup
  ( -- * Creating a Request
    listReportsForReportGroup,
    ListReportsForReportGroup,

    -- * Request Lenses
    lrfrgSortOrder,
    lrfrgNextToken,
    lrfrgFilter,
    lrfrgMaxResults,
    lrfrgReportGroupARN,

    -- * Destructuring the Response
    listReportsForReportGroupResponse,
    ListReportsForReportGroupResponse,

    -- * Response Lenses
    lrfrgrsReports,
    lrfrgrsNextToken,
    lrfrgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listReportsForReportGroup' smart constructor.
data ListReportsForReportGroup = ListReportsForReportGroup'
  { _lrfrgSortOrder ::
      !(Maybe SortOrderType),
    _lrfrgNextToken :: !(Maybe Text),
    _lrfrgFilter :: !(Maybe ReportFilter),
    _lrfrgMaxResults :: !(Maybe Nat),
    _lrfrgReportGroupARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReportsForReportGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfrgSortOrder' - Use to specify whether the results are returned in ascending or descending order.
--
-- * 'lrfrgNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lrfrgFilter' - A @ReportFilter@ object used to filter the returned reports.
--
-- * 'lrfrgMaxResults' - The maximum number of paginated reports in this report group returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100.
--
-- * 'lrfrgReportGroupARN' - The ARN of the report group for which you want to return report ARNs.
listReportsForReportGroup ::
  -- | 'lrfrgReportGroupARN'
  Text ->
  ListReportsForReportGroup
listReportsForReportGroup pReportGroupARN_ =
  ListReportsForReportGroup'
    { _lrfrgSortOrder = Nothing,
      _lrfrgNextToken = Nothing,
      _lrfrgFilter = Nothing,
      _lrfrgMaxResults = Nothing,
      _lrfrgReportGroupARN = pReportGroupARN_
    }

-- | Use to specify whether the results are returned in ascending or descending order.
lrfrgSortOrder :: Lens' ListReportsForReportGroup (Maybe SortOrderType)
lrfrgSortOrder = lens _lrfrgSortOrder (\s a -> s {_lrfrgSortOrder = a})

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lrfrgNextToken :: Lens' ListReportsForReportGroup (Maybe Text)
lrfrgNextToken = lens _lrfrgNextToken (\s a -> s {_lrfrgNextToken = a})

-- | A @ReportFilter@ object used to filter the returned reports.
lrfrgFilter :: Lens' ListReportsForReportGroup (Maybe ReportFilter)
lrfrgFilter = lens _lrfrgFilter (\s a -> s {_lrfrgFilter = a})

-- | The maximum number of paginated reports in this report group returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100.
lrfrgMaxResults :: Lens' ListReportsForReportGroup (Maybe Natural)
lrfrgMaxResults = lens _lrfrgMaxResults (\s a -> s {_lrfrgMaxResults = a}) . mapping _Nat

-- | The ARN of the report group for which you want to return report ARNs.
lrfrgReportGroupARN :: Lens' ListReportsForReportGroup Text
lrfrgReportGroupARN = lens _lrfrgReportGroupARN (\s a -> s {_lrfrgReportGroupARN = a})

instance AWSPager ListReportsForReportGroup where
  page rq rs
    | stop (rs ^. lrfrgrsNextToken) = Nothing
    | stop (rs ^. lrfrgrsReports) = Nothing
    | otherwise = Just $ rq & lrfrgNextToken .~ rs ^. lrfrgrsNextToken

instance AWSRequest ListReportsForReportGroup where
  type
    Rs ListReportsForReportGroup =
      ListReportsForReportGroupResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          ListReportsForReportGroupResponse'
            <$> (x .?> "reports") <*> (x .?> "nextToken") <*> (pure (fromEnum s))
      )

instance Hashable ListReportsForReportGroup

instance NFData ListReportsForReportGroup

instance ToHeaders ListReportsForReportGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.ListReportsForReportGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListReportsForReportGroup where
  toJSON ListReportsForReportGroup' {..} =
    object
      ( catMaybes
          [ ("sortOrder" .=) <$> _lrfrgSortOrder,
            ("nextToken" .=) <$> _lrfrgNextToken,
            ("filter" .=) <$> _lrfrgFilter,
            ("maxResults" .=) <$> _lrfrgMaxResults,
            Just ("reportGroupArn" .= _lrfrgReportGroupARN)
          ]
      )

instance ToPath ListReportsForReportGroup where
  toPath = const "/"

instance ToQuery ListReportsForReportGroup where
  toQuery = const mempty

-- | /See:/ 'listReportsForReportGroupResponse' smart constructor.
data ListReportsForReportGroupResponse = ListReportsForReportGroupResponse'
  { _lrfrgrsReports ::
      !(Maybe (List1 Text)),
    _lrfrgrsNextToken ::
      !(Maybe Text),
    _lrfrgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReportsForReportGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfrgrsReports' - The list of report ARNs.
--
-- * 'lrfrgrsNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lrfrgrsResponseStatus' - -- | The response status code.
listReportsForReportGroupResponse ::
  -- | 'lrfrgrsResponseStatus'
  Int ->
  ListReportsForReportGroupResponse
listReportsForReportGroupResponse pResponseStatus_ =
  ListReportsForReportGroupResponse'
    { _lrfrgrsReports = Nothing,
      _lrfrgrsNextToken = Nothing,
      _lrfrgrsResponseStatus = pResponseStatus_
    }

-- | The list of report ARNs.
lrfrgrsReports :: Lens' ListReportsForReportGroupResponse (Maybe (NonEmpty Text))
lrfrgrsReports = lens _lrfrgrsReports (\s a -> s {_lrfrgrsReports = a}) . mapping _List1

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lrfrgrsNextToken :: Lens' ListReportsForReportGroupResponse (Maybe Text)
lrfrgrsNextToken = lens _lrfrgrsNextToken (\s a -> s {_lrfrgrsNextToken = a})

-- | -- | The response status code.
lrfrgrsResponseStatus :: Lens' ListReportsForReportGroupResponse Int
lrfrgrsResponseStatus = lens _lrfrgrsResponseStatus (\s a -> s {_lrfrgrsResponseStatus = a})

instance NFData ListReportsForReportGroupResponse
