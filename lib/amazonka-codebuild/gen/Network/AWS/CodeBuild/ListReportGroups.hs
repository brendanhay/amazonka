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
-- Module      : Network.AWS.CodeBuild.ListReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list ARNs for the report groups in the current AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReportGroups
  ( -- * Creating a Request
    listReportGroups,
    ListReportGroups,

    -- * Request Lenses
    lrgSortOrder,
    lrgNextToken,
    lrgMaxResults,
    lrgSortBy,

    -- * Destructuring the Response
    listReportGroupsResponse,
    ListReportGroupsResponse,

    -- * Response Lenses
    lrgrsNextToken,
    lrgrsReportGroups,
    lrgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listReportGroups' smart constructor.
data ListReportGroups = ListReportGroups'
  { _lrgSortOrder ::
      !(Maybe SortOrderType),
    _lrgNextToken :: !(Maybe Text),
    _lrgMaxResults :: !(Maybe Nat),
    _lrgSortBy :: !(Maybe ReportGroupSortByType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReportGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrgSortOrder' - Used to specify the order to sort the list of returned report groups. Valid values are @ASCENDING@ and @DESCENDING@ .
--
-- * 'lrgNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lrgMaxResults' - The maximum number of paginated report groups returned per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
--
-- * 'lrgSortBy' - The criterion to be used to list build report groups. Valid values include:      * @CREATED_TIME@ : List based on when each report group was created.     * @LAST_MODIFIED_TIME@ : List based on when each report group was last changed.     * @NAME@ : List based on each report group's name.
listReportGroups ::
  ListReportGroups
listReportGroups =
  ListReportGroups'
    { _lrgSortOrder = Nothing,
      _lrgNextToken = Nothing,
      _lrgMaxResults = Nothing,
      _lrgSortBy = Nothing
    }

-- | Used to specify the order to sort the list of returned report groups. Valid values are @ASCENDING@ and @DESCENDING@ .
lrgSortOrder :: Lens' ListReportGroups (Maybe SortOrderType)
lrgSortOrder = lens _lrgSortOrder (\s a -> s {_lrgSortOrder = a})

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lrgNextToken :: Lens' ListReportGroups (Maybe Text)
lrgNextToken = lens _lrgNextToken (\s a -> s {_lrgNextToken = a})

-- | The maximum number of paginated report groups returned per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
lrgMaxResults :: Lens' ListReportGroups (Maybe Natural)
lrgMaxResults = lens _lrgMaxResults (\s a -> s {_lrgMaxResults = a}) . mapping _Nat

-- | The criterion to be used to list build report groups. Valid values include:      * @CREATED_TIME@ : List based on when each report group was created.     * @LAST_MODIFIED_TIME@ : List based on when each report group was last changed.     * @NAME@ : List based on each report group's name.
lrgSortBy :: Lens' ListReportGroups (Maybe ReportGroupSortByType)
lrgSortBy = lens _lrgSortBy (\s a -> s {_lrgSortBy = a})

instance AWSPager ListReportGroups where
  page rq rs
    | stop (rs ^. lrgrsNextToken) = Nothing
    | stop (rs ^. lrgrsReportGroups) = Nothing
    | otherwise = Just $ rq & lrgNextToken .~ rs ^. lrgrsNextToken

instance AWSRequest ListReportGroups where
  type Rs ListReportGroups = ListReportGroupsResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          ListReportGroupsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "reportGroups")
            <*> (pure (fromEnum s))
      )

instance Hashable ListReportGroups

instance NFData ListReportGroups

instance ToHeaders ListReportGroups where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.ListReportGroups" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListReportGroups where
  toJSON ListReportGroups' {..} =
    object
      ( catMaybes
          [ ("sortOrder" .=) <$> _lrgSortOrder,
            ("nextToken" .=) <$> _lrgNextToken,
            ("maxResults" .=) <$> _lrgMaxResults,
            ("sortBy" .=) <$> _lrgSortBy
          ]
      )

instance ToPath ListReportGroups where
  toPath = const "/"

instance ToQuery ListReportGroups where
  toQuery = const mempty

-- | /See:/ 'listReportGroupsResponse' smart constructor.
data ListReportGroupsResponse = ListReportGroupsResponse'
  { _lrgrsNextToken ::
      !(Maybe Text),
    _lrgrsReportGroups ::
      !(Maybe (List1 Text)),
    _lrgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReportGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrgrsNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lrgrsReportGroups' - The list of ARNs for the report groups in the current AWS account.
--
-- * 'lrgrsResponseStatus' - -- | The response status code.
listReportGroupsResponse ::
  -- | 'lrgrsResponseStatus'
  Int ->
  ListReportGroupsResponse
listReportGroupsResponse pResponseStatus_ =
  ListReportGroupsResponse'
    { _lrgrsNextToken = Nothing,
      _lrgrsReportGroups = Nothing,
      _lrgrsResponseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lrgrsNextToken :: Lens' ListReportGroupsResponse (Maybe Text)
lrgrsNextToken = lens _lrgrsNextToken (\s a -> s {_lrgrsNextToken = a})

-- | The list of ARNs for the report groups in the current AWS account.
lrgrsReportGroups :: Lens' ListReportGroupsResponse (Maybe (NonEmpty Text))
lrgrsReportGroups = lens _lrgrsReportGroups (\s a -> s {_lrgrsReportGroups = a}) . mapping _List1

-- | -- | The response status code.
lrgrsResponseStatus :: Lens' ListReportGroupsResponse Int
lrgrsResponseStatus = lens _lrgrsResponseStatus (\s a -> s {_lrgrsResponseStatus = a})

instance NFData ListReportGroupsResponse
