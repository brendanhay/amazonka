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
-- Module      : Network.AWS.CodeBuild.ListSharedReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of report groups that are shared with other AWS accounts or users.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedReportGroups
  ( -- * Creating a Request
    listSharedReportGroups,
    ListSharedReportGroups,

    -- * Request Lenses
    lsrgSortOrder,
    lsrgNextToken,
    lsrgMaxResults,
    lsrgSortBy,

    -- * Destructuring the Response
    listSharedReportGroupsResponse,
    ListSharedReportGroupsResponse,

    -- * Response Lenses
    lsrgrsNextToken,
    lsrgrsReportGroups,
    lsrgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSharedReportGroups' smart constructor.
data ListSharedReportGroups = ListSharedReportGroups'
  { _lsrgSortOrder ::
      !(Maybe SortOrderType),
    _lsrgNextToken :: !(Maybe Text),
    _lsrgMaxResults :: !(Maybe Nat),
    _lsrgSortBy ::
      !(Maybe SharedResourceSortByType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSharedReportGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrgSortOrder' - The order in which to list shared report groups. Valid values include:     * @ASCENDING@ : List in ascending order.     * @DESCENDING@ : List in descending order.
--
-- * 'lsrgNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lsrgMaxResults' - The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
--
-- * 'lsrgSortBy' - The criterion to be used to list report groups shared with the current AWS account or user. Valid values include:      * @ARN@ : List based on the ARN.      * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed.
listSharedReportGroups ::
  ListSharedReportGroups
listSharedReportGroups =
  ListSharedReportGroups'
    { _lsrgSortOrder = Nothing,
      _lsrgNextToken = Nothing,
      _lsrgMaxResults = Nothing,
      _lsrgSortBy = Nothing
    }

-- | The order in which to list shared report groups. Valid values include:     * @ASCENDING@ : List in ascending order.     * @DESCENDING@ : List in descending order.
lsrgSortOrder :: Lens' ListSharedReportGroups (Maybe SortOrderType)
lsrgSortOrder = lens _lsrgSortOrder (\s a -> s {_lsrgSortOrder = a})

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lsrgNextToken :: Lens' ListSharedReportGroups (Maybe Text)
lsrgNextToken = lens _lsrgNextToken (\s a -> s {_lsrgNextToken = a})

-- | The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100.
lsrgMaxResults :: Lens' ListSharedReportGroups (Maybe Natural)
lsrgMaxResults = lens _lsrgMaxResults (\s a -> s {_lsrgMaxResults = a}) . mapping _Nat

-- | The criterion to be used to list report groups shared with the current AWS account or user. Valid values include:      * @ARN@ : List based on the ARN.      * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed.
lsrgSortBy :: Lens' ListSharedReportGroups (Maybe SharedResourceSortByType)
lsrgSortBy = lens _lsrgSortBy (\s a -> s {_lsrgSortBy = a})

instance AWSPager ListSharedReportGroups where
  page rq rs
    | stop (rs ^. lsrgrsNextToken) = Nothing
    | stop (rs ^. lsrgrsReportGroups) = Nothing
    | otherwise = Just $ rq & lsrgNextToken .~ rs ^. lsrgrsNextToken

instance AWSRequest ListSharedReportGroups where
  type Rs ListSharedReportGroups = ListSharedReportGroupsResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          ListSharedReportGroupsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "reportGroups")
            <*> (pure (fromEnum s))
      )

instance Hashable ListSharedReportGroups

instance NFData ListSharedReportGroups

instance ToHeaders ListSharedReportGroups where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.ListSharedReportGroups" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSharedReportGroups where
  toJSON ListSharedReportGroups' {..} =
    object
      ( catMaybes
          [ ("sortOrder" .=) <$> _lsrgSortOrder,
            ("nextToken" .=) <$> _lsrgNextToken,
            ("maxResults" .=) <$> _lsrgMaxResults,
            ("sortBy" .=) <$> _lsrgSortBy
          ]
      )

instance ToPath ListSharedReportGroups where
  toPath = const "/"

instance ToQuery ListSharedReportGroups where
  toQuery = const mempty

-- | /See:/ 'listSharedReportGroupsResponse' smart constructor.
data ListSharedReportGroupsResponse = ListSharedReportGroupsResponse'
  { _lsrgrsNextToken ::
      !(Maybe Text),
    _lsrgrsReportGroups ::
      !(Maybe (List1 Text)),
    _lsrgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSharedReportGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrgrsNextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- * 'lsrgrsReportGroups' - The list of ARNs for the report groups shared with the current AWS account or user.
--
-- * 'lsrgrsResponseStatus' - -- | The response status code.
listSharedReportGroupsResponse ::
  -- | 'lsrgrsResponseStatus'
  Int ->
  ListSharedReportGroupsResponse
listSharedReportGroupsResponse pResponseStatus_ =
  ListSharedReportGroupsResponse'
    { _lsrgrsNextToken = Nothing,
      _lsrgrsReportGroups = Nothing,
      _lsrgrsResponseStatus = pResponseStatus_
    }

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
lsrgrsNextToken :: Lens' ListSharedReportGroupsResponse (Maybe Text)
lsrgrsNextToken = lens _lsrgrsNextToken (\s a -> s {_lsrgrsNextToken = a})

-- | The list of ARNs for the report groups shared with the current AWS account or user.
lsrgrsReportGroups :: Lens' ListSharedReportGroupsResponse (Maybe (NonEmpty Text))
lsrgrsReportGroups = lens _lsrgrsReportGroups (\s a -> s {_lsrgrsReportGroups = a}) . mapping _List1

-- | -- | The response status code.
lsrgrsResponseStatus :: Lens' ListSharedReportGroupsResponse Int
lsrgrsResponseStatus = lens _lsrgrsResponseStatus (\s a -> s {_lsrgrsResponseStatus = a})

instance NFData ListSharedReportGroupsResponse
