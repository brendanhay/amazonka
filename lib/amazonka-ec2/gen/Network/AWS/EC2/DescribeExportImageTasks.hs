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
-- Module      : Network.AWS.EC2.DescribeExportImageTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export image tasks or all of your export image tasks.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeExportImageTasks
  ( -- * Creating a Request
    describeExportImageTasks,
    DescribeExportImageTasks,

    -- * Request Lenses
    deitExportImageTaskIds,
    deitFilters,
    deitNextToken,
    deitDryRun,
    deitMaxResults,

    -- * Destructuring the Response
    describeExportImageTasksResponse,
    DescribeExportImageTasksResponse,

    -- * Response Lenses
    deitrsExportImageTasks,
    deitrsNextToken,
    deitrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeExportImageTasks' smart constructor.
data DescribeExportImageTasks = DescribeExportImageTasks'
  { _deitExportImageTaskIds ::
      !(Maybe [Text]),
    _deitFilters :: !(Maybe [Filter]),
    _deitNextToken :: !(Maybe Text),
    _deitDryRun :: !(Maybe Bool),
    _deitMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExportImageTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deitExportImageTaskIds' - The IDs of the export image tasks.
--
-- * 'deitFilters' - Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
--
-- * 'deitNextToken' - A token that indicates the next page of results.
--
-- * 'deitDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'deitMaxResults' - The maximum number of results to return in a single call.
describeExportImageTasks ::
  DescribeExportImageTasks
describeExportImageTasks =
  DescribeExportImageTasks'
    { _deitExportImageTaskIds = Nothing,
      _deitFilters = Nothing,
      _deitNextToken = Nothing,
      _deitDryRun = Nothing,
      _deitMaxResults = Nothing
    }

-- | The IDs of the export image tasks.
deitExportImageTaskIds :: Lens' DescribeExportImageTasks [Text]
deitExportImageTaskIds = lens _deitExportImageTaskIds (\s a -> s {_deitExportImageTaskIds = a}) . _Default . _Coerce

-- | Filter tasks using the @task-state@ filter and one of the following values: @active@ , @completed@ , @deleting@ , or @deleted@ .
deitFilters :: Lens' DescribeExportImageTasks [Filter]
deitFilters = lens _deitFilters (\s a -> s {_deitFilters = a}) . _Default . _Coerce

-- | A token that indicates the next page of results.
deitNextToken :: Lens' DescribeExportImageTasks (Maybe Text)
deitNextToken = lens _deitNextToken (\s a -> s {_deitNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deitDryRun :: Lens' DescribeExportImageTasks (Maybe Bool)
deitDryRun = lens _deitDryRun (\s a -> s {_deitDryRun = a})

-- | The maximum number of results to return in a single call.
deitMaxResults :: Lens' DescribeExportImageTasks (Maybe Natural)
deitMaxResults = lens _deitMaxResults (\s a -> s {_deitMaxResults = a}) . mapping _Nat

instance AWSPager DescribeExportImageTasks where
  page rq rs
    | stop (rs ^. deitrsNextToken) = Nothing
    | stop (rs ^. deitrsExportImageTasks) = Nothing
    | otherwise = Just $ rq & deitNextToken .~ rs ^. deitrsNextToken

instance AWSRequest DescribeExportImageTasks where
  type Rs DescribeExportImageTasks = DescribeExportImageTasksResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeExportImageTasksResponse'
            <$> ( x .@? "exportImageTaskSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeExportImageTasks

instance NFData DescribeExportImageTasks

instance ToHeaders DescribeExportImageTasks where
  toHeaders = const mempty

instance ToPath DescribeExportImageTasks where
  toPath = const "/"

instance ToQuery DescribeExportImageTasks where
  toQuery DescribeExportImageTasks' {..} =
    mconcat
      [ "Action" =: ("DescribeExportImageTasks" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          (toQueryList "ExportImageTaskId" <$> _deitExportImageTaskIds),
        toQuery (toQueryList "Filter" <$> _deitFilters),
        "NextToken" =: _deitNextToken,
        "DryRun" =: _deitDryRun,
        "MaxResults" =: _deitMaxResults
      ]

-- | /See:/ 'describeExportImageTasksResponse' smart constructor.
data DescribeExportImageTasksResponse = DescribeExportImageTasksResponse'
  { _deitrsExportImageTasks ::
      !( Maybe
           [ExportImageTask]
       ),
    _deitrsNextToken ::
      !(Maybe Text),
    _deitrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExportImageTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deitrsExportImageTasks' - Information about the export image tasks.
--
-- * 'deitrsNextToken' - The token to use to get the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'deitrsResponseStatus' - -- | The response status code.
describeExportImageTasksResponse ::
  -- | 'deitrsResponseStatus'
  Int ->
  DescribeExportImageTasksResponse
describeExportImageTasksResponse pResponseStatus_ =
  DescribeExportImageTasksResponse'
    { _deitrsExportImageTasks =
        Nothing,
      _deitrsNextToken = Nothing,
      _deitrsResponseStatus = pResponseStatus_
    }

-- | Information about the export image tasks.
deitrsExportImageTasks :: Lens' DescribeExportImageTasksResponse [ExportImageTask]
deitrsExportImageTasks = lens _deitrsExportImageTasks (\s a -> s {_deitrsExportImageTasks = a}) . _Default . _Coerce

-- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
deitrsNextToken :: Lens' DescribeExportImageTasksResponse (Maybe Text)
deitrsNextToken = lens _deitrsNextToken (\s a -> s {_deitrsNextToken = a})

-- | -- | The response status code.
deitrsResponseStatus :: Lens' DescribeExportImageTasksResponse Int
deitrsResponseStatus = lens _deitrsResponseStatus (\s a -> s {_deitrsResponseStatus = a})

instance NFData DescribeExportImageTasksResponse
