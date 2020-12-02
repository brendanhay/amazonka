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
-- Module      : Network.AWS.Discovery.DescribeImportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of import tasks for your account, including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
module Network.AWS.Discovery.DescribeImportTasks
  ( -- * Creating a Request
    describeImportTasks,
    DescribeImportTasks,

    -- * Request Lenses
    ditFilters,
    ditNextToken,
    ditMaxResults,

    -- * Destructuring the Response
    describeImportTasksResponse,
    DescribeImportTasksResponse,

    -- * Response Lenses
    ditrsTasks,
    ditrsNextToken,
    ditrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImportTasks' smart constructor.
data DescribeImportTasks = DescribeImportTasks'
  { _ditFilters ::
      !(Maybe [ImportTaskFilter]),
    _ditNextToken :: !(Maybe Text),
    _ditMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImportTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ditFilters' - An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
--
-- * 'ditNextToken' - The token to request a specific page of results.
--
-- * 'ditMaxResults' - The maximum number of results that you want this request to return, up to 100.
describeImportTasks ::
  DescribeImportTasks
describeImportTasks =
  DescribeImportTasks'
    { _ditFilters = Nothing,
      _ditNextToken = Nothing,
      _ditMaxResults = Nothing
    }

-- | An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
ditFilters :: Lens' DescribeImportTasks [ImportTaskFilter]
ditFilters = lens _ditFilters (\s a -> s {_ditFilters = a}) . _Default . _Coerce

-- | The token to request a specific page of results.
ditNextToken :: Lens' DescribeImportTasks (Maybe Text)
ditNextToken = lens _ditNextToken (\s a -> s {_ditNextToken = a})

-- | The maximum number of results that you want this request to return, up to 100.
ditMaxResults :: Lens' DescribeImportTasks (Maybe Natural)
ditMaxResults = lens _ditMaxResults (\s a -> s {_ditMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeImportTasks where
  type Rs DescribeImportTasks = DescribeImportTasksResponse
  request = postJSON discovery
  response =
    receiveJSON
      ( \s h x ->
          DescribeImportTasksResponse'
            <$> (x .?> "tasks" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeImportTasks

instance NFData DescribeImportTasks

instance ToHeaders DescribeImportTasks where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSPoseidonService_V2015_11_01.DescribeImportTasks" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeImportTasks where
  toJSON DescribeImportTasks' {..} =
    object
      ( catMaybes
          [ ("filters" .=) <$> _ditFilters,
            ("nextToken" .=) <$> _ditNextToken,
            ("maxResults" .=) <$> _ditMaxResults
          ]
      )

instance ToPath DescribeImportTasks where
  toPath = const "/"

instance ToQuery DescribeImportTasks where
  toQuery = const mempty

-- | /See:/ 'describeImportTasksResponse' smart constructor.
data DescribeImportTasksResponse = DescribeImportTasksResponse'
  { _ditrsTasks ::
      !(Maybe [ImportTask]),
    _ditrsNextToken :: !(Maybe Text),
    _ditrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeImportTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ditrsTasks' - A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
--
-- * 'ditrsNextToken' - The token to request the next page of results.
--
-- * 'ditrsResponseStatus' - -- | The response status code.
describeImportTasksResponse ::
  -- | 'ditrsResponseStatus'
  Int ->
  DescribeImportTasksResponse
describeImportTasksResponse pResponseStatus_ =
  DescribeImportTasksResponse'
    { _ditrsTasks = Nothing,
      _ditrsNextToken = Nothing,
      _ditrsResponseStatus = pResponseStatus_
    }

-- | A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
ditrsTasks :: Lens' DescribeImportTasksResponse [ImportTask]
ditrsTasks = lens _ditrsTasks (\s a -> s {_ditrsTasks = a}) . _Default . _Coerce

-- | The token to request the next page of results.
ditrsNextToken :: Lens' DescribeImportTasksResponse (Maybe Text)
ditrsNextToken = lens _ditrsNextToken (\s a -> s {_ditrsNextToken = a})

-- | -- | The response status code.
ditrsResponseStatus :: Lens' DescribeImportTasksResponse Int
ditrsResponseStatus = lens _ditrsResponseStatus (\s a -> s {_ditrsResponseStatus = a})

instance NFData DescribeImportTasksResponse
