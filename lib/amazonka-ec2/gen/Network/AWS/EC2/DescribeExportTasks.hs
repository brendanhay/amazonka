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
-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export instance tasks or all of your export instance tasks.
module Network.AWS.EC2.DescribeExportTasks
  ( -- * Creating a Request
    describeExportTasks,
    DescribeExportTasks,

    -- * Request Lenses
    detFilters,
    detExportTaskIds,

    -- * Destructuring the Response
    describeExportTasksResponse,
    DescribeExportTasksResponse,

    -- * Response Lenses
    detrsExportTasks,
    detrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { _detFilters ::
      !(Maybe [Filter]),
    _detExportTaskIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detFilters' - the filters for the export tasks.
--
-- * 'detExportTaskIds' - The export task IDs.
describeExportTasks ::
  DescribeExportTasks
describeExportTasks =
  DescribeExportTasks'
    { _detFilters = Nothing,
      _detExportTaskIds = Nothing
    }

-- | the filters for the export tasks.
detFilters :: Lens' DescribeExportTasks [Filter]
detFilters = lens _detFilters (\s a -> s {_detFilters = a}) . _Default . _Coerce

-- | The export task IDs.
detExportTaskIds :: Lens' DescribeExportTasks [Text]
detExportTaskIds = lens _detExportTaskIds (\s a -> s {_detExportTaskIds = a}) . _Default . _Coerce

instance AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeExportTasksResponse'
            <$> (x .@? "exportTaskSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeExportTasks

instance NFData DescribeExportTasks

instance ToHeaders DescribeExportTasks where
  toHeaders = const mempty

instance ToPath DescribeExportTasks where
  toPath = const "/"

instance ToQuery DescribeExportTasks where
  toQuery DescribeExportTasks' {..} =
    mconcat
      [ "Action" =: ("DescribeExportTasks" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _detFilters),
        toQuery (toQueryList "ExportTaskId" <$> _detExportTaskIds)
      ]

-- | /See:/ 'describeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { _detrsExportTasks ::
      !(Maybe [ExportTask]),
    _detrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsExportTasks' - Information about the export tasks.
--
-- * 'detrsResponseStatus' - -- | The response status code.
describeExportTasksResponse ::
  -- | 'detrsResponseStatus'
  Int ->
  DescribeExportTasksResponse
describeExportTasksResponse pResponseStatus_ =
  DescribeExportTasksResponse'
    { _detrsExportTasks = Nothing,
      _detrsResponseStatus = pResponseStatus_
    }

-- | Information about the export tasks.
detrsExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrsExportTasks = lens _detrsExportTasks (\s a -> s {_detrsExportTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
detrsResponseStatus :: Lens' DescribeExportTasksResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\s a -> s {_detrsResponseStatus = a})

instance NFData DescribeExportTasksResponse
