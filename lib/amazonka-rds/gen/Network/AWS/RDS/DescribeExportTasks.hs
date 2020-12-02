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
-- Module      : Network.AWS.RDS.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a snapshot export to Amazon S3. This API operation supports pagination.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeExportTasks
  ( -- * Creating a Request
    describeExportTasks,
    DescribeExportTasks,

    -- * Request Lenses
    detSourceARN,
    detFilters,
    detMarker,
    detExportTaskIdentifier,
    detMaxRecords,

    -- * Destructuring the Response
    describeExportTasksResponse,
    DescribeExportTasksResponse,

    -- * Response Lenses
    detrsMarker,
    detrsExportTasks,
    detrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { _detSourceARN ::
      !(Maybe Text),
    _detFilters :: !(Maybe [Filter]),
    _detMarker :: !(Maybe Text),
    _detExportTaskIdentifier :: !(Maybe Text),
    _detMaxRecords :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detSourceARN' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- * 'detFilters' - Filters specify one or more snapshot exports to describe. The filters are specified as name-value pairs that define what to include in the output. Filter names and values are case-sensitive. Supported filters include the following:      * @export-task-identifier@ - An identifier for the snapshot export task.     * @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.     * @source-arn@ - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3     * @status@ - The status of the export task. Must be lowercase, for example, @complete@ .
--
-- * 'detMarker' - An optional pagination token provided by a previous @DescribeExportTasks@ request. If you specify this parameter, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
--
-- * 'detExportTaskIdentifier' - The identifier of the snapshot export task to be described.
--
-- * 'detMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified value, a pagination token called a marker is included in the response. You can use the marker in a later @DescribeExportTasks@ request to retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
describeExportTasks ::
  DescribeExportTasks
describeExportTasks =
  DescribeExportTasks'
    { _detSourceARN = Nothing,
      _detFilters = Nothing,
      _detMarker = Nothing,
      _detExportTaskIdentifier = Nothing,
      _detMaxRecords = Nothing
    }

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
detSourceARN :: Lens' DescribeExportTasks (Maybe Text)
detSourceARN = lens _detSourceARN (\s a -> s {_detSourceARN = a})

-- | Filters specify one or more snapshot exports to describe. The filters are specified as name-value pairs that define what to include in the output. Filter names and values are case-sensitive. Supported filters include the following:      * @export-task-identifier@ - An identifier for the snapshot export task.     * @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.     * @source-arn@ - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3     * @status@ - The status of the export task. Must be lowercase, for example, @complete@ .
detFilters :: Lens' DescribeExportTasks [Filter]
detFilters = lens _detFilters (\s a -> s {_detFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeExportTasks@ request. If you specify this parameter, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
detMarker :: Lens' DescribeExportTasks (Maybe Text)
detMarker = lens _detMarker (\s a -> s {_detMarker = a})

-- | The identifier of the snapshot export task to be described.
detExportTaskIdentifier :: Lens' DescribeExportTasks (Maybe Text)
detExportTaskIdentifier = lens _detExportTaskIdentifier (\s a -> s {_detExportTaskIdentifier = a})

-- | The maximum number of records to include in the response. If more records exist than the specified value, a pagination token called a marker is included in the response. You can use the marker in a later @DescribeExportTasks@ request to retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
detMaxRecords :: Lens' DescribeExportTasks (Maybe Natural)
detMaxRecords = lens _detMaxRecords (\s a -> s {_detMaxRecords = a}) . mapping _Nat

instance AWSPager DescribeExportTasks where
  page rq rs
    | stop (rs ^. detrsMarker) = Nothing
    | stop (rs ^. detrsExportTasks) = Nothing
    | otherwise = Just $ rq & detMarker .~ rs ^. detrsMarker

instance AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeExportTasksResult"
      ( \s h x ->
          DescribeExportTasksResponse'
            <$> (x .@? "Marker")
            <*> ( x .@? "ExportTasks" .!@ mempty
                    >>= may (parseXMLList "ExportTask")
                )
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
        "Version" =: ("2014-10-31" :: ByteString),
        "SourceArn" =: _detSourceARN,
        "Filters" =: toQuery (toQueryList "Filter" <$> _detFilters),
        "Marker" =: _detMarker,
        "ExportTaskIdentifier" =: _detExportTaskIdentifier,
        "MaxRecords" =: _detMaxRecords
      ]

-- | /See:/ 'describeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { _detrsMarker ::
      !(Maybe Text),
    _detrsExportTasks ::
      !(Maybe [ExportTask]),
    _detrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsMarker' - A pagination token that can be used in a later @DescribeExportTasks@ request. A marker is used for pagination to identify the location to begin output for the next response of @DescribeExportTasks@ .
--
-- * 'detrsExportTasks' - Information about an export of a snapshot to Amazon S3.
--
-- * 'detrsResponseStatus' - -- | The response status code.
describeExportTasksResponse ::
  -- | 'detrsResponseStatus'
  Int ->
  DescribeExportTasksResponse
describeExportTasksResponse pResponseStatus_ =
  DescribeExportTasksResponse'
    { _detrsMarker = Nothing,
      _detrsExportTasks = Nothing,
      _detrsResponseStatus = pResponseStatus_
    }

-- | A pagination token that can be used in a later @DescribeExportTasks@ request. A marker is used for pagination to identify the location to begin output for the next response of @DescribeExportTasks@ .
detrsMarker :: Lens' DescribeExportTasksResponse (Maybe Text)
detrsMarker = lens _detrsMarker (\s a -> s {_detrsMarker = a})

-- | Information about an export of a snapshot to Amazon S3.
detrsExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrsExportTasks = lens _detrsExportTasks (\s a -> s {_detrsExportTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
detrsResponseStatus :: Lens' DescribeExportTasksResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\s a -> s {_detrsResponseStatus = a})

instance NFData DescribeExportTasksResponse
