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
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the task assessment results from Amazon S3. This action always returns the latest results.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
  ( -- * Creating a Request
    describeReplicationTaskAssessmentResults,
    DescribeReplicationTaskAssessmentResults,

    -- * Request Lenses
    dReplicationTaskARN,
    dMarker,
    dMaxRecords,

    -- * Destructuring the Response
    describeReplicationTaskAssessmentResultsResponse,
    DescribeReplicationTaskAssessmentResultsResponse,

    -- * Response Lenses
    drtarrrsBucketName,
    drtarrrsMarker,
    drtarrrsReplicationTaskAssessmentResults,
    drtarrrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeReplicationTaskAssessmentResults' smart constructor.
data DescribeReplicationTaskAssessmentResults = DescribeReplicationTaskAssessmentResults'
  { _dReplicationTaskARN ::
      !( Maybe
           Text
       ),
    _dMarker ::
      !( Maybe
           Text
       ),
    _dMaxRecords ::
      !( Maybe
           Int
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReplicationTaskAssessmentResults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dReplicationTaskARN' - The Amazon Resource Name (ARN) string that uniquely identifies the task. When this input parameter is specified, the API returns only one result and ignore the values of the @MaxRecords@ and @Marker@ parameters.
--
-- * 'dMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeReplicationTaskAssessmentResults ::
  DescribeReplicationTaskAssessmentResults
describeReplicationTaskAssessmentResults =
  DescribeReplicationTaskAssessmentResults'
    { _dReplicationTaskARN =
        Nothing,
      _dMarker = Nothing,
      _dMaxRecords = Nothing
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the task. When this input parameter is specified, the API returns only one result and ignore the values of the @MaxRecords@ and @Marker@ parameters.
dReplicationTaskARN :: Lens' DescribeReplicationTaskAssessmentResults (Maybe Text)
dReplicationTaskARN = lens _dReplicationTaskARN (\s a -> s {_dReplicationTaskARN = a})

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dMarker :: Lens' DescribeReplicationTaskAssessmentResults (Maybe Text)
dMarker = lens _dMarker (\s a -> s {_dMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dMaxRecords :: Lens' DescribeReplicationTaskAssessmentResults (Maybe Int)
dMaxRecords = lens _dMaxRecords (\s a -> s {_dMaxRecords = a})

instance AWSPager DescribeReplicationTaskAssessmentResults where
  page rq rs
    | stop (rs ^. drtarrrsMarker) = Nothing
    | stop (rs ^. drtarrrsReplicationTaskAssessmentResults) = Nothing
    | otherwise = Just $ rq & dMarker .~ rs ^. drtarrrsMarker

instance AWSRequest DescribeReplicationTaskAssessmentResults where
  type
    Rs DescribeReplicationTaskAssessmentResults =
      DescribeReplicationTaskAssessmentResultsResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentResultsResponse'
            <$> (x .?> "BucketName")
            <*> (x .?> "Marker")
            <*> (x .?> "ReplicationTaskAssessmentResults" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReplicationTaskAssessmentResults

instance NFData DescribeReplicationTaskAssessmentResults

instance ToHeaders DescribeReplicationTaskAssessmentResults where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentResults" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeReplicationTaskAssessmentResults where
  toJSON DescribeReplicationTaskAssessmentResults' {..} =
    object
      ( catMaybes
          [ ("ReplicationTaskArn" .=) <$> _dReplicationTaskARN,
            ("Marker" .=) <$> _dMarker,
            ("MaxRecords" .=) <$> _dMaxRecords
          ]
      )

instance ToPath DescribeReplicationTaskAssessmentResults where
  toPath = const "/"

instance ToQuery DescribeReplicationTaskAssessmentResults where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeReplicationTaskAssessmentResultsResponse' smart constructor.
data DescribeReplicationTaskAssessmentResultsResponse = DescribeReplicationTaskAssessmentResultsResponse'
  { _drtarrrsBucketName ::
      !( Maybe
           Text
       ),
    _drtarrrsMarker ::
      !( Maybe
           Text
       ),
    _drtarrrsReplicationTaskAssessmentResults ::
      !( Maybe
           [ReplicationTaskAssessmentResult]
       ),
    _drtarrrsResponseStatus ::
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

-- | Creates a value of 'DescribeReplicationTaskAssessmentResultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtarrrsBucketName' - - The Amazon S3 bucket where the task assessment report is located.
--
-- * 'drtarrrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drtarrrsReplicationTaskAssessmentResults' - The task assessment report.
--
-- * 'drtarrrsResponseStatus' - -- | The response status code.
describeReplicationTaskAssessmentResultsResponse ::
  -- | 'drtarrrsResponseStatus'
  Int ->
  DescribeReplicationTaskAssessmentResultsResponse
describeReplicationTaskAssessmentResultsResponse pResponseStatus_ =
  DescribeReplicationTaskAssessmentResultsResponse'
    { _drtarrrsBucketName =
        Nothing,
      _drtarrrsMarker = Nothing,
      _drtarrrsReplicationTaskAssessmentResults =
        Nothing,
      _drtarrrsResponseStatus = pResponseStatus_
    }

-- | - The Amazon S3 bucket where the task assessment report is located.
drtarrrsBucketName :: Lens' DescribeReplicationTaskAssessmentResultsResponse (Maybe Text)
drtarrrsBucketName = lens _drtarrrsBucketName (\s a -> s {_drtarrrsBucketName = a})

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drtarrrsMarker :: Lens' DescribeReplicationTaskAssessmentResultsResponse (Maybe Text)
drtarrrsMarker = lens _drtarrrsMarker (\s a -> s {_drtarrrsMarker = a})

-- | The task assessment report.
drtarrrsReplicationTaskAssessmentResults :: Lens' DescribeReplicationTaskAssessmentResultsResponse [ReplicationTaskAssessmentResult]
drtarrrsReplicationTaskAssessmentResults = lens _drtarrrsReplicationTaskAssessmentResults (\s a -> s {_drtarrrsReplicationTaskAssessmentResults = a}) . _Default . _Coerce

-- | -- | The response status code.
drtarrrsResponseStatus :: Lens' DescribeReplicationTaskAssessmentResultsResponse Int
drtarrrsResponseStatus = lens _drtarrrsResponseStatus (\s a -> s {_drtarrrsResponseStatus = a})

instance NFData DescribeReplicationTaskAssessmentResultsResponse
