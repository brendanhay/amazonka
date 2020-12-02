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
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of premigration assessment runs based on filter settings.
--
--
-- These filter settings can specify a combination of premigration assessment runs, migration tasks, replication instances, and assessment run status values.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
  ( -- * Creating a Request
    describeReplicationTaskAssessmentRuns,
    DescribeReplicationTaskAssessmentRuns,

    -- * Request Lenses
    drtarFilters,
    drtarMarker,
    drtarMaxRecords,

    -- * Destructuring the Response
    describeReplicationTaskAssessmentRunsResponse,
    DescribeReplicationTaskAssessmentRunsResponse,

    -- * Response Lenses
    drtarsrsReplicationTaskAssessmentRuns,
    drtarsrsMarker,
    drtarsrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeReplicationTaskAssessmentRuns' smart constructor.
data DescribeReplicationTaskAssessmentRuns = DescribeReplicationTaskAssessmentRuns'
  { _drtarFilters ::
      !( Maybe
           [Filter]
       ),
    _drtarMarker ::
      !(Maybe Text),
    _drtarMaxRecords ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReplicationTaskAssessmentRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtarFilters' - Filters applied to the premigration assessment runs described in the form of key-value pairs. Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @replication-instance-arn@ , @status@
--
-- * 'drtarMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drtarMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
describeReplicationTaskAssessmentRuns ::
  DescribeReplicationTaskAssessmentRuns
describeReplicationTaskAssessmentRuns =
  DescribeReplicationTaskAssessmentRuns'
    { _drtarFilters = Nothing,
      _drtarMarker = Nothing,
      _drtarMaxRecords = Nothing
    }

-- | Filters applied to the premigration assessment runs described in the form of key-value pairs. Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @replication-instance-arn@ , @status@
drtarFilters :: Lens' DescribeReplicationTaskAssessmentRuns [Filter]
drtarFilters = lens _drtarFilters (\s a -> s {_drtarFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drtarMarker :: Lens' DescribeReplicationTaskAssessmentRuns (Maybe Text)
drtarMarker = lens _drtarMarker (\s a -> s {_drtarMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
drtarMaxRecords :: Lens' DescribeReplicationTaskAssessmentRuns (Maybe Int)
drtarMaxRecords = lens _drtarMaxRecords (\s a -> s {_drtarMaxRecords = a})

instance AWSRequest DescribeReplicationTaskAssessmentRuns where
  type
    Rs DescribeReplicationTaskAssessmentRuns =
      DescribeReplicationTaskAssessmentRunsResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentRunsResponse'
            <$> (x .?> "ReplicationTaskAssessmentRuns" .!@ mempty)
            <*> (x .?> "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReplicationTaskAssessmentRuns

instance NFData DescribeReplicationTaskAssessmentRuns

instance ToHeaders DescribeReplicationTaskAssessmentRuns where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentRuns" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeReplicationTaskAssessmentRuns where
  toJSON DescribeReplicationTaskAssessmentRuns' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _drtarFilters,
            ("Marker" .=) <$> _drtarMarker,
            ("MaxRecords" .=) <$> _drtarMaxRecords
          ]
      )

instance ToPath DescribeReplicationTaskAssessmentRuns where
  toPath = const "/"

instance ToQuery DescribeReplicationTaskAssessmentRuns where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeReplicationTaskAssessmentRunsResponse' smart constructor.
data DescribeReplicationTaskAssessmentRunsResponse = DescribeReplicationTaskAssessmentRunsResponse'
  { _drtarsrsReplicationTaskAssessmentRuns ::
      !( Maybe
           [ReplicationTaskAssessmentRun]
       ),
    _drtarsrsMarker ::
      !( Maybe
           Text
       ),
    _drtarsrsResponseStatus ::
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

-- | Creates a value of 'DescribeReplicationTaskAssessmentRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtarsrsReplicationTaskAssessmentRuns' - One or more premigration assessment runs as specified by @Filters@ .
--
-- * 'drtarsrsMarker' - A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- * 'drtarsrsResponseStatus' - -- | The response status code.
describeReplicationTaskAssessmentRunsResponse ::
  -- | 'drtarsrsResponseStatus'
  Int ->
  DescribeReplicationTaskAssessmentRunsResponse
describeReplicationTaskAssessmentRunsResponse pResponseStatus_ =
  DescribeReplicationTaskAssessmentRunsResponse'
    { _drtarsrsReplicationTaskAssessmentRuns =
        Nothing,
      _drtarsrsMarker = Nothing,
      _drtarsrsResponseStatus = pResponseStatus_
    }

-- | One or more premigration assessment runs as specified by @Filters@ .
drtarsrsReplicationTaskAssessmentRuns :: Lens' DescribeReplicationTaskAssessmentRunsResponse [ReplicationTaskAssessmentRun]
drtarsrsReplicationTaskAssessmentRuns = lens _drtarsrsReplicationTaskAssessmentRuns (\s a -> s {_drtarsrsReplicationTaskAssessmentRuns = a}) . _Default . _Coerce

-- | A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
drtarsrsMarker :: Lens' DescribeReplicationTaskAssessmentRunsResponse (Maybe Text)
drtarsrsMarker = lens _drtarsrsMarker (\s a -> s {_drtarsrsMarker = a})

-- | -- | The response status code.
drtarsrsResponseStatus :: Lens' DescribeReplicationTaskAssessmentRunsResponse Int
drtarsrsResponseStatus = lens _drtarsrsResponseStatus (\s a -> s {_drtarsrsResponseStatus = a})

instance NFData DescribeReplicationTaskAssessmentRunsResponse
