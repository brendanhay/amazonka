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
-- Module      : Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of individual assessments based on filter settings.
--
--
-- These filter settings can specify a combination of premigration assessment runs, migration tasks, and assessment status values.
module Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
  ( -- * Creating a Request
    describeReplicationTaskIndividualAssessments,
    DescribeReplicationTaskIndividualAssessments,

    -- * Request Lenses
    drtiaFilters,
    drtiaMarker,
    drtiaMaxRecords,

    -- * Destructuring the Response
    describeReplicationTaskIndividualAssessmentsResponse,
    DescribeReplicationTaskIndividualAssessmentsResponse,

    -- * Response Lenses
    drtiarsReplicationTaskIndividualAssessments,
    drtiarsMarker,
    drtiarsResponseStatus,
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
-- /See:/ 'describeReplicationTaskIndividualAssessments' smart constructor.
data DescribeReplicationTaskIndividualAssessments = DescribeReplicationTaskIndividualAssessments'
  { _drtiaFilters ::
      !( Maybe
           [Filter]
       ),
    _drtiaMarker ::
      !( Maybe
           Text
       ),
    _drtiaMaxRecords ::
      !( Maybe
           Int
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeReplicationTaskIndividualAssessments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtiaFilters' - Filters applied to the individual assessments described in the form of key-value pairs. Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @status@
--
-- * 'drtiaMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drtiaMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
describeReplicationTaskIndividualAssessments ::
  DescribeReplicationTaskIndividualAssessments
describeReplicationTaskIndividualAssessments =
  DescribeReplicationTaskIndividualAssessments'
    { _drtiaFilters =
        Nothing,
      _drtiaMarker = Nothing,
      _drtiaMaxRecords = Nothing
    }

-- | Filters applied to the individual assessments described in the form of key-value pairs. Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @status@
drtiaFilters :: Lens' DescribeReplicationTaskIndividualAssessments [Filter]
drtiaFilters = lens _drtiaFilters (\s a -> s {_drtiaFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drtiaMarker :: Lens' DescribeReplicationTaskIndividualAssessments (Maybe Text)
drtiaMarker = lens _drtiaMarker (\s a -> s {_drtiaMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
drtiaMaxRecords :: Lens' DescribeReplicationTaskIndividualAssessments (Maybe Int)
drtiaMaxRecords = lens _drtiaMaxRecords (\s a -> s {_drtiaMaxRecords = a})

instance AWSRequest DescribeReplicationTaskIndividualAssessments where
  type
    Rs DescribeReplicationTaskIndividualAssessments =
      DescribeReplicationTaskIndividualAssessmentsResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          DescribeReplicationTaskIndividualAssessmentsResponse'
            <$> (x .?> "ReplicationTaskIndividualAssessments" .!@ mempty)
            <*> (x .?> "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReplicationTaskIndividualAssessments

instance NFData DescribeReplicationTaskIndividualAssessments

instance ToHeaders DescribeReplicationTaskIndividualAssessments where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonDMSv20160101.DescribeReplicationTaskIndividualAssessments" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeReplicationTaskIndividualAssessments where
  toJSON DescribeReplicationTaskIndividualAssessments' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _drtiaFilters,
            ("Marker" .=) <$> _drtiaMarker,
            ("MaxRecords" .=) <$> _drtiaMaxRecords
          ]
      )

instance ToPath DescribeReplicationTaskIndividualAssessments where
  toPath = const "/"

instance ToQuery DescribeReplicationTaskIndividualAssessments where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeReplicationTaskIndividualAssessmentsResponse' smart constructor.
data DescribeReplicationTaskIndividualAssessmentsResponse = DescribeReplicationTaskIndividualAssessmentsResponse'
  { _drtiarsReplicationTaskIndividualAssessments ::
      !( Maybe
           [ReplicationTaskIndividualAssessment]
       ),
    _drtiarsMarker ::
      !( Maybe
           Text
       ),
    _drtiarsResponseStatus ::
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

-- | Creates a value of 'DescribeReplicationTaskIndividualAssessmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drtiarsReplicationTaskIndividualAssessments' - One or more individual assessments as specified by @Filters@ .
--
-- * 'drtiarsMarker' - A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- * 'drtiarsResponseStatus' - -- | The response status code.
describeReplicationTaskIndividualAssessmentsResponse ::
  -- | 'drtiarsResponseStatus'
  Int ->
  DescribeReplicationTaskIndividualAssessmentsResponse
describeReplicationTaskIndividualAssessmentsResponse
  pResponseStatus_ =
    DescribeReplicationTaskIndividualAssessmentsResponse'
      { _drtiarsReplicationTaskIndividualAssessments =
          Nothing,
        _drtiarsMarker = Nothing,
        _drtiarsResponseStatus = pResponseStatus_
      }

-- | One or more individual assessments as specified by @Filters@ .
drtiarsReplicationTaskIndividualAssessments :: Lens' DescribeReplicationTaskIndividualAssessmentsResponse [ReplicationTaskIndividualAssessment]
drtiarsReplicationTaskIndividualAssessments = lens _drtiarsReplicationTaskIndividualAssessments (\s a -> s {_drtiarsReplicationTaskIndividualAssessments = a}) . _Default . _Coerce

-- | A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
drtiarsMarker :: Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Maybe Text)
drtiarsMarker = lens _drtiarsMarker (\s a -> s {_drtiarsMarker = a})

-- | -- | The response status code.
drtiarsResponseStatus :: Lens' DescribeReplicationTaskIndividualAssessmentsResponse Int
drtiarsResponseStatus = lens _drtiarsResponseStatus (\s a -> s {_drtiarsResponseStatus = a})

instance
  NFData
    DescribeReplicationTaskIndividualAssessmentsResponse
