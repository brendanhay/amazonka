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
-- Module      : Network.AWS.Redshift.DescribeClusterDBRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterDbRevision@ objects.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterDBRevisions
  ( -- * Creating a Request
    describeClusterDBRevisions,
    DescribeClusterDBRevisions,

    -- * Request Lenses
    dcdrClusterIdentifier,
    dcdrMarker,
    dcdrMaxRecords,

    -- * Destructuring the Response
    describeClusterDBRevisionsResponse,
    DescribeClusterDBRevisionsResponse,

    -- * Response Lenses
    dcdrrsClusterDBRevisions,
    dcdrrsMarker,
    dcdrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClusterDBRevisions' smart constructor.
data DescribeClusterDBRevisions = DescribeClusterDBRevisions'
  { _dcdrClusterIdentifier ::
      !(Maybe Text),
    _dcdrMarker :: !(Maybe Text),
    _dcdrMaxRecords :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeClusterDBRevisions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdrClusterIdentifier' - A unique identifier for a cluster whose @ClusterDbRevisions@ you are requesting. This parameter is case sensitive. All clusters defined for an account are returned by default.
--
-- * 'dcdrMarker' - An optional parameter that specifies the starting point for returning a set of response records. When the results of a @DescribeClusterDbRevisions@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.  Constraints: You can specify either the @ClusterIdentifier@ parameter, or the @marker@ parameter, but not both.
--
-- * 'dcdrMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified MaxRecords value, a value is returned in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.  Default: 100 Constraints: minimum 20, maximum 100.
describeClusterDBRevisions ::
  DescribeClusterDBRevisions
describeClusterDBRevisions =
  DescribeClusterDBRevisions'
    { _dcdrClusterIdentifier = Nothing,
      _dcdrMarker = Nothing,
      _dcdrMaxRecords = Nothing
    }

-- | A unique identifier for a cluster whose @ClusterDbRevisions@ you are requesting. This parameter is case sensitive. All clusters defined for an account are returned by default.
dcdrClusterIdentifier :: Lens' DescribeClusterDBRevisions (Maybe Text)
dcdrClusterIdentifier = lens _dcdrClusterIdentifier (\s a -> s {_dcdrClusterIdentifier = a})

-- | An optional parameter that specifies the starting point for returning a set of response records. When the results of a @DescribeClusterDbRevisions@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.  Constraints: You can specify either the @ClusterIdentifier@ parameter, or the @marker@ parameter, but not both.
dcdrMarker :: Lens' DescribeClusterDBRevisions (Maybe Text)
dcdrMarker = lens _dcdrMarker (\s a -> s {_dcdrMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified MaxRecords value, a value is returned in the @marker@ field of the response. You can retrieve the next set of response records by providing the returned @marker@ value in the @marker@ parameter and retrying the request.  Default: 100 Constraints: minimum 20, maximum 100.
dcdrMaxRecords :: Lens' DescribeClusterDBRevisions (Maybe Int)
dcdrMaxRecords = lens _dcdrMaxRecords (\s a -> s {_dcdrMaxRecords = a})

instance AWSPager DescribeClusterDBRevisions where
  page rq rs
    | stop (rs ^. dcdrrsMarker) = Nothing
    | stop (rs ^. dcdrrsClusterDBRevisions) = Nothing
    | otherwise = Just $ rq & dcdrMarker .~ rs ^. dcdrrsMarker

instance AWSRequest DescribeClusterDBRevisions where
  type
    Rs DescribeClusterDBRevisions =
      DescribeClusterDBRevisionsResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "DescribeClusterDbRevisionsResult"
      ( \s h x ->
          DescribeClusterDBRevisionsResponse'
            <$> ( x .@? "ClusterDbRevisions" .!@ mempty
                    >>= may (parseXMLList "ClusterDbRevision")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeClusterDBRevisions

instance NFData DescribeClusterDBRevisions

instance ToHeaders DescribeClusterDBRevisions where
  toHeaders = const mempty

instance ToPath DescribeClusterDBRevisions where
  toPath = const "/"

instance ToQuery DescribeClusterDBRevisions where
  toQuery DescribeClusterDBRevisions' {..} =
    mconcat
      [ "Action" =: ("DescribeClusterDbRevisions" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "ClusterIdentifier" =: _dcdrClusterIdentifier,
        "Marker" =: _dcdrMarker,
        "MaxRecords" =: _dcdrMaxRecords
      ]

-- | /See:/ 'describeClusterDBRevisionsResponse' smart constructor.
data DescribeClusterDBRevisionsResponse = DescribeClusterDBRevisionsResponse'
  { _dcdrrsClusterDBRevisions ::
      !( Maybe
           [ClusterDBRevision]
       ),
    _dcdrrsMarker ::
      !(Maybe Text),
    _dcdrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeClusterDBRevisionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdrrsClusterDBRevisions' - A list of revisions.
--
-- * 'dcdrrsMarker' - A string representing the starting point for the next set of revisions. If a value is returned in a response, you can retrieve the next set of revisions by providing the value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all revisions have already been returned.
--
-- * 'dcdrrsResponseStatus' - -- | The response status code.
describeClusterDBRevisionsResponse ::
  -- | 'dcdrrsResponseStatus'
  Int ->
  DescribeClusterDBRevisionsResponse
describeClusterDBRevisionsResponse pResponseStatus_ =
  DescribeClusterDBRevisionsResponse'
    { _dcdrrsClusterDBRevisions =
        Nothing,
      _dcdrrsMarker = Nothing,
      _dcdrrsResponseStatus = pResponseStatus_
    }

-- | A list of revisions.
dcdrrsClusterDBRevisions :: Lens' DescribeClusterDBRevisionsResponse [ClusterDBRevision]
dcdrrsClusterDBRevisions = lens _dcdrrsClusterDBRevisions (\s a -> s {_dcdrrsClusterDBRevisions = a}) . _Default . _Coerce

-- | A string representing the starting point for the next set of revisions. If a value is returned in a response, you can retrieve the next set of revisions by providing the value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all revisions have already been returned.
dcdrrsMarker :: Lens' DescribeClusterDBRevisionsResponse (Maybe Text)
dcdrrsMarker = lens _dcdrrsMarker (\s a -> s {_dcdrrsMarker = a})

-- | -- | The response status code.
dcdrrsResponseStatus :: Lens' DescribeClusterDBRevisionsResponse Int
dcdrrsResponseStatus = lens _dcdrrsResponseStatus (\s a -> s {_dcdrrsResponseStatus = a})

instance NFData DescribeClusterDBRevisionsResponse
