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
-- Module      : Network.AWS.ElastiCache.DescribeUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of users.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUsers
  ( -- * Creating a Request
    describeUsers,
    DescribeUsers,

    -- * Request Lenses
    duFilters,
    duEngine,
    duUserId,
    duMarker,
    duMaxRecords,

    -- * Destructuring the Response
    describeUsersResponse,
    DescribeUsersResponse,

    -- * Response Lenses
    dursUsers,
    dursMarker,
    dursResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { _duFilters ::
      !(Maybe [Filter]),
    _duEngine :: !(Maybe Text),
    _duUserId :: !(Maybe Text),
    _duMarker :: !(Maybe Text),
    _duMaxRecords :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duFilters' - Filter to determine the list of User IDs to return.
--
-- * 'duEngine' - The Redis engine.
--
-- * 'duUserId' - The ID of the user.
--
-- * 'duMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- * 'duMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
describeUsers ::
  DescribeUsers
describeUsers =
  DescribeUsers'
    { _duFilters = Nothing,
      _duEngine = Nothing,
      _duUserId = Nothing,
      _duMarker = Nothing,
      _duMaxRecords = Nothing
    }

-- | Filter to determine the list of User IDs to return.
duFilters :: Lens' DescribeUsers [Filter]
duFilters = lens _duFilters (\s a -> s {_duFilters = a}) . _Default . _Coerce

-- | The Redis engine.
duEngine :: Lens' DescribeUsers (Maybe Text)
duEngine = lens _duEngine (\s a -> s {_duEngine = a})

-- | The ID of the user.
duUserId :: Lens' DescribeUsers (Maybe Text)
duUserId = lens _duUserId (\s a -> s {_duUserId = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
duMarker :: Lens' DescribeUsers (Maybe Text)
duMarker = lens _duMarker (\s a -> s {_duMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
duMaxRecords :: Lens' DescribeUsers (Maybe Int)
duMaxRecords = lens _duMaxRecords (\s a -> s {_duMaxRecords = a})

instance AWSPager DescribeUsers where
  page rq rs
    | stop (rs ^. dursMarker) = Nothing
    | stop (rs ^. dursUsers) = Nothing
    | otherwise = Just $ rq & duMarker .~ rs ^. dursMarker

instance AWSRequest DescribeUsers where
  type Rs DescribeUsers = DescribeUsersResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DescribeUsersResult"
      ( \s h x ->
          DescribeUsersResponse'
            <$> (x .@? "Users" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeUsers

instance NFData DescribeUsers

instance ToHeaders DescribeUsers where
  toHeaders = const mempty

instance ToPath DescribeUsers where
  toPath = const "/"

instance ToQuery DescribeUsers where
  toQuery DescribeUsers' {..} =
    mconcat
      [ "Action" =: ("DescribeUsers" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "Filters" =: toQuery (toQueryList "member" <$> _duFilters),
        "Engine" =: _duEngine,
        "UserId" =: _duUserId,
        "Marker" =: _duMarker,
        "MaxRecords" =: _duMaxRecords
      ]

-- | /See:/ 'describeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { _dursUsers ::
      !(Maybe [User]),
    _dursMarker :: !(Maybe Text),
    _dursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursUsers' - A list of users.
--
-- * 'dursMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- * 'dursResponseStatus' - -- | The response status code.
describeUsersResponse ::
  -- | 'dursResponseStatus'
  Int ->
  DescribeUsersResponse
describeUsersResponse pResponseStatus_ =
  DescribeUsersResponse'
    { _dursUsers = Nothing,
      _dursMarker = Nothing,
      _dursResponseStatus = pResponseStatus_
    }

-- | A list of users.
dursUsers :: Lens' DescribeUsersResponse [User]
dursUsers = lens _dursUsers (\s a -> s {_dursUsers = a}) . _Default . _Coerce

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
dursMarker :: Lens' DescribeUsersResponse (Maybe Text)
dursMarker = lens _dursMarker (\s a -> s {_dursMarker = a})

-- | -- | The response status code.
dursResponseStatus :: Lens' DescribeUsersResponse Int
dursResponseStatus = lens _dursResponseStatus (\s a -> s {_dursResponseStatus = a})

instance NFData DescribeUsersResponse
