{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified users. You can describe all users or filter the results (for example, by status or organization).
--
--
-- By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.
--
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeUsers
    (
    -- * Creating a Request
      describeUsers
    , DescribeUsers
    -- * Request Lenses
    , duInclude
    , duUserIds
    , duAuthenticationToken
    , duSort
    , duMarker
    , duQuery
    , duLimit
    , duOrder
    , duOrganizationId
    , duFields

    -- * Destructuring the Response
    , describeUsersResponse
    , DescribeUsersResponse
    -- * Response Lenses
    , dursUsers
    , dursTotalNumberOfUsers
    , dursMarker
    , dursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { _duInclude             :: !(Maybe UserFilterType)
  , _duUserIds             :: !(Maybe Text)
  , _duAuthenticationToken :: !(Maybe (Sensitive Text))
  , _duSort                :: !(Maybe UserSortType)
  , _duMarker              :: !(Maybe Text)
  , _duQuery               :: !(Maybe (Sensitive Text))
  , _duLimit               :: !(Maybe Nat)
  , _duOrder               :: !(Maybe OrderType)
  , _duOrganizationId      :: !(Maybe Text)
  , _duFields              :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duInclude' - The state of the users. Specify "ALL" to include inactive users.
--
-- * 'duUserIds' - The IDs of the users.
--
-- * 'duAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'duSort' - The sorting criteria.
--
-- * 'duMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'duQuery' - A query to filter users by user name.
--
-- * 'duLimit' - The maximum number of items to return.
--
-- * 'duOrder' - The order for the results.
--
-- * 'duOrganizationId' - The ID of the organization.
--
-- * 'duFields' - A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
describeUsers
    :: DescribeUsers
describeUsers =
  DescribeUsers'
    { _duInclude = Nothing
    , _duUserIds = Nothing
    , _duAuthenticationToken = Nothing
    , _duSort = Nothing
    , _duMarker = Nothing
    , _duQuery = Nothing
    , _duLimit = Nothing
    , _duOrder = Nothing
    , _duOrganizationId = Nothing
    , _duFields = Nothing
    }


-- | The state of the users. Specify "ALL" to include inactive users.
duInclude :: Lens' DescribeUsers (Maybe UserFilterType)
duInclude = lens _duInclude (\ s a -> s{_duInclude = a})

-- | The IDs of the users.
duUserIds :: Lens' DescribeUsers (Maybe Text)
duUserIds = lens _duUserIds (\ s a -> s{_duUserIds = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
duAuthenticationToken :: Lens' DescribeUsers (Maybe Text)
duAuthenticationToken = lens _duAuthenticationToken (\ s a -> s{_duAuthenticationToken = a}) . mapping _Sensitive

-- | The sorting criteria.
duSort :: Lens' DescribeUsers (Maybe UserSortType)
duSort = lens _duSort (\ s a -> s{_duSort = a})

-- | The marker for the next set of results. (You received this marker from a previous call.)
duMarker :: Lens' DescribeUsers (Maybe Text)
duMarker = lens _duMarker (\ s a -> s{_duMarker = a})

-- | A query to filter users by user name.
duQuery :: Lens' DescribeUsers (Maybe Text)
duQuery = lens _duQuery (\ s a -> s{_duQuery = a}) . mapping _Sensitive

-- | The maximum number of items to return.
duLimit :: Lens' DescribeUsers (Maybe Natural)
duLimit = lens _duLimit (\ s a -> s{_duLimit = a}) . mapping _Nat

-- | The order for the results.
duOrder :: Lens' DescribeUsers (Maybe OrderType)
duOrder = lens _duOrder (\ s a -> s{_duOrder = a})

-- | The ID of the organization.
duOrganizationId :: Lens' DescribeUsers (Maybe Text)
duOrganizationId = lens _duOrganizationId (\ s a -> s{_duOrganizationId = a})

-- | A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
duFields :: Lens' DescribeUsers (Maybe Text)
duFields = lens _duFields (\ s a -> s{_duFields = a})

instance AWSPager DescribeUsers where
        page rq rs
          | stop (rs ^. dursMarker) = Nothing
          | stop (rs ^. dursUsers) = Nothing
          | otherwise =
            Just $ rq & duMarker .~ rs ^. dursMarker

instance AWSRequest DescribeUsers where
        type Rs DescribeUsers = DescribeUsersResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUsersResponse' <$>
                   (x .?> "Users" .!@ mempty) <*>
                     (x .?> "TotalNumberOfUsers")
                     <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeUsers where

instance NFData DescribeUsers where

instance ToHeaders DescribeUsers where
        toHeaders DescribeUsers'{..}
          = mconcat
              ["Authentication" =# _duAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeUsers where
        toPath = const "/api/v1/users"

instance ToQuery DescribeUsers where
        toQuery DescribeUsers'{..}
          = mconcat
              ["include" =: _duInclude, "userIds" =: _duUserIds,
               "sort" =: _duSort, "marker" =: _duMarker,
               "query" =: _duQuery, "limit" =: _duLimit,
               "order" =: _duOrder,
               "organizationId" =: _duOrganizationId,
               "fields" =: _duFields]

-- | /See:/ 'describeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { _dursUsers              :: !(Maybe [User])
  , _dursTotalNumberOfUsers :: !(Maybe Integer)
  , _dursMarker             :: !(Maybe Text)
  , _dursResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursUsers' - The users.
--
-- * 'dursTotalNumberOfUsers' - The total number of users included in the results.
--
-- * 'dursMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dursResponseStatus' - -- | The response status code.
describeUsersResponse
    :: Int -- ^ 'dursResponseStatus'
    -> DescribeUsersResponse
describeUsersResponse pResponseStatus_ =
  DescribeUsersResponse'
    { _dursUsers = Nothing
    , _dursTotalNumberOfUsers = Nothing
    , _dursMarker = Nothing
    , _dursResponseStatus = pResponseStatus_
    }


-- | The users.
dursUsers :: Lens' DescribeUsersResponse [User]
dursUsers = lens _dursUsers (\ s a -> s{_dursUsers = a}) . _Default . _Coerce

-- | The total number of users included in the results.
dursTotalNumberOfUsers :: Lens' DescribeUsersResponse (Maybe Integer)
dursTotalNumberOfUsers = lens _dursTotalNumberOfUsers (\ s a -> s{_dursTotalNumberOfUsers = a})

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dursMarker :: Lens' DescribeUsersResponse (Maybe Text)
dursMarker = lens _dursMarker (\ s a -> s{_dursMarker = a})

-- | -- | The response status code.
dursResponseStatus :: Lens' DescribeUsersResponse Int
dursResponseStatus = lens _dursResponseStatus (\ s a -> s{_dursResponseStatus = a})

instance NFData DescribeUsersResponse where
