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
-- Module      : Network.AWS.AlexaBusiness.SearchRooms
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches rooms and lists the ones that meet a set of filter and sort criteria.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchRooms
    (
    -- * Creating a Request
      searchRooms
    , SearchRooms
    -- * Request Lenses
    , srFilters
    , srSortCriteria
    , srNextToken
    , srMaxResults

    -- * Destructuring the Response
    , searchRoomsResponse
    , SearchRoomsResponse
    -- * Response Lenses
    , srrsRooms
    , srrsNextToken
    , srrsTotalCount
    , srrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchRooms' smart constructor.
data SearchRooms = SearchRooms'
  { _srFilters      :: !(Maybe [Filter])
  , _srSortCriteria :: !(Maybe [Sort])
  , _srNextToken    :: !(Maybe Text)
  , _srMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchRooms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srFilters' - The filters to use to list a specified set of rooms. The supported filter keys are RoomName and ProfileName.
--
-- * 'srSortCriteria' - The sort order to use in listing the specified set of rooms. The supported sort keys are RoomName and ProfileName.
--
-- * 'srNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'srMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
searchRooms
    :: SearchRooms
searchRooms =
  SearchRooms'
    { _srFilters = Nothing
    , _srSortCriteria = Nothing
    , _srNextToken = Nothing
    , _srMaxResults = Nothing
    }


-- | The filters to use to list a specified set of rooms. The supported filter keys are RoomName and ProfileName.
srFilters :: Lens' SearchRooms [Filter]
srFilters = lens _srFilters (\ s a -> s{_srFilters = a}) . _Default . _Coerce

-- | The sort order to use in listing the specified set of rooms. The supported sort keys are RoomName and ProfileName.
srSortCriteria :: Lens' SearchRooms [Sort]
srSortCriteria = lens _srSortCriteria (\ s a -> s{_srSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
srNextToken :: Lens' SearchRooms (Maybe Text)
srNextToken = lens _srNextToken (\ s a -> s{_srNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
srMaxResults :: Lens' SearchRooms (Maybe Natural)
srMaxResults = lens _srMaxResults (\ s a -> s{_srMaxResults = a}) . mapping _Nat

instance AWSPager SearchRooms where
        page rq rs
          | stop (rs ^. srrsNextToken) = Nothing
          | stop (rs ^. srrsRooms) = Nothing
          | otherwise =
            Just $ rq & srNextToken .~ rs ^. srrsNextToken

instance AWSRequest SearchRooms where
        type Rs SearchRooms = SearchRoomsResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 SearchRoomsResponse' <$>
                   (x .?> "Rooms" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (x .?> "TotalCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchRooms where

instance NFData SearchRooms where

instance ToHeaders SearchRooms where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.SearchRooms" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchRooms where
        toJSON SearchRooms'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _srFilters,
                  ("SortCriteria" .=) <$> _srSortCriteria,
                  ("NextToken" .=) <$> _srNextToken,
                  ("MaxResults" .=) <$> _srMaxResults])

instance ToPath SearchRooms where
        toPath = const "/"

instance ToQuery SearchRooms where
        toQuery = const mempty

-- | /See:/ 'searchRoomsResponse' smart constructor.
data SearchRoomsResponse = SearchRoomsResponse'
  { _srrsRooms          :: !(Maybe [RoomData])
  , _srrsNextToken      :: !(Maybe Text)
  , _srrsTotalCount     :: !(Maybe Int)
  , _srrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchRoomsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrsRooms' - The rooms that meet the specified set of filter criteria, in sort order.
--
-- * 'srrsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'srrsTotalCount' - The total number of rooms returned.
--
-- * 'srrsResponseStatus' - -- | The response status code.
searchRoomsResponse
    :: Int -- ^ 'srrsResponseStatus'
    -> SearchRoomsResponse
searchRoomsResponse pResponseStatus_ =
  SearchRoomsResponse'
    { _srrsRooms = Nothing
    , _srrsNextToken = Nothing
    , _srrsTotalCount = Nothing
    , _srrsResponseStatus = pResponseStatus_
    }


-- | The rooms that meet the specified set of filter criteria, in sort order.
srrsRooms :: Lens' SearchRoomsResponse [RoomData]
srrsRooms = lens _srrsRooms (\ s a -> s{_srrsRooms = a}) . _Default . _Coerce

-- | The token returned to indicate that there is more data available.
srrsNextToken :: Lens' SearchRoomsResponse (Maybe Text)
srrsNextToken = lens _srrsNextToken (\ s a -> s{_srrsNextToken = a})

-- | The total number of rooms returned.
srrsTotalCount :: Lens' SearchRoomsResponse (Maybe Int)
srrsTotalCount = lens _srrsTotalCount (\ s a -> s{_srrsTotalCount = a})

-- | -- | The response status code.
srrsResponseStatus :: Lens' SearchRoomsResponse Int
srrsResponseStatus = lens _srrsResponseStatus (\ s a -> s{_srrsResponseStatus = a})

instance NFData SearchRoomsResponse where
