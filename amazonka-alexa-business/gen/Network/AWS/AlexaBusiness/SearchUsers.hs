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
-- Module      : Network.AWS.AlexaBusiness.SearchUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches users and lists the ones that meet a set of filter and sort criteria.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchUsers
    (
    -- * Creating a Request
      searchUsers
    , SearchUsers
    -- * Request Lenses
    , suFilters
    , suSortCriteria
    , suNextToken
    , suMaxResults

    -- * Destructuring the Response
    , searchUsersResponse
    , SearchUsersResponse
    -- * Response Lenses
    , sursUsers
    , sursNextToken
    , sursTotalCount
    , sursResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { _suFilters      :: !(Maybe [Filter])
  , _suSortCriteria :: !(Maybe [Sort])
  , _suNextToken    :: !(Maybe Text)
  , _suMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'suFilters' - The filters to use for listing a specific set of users. Required. Supported filter keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
--
-- * 'suSortCriteria' - The sort order to use in listing the filtered set of users. Required. Supported sort keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
--
-- * 'suNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
--
-- * 'suMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
searchUsers
    :: SearchUsers
searchUsers =
  SearchUsers'
    { _suFilters = Nothing
    , _suSortCriteria = Nothing
    , _suNextToken = Nothing
    , _suMaxResults = Nothing
    }


-- | The filters to use for listing a specific set of users. Required. Supported filter keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
suFilters :: Lens' SearchUsers [Filter]
suFilters = lens _suFilters (\ s a -> s{_suFilters = a}) . _Default . _Coerce

-- | The sort order to use in listing the filtered set of users. Required. Supported sort keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
suSortCriteria :: Lens' SearchUsers [Sort]
suSortCriteria = lens _suSortCriteria (\ s a -> s{_suSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
suNextToken :: Lens' SearchUsers (Maybe Text)
suNextToken = lens _suNextToken (\ s a -> s{_suNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
suMaxResults :: Lens' SearchUsers (Maybe Natural)
suMaxResults = lens _suMaxResults (\ s a -> s{_suMaxResults = a}) . mapping _Nat

instance AWSPager SearchUsers where
        page rq rs
          | stop (rs ^. sursNextToken) = Nothing
          | stop (rs ^. sursUsers) = Nothing
          | otherwise =
            Just $ rq & suNextToken .~ rs ^. sursNextToken

instance AWSRequest SearchUsers where
        type Rs SearchUsers = SearchUsersResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 SearchUsersResponse' <$>
                   (x .?> "Users" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (x .?> "TotalCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchUsers where

instance NFData SearchUsers where

instance ToHeaders SearchUsers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.SearchUsers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchUsers where
        toJSON SearchUsers'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _suFilters,
                  ("SortCriteria" .=) <$> _suSortCriteria,
                  ("NextToken" .=) <$> _suNextToken,
                  ("MaxResults" .=) <$> _suMaxResults])

instance ToPath SearchUsers where
        toPath = const "/"

instance ToQuery SearchUsers where
        toQuery = const mempty

-- | /See:/ 'searchUsersResponse' smart constructor.
data SearchUsersResponse = SearchUsersResponse'
  { _sursUsers          :: !(Maybe [UserData])
  , _sursNextToken      :: !(Maybe Text)
  , _sursTotalCount     :: !(Maybe Int)
  , _sursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sursUsers' - The users that meet the specified set of filter criteria, in sort order.
--
-- * 'sursNextToken' - The token returned to indicate that there is more data available.
--
-- * 'sursTotalCount' - The total number of users returned.
--
-- * 'sursResponseStatus' - -- | The response status code.
searchUsersResponse
    :: Int -- ^ 'sursResponseStatus'
    -> SearchUsersResponse
searchUsersResponse pResponseStatus_ =
  SearchUsersResponse'
    { _sursUsers = Nothing
    , _sursNextToken = Nothing
    , _sursTotalCount = Nothing
    , _sursResponseStatus = pResponseStatus_
    }


-- | The users that meet the specified set of filter criteria, in sort order.
sursUsers :: Lens' SearchUsersResponse [UserData]
sursUsers = lens _sursUsers (\ s a -> s{_sursUsers = a}) . _Default . _Coerce

-- | The token returned to indicate that there is more data available.
sursNextToken :: Lens' SearchUsersResponse (Maybe Text)
sursNextToken = lens _sursNextToken (\ s a -> s{_sursNextToken = a})

-- | The total number of users returned.
sursTotalCount :: Lens' SearchUsersResponse (Maybe Int)
sursTotalCount = lens _sursTotalCount (\ s a -> s{_sursTotalCount = a})

-- | -- | The response status code.
sursResponseStatus :: Lens' SearchUsersResponse Int
sursResponseStatus = lens _sursResponseStatus (\ s a -> s{_sursResponseStatus = a})

instance NFData SearchUsersResponse where
