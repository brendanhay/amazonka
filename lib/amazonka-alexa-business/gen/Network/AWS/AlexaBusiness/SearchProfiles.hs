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
-- Module      : Network.AWS.AlexaBusiness.SearchProfiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches room profiles and lists the ones that meet a set of filter criteria.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchProfiles
    (
    -- * Creating a Request
      searchProfiles
    , SearchProfiles
    -- * Request Lenses
    , spFilters
    , spSortCriteria
    , spNextToken
    , spMaxResults

    -- * Destructuring the Response
    , searchProfilesResponse
    , SearchProfilesResponse
    -- * Response Lenses
    , sprsProfiles
    , sprsNextToken
    , sprsTotalCount
    , sprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { _spFilters      :: !(Maybe [Filter])
  , _spSortCriteria :: !(Maybe [Sort])
  , _spNextToken    :: !(Maybe Text)
  , _spMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spFilters' - The filters to use to list a specified set of room profiles. Supported filter keys are ProfileName and Address. Required.
--
-- * 'spSortCriteria' - The sort order to use in listing the specified set of room profiles. Supported sort keys are ProfileName and Address.
--
-- * 'spNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'spMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
searchProfiles
    :: SearchProfiles
searchProfiles =
  SearchProfiles'
    { _spFilters = Nothing
    , _spSortCriteria = Nothing
    , _spNextToken = Nothing
    , _spMaxResults = Nothing
    }


-- | The filters to use to list a specified set of room profiles. Supported filter keys are ProfileName and Address. Required.
spFilters :: Lens' SearchProfiles [Filter]
spFilters = lens _spFilters (\ s a -> s{_spFilters = a}) . _Default . _Coerce

-- | The sort order to use in listing the specified set of room profiles. Supported sort keys are ProfileName and Address.
spSortCriteria :: Lens' SearchProfiles [Sort]
spSortCriteria = lens _spSortCriteria (\ s a -> s{_spSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
spNextToken :: Lens' SearchProfiles (Maybe Text)
spNextToken = lens _spNextToken (\ s a -> s{_spNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
spMaxResults :: Lens' SearchProfiles (Maybe Natural)
spMaxResults = lens _spMaxResults (\ s a -> s{_spMaxResults = a}) . mapping _Nat

instance AWSPager SearchProfiles where
        page rq rs
          | stop (rs ^. sprsNextToken) = Nothing
          | stop (rs ^. sprsProfiles) = Nothing
          | otherwise =
            Just $ rq & spNextToken .~ rs ^. sprsNextToken

instance AWSRequest SearchProfiles where
        type Rs SearchProfiles = SearchProfilesResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 SearchProfilesResponse' <$>
                   (x .?> "Profiles" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (x .?> "TotalCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchProfiles where

instance NFData SearchProfiles where

instance ToHeaders SearchProfiles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.SearchProfiles" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchProfiles where
        toJSON SearchProfiles'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _spFilters,
                  ("SortCriteria" .=) <$> _spSortCriteria,
                  ("NextToken" .=) <$> _spNextToken,
                  ("MaxResults" .=) <$> _spMaxResults])

instance ToPath SearchProfiles where
        toPath = const "/"

instance ToQuery SearchProfiles where
        toQuery = const mempty

-- | /See:/ 'searchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { _sprsProfiles       :: !(Maybe [ProfileData])
  , _sprsNextToken      :: !(Maybe Text)
  , _sprsTotalCount     :: !(Maybe Int)
  , _sprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sprsProfiles' - The profiles that meet the specified set of filter criteria, in sort order.
--
-- * 'sprsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'sprsTotalCount' - The total number of room profiles returned.
--
-- * 'sprsResponseStatus' - -- | The response status code.
searchProfilesResponse
    :: Int -- ^ 'sprsResponseStatus'
    -> SearchProfilesResponse
searchProfilesResponse pResponseStatus_ =
  SearchProfilesResponse'
    { _sprsProfiles = Nothing
    , _sprsNextToken = Nothing
    , _sprsTotalCount = Nothing
    , _sprsResponseStatus = pResponseStatus_
    }


-- | The profiles that meet the specified set of filter criteria, in sort order.
sprsProfiles :: Lens' SearchProfilesResponse [ProfileData]
sprsProfiles = lens _sprsProfiles (\ s a -> s{_sprsProfiles = a}) . _Default . _Coerce

-- | The token returned to indicate that there is more data available.
sprsNextToken :: Lens' SearchProfilesResponse (Maybe Text)
sprsNextToken = lens _sprsNextToken (\ s a -> s{_sprsNextToken = a})

-- | The total number of room profiles returned.
sprsTotalCount :: Lens' SearchProfilesResponse (Maybe Int)
sprsTotalCount = lens _sprsTotalCount (\ s a -> s{_sprsTotalCount = a})

-- | -- | The response status code.
sprsResponseStatus :: Lens' SearchProfilesResponse Int
sprsResponseStatus = lens _sprsResponseStatus (\ s a -> s{_sprsResponseStatus = a})

instance NFData SearchProfilesResponse where
