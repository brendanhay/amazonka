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
-- Module      : Network.AWS.AlexaBusiness.SearchSkillGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches skill groups and lists the ones that meet a set of filter and sort criteria.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchSkillGroups
    (
    -- * Creating a Request
      searchSkillGroups
    , SearchSkillGroups
    -- * Request Lenses
    , ssgFilters
    , ssgSortCriteria
    , ssgNextToken
    , ssgMaxResults

    -- * Destructuring the Response
    , searchSkillGroupsResponse
    , SearchSkillGroupsResponse
    -- * Response Lenses
    , ssgrsNextToken
    , ssgrsSkillGroups
    , ssgrsTotalCount
    , ssgrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchSkillGroups' smart constructor.
data SearchSkillGroups = SearchSkillGroups'
  { _ssgFilters      :: !(Maybe [Filter])
  , _ssgSortCriteria :: !(Maybe [Sort])
  , _ssgNextToken    :: !(Maybe Text)
  , _ssgMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchSkillGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgFilters' - The filters to use to list a specified set of skill groups. The supported filter key is SkillGroupName.
--
-- * 'ssgSortCriteria' - The sort order to use in listing the specified set of skill groups. The supported sort key is SkillGroupName.
--
-- * 'ssgNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
--
-- * 'ssgMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
searchSkillGroups
    :: SearchSkillGroups
searchSkillGroups =
  SearchSkillGroups'
    { _ssgFilters = Nothing
    , _ssgSortCriteria = Nothing
    , _ssgNextToken = Nothing
    , _ssgMaxResults = Nothing
    }


-- | The filters to use to list a specified set of skill groups. The supported filter key is SkillGroupName.
ssgFilters :: Lens' SearchSkillGroups [Filter]
ssgFilters = lens _ssgFilters (\ s a -> s{_ssgFilters = a}) . _Default . _Coerce

-- | The sort order to use in listing the specified set of skill groups. The supported sort key is SkillGroupName.
ssgSortCriteria :: Lens' SearchSkillGroups [Sort]
ssgSortCriteria = lens _ssgSortCriteria (\ s a -> s{_ssgSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
ssgNextToken :: Lens' SearchSkillGroups (Maybe Text)
ssgNextToken = lens _ssgNextToken (\ s a -> s{_ssgNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
ssgMaxResults :: Lens' SearchSkillGroups (Maybe Natural)
ssgMaxResults = lens _ssgMaxResults (\ s a -> s{_ssgMaxResults = a}) . mapping _Nat

instance AWSPager SearchSkillGroups where
        page rq rs
          | stop (rs ^. ssgrsNextToken) = Nothing
          | stop (rs ^. ssgrsSkillGroups) = Nothing
          | otherwise =
            Just $ rq & ssgNextToken .~ rs ^. ssgrsNextToken

instance AWSRequest SearchSkillGroups where
        type Rs SearchSkillGroups = SearchSkillGroupsResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 SearchSkillGroupsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "SkillGroups" .!@ mempty)
                     <*> (x .?> "TotalCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchSkillGroups where

instance NFData SearchSkillGroups where

instance ToHeaders SearchSkillGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.SearchSkillGroups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchSkillGroups where
        toJSON SearchSkillGroups'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _ssgFilters,
                  ("SortCriteria" .=) <$> _ssgSortCriteria,
                  ("NextToken" .=) <$> _ssgNextToken,
                  ("MaxResults" .=) <$> _ssgMaxResults])

instance ToPath SearchSkillGroups where
        toPath = const "/"

instance ToQuery SearchSkillGroups where
        toQuery = const mempty

-- | /See:/ 'searchSkillGroupsResponse' smart constructor.
data SearchSkillGroupsResponse = SearchSkillGroupsResponse'
  { _ssgrsNextToken      :: !(Maybe Text)
  , _ssgrsSkillGroups    :: !(Maybe [SkillGroupData])
  , _ssgrsTotalCount     :: !(Maybe Int)
  , _ssgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchSkillGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgrsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'ssgrsSkillGroups' - The skill groups that meet the filter criteria, in sort order.
--
-- * 'ssgrsTotalCount' - The total number of skill groups returned.
--
-- * 'ssgrsResponseStatus' - -- | The response status code.
searchSkillGroupsResponse
    :: Int -- ^ 'ssgrsResponseStatus'
    -> SearchSkillGroupsResponse
searchSkillGroupsResponse pResponseStatus_ =
  SearchSkillGroupsResponse'
    { _ssgrsNextToken = Nothing
    , _ssgrsSkillGroups = Nothing
    , _ssgrsTotalCount = Nothing
    , _ssgrsResponseStatus = pResponseStatus_
    }


-- | The token returned to indicate that there is more data available.
ssgrsNextToken :: Lens' SearchSkillGroupsResponse (Maybe Text)
ssgrsNextToken = lens _ssgrsNextToken (\ s a -> s{_ssgrsNextToken = a})

-- | The skill groups that meet the filter criteria, in sort order.
ssgrsSkillGroups :: Lens' SearchSkillGroupsResponse [SkillGroupData]
ssgrsSkillGroups = lens _ssgrsSkillGroups (\ s a -> s{_ssgrsSkillGroups = a}) . _Default . _Coerce

-- | The total number of skill groups returned.
ssgrsTotalCount :: Lens' SearchSkillGroupsResponse (Maybe Int)
ssgrsTotalCount = lens _ssgrsTotalCount (\ s a -> s{_ssgrsTotalCount = a})

-- | -- | The response status code.
ssgrsResponseStatus :: Lens' SearchSkillGroupsResponse Int
ssgrsResponseStatus = lens _ssgrsResponseStatus (\ s a -> s{_ssgrsResponseStatus = a})

instance NFData SearchSkillGroupsResponse where
