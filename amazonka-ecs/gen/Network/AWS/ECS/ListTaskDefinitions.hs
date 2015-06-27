{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ECS.ListTaskDefinitions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of task definitions that are registered to your account.
-- You can filter the results by family name with the @familyPrefix@
-- parameter or by status with the @status@ parameter.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitions.html>
module Network.AWS.ECS.ListTaskDefinitions
    (
    -- * Request
      ListTaskDefinitions
    -- ** Request constructor
    , listTaskDefinitions
    -- ** Request lenses
    , ltdStatus
    , ltdFamilyPrefix
    , ltdNextToken
    , ltdSort
    , ltdMaxResults

    -- * Response
    , ListTaskDefinitionsResponse
    -- ** Response constructor
    , listTaskDefinitionsResponse
    -- ** Response lenses
    , ltdrTaskDefinitionARNs
    , ltdrNextToken
    , ltdrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listTaskDefinitions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdStatus'
--
-- * 'ltdFamilyPrefix'
--
-- * 'ltdNextToken'
--
-- * 'ltdSort'
--
-- * 'ltdMaxResults'
data ListTaskDefinitions = ListTaskDefinitions'
    { _ltdStatus       :: Maybe TaskDefinitionStatus
    , _ltdFamilyPrefix :: Maybe Text
    , _ltdNextToken    :: Maybe Text
    , _ltdSort         :: Maybe SortOrder
    , _ltdMaxResults   :: Maybe Int
    } deriving (Eq,Read,Show)

-- | 'ListTaskDefinitions' smart constructor.
listTaskDefinitions :: ListTaskDefinitions
listTaskDefinitions =
    ListTaskDefinitions'
    { _ltdStatus = Nothing
    , _ltdFamilyPrefix = Nothing
    , _ltdNextToken = Nothing
    , _ltdSort = Nothing
    , _ltdMaxResults = Nothing
    }

-- | The task definition status that you want to filter the
-- @ListTaskDefinitions@ results with. By default, only @ACTIVE@ task
-- definitions are listed. By setting this parameter to @INACTIVE@, you can
-- view task definitions that are @INACTIVE@ as long as an active task or
-- service still references them. If you paginate the resulting output, be
-- sure to keep the @status@ value constant in each subsequent request.
ltdStatus :: Lens' ListTaskDefinitions (Maybe TaskDefinitionStatus)
ltdStatus = lens _ltdStatus (\ s a -> s{_ltdStatus = a});

-- | The full family name that you want to filter the @ListTaskDefinitions@
-- results with. Specifying a @familyPrefix@ will limit the listed task
-- definitions to task definition revisions that belong to that family.
ltdFamilyPrefix :: Lens' ListTaskDefinitions (Maybe Text)
ltdFamilyPrefix = lens _ltdFamilyPrefix (\ s a -> s{_ltdFamilyPrefix = a});

-- | The @nextToken@ value returned from a previous paginated
-- @ListTaskDefinitions@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
ltdNextToken :: Lens' ListTaskDefinitions (Maybe Text)
ltdNextToken = lens _ltdNextToken (\ s a -> s{_ltdNextToken = a});

-- | The order in which to sort the results. Valid values are @ASC@ and
-- @DESC@. By default (@ASC@), task definitions are listed
-- lexicographically by family name and in ascending numerical order by
-- revision so that the newest task definitions in a family are listed
-- last. Setting this parameter to @DESC@ reverses the sort order on family
-- name and revision so that the newest task definitions in a family are
-- listed first.
ltdSort :: Lens' ListTaskDefinitions (Maybe SortOrder)
ltdSort = lens _ltdSort (\ s a -> s{_ltdSort = a});

-- | The maximum number of task definition results returned by
-- @ListTaskDefinitions@ in paginated output. When this parameter is used,
-- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListTaskDefinitions@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If this parameter is not used, then @ListTaskDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
ltdMaxResults :: Lens' ListTaskDefinitions (Maybe Int)
ltdMaxResults = lens _ltdMaxResults (\ s a -> s{_ltdMaxResults = a});

instance AWSPager ListTaskDefinitions where
        page rq rs
          | stop (rs ^. ltdrNextToken) = Nothing
          | stop (rs ^. ltdrTaskDefinitionARNs) = Nothing
          | otherwise =
            Just $ rq & ltdNextToken .~ rs ^. ltdrNextToken

instance AWSRequest ListTaskDefinitions where
        type Sv ListTaskDefinitions = ECS
        type Rs ListTaskDefinitions =
             ListTaskDefinitionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTaskDefinitionsResponse' <$>
                   (x .?> "taskDefinitionArns" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListTaskDefinitions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.ListTaskDefinitions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTaskDefinitions where
        toJSON ListTaskDefinitions'{..}
          = object
              ["status" .= _ltdStatus,
               "familyPrefix" .= _ltdFamilyPrefix,
               "nextToken" .= _ltdNextToken, "sort" .= _ltdSort,
               "maxResults" .= _ltdMaxResults]

instance ToPath ListTaskDefinitions where
        toPath = const "/"

instance ToQuery ListTaskDefinitions where
        toQuery = const mempty

-- | /See:/ 'listTaskDefinitionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdrTaskDefinitionARNs'
--
-- * 'ltdrNextToken'
--
-- * 'ltdrStatus'
data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse'
    { _ltdrTaskDefinitionARNs :: Maybe [Text]
    , _ltdrNextToken          :: Maybe Text
    , _ltdrStatus             :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListTaskDefinitionsResponse' smart constructor.
listTaskDefinitionsResponse :: Int -> ListTaskDefinitionsResponse
listTaskDefinitionsResponse pStatus =
    ListTaskDefinitionsResponse'
    { _ltdrTaskDefinitionARNs = Nothing
    , _ltdrNextToken = Nothing
    , _ltdrStatus = pStatus
    }

-- | The list of task definition Amazon Resource Name (ARN) entries for the
-- @ListTaskDefintions@ request.
ltdrTaskDefinitionARNs :: Lens' ListTaskDefinitionsResponse [Text]
ltdrTaskDefinitionARNs = lens _ltdrTaskDefinitionARNs (\ s a -> s{_ltdrTaskDefinitionARNs = a}) . _Default;

-- | The @nextToken@ value to include in a future @ListTaskDefinitions@
-- request. When the results of a @ListTaskDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
ltdrNextToken :: Lens' ListTaskDefinitionsResponse (Maybe Text)
ltdrNextToken = lens _ltdrNextToken (\ s a -> s{_ltdrNextToken = a});

-- | FIXME: Undocumented member.
ltdrStatus :: Lens' ListTaskDefinitionsResponse Int
ltdrStatus = lens _ltdrStatus (\ s a -> s{_ltdrStatus = a});
