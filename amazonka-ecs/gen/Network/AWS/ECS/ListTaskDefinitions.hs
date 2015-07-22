{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListTaskDefinitions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definitions that are registered to your account.
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
    , ltdrqStatus
    , ltdrqFamilyPrefix
    , ltdrqNextToken
    , ltdrqSort
    , ltdrqMaxResults

    -- * Response
    , ListTaskDefinitionsResponse
    -- ** Response constructor
    , listTaskDefinitionsResponse
    -- ** Response lenses
    , ltdrsTaskDefinitionARNs
    , ltdrsNextToken
    , ltdrsStatus
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
-- * 'ltdrqStatus'
--
-- * 'ltdrqFamilyPrefix'
--
-- * 'ltdrqNextToken'
--
-- * 'ltdrqSort'
--
-- * 'ltdrqMaxResults'
data ListTaskDefinitions = ListTaskDefinitions'
    { _ltdrqStatus       :: !(Maybe TaskDefinitionStatus)
    , _ltdrqFamilyPrefix :: !(Maybe Text)
    , _ltdrqNextToken    :: !(Maybe Text)
    , _ltdrqSort         :: !(Maybe SortOrder)
    , _ltdrqMaxResults   :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTaskDefinitions' smart constructor.
listTaskDefinitions :: ListTaskDefinitions
listTaskDefinitions =
    ListTaskDefinitions'
    { _ltdrqStatus = Nothing
    , _ltdrqFamilyPrefix = Nothing
    , _ltdrqNextToken = Nothing
    , _ltdrqSort = Nothing
    , _ltdrqMaxResults = Nothing
    }

-- | The task definition status that you want to filter the
-- @ListTaskDefinitions@ results with. By default, only @ACTIVE@ task
-- definitions are listed. By setting this parameter to @INACTIVE@, you can
-- view task definitions that are @INACTIVE@ as long as an active task or
-- service still references them. If you paginate the resulting output, be
-- sure to keep the @status@ value constant in each subsequent request.
ltdrqStatus :: Lens' ListTaskDefinitions (Maybe TaskDefinitionStatus)
ltdrqStatus = lens _ltdrqStatus (\ s a -> s{_ltdrqStatus = a});

-- | The full family name that you want to filter the @ListTaskDefinitions@
-- results with. Specifying a @familyPrefix@ will limit the listed task
-- definitions to task definition revisions that belong to that family.
ltdrqFamilyPrefix :: Lens' ListTaskDefinitions (Maybe Text)
ltdrqFamilyPrefix = lens _ltdrqFamilyPrefix (\ s a -> s{_ltdrqFamilyPrefix = a});

-- | The @nextToken@ value returned from a previous paginated
-- @ListTaskDefinitions@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
ltdrqNextToken :: Lens' ListTaskDefinitions (Maybe Text)
ltdrqNextToken = lens _ltdrqNextToken (\ s a -> s{_ltdrqNextToken = a});

-- | The order in which to sort the results. Valid values are @ASC@ and
-- @DESC@. By default (@ASC@), task definitions are listed
-- lexicographically by family name and in ascending numerical order by
-- revision so that the newest task definitions in a family are listed
-- last. Setting this parameter to @DESC@ reverses the sort order on family
-- name and revision so that the newest task definitions in a family are
-- listed first.
ltdrqSort :: Lens' ListTaskDefinitions (Maybe SortOrder)
ltdrqSort = lens _ltdrqSort (\ s a -> s{_ltdrqSort = a});

-- | The maximum number of task definition results returned by
-- @ListTaskDefinitions@ in paginated output. When this parameter is used,
-- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListTaskDefinitions@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If this parameter is not used, then @ListTaskDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
ltdrqMaxResults :: Lens' ListTaskDefinitions (Maybe Int)
ltdrqMaxResults = lens _ltdrqMaxResults (\ s a -> s{_ltdrqMaxResults = a});

instance AWSPager ListTaskDefinitions where
        page rq rs
          | stop (rs ^. ltdrsNextToken) = Nothing
          | stop (rs ^. ltdrsTaskDefinitionARNs) = Nothing
          | otherwise =
            Just $ rq & ltdrqNextToken .~ rs ^. ltdrsNextToken

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
              ["status" .= _ltdrqStatus,
               "familyPrefix" .= _ltdrqFamilyPrefix,
               "nextToken" .= _ltdrqNextToken, "sort" .= _ltdrqSort,
               "maxResults" .= _ltdrqMaxResults]

instance ToPath ListTaskDefinitions where
        toPath = const "/"

instance ToQuery ListTaskDefinitions where
        toQuery = const mempty

-- | /See:/ 'listTaskDefinitionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdrsTaskDefinitionARNs'
--
-- * 'ltdrsNextToken'
--
-- * 'ltdrsStatus'
data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse'
    { _ltdrsTaskDefinitionARNs :: !(Maybe [Text])
    , _ltdrsNextToken          :: !(Maybe Text)
    , _ltdrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTaskDefinitionsResponse' smart constructor.
listTaskDefinitionsResponse :: Int -> ListTaskDefinitionsResponse
listTaskDefinitionsResponse pStatus_ =
    ListTaskDefinitionsResponse'
    { _ltdrsTaskDefinitionARNs = Nothing
    , _ltdrsNextToken = Nothing
    , _ltdrsStatus = pStatus_
    }

-- | The list of task definition Amazon Resource Name (ARN) entries for the
-- @ListTaskDefintions@ request.
ltdrsTaskDefinitionARNs :: Lens' ListTaskDefinitionsResponse [Text]
ltdrsTaskDefinitionARNs = lens _ltdrsTaskDefinitionARNs (\ s a -> s{_ltdrsTaskDefinitionARNs = a}) . _Default;

-- | The @nextToken@ value to include in a future @ListTaskDefinitions@
-- request. When the results of a @ListTaskDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
ltdrsNextToken :: Lens' ListTaskDefinitionsResponse (Maybe Text)
ltdrsNextToken = lens _ltdrsNextToken (\ s a -> s{_ltdrsNextToken = a});

-- | FIXME: Undocumented member.
ltdrsStatus :: Lens' ListTaskDefinitionsResponse Int
ltdrsStatus = lens _ltdrsStatus (\ s a -> s{_ltdrsStatus = a});
