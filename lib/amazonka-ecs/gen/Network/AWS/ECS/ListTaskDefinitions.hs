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
-- Module      : Network.AWS.ECS.ListTaskDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definitions that are registered to your account. You can filter the results by family name with the @familyPrefix@ parameter or by status with the @status@ parameter.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTaskDefinitions
    (
    -- * Creating a Request
      listTaskDefinitions
    , ListTaskDefinitions
    -- * Request Lenses
    , ltdStatus
    , ltdFamilyPrefix
    , ltdNextToken
    , ltdSort
    , ltdMaxResults

    -- * Destructuring the Response
    , listTaskDefinitionsResponse
    , ListTaskDefinitionsResponse
    -- * Response Lenses
    , ltdrsTaskDefinitionARNs
    , ltdrsNextToken
    , ltdrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTaskDefinitions' smart constructor.
data ListTaskDefinitions = ListTaskDefinitions'
  { _ltdStatus       :: !(Maybe TaskDefinitionStatus)
  , _ltdFamilyPrefix :: !(Maybe Text)
  , _ltdNextToken    :: !(Maybe Text)
  , _ltdSort         :: !(Maybe SortOrder)
  , _ltdMaxResults   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTaskDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltdStatus' - The task definition status with which to filter the @ListTaskDefinitions@ results. By default, only @ACTIVE@ task definitions are listed. By setting this parameter to @INACTIVE@ , you can view task definitions that are @INACTIVE@ as long as an active task or service still references them. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
--
-- * 'ltdFamilyPrefix' - The full family name with which to filter the @ListTaskDefinitions@ results. Specifying a @familyPrefix@ limits the listed task definitions to task definition revisions that belong to that family.
--
-- * 'ltdNextToken' - The @nextToken@ value returned from a previous paginated @ListTaskDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- * 'ltdSort' - The order in which to sort the results. Valid values are @ASC@ and @DESC@ . By default (@ASC@ ), task definitions are listed lexicographically by family name and in ascending numerical order by revision so that the newest task definitions in a family are listed last. Setting this parameter to @DESC@ reverses the sort order on family name and revision so that the newest task definitions in a family are listed first.
--
-- * 'ltdMaxResults' - The maximum number of task definition results returned by @ListTaskDefinitions@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
listTaskDefinitions
    :: ListTaskDefinitions
listTaskDefinitions =
  ListTaskDefinitions'
    { _ltdStatus = Nothing
    , _ltdFamilyPrefix = Nothing
    , _ltdNextToken = Nothing
    , _ltdSort = Nothing
    , _ltdMaxResults = Nothing
    }


-- | The task definition status with which to filter the @ListTaskDefinitions@ results. By default, only @ACTIVE@ task definitions are listed. By setting this parameter to @INACTIVE@ , you can view task definitions that are @INACTIVE@ as long as an active task or service still references them. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
ltdStatus :: Lens' ListTaskDefinitions (Maybe TaskDefinitionStatus)
ltdStatus = lens _ltdStatus (\ s a -> s{_ltdStatus = a})

-- | The full family name with which to filter the @ListTaskDefinitions@ results. Specifying a @familyPrefix@ limits the listed task definitions to task definition revisions that belong to that family.
ltdFamilyPrefix :: Lens' ListTaskDefinitions (Maybe Text)
ltdFamilyPrefix = lens _ltdFamilyPrefix (\ s a -> s{_ltdFamilyPrefix = a})

-- | The @nextToken@ value returned from a previous paginated @ListTaskDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
ltdNextToken :: Lens' ListTaskDefinitions (Maybe Text)
ltdNextToken = lens _ltdNextToken (\ s a -> s{_ltdNextToken = a})

-- | The order in which to sort the results. Valid values are @ASC@ and @DESC@ . By default (@ASC@ ), task definitions are listed lexicographically by family name and in ascending numerical order by revision so that the newest task definitions in a family are listed last. Setting this parameter to @DESC@ reverses the sort order on family name and revision so that the newest task definitions in a family are listed first.
ltdSort :: Lens' ListTaskDefinitions (Maybe SortOrder)
ltdSort = lens _ltdSort (\ s a -> s{_ltdSort = a})

-- | The maximum number of task definition results returned by @ListTaskDefinitions@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
ltdMaxResults :: Lens' ListTaskDefinitions (Maybe Int)
ltdMaxResults = lens _ltdMaxResults (\ s a -> s{_ltdMaxResults = a})

instance AWSPager ListTaskDefinitions where
        page rq rs
          | stop (rs ^. ltdrsNextToken) = Nothing
          | stop (rs ^. ltdrsTaskDefinitionARNs) = Nothing
          | otherwise =
            Just $ rq & ltdNextToken .~ rs ^. ltdrsNextToken

instance AWSRequest ListTaskDefinitions where
        type Rs ListTaskDefinitions =
             ListTaskDefinitionsResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 ListTaskDefinitionsResponse' <$>
                   (x .?> "taskDefinitionArns" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTaskDefinitions where

instance NFData ListTaskDefinitions where

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
              (catMaybes
                 [("status" .=) <$> _ltdStatus,
                  ("familyPrefix" .=) <$> _ltdFamilyPrefix,
                  ("nextToken" .=) <$> _ltdNextToken,
                  ("sort" .=) <$> _ltdSort,
                  ("maxResults" .=) <$> _ltdMaxResults])

instance ToPath ListTaskDefinitions where
        toPath = const "/"

instance ToQuery ListTaskDefinitions where
        toQuery = const mempty

-- | /See:/ 'listTaskDefinitionsResponse' smart constructor.
data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse'
  { _ltdrsTaskDefinitionARNs :: !(Maybe [Text])
  , _ltdrsNextToken          :: !(Maybe Text)
  , _ltdrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTaskDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltdrsTaskDefinitionARNs' - The list of task definition Amazon Resource Name (ARN) entries for the @ListTaskDefinitions@ request.
--
-- * 'ltdrsNextToken' - The @nextToken@ value to include in a future @ListTaskDefinitions@ request. When the results of a @ListTaskDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ltdrsResponseStatus' - -- | The response status code.
listTaskDefinitionsResponse
    :: Int -- ^ 'ltdrsResponseStatus'
    -> ListTaskDefinitionsResponse
listTaskDefinitionsResponse pResponseStatus_ =
  ListTaskDefinitionsResponse'
    { _ltdrsTaskDefinitionARNs = Nothing
    , _ltdrsNextToken = Nothing
    , _ltdrsResponseStatus = pResponseStatus_
    }


-- | The list of task definition Amazon Resource Name (ARN) entries for the @ListTaskDefinitions@ request.
ltdrsTaskDefinitionARNs :: Lens' ListTaskDefinitionsResponse [Text]
ltdrsTaskDefinitionARNs = lens _ltdrsTaskDefinitionARNs (\ s a -> s{_ltdrsTaskDefinitionARNs = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @ListTaskDefinitions@ request. When the results of a @ListTaskDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
ltdrsNextToken :: Lens' ListTaskDefinitionsResponse (Maybe Text)
ltdrsNextToken = lens _ltdrsNextToken (\ s a -> s{_ltdrsNextToken = a})

-- | -- | The response status code.
ltdrsResponseStatus :: Lens' ListTaskDefinitionsResponse Int
ltdrsResponseStatus = lens _ltdrsResponseStatus (\ s a -> s{_ltdrsResponseStatus = a})

instance NFData ListTaskDefinitionsResponse where
