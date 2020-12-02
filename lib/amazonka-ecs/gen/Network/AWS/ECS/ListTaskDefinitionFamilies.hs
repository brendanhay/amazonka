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
-- Module      : Network.AWS.ECS.ListTaskDefinitionFamilies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definition families that are registered to your account (which may include task definition families that no longer have any @ACTIVE@ task definition revisions).
--
--
-- You can filter out task definition families that do not contain any @ACTIVE@ task definition revisions by setting the @status@ parameter to @ACTIVE@ . You can also filter the results with the @familyPrefix@ parameter.
--
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTaskDefinitionFamilies
    (
    -- * Creating a Request
      listTaskDefinitionFamilies
    , ListTaskDefinitionFamilies
    -- * Request Lenses
    , ltdfStatus
    , ltdfFamilyPrefix
    , ltdfNextToken
    , ltdfMaxResults

    -- * Destructuring the Response
    , listTaskDefinitionFamiliesResponse
    , ListTaskDefinitionFamiliesResponse
    -- * Response Lenses
    , ltdfrsFamilies
    , ltdfrsNextToken
    , ltdfrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTaskDefinitionFamilies' smart constructor.
data ListTaskDefinitionFamilies = ListTaskDefinitionFamilies'
  { _ltdfStatus       :: !(Maybe TaskDefinitionFamilyStatus)
  , _ltdfFamilyPrefix :: !(Maybe Text)
  , _ltdfNextToken    :: !(Maybe Text)
  , _ltdfMaxResults   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTaskDefinitionFamilies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltdfStatus' - The task definition family status with which to filter the @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and @INACTIVE@ task definition families are listed. If this parameter is set to @ACTIVE@ , only task definition families that have an @ACTIVE@ task definition revision are returned. If this parameter is set to @INACTIVE@ , only task definition families that do not have any @ACTIVE@ task definition revisions are returned. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
--
-- * 'ltdfFamilyPrefix' - The @familyPrefix@ is a string that is used to filter the results of @ListTaskDefinitionFamilies@ . If you specify a @familyPrefix@ , only task definition family names that begin with the @familyPrefix@ string are returned.
--
-- * 'ltdfNextToken' - The @nextToken@ value returned from a previous paginated @ListTaskDefinitionFamilies@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- * 'ltdfMaxResults' - The maximum number of task definition family results returned by @ListTaskDefinitionFamilies@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitionFamilies@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a @nextToken@ value if applicable.
listTaskDefinitionFamilies
    :: ListTaskDefinitionFamilies
listTaskDefinitionFamilies =
  ListTaskDefinitionFamilies'
    { _ltdfStatus = Nothing
    , _ltdfFamilyPrefix = Nothing
    , _ltdfNextToken = Nothing
    , _ltdfMaxResults = Nothing
    }


-- | The task definition family status with which to filter the @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and @INACTIVE@ task definition families are listed. If this parameter is set to @ACTIVE@ , only task definition families that have an @ACTIVE@ task definition revision are returned. If this parameter is set to @INACTIVE@ , only task definition families that do not have any @ACTIVE@ task definition revisions are returned. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
ltdfStatus :: Lens' ListTaskDefinitionFamilies (Maybe TaskDefinitionFamilyStatus)
ltdfStatus = lens _ltdfStatus (\ s a -> s{_ltdfStatus = a})

-- | The @familyPrefix@ is a string that is used to filter the results of @ListTaskDefinitionFamilies@ . If you specify a @familyPrefix@ , only task definition family names that begin with the @familyPrefix@ string are returned.
ltdfFamilyPrefix :: Lens' ListTaskDefinitionFamilies (Maybe Text)
ltdfFamilyPrefix = lens _ltdfFamilyPrefix (\ s a -> s{_ltdfFamilyPrefix = a})

-- | The @nextToken@ value returned from a previous paginated @ListTaskDefinitionFamilies@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
ltdfNextToken :: Lens' ListTaskDefinitionFamilies (Maybe Text)
ltdfNextToken = lens _ltdfNextToken (\ s a -> s{_ltdfNextToken = a})

-- | The maximum number of task definition family results returned by @ListTaskDefinitionFamilies@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitionFamilies@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a @nextToken@ value if applicable.
ltdfMaxResults :: Lens' ListTaskDefinitionFamilies (Maybe Int)
ltdfMaxResults = lens _ltdfMaxResults (\ s a -> s{_ltdfMaxResults = a})

instance AWSPager ListTaskDefinitionFamilies where
        page rq rs
          | stop (rs ^. ltdfrsNextToken) = Nothing
          | stop (rs ^. ltdfrsFamilies) = Nothing
          | otherwise =
            Just $ rq & ltdfNextToken .~ rs ^. ltdfrsNextToken

instance AWSRequest ListTaskDefinitionFamilies where
        type Rs ListTaskDefinitionFamilies =
             ListTaskDefinitionFamiliesResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 ListTaskDefinitionFamiliesResponse' <$>
                   (x .?> "families" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTaskDefinitionFamilies where

instance NFData ListTaskDefinitionFamilies where

instance ToHeaders ListTaskDefinitionFamilies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.ListTaskDefinitionFamilies"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTaskDefinitionFamilies where
        toJSON ListTaskDefinitionFamilies'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _ltdfStatus,
                  ("familyPrefix" .=) <$> _ltdfFamilyPrefix,
                  ("nextToken" .=) <$> _ltdfNextToken,
                  ("maxResults" .=) <$> _ltdfMaxResults])

instance ToPath ListTaskDefinitionFamilies where
        toPath = const "/"

instance ToQuery ListTaskDefinitionFamilies where
        toQuery = const mempty

-- | /See:/ 'listTaskDefinitionFamiliesResponse' smart constructor.
data ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'
  { _ltdfrsFamilies       :: !(Maybe [Text])
  , _ltdfrsNextToken      :: !(Maybe Text)
  , _ltdfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTaskDefinitionFamiliesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltdfrsFamilies' - The list of task definition family names that match the @ListTaskDefinitionFamilies@ request.
--
-- * 'ltdfrsNextToken' - The @nextToken@ value to include in a future @ListTaskDefinitionFamilies@ request. When the results of a @ListTaskDefinitionFamilies@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ltdfrsResponseStatus' - -- | The response status code.
listTaskDefinitionFamiliesResponse
    :: Int -- ^ 'ltdfrsResponseStatus'
    -> ListTaskDefinitionFamiliesResponse
listTaskDefinitionFamiliesResponse pResponseStatus_ =
  ListTaskDefinitionFamiliesResponse'
    { _ltdfrsFamilies = Nothing
    , _ltdfrsNextToken = Nothing
    , _ltdfrsResponseStatus = pResponseStatus_
    }


-- | The list of task definition family names that match the @ListTaskDefinitionFamilies@ request.
ltdfrsFamilies :: Lens' ListTaskDefinitionFamiliesResponse [Text]
ltdfrsFamilies = lens _ltdfrsFamilies (\ s a -> s{_ltdfrsFamilies = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @ListTaskDefinitionFamilies@ request. When the results of a @ListTaskDefinitionFamilies@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
ltdfrsNextToken :: Lens' ListTaskDefinitionFamiliesResponse (Maybe Text)
ltdfrsNextToken = lens _ltdfrsNextToken (\ s a -> s{_ltdfrsNextToken = a})

-- | -- | The response status code.
ltdfrsResponseStatus :: Lens' ListTaskDefinitionFamiliesResponse Int
ltdfrsResponseStatus = lens _ltdfrsResponseStatus (\ s a -> s{_ltdfrsResponseStatus = a})

instance NFData ListTaskDefinitionFamiliesResponse
         where
