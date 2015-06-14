{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- parameter.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitions.html>
module Network.AWS.ECS.ListTaskDefinitions
    (
    -- * Request
      ListTaskDefinitions
    -- ** Request constructor
    , listTaskDefinitions
    -- ** Request lenses
    , ltdFamilyPrefix
    , ltdNextToken
    , ltdMaxResults

    -- * Response
    , ListTaskDefinitionsResponse
    -- ** Response constructor
    , listTaskDefinitionsResponse
    -- ** Response lenses
    , ltdrTaskDefinitionARNs
    , ltdrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'listTaskDefinitions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdFamilyPrefix'
--
-- * 'ltdNextToken'
--
-- * 'ltdMaxResults'
data ListTaskDefinitions = ListTaskDefinitions'{_ltdFamilyPrefix :: Maybe Text, _ltdNextToken :: Maybe Text, _ltdMaxResults :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ListTaskDefinitions' smart constructor.
listTaskDefinitions :: ListTaskDefinitions
listTaskDefinitions = ListTaskDefinitions'{_ltdFamilyPrefix = Nothing, _ltdNextToken = Nothing, _ltdMaxResults = Nothing};

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

instance AWSRequest ListTaskDefinitions where
        type Sv ListTaskDefinitions = ECS
        type Rs ListTaskDefinitions =
             ListTaskDefinitionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTaskDefinitionsResponse' <$>
                   x .?> "taskDefinitionArns" .!@ mempty <*>
                     x .?> "nextToken")

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
              ["familyPrefix" .= _ltdFamilyPrefix,
               "nextToken" .= _ltdNextToken,
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
data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse'{_ltdrTaskDefinitionARNs :: Maybe [Text], _ltdrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListTaskDefinitionsResponse' smart constructor.
listTaskDefinitionsResponse :: ListTaskDefinitionsResponse
listTaskDefinitionsResponse = ListTaskDefinitionsResponse'{_ltdrTaskDefinitionARNs = Nothing, _ltdrNextToken = Nothing};

-- | The list of task definition Amazon Resource Name (ARN) entries for the
-- @ListTaskDefintions@ request.
ltdrTaskDefinitionARNs :: Lens' ListTaskDefinitionsResponse (Maybe [Text])
ltdrTaskDefinitionARNs = lens _ltdrTaskDefinitionARNs (\ s a -> s{_ltdrTaskDefinitionARNs = a});

-- | The @nextToken@ value to include in a future @ListTaskDefinitions@
-- request. When the results of a @ListTaskDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
ltdrNextToken :: Lens' ListTaskDefinitionsResponse (Maybe Text)
ltdrNextToken = lens _ltdrNextToken (\ s a -> s{_ltdrNextToken = a});
