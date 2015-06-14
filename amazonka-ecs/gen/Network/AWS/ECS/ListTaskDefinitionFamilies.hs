{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.ListTaskDefinitionFamilies
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

-- | Returns a list of task definition families that are registered to your
-- account. You can filter the results with the @familyPrefix@ parameter.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTaskDefinitionFamilies.html>
module Network.AWS.ECS.ListTaskDefinitionFamilies
    (
    -- * Request
      ListTaskDefinitionFamilies
    -- ** Request constructor
    , listTaskDefinitionFamilies
    -- ** Request lenses
    , ltdfFamilyPrefix
    , ltdfNextToken
    , ltdfMaxResults

    -- * Response
    , ListTaskDefinitionFamiliesResponse
    -- ** Response constructor
    , listTaskDefinitionFamiliesResponse
    -- ** Response lenses
    , ltdfrFamilies
    , ltdfrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'listTaskDefinitionFamilies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdfFamilyPrefix'
--
-- * 'ltdfNextToken'
--
-- * 'ltdfMaxResults'
data ListTaskDefinitionFamilies = ListTaskDefinitionFamilies'{_ltdfFamilyPrefix :: Maybe Text, _ltdfNextToken :: Maybe Text, _ltdfMaxResults :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ListTaskDefinitionFamilies' smart constructor.
listTaskDefinitionFamilies :: ListTaskDefinitionFamilies
listTaskDefinitionFamilies = ListTaskDefinitionFamilies'{_ltdfFamilyPrefix = Nothing, _ltdfNextToken = Nothing, _ltdfMaxResults = Nothing};

-- | The @familyPrefix@ is a string that is used to filter the results of
-- @ListTaskDefinitionFamilies@. If you specify a @familyPrefix@, only task
-- definition family names that begin with the @familyPrefix@ string are
-- returned.
ltdfFamilyPrefix :: Lens' ListTaskDefinitionFamilies (Maybe Text)
ltdfFamilyPrefix = lens _ltdfFamilyPrefix (\ s a -> s{_ltdfFamilyPrefix = a});

-- | The @nextToken@ value returned from a previous paginated
-- @ListTaskDefinitionFamilies@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
ltdfNextToken :: Lens' ListTaskDefinitionFamilies (Maybe Text)
ltdfNextToken = lens _ltdfNextToken (\ s a -> s{_ltdfNextToken = a});

-- | The maximum number of task definition family results returned by
-- @ListTaskDefinitionFamilies@ in paginated output. When this parameter is
-- used, @ListTaskDefinitions@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @ListTaskDefinitionFamilies@ request with the returned @nextToken@
-- value. This value can be between 1 and 100. If this parameter is not
-- used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a
-- @nextToken@ value if applicable.
ltdfMaxResults :: Lens' ListTaskDefinitionFamilies (Maybe Int)
ltdfMaxResults = lens _ltdfMaxResults (\ s a -> s{_ltdfMaxResults = a});

instance AWSRequest ListTaskDefinitionFamilies where
        type Sv ListTaskDefinitionFamilies = ECS
        type Rs ListTaskDefinitionFamilies =
             ListTaskDefinitionFamiliesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTaskDefinitionFamiliesResponse' <$>
                   x .?> "families" .!@ mempty <*> x .?> "nextToken")

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
              ["familyPrefix" .= _ltdfFamilyPrefix,
               "nextToken" .= _ltdfNextToken,
               "maxResults" .= _ltdfMaxResults]

instance ToPath ListTaskDefinitionFamilies where
        toPath = const "/"

instance ToQuery ListTaskDefinitionFamilies where
        toQuery = const mempty

-- | /See:/ 'listTaskDefinitionFamiliesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdfrFamilies'
--
-- * 'ltdfrNextToken'
data ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'{_ltdfrFamilies :: Maybe [Text], _ltdfrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListTaskDefinitionFamiliesResponse' smart constructor.
listTaskDefinitionFamiliesResponse :: ListTaskDefinitionFamiliesResponse
listTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'{_ltdfrFamilies = Nothing, _ltdfrNextToken = Nothing};

-- | The list of task definition family names that match the
-- @ListTaskDefinitionFamilies@ request.
ltdfrFamilies :: Lens' ListTaskDefinitionFamiliesResponse (Maybe [Text])
ltdfrFamilies = lens _ltdfrFamilies (\ s a -> s{_ltdfrFamilies = a});

-- | The @nextToken@ value to include in a future
-- @ListTaskDefinitionFamilies@ request. When the results of a
-- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
ltdfrNextToken :: Lens' ListTaskDefinitionFamiliesResponse (Maybe Text)
ltdfrNextToken = lens _ltdfrNextToken (\ s a -> s{_ltdfrNextToken = a});
