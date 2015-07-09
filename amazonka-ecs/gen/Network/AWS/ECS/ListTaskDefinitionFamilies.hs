{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListTaskDefinitionFamilies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definition families that are registered to your
-- account (which may include task definition families that no longer have
-- any @ACTIVE@ task definitions). You can filter the results with the
-- @familyPrefix@ parameter.
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
    , ltdfrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listTaskDefinitionFamilies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltdfFamilyPrefix'
--
-- * 'ltdfNextToken'
--
-- * 'ltdfMaxResults'
data ListTaskDefinitionFamilies = ListTaskDefinitionFamilies'
    { _ltdfFamilyPrefix :: !(Maybe Text)
    , _ltdfNextToken    :: !(Maybe Text)
    , _ltdfMaxResults   :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTaskDefinitionFamilies' smart constructor.
listTaskDefinitionFamilies :: ListTaskDefinitionFamilies
listTaskDefinitionFamilies =
    ListTaskDefinitionFamilies'
    { _ltdfFamilyPrefix = Nothing
    , _ltdfNextToken = Nothing
    , _ltdfMaxResults = Nothing
    }

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

instance AWSPager ListTaskDefinitionFamilies where
        page rq rs
          | stop (rs ^. ltdfrNextToken) = Nothing
          | stop (rs ^. ltdfrFamilies) = Nothing
          | otherwise =
            Just $ rq & ltdfNextToken .~ rs ^. ltdfrNextToken

instance AWSRequest ListTaskDefinitionFamilies where
        type Sv ListTaskDefinitionFamilies = ECS
        type Rs ListTaskDefinitionFamilies =
             ListTaskDefinitionFamiliesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTaskDefinitionFamiliesResponse' <$>
                   (x .?> "families" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

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
--
-- * 'ltdfrStatus'
data ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'
    { _ltdfrFamilies  :: !(Maybe [Text])
    , _ltdfrNextToken :: !(Maybe Text)
    , _ltdfrStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTaskDefinitionFamiliesResponse' smart constructor.
listTaskDefinitionFamiliesResponse :: Int -> ListTaskDefinitionFamiliesResponse
listTaskDefinitionFamiliesResponse pStatus =
    ListTaskDefinitionFamiliesResponse'
    { _ltdfrFamilies = Nothing
    , _ltdfrNextToken = Nothing
    , _ltdfrStatus = pStatus
    }

-- | The list of task definition family names that match the
-- @ListTaskDefinitionFamilies@ request.
ltdfrFamilies :: Lens' ListTaskDefinitionFamiliesResponse [Text]
ltdfrFamilies = lens _ltdfrFamilies (\ s a -> s{_ltdfrFamilies = a}) . _Default;

-- | The @nextToken@ value to include in a future
-- @ListTaskDefinitionFamilies@ request. When the results of a
-- @ListTaskDefinitionFamilies@ request exceed @maxResults@, this value can
-- be used to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
ltdfrNextToken :: Lens' ListTaskDefinitionFamiliesResponse (Maybe Text)
ltdfrNextToken = lens _ltdfrNextToken (\ s a -> s{_ltdfrNextToken = a});

-- | FIXME: Undocumented member.
ltdfrStatus :: Lens' ListTaskDefinitionFamiliesResponse Int
ltdfrStatus = lens _ltdfrStatus (\ s a -> s{_ltdfrStatus = a});
