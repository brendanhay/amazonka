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
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by the specified events, based on the specified filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service. Events that have impact beyond that of the affected entities, or where the extent of impact is unknown, include at least one entity indicating this.
--
--
-- At least one event ARN is required. Results are sorted by the @lastUpdatedTime@ of the entity, starting with the most recent.
--
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntities
    (
    -- * Creating a Request
      describeAffectedEntities
    , DescribeAffectedEntities
    -- * Request Lenses
    , daeLocale
    , daeNextToken
    , daeMaxResults
    , daeFilter

    -- * Destructuring the Response
    , describeAffectedEntitiesResponse
    , DescribeAffectedEntitiesResponse
    -- * Response Lenses
    , daersEntities
    , daersNextToken
    , daersResponseStatus
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAffectedEntities' smart constructor.
data DescribeAffectedEntities = DescribeAffectedEntities'
  { _daeLocale     :: !(Maybe Text)
  , _daeNextToken  :: !(Maybe Text)
  , _daeMaxResults :: !(Maybe Nat)
  , _daeFilter     :: !EntityFilter
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAffectedEntities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daeLocale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- * 'daeNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'daeMaxResults' - The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- * 'daeFilter' - Values to narrow the results returned. At least one event ARN is required.
describeAffectedEntities
    :: EntityFilter -- ^ 'daeFilter'
    -> DescribeAffectedEntities
describeAffectedEntities pFilter_ =
  DescribeAffectedEntities'
    { _daeLocale = Nothing
    , _daeNextToken = Nothing
    , _daeMaxResults = Nothing
    , _daeFilter = pFilter_
    }


-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
daeLocale :: Lens' DescribeAffectedEntities (Maybe Text)
daeLocale = lens _daeLocale (\ s a -> s{_daeLocale = a})

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
daeNextToken :: Lens' DescribeAffectedEntities (Maybe Text)
daeNextToken = lens _daeNextToken (\ s a -> s{_daeNextToken = a})

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
daeMaxResults :: Lens' DescribeAffectedEntities (Maybe Natural)
daeMaxResults = lens _daeMaxResults (\ s a -> s{_daeMaxResults = a}) . mapping _Nat

-- | Values to narrow the results returned. At least one event ARN is required.
daeFilter :: Lens' DescribeAffectedEntities EntityFilter
daeFilter = lens _daeFilter (\ s a -> s{_daeFilter = a})

instance AWSPager DescribeAffectedEntities where
        page rq rs
          | stop (rs ^. daersNextToken) = Nothing
          | stop (rs ^. daersEntities) = Nothing
          | otherwise =
            Just $ rq & daeNextToken .~ rs ^. daersNextToken

instance AWSRequest DescribeAffectedEntities where
        type Rs DescribeAffectedEntities =
             DescribeAffectedEntitiesResponse
        request = postJSON awsHealth
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAffectedEntitiesResponse' <$>
                   (x .?> "entities" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAffectedEntities where

instance NFData DescribeAffectedEntities where

instance ToHeaders DescribeAffectedEntities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSHealth_20160804.DescribeAffectedEntities" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAffectedEntities where
        toJSON DescribeAffectedEntities'{..}
          = object
              (catMaybes
                 [("locale" .=) <$> _daeLocale,
                  ("nextToken" .=) <$> _daeNextToken,
                  ("maxResults" .=) <$> _daeMaxResults,
                  Just ("filter" .= _daeFilter)])

instance ToPath DescribeAffectedEntities where
        toPath = const "/"

instance ToQuery DescribeAffectedEntities where
        toQuery = const mempty

-- | /See:/ 'describeAffectedEntitiesResponse' smart constructor.
data DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse'
  { _daersEntities       :: !(Maybe [AffectedEntity])
  , _daersNextToken      :: !(Maybe Text)
  , _daersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAffectedEntitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daersEntities' - The entities that match the filter criteria.
--
-- * 'daersNextToken' - If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- * 'daersResponseStatus' - -- | The response status code.
describeAffectedEntitiesResponse
    :: Int -- ^ 'daersResponseStatus'
    -> DescribeAffectedEntitiesResponse
describeAffectedEntitiesResponse pResponseStatus_ =
  DescribeAffectedEntitiesResponse'
    { _daersEntities = Nothing
    , _daersNextToken = Nothing
    , _daersResponseStatus = pResponseStatus_
    }


-- | The entities that match the filter criteria.
daersEntities :: Lens' DescribeAffectedEntitiesResponse [AffectedEntity]
daersEntities = lens _daersEntities (\ s a -> s{_daersEntities = a}) . _Default . _Coerce

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
daersNextToken :: Lens' DescribeAffectedEntitiesResponse (Maybe Text)
daersNextToken = lens _daersNextToken (\ s a -> s{_daersNextToken = a})

-- | -- | The response status code.
daersResponseStatus :: Lens' DescribeAffectedEntitiesResponse Int
daersResponseStatus = lens _daersResponseStatus (\ s a -> s{_daersResponseStatus = a})

instance NFData DescribeAffectedEntitiesResponse
         where
