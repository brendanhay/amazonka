{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.ListIdentityPoolUsage
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

-- | Gets a list of identity pools registered with Cognito.
--
-- ListIdentityPoolUsage can only be called with developer credentials. You
-- cannot make this API call with the temporary user credentials provided
-- by Cognito Identity.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_ListIdentityPoolUsage.html>
module Network.AWS.CognitoSync.ListIdentityPoolUsage
    (
    -- * Request
      ListIdentityPoolUsage
    -- ** Request constructor
    , listIdentityPoolUsage
    -- ** Request lenses
    , lipuNextToken
    , lipuMaxResults

    -- * Response
    , ListIdentityPoolUsageResponse
    -- ** Response constructor
    , listIdentityPoolUsageResponse
    -- ** Response lenses
    , lipurIdentityPoolUsages
    , lipurCount
    , lipurNextToken
    , lipurMaxResults
    , lipurStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request for usage information on an identity pool.
--
-- /See:/ 'listIdentityPoolUsage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipuNextToken'
--
-- * 'lipuMaxResults'
data ListIdentityPoolUsage = ListIdentityPoolUsage'
    { _lipuNextToken  :: Maybe Text
    , _lipuMaxResults :: Maybe Int
    } deriving (Eq,Read,Show)

-- | 'ListIdentityPoolUsage' smart constructor.
listIdentityPoolUsage :: ListIdentityPoolUsage
listIdentityPoolUsage =
    ListIdentityPoolUsage'
    { _lipuNextToken = Nothing
    , _lipuMaxResults = Nothing
    }

-- | A pagination token for obtaining the next page of results.
lipuNextToken :: Lens' ListIdentityPoolUsage (Maybe Text)
lipuNextToken = lens _lipuNextToken (\ s a -> s{_lipuNextToken = a});

-- | The maximum number of results to be returned.
lipuMaxResults :: Lens' ListIdentityPoolUsage (Maybe Int)
lipuMaxResults = lens _lipuMaxResults (\ s a -> s{_lipuMaxResults = a});

instance AWSRequest ListIdentityPoolUsage where
        type Sv ListIdentityPoolUsage = CognitoSync
        type Rs ListIdentityPoolUsage =
             ListIdentityPoolUsageResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentityPoolUsageResponse' <$>
                   (x .?> "IdentityPoolUsages" .!@ mempty) <*>
                     (x .?> "Count")
                     <*> (x .?> "NextToken")
                     <*> (x .?> "MaxResults")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListIdentityPoolUsage where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListIdentityPoolUsage where
        toPath = const "/identitypools"

instance ToQuery ListIdentityPoolUsage where
        toQuery ListIdentityPoolUsage'{..}
          = mconcat
              ["nextToken" =: _lipuNextToken,
               "maxResults" =: _lipuMaxResults]

-- | Returned for a successful ListIdentityPoolUsage request.
--
-- /See:/ 'listIdentityPoolUsageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipurIdentityPoolUsages'
--
-- * 'lipurCount'
--
-- * 'lipurNextToken'
--
-- * 'lipurMaxResults'
--
-- * 'lipurStatus'
data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse'
    { _lipurIdentityPoolUsages :: Maybe [IdentityPoolUsage]
    , _lipurCount              :: Maybe Int
    , _lipurNextToken          :: Maybe Text
    , _lipurMaxResults         :: Maybe Int
    , _lipurStatus             :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListIdentityPoolUsageResponse' smart constructor.
listIdentityPoolUsageResponse :: Int -> ListIdentityPoolUsageResponse
listIdentityPoolUsageResponse pStatus =
    ListIdentityPoolUsageResponse'
    { _lipurIdentityPoolUsages = Nothing
    , _lipurCount = Nothing
    , _lipurNextToken = Nothing
    , _lipurMaxResults = Nothing
    , _lipurStatus = pStatus
    }

-- | Usage information for the identity pools.
lipurIdentityPoolUsages :: Lens' ListIdentityPoolUsageResponse [IdentityPoolUsage]
lipurIdentityPoolUsages = lens _lipurIdentityPoolUsages (\ s a -> s{_lipurIdentityPoolUsages = a}) . _Default;

-- | Total number of identities for the identity pool.
lipurCount :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipurCount = lens _lipurCount (\ s a -> s{_lipurCount = a});

-- | A pagination token for obtaining the next page of results.
lipurNextToken :: Lens' ListIdentityPoolUsageResponse (Maybe Text)
lipurNextToken = lens _lipurNextToken (\ s a -> s{_lipurNextToken = a});

-- | The maximum number of results to be returned.
lipurMaxResults :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipurMaxResults = lens _lipurMaxResults (\ s a -> s{_lipurMaxResults = a});

-- | FIXME: Undocumented member.
lipurStatus :: Lens' ListIdentityPoolUsageResponse Int
lipurStatus = lens _lipurStatus (\ s a -> s{_lipurStatus = a});
