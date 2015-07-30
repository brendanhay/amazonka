{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.ListIdentityPoolUsage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of identity pools registered with Cognito.
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
    , lipursIdentityPoolUsages
    , lipursCount
    , lipursNextToken
    , lipursMaxResults
    , lipursStatus
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
    { _lipuNextToken  :: !(Maybe Text)
    , _lipuMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
-- * 'lipursIdentityPoolUsages'
--
-- * 'lipursCount'
--
-- * 'lipursNextToken'
--
-- * 'lipursMaxResults'
--
-- * 'lipursStatus'
data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse'
    { _lipursIdentityPoolUsages :: !(Maybe [IdentityPoolUsage])
    , _lipursCount              :: !(Maybe Int)
    , _lipursNextToken          :: !(Maybe Text)
    , _lipursMaxResults         :: !(Maybe Int)
    , _lipursStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentityPoolUsageResponse' smart constructor.
listIdentityPoolUsageResponse :: Int -> ListIdentityPoolUsageResponse
listIdentityPoolUsageResponse pStatus_ =
    ListIdentityPoolUsageResponse'
    { _lipursIdentityPoolUsages = Nothing
    , _lipursCount = Nothing
    , _lipursNextToken = Nothing
    , _lipursMaxResults = Nothing
    , _lipursStatus = pStatus_
    }

-- | Usage information for the identity pools.
lipursIdentityPoolUsages :: Lens' ListIdentityPoolUsageResponse [IdentityPoolUsage]
lipursIdentityPoolUsages = lens _lipursIdentityPoolUsages (\ s a -> s{_lipursIdentityPoolUsages = a}) . _Default . _Coerce;

-- | Total number of identities for the identity pool.
lipursCount :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipursCount = lens _lipursCount (\ s a -> s{_lipursCount = a});

-- | A pagination token for obtaining the next page of results.
lipursNextToken :: Lens' ListIdentityPoolUsageResponse (Maybe Text)
lipursNextToken = lens _lipursNextToken (\ s a -> s{_lipursNextToken = a});

-- | The maximum number of results to be returned.
lipursMaxResults :: Lens' ListIdentityPoolUsageResponse (Maybe Int)
lipursMaxResults = lens _lipursMaxResults (\ s a -> s{_lipursMaxResults = a});

-- | FIXME: Undocumented member.
lipursStatus :: Lens' ListIdentityPoolUsageResponse Int
lipursStatus = lens _lipursStatus (\ s a -> s{_lipursStatus = a});
