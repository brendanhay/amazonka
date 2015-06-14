{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.ListIdentities
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

-- | Lists the identities in a pool.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_ListIdentities.html>
module Network.AWS.CognitoIdentity.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , liHideDisabled
    , liIdentityPoolId
    , liMaxResults
    , liNextToken

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , listIdentitiesResponse
    -- ** Response lenses
    , lirIdentities
    , lirIdentityPoolId
    , lirNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'listIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liHideDisabled'
--
-- * 'liIdentityPoolId'
--
-- * 'liMaxResults'
--
-- * 'liNextToken'
data ListIdentities = ListIdentities'{_liHideDisabled :: Maybe Bool, _liIdentityPoolId :: Text, _liMaxResults :: Nat, _liNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListIdentities' smart constructor.
listIdentities :: Text -> Natural -> Text -> ListIdentities
listIdentities pIdentityPoolId pMaxResults pNextToken = ListIdentities'{_liHideDisabled = Nothing, _liIdentityPoolId = pIdentityPoolId, _liMaxResults = _Nat # pMaxResults, _liNextToken = pNextToken};

-- | An optional boolean parameter that allows you to hide disabled
-- identities. If omitted, the ListIdentities API will include disabled
-- identities in the response.
liHideDisabled :: Lens' ListIdentities (Maybe Bool)
liHideDisabled = lens _liHideDisabled (\ s a -> s{_liHideDisabled = a});

-- | An identity pool ID in the format REGION:GUID.
liIdentityPoolId :: Lens' ListIdentities Text
liIdentityPoolId = lens _liIdentityPoolId (\ s a -> s{_liIdentityPoolId = a});

-- | The maximum number of identities to return.
liMaxResults :: Lens' ListIdentities Natural
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . _Nat;

-- | A pagination token.
liNextToken :: Lens' ListIdentities Text
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a});

instance AWSRequest ListIdentities where
        type Sv ListIdentities = CognitoIdentity
        type Rs ListIdentities = ListIdentitiesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentitiesResponse' <$>
                   x .?> "Identities" .!@ mempty <*>
                     x .:> "IdentityPoolId"
                     <*> x .:> "NextToken")

instance ToHeaders ListIdentities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.ListIdentities" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListIdentities where
        toJSON ListIdentities'{..}
          = object
              ["HideDisabled" .= _liHideDisabled,
               "IdentityPoolId" .= _liIdentityPoolId,
               "MaxResults" .= _liMaxResults,
               "NextToken" .= _liNextToken]

instance ToPath ListIdentities where
        toPath = const "/"

instance ToQuery ListIdentities where
        toQuery = const mempty

-- | /See:/ 'listIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirIdentities'
--
-- * 'lirIdentityPoolId'
--
-- * 'lirNextToken'
data ListIdentitiesResponse = ListIdentitiesResponse'{_lirIdentities :: [IdentityDescription], _lirIdentityPoolId :: Text, _lirNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListIdentitiesResponse' smart constructor.
listIdentitiesResponse :: Text -> Text -> ListIdentitiesResponse
listIdentitiesResponse pIdentityPoolId pNextToken = ListIdentitiesResponse'{_lirIdentities = mempty, _lirIdentityPoolId = pIdentityPoolId, _lirNextToken = pNextToken};

-- | An object containing a set of identities and associated mappings.
lirIdentities :: Lens' ListIdentitiesResponse [IdentityDescription]
lirIdentities = lens _lirIdentities (\ s a -> s{_lirIdentities = a});

-- | An identity pool ID in the format REGION:GUID.
lirIdentityPoolId :: Lens' ListIdentitiesResponse Text
lirIdentityPoolId = lens _lirIdentityPoolId (\ s a -> s{_lirIdentityPoolId = a});

-- | A pagination token.
lirNextToken :: Lens' ListIdentitiesResponse Text
lirNextToken = lens _lirNextToken (\ s a -> s{_lirNextToken = a});
