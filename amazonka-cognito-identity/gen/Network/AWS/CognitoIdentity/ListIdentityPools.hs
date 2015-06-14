{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
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

-- | Lists all of the Cognito identity pools registered for your account.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_ListIdentityPools.html>
module Network.AWS.CognitoIdentity.ListIdentityPools
    (
    -- * Request
      ListIdentityPools
    -- ** Request constructor
    , listIdentityPools
    -- ** Request lenses
    , lipMaxResults
    , lipNextToken

    -- * Response
    , ListIdentityPoolsResponse
    -- ** Response constructor
    , listIdentityPoolsResponse
    -- ** Response lenses
    , liprIdentityPools
    , liprNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'listIdentityPools' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipMaxResults'
--
-- * 'lipNextToken'
data ListIdentityPools = ListIdentityPools'{_lipMaxResults :: Nat, _lipNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListIdentityPools' smart constructor.
listIdentityPools :: Natural -> Text -> ListIdentityPools
listIdentityPools pMaxResults pNextToken = ListIdentityPools'{_lipMaxResults = _Nat # pMaxResults, _lipNextToken = pNextToken};

-- | The maximum number of identities to return.
lipMaxResults :: Lens' ListIdentityPools Natural
lipMaxResults = lens _lipMaxResults (\ s a -> s{_lipMaxResults = a}) . _Nat;

-- | A pagination token.
lipNextToken :: Lens' ListIdentityPools Text
lipNextToken = lens _lipNextToken (\ s a -> s{_lipNextToken = a});

instance AWSRequest ListIdentityPools where
        type Sv ListIdentityPools = CognitoIdentity
        type Rs ListIdentityPools = ListIdentityPoolsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentityPoolsResponse' <$>
                   x .?> "IdentityPools" .!@ mempty <*>
                     x .:> "NextToken")

instance ToHeaders ListIdentityPools where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.ListIdentityPools" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListIdentityPools where
        toJSON ListIdentityPools'{..}
          = object
              ["MaxResults" .= _lipMaxResults,
               "NextToken" .= _lipNextToken]

instance ToPath ListIdentityPools where
        toPath = const "/"

instance ToQuery ListIdentityPools where
        toQuery = const mempty

-- | /See:/ 'listIdentityPoolsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liprIdentityPools'
--
-- * 'liprNextToken'
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'{_liprIdentityPools :: [IdentityPoolShortDescription], _liprNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListIdentityPoolsResponse' smart constructor.
listIdentityPoolsResponse :: Text -> ListIdentityPoolsResponse
listIdentityPoolsResponse pNextToken = ListIdentityPoolsResponse'{_liprIdentityPools = mempty, _liprNextToken = pNextToken};

-- | The identity pools returned by the ListIdentityPools action.
liprIdentityPools :: Lens' ListIdentityPoolsResponse [IdentityPoolShortDescription]
liprIdentityPools = lens _liprIdentityPools (\ s a -> s{_liprIdentityPools = a});

-- | A pagination token.
liprNextToken :: Lens' ListIdentityPoolsResponse Text
liprNextToken = lens _liprNextToken (\ s a -> s{_liprNextToken = a});
