{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , lipNextToken
    , lipMaxResults

    -- * Response
    , ListIdentityPoolsResponse
    -- ** Response constructor
    , listIdentityPoolsResponse
    -- ** Response lenses
    , liprIdentityPools
    , liprNextToken
    , liprStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the ListIdentityPools action.
--
-- /See:/ 'listIdentityPools' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lipNextToken'
--
-- * 'lipMaxResults'
data ListIdentityPools = ListIdentityPools'
    { _lipNextToken  :: !(Maybe Text)
    , _lipMaxResults :: !Nat
    } deriving (Eq,Read,Show)

-- | 'ListIdentityPools' smart constructor.
listIdentityPools :: Natural -> ListIdentityPools
listIdentityPools pMaxResults =
    ListIdentityPools'
    { _lipNextToken = Nothing
    , _lipMaxResults = _Nat # pMaxResults
    }

-- | A pagination token.
lipNextToken :: Lens' ListIdentityPools (Maybe Text)
lipNextToken = lens _lipNextToken (\ s a -> s{_lipNextToken = a});

-- | The maximum number of identities to return.
lipMaxResults :: Lens' ListIdentityPools Natural
lipMaxResults = lens _lipMaxResults (\ s a -> s{_lipMaxResults = a}) . _Nat;

instance AWSRequest ListIdentityPools where
        type Sv ListIdentityPools = CognitoIdentity
        type Rs ListIdentityPools = ListIdentityPoolsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentityPoolsResponse' <$>
                   (x .?> "IdentityPools" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

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
              ["NextToken" .= _lipNextToken,
               "MaxResults" .= _lipMaxResults]

instance ToPath ListIdentityPools where
        toPath = const "/"

instance ToQuery ListIdentityPools where
        toQuery = const mempty

-- | The result of a successful ListIdentityPools action.
--
-- /See:/ 'listIdentityPoolsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liprIdentityPools'
--
-- * 'liprNextToken'
--
-- * 'liprStatus'
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'
    { _liprIdentityPools :: !(Maybe [IdentityPoolShortDescription])
    , _liprNextToken     :: !(Maybe Text)
    , _liprStatus        :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListIdentityPoolsResponse' smart constructor.
listIdentityPoolsResponse :: Int -> ListIdentityPoolsResponse
listIdentityPoolsResponse pStatus =
    ListIdentityPoolsResponse'
    { _liprIdentityPools = Nothing
    , _liprNextToken = Nothing
    , _liprStatus = pStatus
    }

-- | The identity pools returned by the ListIdentityPools action.
liprIdentityPools :: Lens' ListIdentityPoolsResponse [IdentityPoolShortDescription]
liprIdentityPools = lens _liprIdentityPools (\ s a -> s{_liprIdentityPools = a}) . _Default;

-- | A pagination token.
liprNextToken :: Lens' ListIdentityPoolsResponse (Maybe Text)
liprNextToken = lens _liprNextToken (\ s a -> s{_liprNextToken = a});

-- | FIXME: Undocumented member.
liprStatus :: Lens' ListIdentityPoolsResponse Int
liprStatus = lens _liprStatus (\ s a -> s{_liprStatus = a});
