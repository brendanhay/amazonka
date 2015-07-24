{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the Cognito identity pools registered for your account.
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
    , liprsIdentityPools
    , liprsNextToken
    , liprsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentityPools' smart constructor.
listIdentityPools :: Natural -> ListIdentityPools
listIdentityPools pMaxResults_ =
    ListIdentityPools'
    { _lipNextToken = Nothing
    , _lipMaxResults = _Nat # pMaxResults_
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
-- * 'liprsIdentityPools'
--
-- * 'liprsNextToken'
--
-- * 'liprsStatus'
data ListIdentityPoolsResponse = ListIdentityPoolsResponse'
    { _liprsIdentityPools :: !(Maybe [IdentityPoolShortDescription])
    , _liprsNextToken     :: !(Maybe Text)
    , _liprsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentityPoolsResponse' smart constructor.
listIdentityPoolsResponse :: Int -> ListIdentityPoolsResponse
listIdentityPoolsResponse pStatus_ =
    ListIdentityPoolsResponse'
    { _liprsIdentityPools = Nothing
    , _liprsNextToken = Nothing
    , _liprsStatus = pStatus_
    }

-- | The identity pools returned by the ListIdentityPools action.
liprsIdentityPools :: Lens' ListIdentityPoolsResponse [IdentityPoolShortDescription]
liprsIdentityPools = lens _liprsIdentityPools (\ s a -> s{_liprsIdentityPools = a}) . _Default . _Coerce;

-- | A pagination token.
liprsNextToken :: Lens' ListIdentityPoolsResponse (Maybe Text)
liprsNextToken = lens _liprsNextToken (\ s a -> s{_liprsNextToken = a});

-- | FIXME: Undocumented member.
liprsStatus :: Lens' ListIdentityPoolsResponse Int
liprsStatus = lens _liprsStatus (\ s a -> s{_liprsStatus = a});
