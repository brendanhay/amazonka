{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.ListIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the identities in a pool.
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
    , liNextToken
    , liIdentityPoolId
    , liMaxResults

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , listIdentitiesResponse
    -- ** Response lenses
    , lirIdentityPoolId
    , lirNextToken
    , lirIdentities
    , lirStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the ListIdentities action.
--
-- /See:/ 'listIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liHideDisabled'
--
-- * 'liNextToken'
--
-- * 'liIdentityPoolId'
--
-- * 'liMaxResults'
data ListIdentities = ListIdentities'
    { _liHideDisabled   :: !(Maybe Bool)
    , _liNextToken      :: !(Maybe Text)
    , _liIdentityPoolId :: !Text
    , _liMaxResults     :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentities' smart constructor.
listIdentities :: Text -> Natural -> ListIdentities
listIdentities pIdentityPoolId pMaxResults =
    ListIdentities'
    { _liHideDisabled = Nothing
    , _liNextToken = Nothing
    , _liIdentityPoolId = pIdentityPoolId
    , _liMaxResults = _Nat # pMaxResults
    }

-- | An optional boolean parameter that allows you to hide disabled
-- identities. If omitted, the ListIdentities API will include disabled
-- identities in the response.
liHideDisabled :: Lens' ListIdentities (Maybe Bool)
liHideDisabled = lens _liHideDisabled (\ s a -> s{_liHideDisabled = a});

-- | A pagination token.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a});

-- | An identity pool ID in the format REGION:GUID.
liIdentityPoolId :: Lens' ListIdentities Text
liIdentityPoolId = lens _liIdentityPoolId (\ s a -> s{_liIdentityPoolId = a});

-- | The maximum number of identities to return.
liMaxResults :: Lens' ListIdentities Natural
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . _Nat;

instance AWSRequest ListIdentities where
        type Sv ListIdentities = CognitoIdentity
        type Rs ListIdentities = ListIdentitiesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentitiesResponse' <$>
                   (x .?> "IdentityPoolId") <*> (x .?> "NextToken") <*>
                     (x .?> "Identities" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
               "NextToken" .= _liNextToken,
               "IdentityPoolId" .= _liIdentityPoolId,
               "MaxResults" .= _liMaxResults]

instance ToPath ListIdentities where
        toPath = const "/"

instance ToQuery ListIdentities where
        toQuery = const mempty

-- | The response to a ListIdentities request.
--
-- /See:/ 'listIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirIdentityPoolId'
--
-- * 'lirNextToken'
--
-- * 'lirIdentities'
--
-- * 'lirStatus'
data ListIdentitiesResponse = ListIdentitiesResponse'
    { _lirIdentityPoolId :: !(Maybe Text)
    , _lirNextToken      :: !(Maybe Text)
    , _lirIdentities     :: !(Maybe [IdentityDescription])
    , _lirStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentitiesResponse' smart constructor.
listIdentitiesResponse :: Int -> ListIdentitiesResponse
listIdentitiesResponse pStatus =
    ListIdentitiesResponse'
    { _lirIdentityPoolId = Nothing
    , _lirNextToken = Nothing
    , _lirIdentities = Nothing
    , _lirStatus = pStatus
    }

-- | An identity pool ID in the format REGION:GUID.
lirIdentityPoolId :: Lens' ListIdentitiesResponse (Maybe Text)
lirIdentityPoolId = lens _lirIdentityPoolId (\ s a -> s{_lirIdentityPoolId = a});

-- | A pagination token.
lirNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirNextToken = lens _lirNextToken (\ s a -> s{_lirNextToken = a});

-- | An object containing a set of identities and associated mappings.
lirIdentities :: Lens' ListIdentitiesResponse [IdentityDescription]
lirIdentities = lens _lirIdentities (\ s a -> s{_lirIdentities = a}) . _Default;

-- | FIXME: Undocumented member.
lirStatus :: Lens' ListIdentitiesResponse Int
lirStatus = lens _lirStatus (\ s a -> s{_lirStatus = a});
