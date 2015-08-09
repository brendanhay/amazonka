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
-- Module      : Network.AWS.CognitoIdentity.ListIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the identities in a pool.
--
-- You must use AWS Developer credentials to call this API.
--
-- /See:/ <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_ListIdentities.html AWS API Reference> for ListIdentities.
module Network.AWS.CognitoIdentity.ListIdentities
    (
    -- * Creating a Request
      ListIdentities
    , listIdentities
    -- * Request Lenses
    , liHideDisabled
    , liNextToken
    , liIdentityPoolId
    , liMaxResults

    -- * Destructuring the Response
    , ListIdentitiesResponse
    , listIdentitiesResponse
    -- * Response Lenses
    , lirsIdentityPoolId
    , lirsNextToken
    , lirsIdentities
    , lirsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.CognitoIdentity.Types.Product
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
listIdentities pIdentityPoolId_ pMaxResults_ =
    ListIdentities'
    { _liHideDisabled = Nothing
    , _liNextToken = Nothing
    , _liIdentityPoolId = pIdentityPoolId_
    , _liMaxResults = _Nat # pMaxResults_
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
-- * 'lirsIdentityPoolId'
--
-- * 'lirsNextToken'
--
-- * 'lirsIdentities'
--
-- * 'lirsStatus'
data ListIdentitiesResponse = ListIdentitiesResponse'
    { _lirsIdentityPoolId :: !(Maybe Text)
    , _lirsNextToken      :: !(Maybe Text)
    , _lirsIdentities     :: !(Maybe [IdentityDescription])
    , _lirsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListIdentitiesResponse' smart constructor.
listIdentitiesResponse :: Int -> ListIdentitiesResponse
listIdentitiesResponse pStatus_ =
    ListIdentitiesResponse'
    { _lirsIdentityPoolId = Nothing
    , _lirsNextToken = Nothing
    , _lirsIdentities = Nothing
    , _lirsStatus = pStatus_
    }

-- | An identity pool ID in the format REGION:GUID.
lirsIdentityPoolId :: Lens' ListIdentitiesResponse (Maybe Text)
lirsIdentityPoolId = lens _lirsIdentityPoolId (\ s a -> s{_lirsIdentityPoolId = a});

-- | A pagination token.
lirsNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a});

-- | An object containing a set of identities and associated mappings.
lirsIdentities :: Lens' ListIdentitiesResponse [IdentityDescription]
lirsIdentities = lens _lirsIdentities (\ s a -> s{_lirsIdentities = a}) . _Default . _Coerce;

-- | Undocumented member.
lirsStatus :: Lens' ListIdentitiesResponse Int
lirsStatus = lens _lirsStatus (\ s a -> s{_lirsStatus = a});
