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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the identities in a pool.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.ListIdentities
    (
    -- * Creating a Request
      listIdentities
    , ListIdentities
    -- * Request Lenses
    , liHideDisabled
    , liNextToken
    , liIdentityPoolId
    , liMaxResults

    -- * Destructuring the Response
    , listIdentitiesResponse
    , ListIdentitiesResponse
    -- * Response Lenses
    , lirsIdentityPoolId
    , lirsNextToken
    , lirsIdentities
    , lirsResponseStatus
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the ListIdentities action.
--
--
--
-- /See:/ 'listIdentities' smart constructor.
data ListIdentities = ListIdentities'
  { _liHideDisabled   :: !(Maybe Bool)
  , _liNextToken      :: !(Maybe Text)
  , _liIdentityPoolId :: !Text
  , _liMaxResults     :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liHideDisabled' - An optional boolean parameter that allows you to hide disabled identities. If omitted, the ListIdentities API will include disabled identities in the response.
--
-- * 'liNextToken' - A pagination token.
--
-- * 'liIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'liMaxResults' - The maximum number of identities to return.
listIdentities
    :: Text -- ^ 'liIdentityPoolId'
    -> Natural -- ^ 'liMaxResults'
    -> ListIdentities
listIdentities pIdentityPoolId_ pMaxResults_ =
  ListIdentities'
    { _liHideDisabled = Nothing
    , _liNextToken = Nothing
    , _liIdentityPoolId = pIdentityPoolId_
    , _liMaxResults = _Nat # pMaxResults_
    }


-- | An optional boolean parameter that allows you to hide disabled identities. If omitted, the ListIdentities API will include disabled identities in the response.
liHideDisabled :: Lens' ListIdentities (Maybe Bool)
liHideDisabled = lens _liHideDisabled (\ s a -> s{_liHideDisabled = a})

-- | A pagination token.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | An identity pool ID in the format REGION:GUID.
liIdentityPoolId :: Lens' ListIdentities Text
liIdentityPoolId = lens _liIdentityPoolId (\ s a -> s{_liIdentityPoolId = a})

-- | The maximum number of identities to return.
liMaxResults :: Lens' ListIdentities Natural
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . _Nat

instance AWSRequest ListIdentities where
        type Rs ListIdentities = ListIdentitiesResponse
        request = postJSON cognitoIdentity
        response
          = receiveJSON
              (\ s h x ->
                 ListIdentitiesResponse' <$>
                   (x .?> "IdentityPoolId") <*> (x .?> "NextToken") <*>
                     (x .?> "Identities" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListIdentities where

instance NFData ListIdentities where

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
              (catMaybes
                 [("HideDisabled" .=) <$> _liHideDisabled,
                  ("NextToken" .=) <$> _liNextToken,
                  Just ("IdentityPoolId" .= _liIdentityPoolId),
                  Just ("MaxResults" .= _liMaxResults)])

instance ToPath ListIdentities where
        toPath = const "/"

instance ToQuery ListIdentities where
        toQuery = const mempty

-- | The response to a ListIdentities request.
--
--
--
-- /See:/ 'listIdentitiesResponse' smart constructor.
data ListIdentitiesResponse = ListIdentitiesResponse'
  { _lirsIdentityPoolId :: !(Maybe Text)
  , _lirsNextToken      :: !(Maybe Text)
  , _lirsIdentities     :: !(Maybe [IdentityDescription])
  , _lirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIdentitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsIdentityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- * 'lirsNextToken' - A pagination token.
--
-- * 'lirsIdentities' - An object containing a set of identities and associated mappings.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listIdentitiesResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListIdentitiesResponse
listIdentitiesResponse pResponseStatus_ =
  ListIdentitiesResponse'
    { _lirsIdentityPoolId = Nothing
    , _lirsNextToken = Nothing
    , _lirsIdentities = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | An identity pool ID in the format REGION:GUID.
lirsIdentityPoolId :: Lens' ListIdentitiesResponse (Maybe Text)
lirsIdentityPoolId = lens _lirsIdentityPoolId (\ s a -> s{_lirsIdentityPoolId = a})

-- | A pagination token.
lirsNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | An object containing a set of identities and associated mappings.
lirsIdentities :: Lens' ListIdentitiesResponse [IdentityDescription]
lirsIdentities = lens _lirsIdentities (\ s a -> s{_lirsIdentities = a}) . _Default . _Coerce

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListIdentitiesResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListIdentitiesResponse where
