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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserPoolClients
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the clients that have been created for the specified user pool.
--
--
module Network.AWS.CognitoIdentityProvider.ListUserPoolClients
    (
    -- * Creating a Request
      listUserPoolClients
    , ListUserPoolClients
    -- * Request Lenses
    , lupcNextToken
    , lupcMaxResults
    , lupcUserPoolId

    -- * Destructuring the Response
    , listUserPoolClientsResponse
    , ListUserPoolClientsResponse
    -- * Response Lenses
    , lupcrsNextToken
    , lupcrsUserPoolClients
    , lupcrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to list the user pool clients.
--
--
--
-- /See:/ 'listUserPoolClients' smart constructor.
data ListUserPoolClients = ListUserPoolClients'
  { _lupcNextToken  :: !(Maybe Text)
  , _lupcMaxResults :: !(Maybe Nat)
  , _lupcUserPoolId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserPoolClients' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lupcNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lupcMaxResults' - The maximum number of results you want the request to return when listing the user pool clients.
--
-- * 'lupcUserPoolId' - The user pool ID for the user pool where you want to list user pool clients.
listUserPoolClients
    :: Text -- ^ 'lupcUserPoolId'
    -> ListUserPoolClients
listUserPoolClients pUserPoolId_ =
  ListUserPoolClients'
    { _lupcNextToken = Nothing
    , _lupcMaxResults = Nothing
    , _lupcUserPoolId = pUserPoolId_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lupcNextToken :: Lens' ListUserPoolClients (Maybe Text)
lupcNextToken = lens _lupcNextToken (\ s a -> s{_lupcNextToken = a})

-- | The maximum number of results you want the request to return when listing the user pool clients.
lupcMaxResults :: Lens' ListUserPoolClients (Maybe Natural)
lupcMaxResults = lens _lupcMaxResults (\ s a -> s{_lupcMaxResults = a}) . mapping _Nat

-- | The user pool ID for the user pool where you want to list user pool clients.
lupcUserPoolId :: Lens' ListUserPoolClients Text
lupcUserPoolId = lens _lupcUserPoolId (\ s a -> s{_lupcUserPoolId = a})

instance AWSRequest ListUserPoolClients where
        type Rs ListUserPoolClients =
             ListUserPoolClientsResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ListUserPoolClientsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "UserPoolClients" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListUserPoolClients where

instance NFData ListUserPoolClients where

instance ToHeaders ListUserPoolClients where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ListUserPoolClients"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUserPoolClients where
        toJSON ListUserPoolClients'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lupcNextToken,
                  ("MaxResults" .=) <$> _lupcMaxResults,
                  Just ("UserPoolId" .= _lupcUserPoolId)])

instance ToPath ListUserPoolClients where
        toPath = const "/"

instance ToQuery ListUserPoolClients where
        toQuery = const mempty

-- | Represents the response from the server that lists user pool clients.
--
--
--
-- /See:/ 'listUserPoolClientsResponse' smart constructor.
data ListUserPoolClientsResponse = ListUserPoolClientsResponse'
  { _lupcrsNextToken       :: !(Maybe Text)
  , _lupcrsUserPoolClients :: !(Maybe [UserPoolClientDescription])
  , _lupcrsResponseStatus  :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserPoolClientsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lupcrsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lupcrsUserPoolClients' - The user pool clients in the response that lists user pool clients.
--
-- * 'lupcrsResponseStatus' - -- | The response status code.
listUserPoolClientsResponse
    :: Int -- ^ 'lupcrsResponseStatus'
    -> ListUserPoolClientsResponse
listUserPoolClientsResponse pResponseStatus_ =
  ListUserPoolClientsResponse'
    { _lupcrsNextToken = Nothing
    , _lupcrsUserPoolClients = Nothing
    , _lupcrsResponseStatus = pResponseStatus_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lupcrsNextToken :: Lens' ListUserPoolClientsResponse (Maybe Text)
lupcrsNextToken = lens _lupcrsNextToken (\ s a -> s{_lupcrsNextToken = a})

-- | The user pool clients in the response that lists user pool clients.
lupcrsUserPoolClients :: Lens' ListUserPoolClientsResponse [UserPoolClientDescription]
lupcrsUserPoolClients = lens _lupcrsUserPoolClients (\ s a -> s{_lupcrsUserPoolClients = a}) . _Default . _Coerce

-- | -- | The response status code.
lupcrsResponseStatus :: Lens' ListUserPoolClientsResponse Int
lupcrsResponseStatus = lens _lupcrsResponseStatus (\ s a -> s{_lupcrsResponseStatus = a})

instance NFData ListUserPoolClientsResponse where
