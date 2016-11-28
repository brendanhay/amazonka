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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to update a specific attribute (one at a time).
--
--
module Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
    (
    -- * Creating a Request
      updateUserAttributes
    , UpdateUserAttributes
    -- * Request Lenses
    , uuaAccessToken
    , uuaUserAttributes

    -- * Destructuring the Response
    , updateUserAttributesResponse
    , UpdateUserAttributesResponse
    -- * Response Lenses
    , uuarsCodeDeliveryDetailsList
    , uuarsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to update user attributes.
--
--
--
-- /See:/ 'updateUserAttributes' smart constructor.
data UpdateUserAttributes = UpdateUserAttributes'
    { _uuaAccessToken    :: !(Maybe (Sensitive Text))
    , _uuaUserAttributes :: ![AttributeType]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateUserAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuaAccessToken' - The access token for the request to update user attributes.
--
-- * 'uuaUserAttributes' - An array of name-value pairs representing user attributes.
updateUserAttributes
    :: UpdateUserAttributes
updateUserAttributes =
    UpdateUserAttributes'
    { _uuaAccessToken = Nothing
    , _uuaUserAttributes = mempty
    }

-- | The access token for the request to update user attributes.
uuaAccessToken :: Lens' UpdateUserAttributes (Maybe Text)
uuaAccessToken = lens _uuaAccessToken (\ s a -> s{_uuaAccessToken = a}) . mapping _Sensitive;

-- | An array of name-value pairs representing user attributes.
uuaUserAttributes :: Lens' UpdateUserAttributes [AttributeType]
uuaUserAttributes = lens _uuaUserAttributes (\ s a -> s{_uuaUserAttributes = a}) . _Coerce;

instance AWSRequest UpdateUserAttributes where
        type Rs UpdateUserAttributes =
             UpdateUserAttributesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserAttributesResponse' <$>
                   (x .?> "CodeDeliveryDetailsList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable UpdateUserAttributes

instance NFData UpdateUserAttributes

instance ToHeaders UpdateUserAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateUserAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserAttributes where
        toJSON UpdateUserAttributes'{..}
          = object
              (catMaybes
                 [("AccessToken" .=) <$> _uuaAccessToken,
                  Just ("UserAttributes" .= _uuaUserAttributes)])

instance ToPath UpdateUserAttributes where
        toPath = const "/"

instance ToQuery UpdateUserAttributes where
        toQuery = const mempty

-- | Represents the response from the server for the request to update user attributes.
--
--
--
-- /See:/ 'updateUserAttributesResponse' smart constructor.
data UpdateUserAttributesResponse = UpdateUserAttributesResponse'
    { _uuarsCodeDeliveryDetailsList :: !(Maybe [CodeDeliveryDetailsType])
    , _uuarsResponseStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateUserAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuarsCodeDeliveryDetailsList' - The code delivery details list from the server for the request to update user attributes.
--
-- * 'uuarsResponseStatus' - -- | The response status code.
updateUserAttributesResponse
    :: Int -- ^ 'uuarsResponseStatus'
    -> UpdateUserAttributesResponse
updateUserAttributesResponse pResponseStatus_ =
    UpdateUserAttributesResponse'
    { _uuarsCodeDeliveryDetailsList = Nothing
    , _uuarsResponseStatus = pResponseStatus_
    }

-- | The code delivery details list from the server for the request to update user attributes.
uuarsCodeDeliveryDetailsList :: Lens' UpdateUserAttributesResponse [CodeDeliveryDetailsType]
uuarsCodeDeliveryDetailsList = lens _uuarsCodeDeliveryDetailsList (\ s a -> s{_uuarsCodeDeliveryDetailsList = a}) . _Default . _Coerce;

-- | -- | The response status code.
uuarsResponseStatus :: Lens' UpdateUserAttributesResponse Int
uuarsResponseStatus = lens _uuarsResponseStatus (\ s a -> s{_uuarsResponseStatus = a});

instance NFData UpdateUserAttributesResponse
