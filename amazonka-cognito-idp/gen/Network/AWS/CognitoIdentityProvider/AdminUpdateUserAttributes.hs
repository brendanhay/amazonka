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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user's attributes, including developer attributes, as an administrator. Works on any user.
--
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- In addition to updating user attributes, this API can also be used to mark phone and email as verified.
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
    (
    -- * Creating a Request
      adminUpdateUserAttributes
    , AdminUpdateUserAttributes
    -- * Request Lenses
    , auuaUserPoolId
    , auuaUsername
    , auuaUserAttributes

    -- * Destructuring the Response
    , adminUpdateUserAttributesResponse
    , AdminUpdateUserAttributesResponse
    -- * Response Lenses
    , auuarsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to update the user's attributes as an administrator.
--
--
--
-- /See:/ 'adminUpdateUserAttributes' smart constructor.
data AdminUpdateUserAttributes = AdminUpdateUserAttributes'
  { _auuaUserPoolId     :: !Text
  , _auuaUsername       :: !(Sensitive Text)
  , _auuaUserAttributes :: ![AttributeType]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUpdateUserAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auuaUserPoolId' - The user pool ID for the user pool where you want to update user attributes.
--
-- * 'auuaUsername' - The user name of the user for whom you want to update user attributes.
--
-- * 'auuaUserAttributes' - An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
adminUpdateUserAttributes
    :: Text -- ^ 'auuaUserPoolId'
    -> Text -- ^ 'auuaUsername'
    -> AdminUpdateUserAttributes
adminUpdateUserAttributes pUserPoolId_ pUsername_ =
  AdminUpdateUserAttributes'
    { _auuaUserPoolId = pUserPoolId_
    , _auuaUsername = _Sensitive # pUsername_
    , _auuaUserAttributes = mempty
    }


-- | The user pool ID for the user pool where you want to update user attributes.
auuaUserPoolId :: Lens' AdminUpdateUserAttributes Text
auuaUserPoolId = lens _auuaUserPoolId (\ s a -> s{_auuaUserPoolId = a})

-- | The user name of the user for whom you want to update user attributes.
auuaUsername :: Lens' AdminUpdateUserAttributes Text
auuaUsername = lens _auuaUsername (\ s a -> s{_auuaUsername = a}) . _Sensitive

-- | An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
auuaUserAttributes :: Lens' AdminUpdateUserAttributes [AttributeType]
auuaUserAttributes = lens _auuaUserAttributes (\ s a -> s{_auuaUserAttributes = a}) . _Coerce

instance AWSRequest AdminUpdateUserAttributes where
        type Rs AdminUpdateUserAttributes =
             AdminUpdateUserAttributesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminUpdateUserAttributesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminUpdateUserAttributes where

instance NFData AdminUpdateUserAttributes where

instance ToHeaders AdminUpdateUserAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminUpdateUserAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminUpdateUserAttributes where
        toJSON AdminUpdateUserAttributes'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _auuaUserPoolId),
                  Just ("Username" .= _auuaUsername),
                  Just ("UserAttributes" .= _auuaUserAttributes)])

instance ToPath AdminUpdateUserAttributes where
        toPath = const "/"

instance ToQuery AdminUpdateUserAttributes where
        toQuery = const mempty

-- | Represents the response from the server for the request to update user attributes as an administrator.
--
--
--
-- /See:/ 'adminUpdateUserAttributesResponse' smart constructor.
newtype AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse'
  { _auuarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUpdateUserAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auuarsResponseStatus' - -- | The response status code.
adminUpdateUserAttributesResponse
    :: Int -- ^ 'auuarsResponseStatus'
    -> AdminUpdateUserAttributesResponse
adminUpdateUserAttributesResponse pResponseStatus_ =
  AdminUpdateUserAttributesResponse' {_auuarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
auuarsResponseStatus :: Lens' AdminUpdateUserAttributesResponse Int
auuarsResponseStatus = lens _auuarsResponseStatus (\ s a -> s{_auuarsResponseStatus = a})

instance NFData AdminUpdateUserAttributesResponse
         where
