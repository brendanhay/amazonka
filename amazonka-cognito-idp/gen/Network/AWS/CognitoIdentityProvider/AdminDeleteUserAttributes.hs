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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the user attributes in a user pool as an administrator. Works on any user.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
    (
    -- * Creating a Request
      adminDeleteUserAttributes
    , AdminDeleteUserAttributes
    -- * Request Lenses
    , aduaUserPoolId
    , aduaUsername
    , aduaUserAttributeNames

    -- * Destructuring the Response
    , adminDeleteUserAttributesResponse
    , AdminDeleteUserAttributesResponse
    -- * Response Lenses
    , aduarsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to delete user attributes as an administrator.
--
--
--
-- /See:/ 'adminDeleteUserAttributes' smart constructor.
data AdminDeleteUserAttributes = AdminDeleteUserAttributes'
  { _aduaUserPoolId         :: !Text
  , _aduaUsername           :: !(Sensitive Text)
  , _aduaUserAttributeNames :: ![Text]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminDeleteUserAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aduaUserPoolId' - The user pool ID for the user pool where you want to delete user attributes.
--
-- * 'aduaUsername' - The user name of the user from which you would like to delete attributes.
--
-- * 'aduaUserAttributeNames' - An array of strings representing the user attribute names you wish to delete. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
adminDeleteUserAttributes
    :: Text -- ^ 'aduaUserPoolId'
    -> Text -- ^ 'aduaUsername'
    -> AdminDeleteUserAttributes
adminDeleteUserAttributes pUserPoolId_ pUsername_ =
  AdminDeleteUserAttributes'
    { _aduaUserPoolId = pUserPoolId_
    , _aduaUsername = _Sensitive # pUsername_
    , _aduaUserAttributeNames = mempty
    }


-- | The user pool ID for the user pool where you want to delete user attributes.
aduaUserPoolId :: Lens' AdminDeleteUserAttributes Text
aduaUserPoolId = lens _aduaUserPoolId (\ s a -> s{_aduaUserPoolId = a})

-- | The user name of the user from which you would like to delete attributes.
aduaUsername :: Lens' AdminDeleteUserAttributes Text
aduaUsername = lens _aduaUsername (\ s a -> s{_aduaUsername = a}) . _Sensitive

-- | An array of strings representing the user attribute names you wish to delete. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
aduaUserAttributeNames :: Lens' AdminDeleteUserAttributes [Text]
aduaUserAttributeNames = lens _aduaUserAttributeNames (\ s a -> s{_aduaUserAttributeNames = a}) . _Coerce

instance AWSRequest AdminDeleteUserAttributes where
        type Rs AdminDeleteUserAttributes =
             AdminDeleteUserAttributesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminDeleteUserAttributesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminDeleteUserAttributes where

instance NFData AdminDeleteUserAttributes where

instance ToHeaders AdminDeleteUserAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminDeleteUserAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminDeleteUserAttributes where
        toJSON AdminDeleteUserAttributes'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _aduaUserPoolId),
                  Just ("Username" .= _aduaUsername),
                  Just
                    ("UserAttributeNames" .= _aduaUserAttributeNames)])

instance ToPath AdminDeleteUserAttributes where
        toPath = const "/"

instance ToQuery AdminDeleteUserAttributes where
        toQuery = const mempty

-- | Represents the response received from the server for a request to delete user attributes.
--
--
--
-- /See:/ 'adminDeleteUserAttributesResponse' smart constructor.
newtype AdminDeleteUserAttributesResponse = AdminDeleteUserAttributesResponse'
  { _aduarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminDeleteUserAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aduarsResponseStatus' - -- | The response status code.
adminDeleteUserAttributesResponse
    :: Int -- ^ 'aduarsResponseStatus'
    -> AdminDeleteUserAttributesResponse
adminDeleteUserAttributesResponse pResponseStatus_ =
  AdminDeleteUserAttributesResponse' {_aduarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aduarsResponseStatus :: Lens' AdminDeleteUserAttributesResponse Int
aduarsResponseStatus = lens _aduarsResponseStatus (\ s a -> s{_aduarsResponseStatus = a})

instance NFData AdminDeleteUserAttributesResponse
         where
