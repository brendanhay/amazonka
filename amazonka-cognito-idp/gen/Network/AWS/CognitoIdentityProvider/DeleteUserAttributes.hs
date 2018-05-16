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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the attributes for a user.
--
--
module Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
    (
    -- * Creating a Request
      deleteUserAttributes
    , DeleteUserAttributes
    -- * Request Lenses
    , duaUserAttributeNames
    , duaAccessToken

    -- * Destructuring the Response
    , deleteUserAttributesResponse
    , DeleteUserAttributesResponse
    -- * Response Lenses
    , duarsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to delete user attributes.
--
--
--
-- /See:/ 'deleteUserAttributes' smart constructor.
data DeleteUserAttributes = DeleteUserAttributes'
  { _duaUserAttributeNames :: ![Text]
  , _duaAccessToken        :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duaUserAttributeNames' - An array of strings representing the user attribute names you wish to delete. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- * 'duaAccessToken' - The access token used in the request to delete user attributes.
deleteUserAttributes
    :: Text -- ^ 'duaAccessToken'
    -> DeleteUserAttributes
deleteUserAttributes pAccessToken_ =
  DeleteUserAttributes'
    { _duaUserAttributeNames = mempty
    , _duaAccessToken = _Sensitive # pAccessToken_
    }


-- | An array of strings representing the user attribute names you wish to delete. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
duaUserAttributeNames :: Lens' DeleteUserAttributes [Text]
duaUserAttributeNames = lens _duaUserAttributeNames (\ s a -> s{_duaUserAttributeNames = a}) . _Coerce

-- | The access token used in the request to delete user attributes.
duaAccessToken :: Lens' DeleteUserAttributes Text
duaAccessToken = lens _duaAccessToken (\ s a -> s{_duaAccessToken = a}) . _Sensitive

instance AWSRequest DeleteUserAttributes where
        type Rs DeleteUserAttributes =
             DeleteUserAttributesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteUserAttributesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteUserAttributes where

instance NFData DeleteUserAttributes where

instance ToHeaders DeleteUserAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteUserAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUserAttributes where
        toJSON DeleteUserAttributes'{..}
          = object
              (catMaybes
                 [Just
                    ("UserAttributeNames" .= _duaUserAttributeNames),
                  Just ("AccessToken" .= _duaAccessToken)])

instance ToPath DeleteUserAttributes where
        toPath = const "/"

instance ToQuery DeleteUserAttributes where
        toQuery = const mempty

-- | Represents the response from the server to delete user attributes.
--
--
--
-- /See:/ 'deleteUserAttributesResponse' smart constructor.
newtype DeleteUserAttributesResponse = DeleteUserAttributesResponse'
  { _duarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duarsResponseStatus' - -- | The response status code.
deleteUserAttributesResponse
    :: Int -- ^ 'duarsResponseStatus'
    -> DeleteUserAttributesResponse
deleteUserAttributesResponse pResponseStatus_ =
  DeleteUserAttributesResponse' {_duarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
duarsResponseStatus :: Lens' DeleteUserAttributesResponse Int
duarsResponseStatus = lens _duarsResponseStatus (\ s a -> s{_duarsResponseStatus = a})

instance NFData DeleteUserAttributesResponse where
