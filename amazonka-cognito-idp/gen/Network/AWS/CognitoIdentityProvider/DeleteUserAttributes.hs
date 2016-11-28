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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , duaAccessToken
    , duaUserAttributeNames

    -- * Destructuring the Response
    , deleteUserAttributesResponse
    , DeleteUserAttributesResponse
    -- * Response Lenses
    , duarsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to delete user attributes.
--
--
--
-- /See:/ 'deleteUserAttributes' smart constructor.
data DeleteUserAttributes = DeleteUserAttributes'
    { _duaAccessToken        :: !(Maybe (Sensitive Text))
    , _duaUserAttributeNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteUserAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duaAccessToken' - The access token used in the request to delete user attributes.
--
-- * 'duaUserAttributeNames' - An array of strings representing the user attribute names you wish to delete.
deleteUserAttributes
    :: DeleteUserAttributes
deleteUserAttributes =
    DeleteUserAttributes'
    { _duaAccessToken = Nothing
    , _duaUserAttributeNames = mempty
    }

-- | The access token used in the request to delete user attributes.
duaAccessToken :: Lens' DeleteUserAttributes (Maybe Text)
duaAccessToken = lens _duaAccessToken (\ s a -> s{_duaAccessToken = a}) . mapping _Sensitive;

-- | An array of strings representing the user attribute names you wish to delete.
duaUserAttributeNames :: Lens' DeleteUserAttributes [Text]
duaUserAttributeNames = lens _duaUserAttributeNames (\ s a -> s{_duaUserAttributeNames = a}) . _Coerce;

instance AWSRequest DeleteUserAttributes where
        type Rs DeleteUserAttributes =
             DeleteUserAttributesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteUserAttributesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteUserAttributes

instance NFData DeleteUserAttributes

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
                 [("AccessToken" .=) <$> _duaAccessToken,
                  Just
                    ("UserAttributeNames" .= _duaUserAttributeNames)])

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteUserAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duarsResponseStatus' - -- | The response status code.
deleteUserAttributesResponse
    :: Int -- ^ 'duarsResponseStatus'
    -> DeleteUserAttributesResponse
deleteUserAttributesResponse pResponseStatus_ =
    DeleteUserAttributesResponse'
    { _duarsResponseStatus = pResponseStatus_
    }

-- | -- | The response status code.
duarsResponseStatus :: Lens' DeleteUserAttributesResponse Int
duarsResponseStatus = lens _duarsResponseStatus (\ s a -> s{_duarsResponseStatus = a});

instance NFData DeleteUserAttributesResponse
