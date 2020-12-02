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
-- Module      : Network.AWS.CognitoIdentityProvider.GlobalSignOut
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs out users from all devices.
--
--
module Network.AWS.CognitoIdentityProvider.GlobalSignOut
    (
    -- * Creating a Request
      globalSignOut
    , GlobalSignOut
    -- * Request Lenses
    , gsoAccessToken

    -- * Destructuring the Response
    , globalSignOutResponse
    , GlobalSignOutResponse
    -- * Response Lenses
    , gsorsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to sign out all devices.
--
--
--
-- /See:/ 'globalSignOut' smart constructor.
newtype GlobalSignOut = GlobalSignOut'
  { _gsoAccessToken :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalSignOut' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsoAccessToken' - The access token.
globalSignOut
    :: Text -- ^ 'gsoAccessToken'
    -> GlobalSignOut
globalSignOut pAccessToken_ =
  GlobalSignOut' {_gsoAccessToken = _Sensitive # pAccessToken_}


-- | The access token.
gsoAccessToken :: Lens' GlobalSignOut Text
gsoAccessToken = lens _gsoAccessToken (\ s a -> s{_gsoAccessToken = a}) . _Sensitive

instance AWSRequest GlobalSignOut where
        type Rs GlobalSignOut = GlobalSignOutResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 GlobalSignOutResponse' <$> (pure (fromEnum s)))

instance Hashable GlobalSignOut where

instance NFData GlobalSignOut where

instance ToHeaders GlobalSignOut where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GlobalSignOut" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GlobalSignOut where
        toJSON GlobalSignOut'{..}
          = object
              (catMaybes [Just ("AccessToken" .= _gsoAccessToken)])

instance ToPath GlobalSignOut where
        toPath = const "/"

instance ToQuery GlobalSignOut where
        toQuery = const mempty

-- | The response to the request to sign out all devices.
--
--
--
-- /See:/ 'globalSignOutResponse' smart constructor.
newtype GlobalSignOutResponse = GlobalSignOutResponse'
  { _gsorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlobalSignOutResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsorsResponseStatus' - -- | The response status code.
globalSignOutResponse
    :: Int -- ^ 'gsorsResponseStatus'
    -> GlobalSignOutResponse
globalSignOutResponse pResponseStatus_ =
  GlobalSignOutResponse' {_gsorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
gsorsResponseStatus :: Lens' GlobalSignOutResponse Int
gsorsResponseStatus = lens _gsorsResponseStatus (\ s a -> s{_gsorsResponseStatus = a})

instance NFData GlobalSignOutResponse where
