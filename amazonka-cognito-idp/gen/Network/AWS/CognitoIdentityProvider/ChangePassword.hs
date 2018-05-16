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
-- Module      : Network.AWS.CognitoIdentityProvider.ChangePassword
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for a specified user in a user pool.
--
--
module Network.AWS.CognitoIdentityProvider.ChangePassword
    (
    -- * Creating a Request
      changePassword
    , ChangePassword
    -- * Request Lenses
    , cpPreviousPassword
    , cpProposedPassword
    , cpAccessToken

    -- * Destructuring the Response
    , changePasswordResponse
    , ChangePasswordResponse
    -- * Response Lenses
    , cprsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to change a user password.
--
--
--
-- /See:/ 'changePassword' smart constructor.
data ChangePassword = ChangePassword'
  { _cpPreviousPassword :: !(Sensitive Text)
  , _cpProposedPassword :: !(Sensitive Text)
  , _cpAccessToken      :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangePassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPreviousPassword' - The old password.
--
-- * 'cpProposedPassword' - The new password.
--
-- * 'cpAccessToken' - The access token.
changePassword
    :: Text -- ^ 'cpPreviousPassword'
    -> Text -- ^ 'cpProposedPassword'
    -> Text -- ^ 'cpAccessToken'
    -> ChangePassword
changePassword pPreviousPassword_ pProposedPassword_ pAccessToken_ =
  ChangePassword'
    { _cpPreviousPassword = _Sensitive # pPreviousPassword_
    , _cpProposedPassword = _Sensitive # pProposedPassword_
    , _cpAccessToken = _Sensitive # pAccessToken_
    }


-- | The old password.
cpPreviousPassword :: Lens' ChangePassword Text
cpPreviousPassword = lens _cpPreviousPassword (\ s a -> s{_cpPreviousPassword = a}) . _Sensitive

-- | The new password.
cpProposedPassword :: Lens' ChangePassword Text
cpProposedPassword = lens _cpProposedPassword (\ s a -> s{_cpProposedPassword = a}) . _Sensitive

-- | The access token.
cpAccessToken :: Lens' ChangePassword Text
cpAccessToken = lens _cpAccessToken (\ s a -> s{_cpAccessToken = a}) . _Sensitive

instance AWSRequest ChangePassword where
        type Rs ChangePassword = ChangePasswordResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 ChangePasswordResponse' <$> (pure (fromEnum s)))

instance Hashable ChangePassword where

instance NFData ChangePassword where

instance ToHeaders ChangePassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ChangePassword"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ChangePassword where
        toJSON ChangePassword'{..}
          = object
              (catMaybes
                 [Just ("PreviousPassword" .= _cpPreviousPassword),
                  Just ("ProposedPassword" .= _cpProposedPassword),
                  Just ("AccessToken" .= _cpAccessToken)])

instance ToPath ChangePassword where
        toPath = const "/"

instance ToQuery ChangePassword where
        toQuery = const mempty

-- | The response from the server to the change password request.
--
--
--
-- /See:/ 'changePasswordResponse' smart constructor.
newtype ChangePasswordResponse = ChangePasswordResponse'
  { _cprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangePasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsResponseStatus' - -- | The response status code.
changePasswordResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> ChangePasswordResponse
changePasswordResponse pResponseStatus_ =
  ChangePasswordResponse' {_cprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cprsResponseStatus :: Lens' ChangePasswordResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData ChangePasswordResponse where
