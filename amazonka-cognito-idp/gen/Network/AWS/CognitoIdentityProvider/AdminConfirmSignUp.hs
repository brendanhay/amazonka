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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms user registration as an admin without using a confirmation code. Works on any user.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
    (
    -- * Creating a Request
      adminConfirmSignUp
    , AdminConfirmSignUp
    -- * Request Lenses
    , acsuUserPoolId
    , acsuUsername

    -- * Destructuring the Response
    , adminConfirmSignUpResponse
    , AdminConfirmSignUpResponse
    -- * Response Lenses
    , acsursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to confirm user registration.
--
--
--
-- /See:/ 'adminConfirmSignUp' smart constructor.
data AdminConfirmSignUp = AdminConfirmSignUp'
  { _acsuUserPoolId :: !Text
  , _acsuUsername   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminConfirmSignUp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsuUserPoolId' - The user pool ID for which you want to confirm user registration.
--
-- * 'acsuUsername' - The user name for which you want to confirm user registration.
adminConfirmSignUp
    :: Text -- ^ 'acsuUserPoolId'
    -> Text -- ^ 'acsuUsername'
    -> AdminConfirmSignUp
adminConfirmSignUp pUserPoolId_ pUsername_ =
  AdminConfirmSignUp'
    {_acsuUserPoolId = pUserPoolId_, _acsuUsername = _Sensitive # pUsername_}


-- | The user pool ID for which you want to confirm user registration.
acsuUserPoolId :: Lens' AdminConfirmSignUp Text
acsuUserPoolId = lens _acsuUserPoolId (\ s a -> s{_acsuUserPoolId = a})

-- | The user name for which you want to confirm user registration.
acsuUsername :: Lens' AdminConfirmSignUp Text
acsuUsername = lens _acsuUsername (\ s a -> s{_acsuUsername = a}) . _Sensitive

instance AWSRequest AdminConfirmSignUp where
        type Rs AdminConfirmSignUp =
             AdminConfirmSignUpResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminConfirmSignUpResponse' <$> (pure (fromEnum s)))

instance Hashable AdminConfirmSignUp where

instance NFData AdminConfirmSignUp where

instance ToHeaders AdminConfirmSignUp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminConfirmSignUp"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminConfirmSignUp where
        toJSON AdminConfirmSignUp'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _acsuUserPoolId),
                  Just ("Username" .= _acsuUsername)])

instance ToPath AdminConfirmSignUp where
        toPath = const "/"

instance ToQuery AdminConfirmSignUp where
        toQuery = const mempty

-- | Represents the response from the server for the request to confirm registration.
--
--
--
-- /See:/ 'adminConfirmSignUpResponse' smart constructor.
newtype AdminConfirmSignUpResponse = AdminConfirmSignUpResponse'
  { _acsursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminConfirmSignUpResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsursResponseStatus' - -- | The response status code.
adminConfirmSignUpResponse
    :: Int -- ^ 'acsursResponseStatus'
    -> AdminConfirmSignUpResponse
adminConfirmSignUpResponse pResponseStatus_ =
  AdminConfirmSignUpResponse' {_acsursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
acsursResponseStatus :: Lens' AdminConfirmSignUpResponse Int
acsursResponseStatus = lens _acsursResponseStatus (\ s a -> s{_acsursResponseStatus = a})

instance NFData AdminConfirmSignUpResponse where
