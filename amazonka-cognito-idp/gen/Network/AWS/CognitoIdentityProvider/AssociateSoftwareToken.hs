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
-- Module      : Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique generated shared secret key code for the user account. The request takes an access token or a session string, but not both.
--
--
module Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
    (
    -- * Creating a Request
      associateSoftwareToken
    , AssociateSoftwareToken
    -- * Request Lenses
    , astAccessToken
    , astSession

    -- * Destructuring the Response
    , associateSoftwareTokenResponse
    , AssociateSoftwareTokenResponse
    -- * Response Lenses
    , astrsSecretCode
    , astrsSession
    , astrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { _astAccessToken :: !(Maybe (Sensitive Text))
  , _astSession     :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSoftwareToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'astAccessToken' - The access token.
--
-- * 'astSession' - The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
associateSoftwareToken
    :: AssociateSoftwareToken
associateSoftwareToken =
  AssociateSoftwareToken' {_astAccessToken = Nothing, _astSession = Nothing}


-- | The access token.
astAccessToken :: Lens' AssociateSoftwareToken (Maybe Text)
astAccessToken = lens _astAccessToken (\ s a -> s{_astAccessToken = a}) . mapping _Sensitive

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
astSession :: Lens' AssociateSoftwareToken (Maybe Text)
astSession = lens _astSession (\ s a -> s{_astSession = a})

instance AWSRequest AssociateSoftwareToken where
        type Rs AssociateSoftwareToken =
             AssociateSoftwareTokenResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AssociateSoftwareTokenResponse' <$>
                   (x .?> "SecretCode") <*> (x .?> "Session") <*>
                     (pure (fromEnum s)))

instance Hashable AssociateSoftwareToken where

instance NFData AssociateSoftwareToken where

instance ToHeaders AssociateSoftwareToken where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AssociateSoftwareToken"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateSoftwareToken where
        toJSON AssociateSoftwareToken'{..}
          = object
              (catMaybes
                 [("AccessToken" .=) <$> _astAccessToken,
                  ("Session" .=) <$> _astSession])

instance ToPath AssociateSoftwareToken where
        toPath = const "/"

instance ToQuery AssociateSoftwareToken where
        toQuery = const mempty

-- | /See:/ 'associateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { _astrsSecretCode     :: !(Maybe (Sensitive Text))
  , _astrsSession        :: !(Maybe Text)
  , _astrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateSoftwareTokenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'astrsSecretCode' - A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
--
-- * 'astrsSession' - The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
--
-- * 'astrsResponseStatus' - -- | The response status code.
associateSoftwareTokenResponse
    :: Int -- ^ 'astrsResponseStatus'
    -> AssociateSoftwareTokenResponse
associateSoftwareTokenResponse pResponseStatus_ =
  AssociateSoftwareTokenResponse'
    { _astrsSecretCode = Nothing
    , _astrsSession = Nothing
    , _astrsResponseStatus = pResponseStatus_
    }


-- | A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
astrsSecretCode :: Lens' AssociateSoftwareTokenResponse (Maybe Text)
astrsSecretCode = lens _astrsSecretCode (\ s a -> s{_astrsSecretCode = a}) . mapping _Sensitive

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
astrsSession :: Lens' AssociateSoftwareTokenResponse (Maybe Text)
astrsSession = lens _astrsSession (\ s a -> s{_astrsSession = a})

-- | -- | The response status code.
astrsResponseStatus :: Lens' AssociateSoftwareTokenResponse Int
astrsResponseStatus = lens _astrsResponseStatus (\ s a -> s{_astrsResponseStatus = a})

instance NFData AssociateSoftwareTokenResponse where
