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
-- Module      : Network.AWS.CognitoIdentityProvider.GetSigningCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This method takes a user pool ID, and returns the signing certificate.
--
--
module Network.AWS.CognitoIdentityProvider.GetSigningCertificate
    (
    -- * Creating a Request
      getSigningCertificate
    , GetSigningCertificate
    -- * Request Lenses
    , gscUserPoolId

    -- * Destructuring the Response
    , getSigningCertificateResponse
    , GetSigningCertificateResponse
    -- * Response Lenses
    , gscrsCertificate
    , gscrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to get a signing certificate from Cognito.
--
--
--
-- /See:/ 'getSigningCertificate' smart constructor.
newtype GetSigningCertificate = GetSigningCertificate'
  { _gscUserPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSigningCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscUserPoolId' - The user pool ID.
getSigningCertificate
    :: Text -- ^ 'gscUserPoolId'
    -> GetSigningCertificate
getSigningCertificate pUserPoolId_ =
  GetSigningCertificate' {_gscUserPoolId = pUserPoolId_}


-- | The user pool ID.
gscUserPoolId :: Lens' GetSigningCertificate Text
gscUserPoolId = lens _gscUserPoolId (\ s a -> s{_gscUserPoolId = a})

instance AWSRequest GetSigningCertificate where
        type Rs GetSigningCertificate =
             GetSigningCertificateResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetSigningCertificateResponse' <$>
                   (x .?> "Certificate") <*> (pure (fromEnum s)))

instance Hashable GetSigningCertificate where

instance NFData GetSigningCertificate where

instance ToHeaders GetSigningCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetSigningCertificate"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSigningCertificate where
        toJSON GetSigningCertificate'{..}
          = object
              (catMaybes [Just ("UserPoolId" .= _gscUserPoolId)])

instance ToPath GetSigningCertificate where
        toPath = const "/"

instance ToQuery GetSigningCertificate where
        toQuery = const mempty

-- | Response from Cognito for a signing certificate request.
--
--
--
-- /See:/ 'getSigningCertificateResponse' smart constructor.
data GetSigningCertificateResponse = GetSigningCertificateResponse'
  { _gscrsCertificate    :: !(Maybe Text)
  , _gscrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSigningCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscrsCertificate' - The signing certificate.
--
-- * 'gscrsResponseStatus' - -- | The response status code.
getSigningCertificateResponse
    :: Int -- ^ 'gscrsResponseStatus'
    -> GetSigningCertificateResponse
getSigningCertificateResponse pResponseStatus_ =
  GetSigningCertificateResponse'
    {_gscrsCertificate = Nothing, _gscrsResponseStatus = pResponseStatus_}


-- | The signing certificate.
gscrsCertificate :: Lens' GetSigningCertificateResponse (Maybe Text)
gscrsCertificate = lens _gscrsCertificate (\ s a -> s{_gscrsCertificate = a})

-- | -- | The response status code.
gscrsResponseStatus :: Lens' GetSigningCertificateResponse Int
gscrsResponseStatus = lens _gscrsResponseStatus (\ s a -> s{_gscrsResponseStatus = a})

instance NFData GetSigningCertificateResponse where
