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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Secure Sockets Layer (SSL) certificate for the custom domain for your user pool.
--
--
-- You can use this operation to provide the Amazon Resource Name (ARN) of a new certificate to Amazon Cognito. You cannot use it to change the domain for a user pool.
--
-- A custom domain is used to host the Amazon Cognito hosted UI, which provides sign-up and sign-in pages for your application. When you set up a custom domain, you provide a certificate that you manage with AWS Certificate Manager (ACM). When necessary, you can use this operation to change the certificate that you applied to your custom domain.
--
-- Usually, this is unnecessary following routine certificate renewal with ACM. When you renew your existing certificate in ACM, the ARN for your certificate remains the same, and your custom domain uses the new certificate automatically.
--
-- However, if you replace your existing certificate with a new one, ACM gives the new certificate a new ARN. To apply the new certificate to your custom domain, you must provide this ARN to Amazon Cognito.
--
-- When you add your new certificate in ACM, you must choose US East (N. Virginia) as the AWS Region.
--
-- After you submit your request, Amazon Cognito requires up to 1 hour to distribute your new certificate to your custom domain.
--
-- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
--
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
    (
    -- * Creating a Request
      updateUserPoolDomain
    , UpdateUserPoolDomain
    -- * Request Lenses
    , uupdDomain
    , uupdUserPoolId
    , uupdCustomDomainConfig

    -- * Destructuring the Response
    , updateUserPoolDomainResponse
    , UpdateUserPoolDomainResponse
    -- * Response Lenses
    , uupdrsCloudFrontDomain
    , uupdrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The UpdateUserPoolDomain request input.
--
--
--
-- /See:/ 'updateUserPoolDomain' smart constructor.
data UpdateUserPoolDomain = UpdateUserPoolDomain'
  { _uupdDomain             :: !Text
  , _uupdUserPoolId         :: !Text
  , _uupdCustomDomainConfig :: !CustomDomainConfigType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPoolDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupdDomain' - The domain name for the custom domain that hosts the sign-up and sign-in pages for your application. For example: @auth.example.com@ .  This string can include only lowercase letters, numbers, and hyphens. Do not use a hyphen for the first or last character. Use periods to separate subdomain names.
--
-- * 'uupdUserPoolId' - The ID of the user pool that is associated with the custom domain that you are updating the certificate for.
--
-- * 'uupdCustomDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in pages for your application. Use this object to specify an SSL certificate that is managed by ACM.
updateUserPoolDomain
    :: Text -- ^ 'uupdDomain'
    -> Text -- ^ 'uupdUserPoolId'
    -> CustomDomainConfigType -- ^ 'uupdCustomDomainConfig'
    -> UpdateUserPoolDomain
updateUserPoolDomain pDomain_ pUserPoolId_ pCustomDomainConfig_ =
  UpdateUserPoolDomain'
    { _uupdDomain = pDomain_
    , _uupdUserPoolId = pUserPoolId_
    , _uupdCustomDomainConfig = pCustomDomainConfig_
    }


-- | The domain name for the custom domain that hosts the sign-up and sign-in pages for your application. For example: @auth.example.com@ .  This string can include only lowercase letters, numbers, and hyphens. Do not use a hyphen for the first or last character. Use periods to separate subdomain names.
uupdDomain :: Lens' UpdateUserPoolDomain Text
uupdDomain = lens _uupdDomain (\ s a -> s{_uupdDomain = a})

-- | The ID of the user pool that is associated with the custom domain that you are updating the certificate for.
uupdUserPoolId :: Lens' UpdateUserPoolDomain Text
uupdUserPoolId = lens _uupdUserPoolId (\ s a -> s{_uupdUserPoolId = a})

-- | The configuration for a custom domain that hosts the sign-up and sign-in pages for your application. Use this object to specify an SSL certificate that is managed by ACM.
uupdCustomDomainConfig :: Lens' UpdateUserPoolDomain CustomDomainConfigType
uupdCustomDomainConfig = lens _uupdCustomDomainConfig (\ s a -> s{_uupdCustomDomainConfig = a})

instance AWSRequest UpdateUserPoolDomain where
        type Rs UpdateUserPoolDomain =
             UpdateUserPoolDomainResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserPoolDomainResponse' <$>
                   (x .?> "CloudFrontDomain") <*> (pure (fromEnum s)))

instance Hashable UpdateUserPoolDomain where

instance NFData UpdateUserPoolDomain where

instance ToHeaders UpdateUserPoolDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateUserPoolDomain"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserPoolDomain where
        toJSON UpdateUserPoolDomain'{..}
          = object
              (catMaybes
                 [Just ("Domain" .= _uupdDomain),
                  Just ("UserPoolId" .= _uupdUserPoolId),
                  Just
                    ("CustomDomainConfig" .= _uupdCustomDomainConfig)])

instance ToPath UpdateUserPoolDomain where
        toPath = const "/"

instance ToQuery UpdateUserPoolDomain where
        toQuery = const mempty

-- | The UpdateUserPoolDomain response output.
--
--
--
-- /See:/ 'updateUserPoolDomainResponse' smart constructor.
data UpdateUserPoolDomainResponse = UpdateUserPoolDomainResponse'
  { _uupdrsCloudFrontDomain :: !(Maybe Text)
  , _uupdrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupdrsCloudFrontDomain' - The Amazon CloudFront endpoint that Amazon Cognito set up when you added the custom domain to your user pool.
--
-- * 'uupdrsResponseStatus' - -- | The response status code.
updateUserPoolDomainResponse
    :: Int -- ^ 'uupdrsResponseStatus'
    -> UpdateUserPoolDomainResponse
updateUserPoolDomainResponse pResponseStatus_ =
  UpdateUserPoolDomainResponse'
    { _uupdrsCloudFrontDomain = Nothing
    , _uupdrsResponseStatus = pResponseStatus_
    }


-- | The Amazon CloudFront endpoint that Amazon Cognito set up when you added the custom domain to your user pool.
uupdrsCloudFrontDomain :: Lens' UpdateUserPoolDomainResponse (Maybe Text)
uupdrsCloudFrontDomain = lens _uupdrsCloudFrontDomain (\ s a -> s{_uupdrsCloudFrontDomain = a})

-- | -- | The response status code.
uupdrsResponseStatus :: Lens' UpdateUserPoolDomainResponse Int
uupdrsResponseStatus = lens _uupdrsResponseStatus (\ s a -> s{_uupdrsResponseStatus = a})

instance NFData UpdateUserPoolDomainResponse where
