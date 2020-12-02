{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain for a user pool.
module Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
  ( -- * Creating a Request
    createUserPoolDomain,
    CreateUserPoolDomain,

    -- * Request Lenses
    cupdCustomDomainConfig,
    cupdDomain,
    cupdUserPoolId,

    -- * Destructuring the Response
    createUserPoolDomainResponse,
    CreateUserPoolDomainResponse,

    -- * Response Lenses
    cupdrsCloudFrontDomain,
    cupdrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUserPoolDomain' smart constructor.
data CreateUserPoolDomain = CreateUserPoolDomain'
  { _cupdCustomDomainConfig ::
      !(Maybe CustomDomainConfigType),
    _cupdDomain :: !Text,
    _cupdUserPoolId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserPoolDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupdCustomDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application. Provide this parameter only if you want to use a custom domain for your user pool. Otherwise, you can exclude this parameter and use the Amazon Cognito hosted domain instead. For more information about the hosted domain and custom domains, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-assign-domain.html Configuring a User Pool Domain> .
--
-- * 'cupdDomain' - The domain string.
--
-- * 'cupdUserPoolId' - The user pool ID.
createUserPoolDomain ::
  -- | 'cupdDomain'
  Text ->
  -- | 'cupdUserPoolId'
  Text ->
  CreateUserPoolDomain
createUserPoolDomain pDomain_ pUserPoolId_ =
  CreateUserPoolDomain'
    { _cupdCustomDomainConfig = Nothing,
      _cupdDomain = pDomain_,
      _cupdUserPoolId = pUserPoolId_
    }

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application. Provide this parameter only if you want to use a custom domain for your user pool. Otherwise, you can exclude this parameter and use the Amazon Cognito hosted domain instead. For more information about the hosted domain and custom domains, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-assign-domain.html Configuring a User Pool Domain> .
cupdCustomDomainConfig :: Lens' CreateUserPoolDomain (Maybe CustomDomainConfigType)
cupdCustomDomainConfig = lens _cupdCustomDomainConfig (\s a -> s {_cupdCustomDomainConfig = a})

-- | The domain string.
cupdDomain :: Lens' CreateUserPoolDomain Text
cupdDomain = lens _cupdDomain (\s a -> s {_cupdDomain = a})

-- | The user pool ID.
cupdUserPoolId :: Lens' CreateUserPoolDomain Text
cupdUserPoolId = lens _cupdUserPoolId (\s a -> s {_cupdUserPoolId = a})

instance AWSRequest CreateUserPoolDomain where
  type Rs CreateUserPoolDomain = CreateUserPoolDomainResponse
  request = postJSON cognitoIdentityProvider
  response =
    receiveJSON
      ( \s h x ->
          CreateUserPoolDomainResponse'
            <$> (x .?> "CloudFrontDomain") <*> (pure (fromEnum s))
      )

instance Hashable CreateUserPoolDomain

instance NFData CreateUserPoolDomain

instance ToHeaders CreateUserPoolDomain where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSCognitoIdentityProviderService.CreateUserPoolDomain" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateUserPoolDomain where
  toJSON CreateUserPoolDomain' {..} =
    object
      ( catMaybes
          [ ("CustomDomainConfig" .=) <$> _cupdCustomDomainConfig,
            Just ("Domain" .= _cupdDomain),
            Just ("UserPoolId" .= _cupdUserPoolId)
          ]
      )

instance ToPath CreateUserPoolDomain where
  toPath = const "/"

instance ToQuery CreateUserPoolDomain where
  toQuery = const mempty

-- | /See:/ 'createUserPoolDomainResponse' smart constructor.
data CreateUserPoolDomainResponse = CreateUserPoolDomainResponse'
  { _cupdrsCloudFrontDomain ::
      !(Maybe Text),
    _cupdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupdrsCloudFrontDomain' - The Amazon CloudFront endpoint that you use as the target of the alias that you set up with your Domain Name Service (DNS) provider.
--
-- * 'cupdrsResponseStatus' - -- | The response status code.
createUserPoolDomainResponse ::
  -- | 'cupdrsResponseStatus'
  Int ->
  CreateUserPoolDomainResponse
createUserPoolDomainResponse pResponseStatus_ =
  CreateUserPoolDomainResponse'
    { _cupdrsCloudFrontDomain = Nothing,
      _cupdrsResponseStatus = pResponseStatus_
    }

-- | The Amazon CloudFront endpoint that you use as the target of the alias that you set up with your Domain Name Service (DNS) provider.
cupdrsCloudFrontDomain :: Lens' CreateUserPoolDomainResponse (Maybe Text)
cupdrsCloudFrontDomain = lens _cupdrsCloudFrontDomain (\s a -> s {_cupdrsCloudFrontDomain = a})

-- | -- | The response status code.
cupdrsResponseStatus :: Lens' CreateUserPoolDomainResponse Int
cupdrsResponseStatus = lens _cupdrsResponseStatus (\s a -> s {_cupdrsResponseStatus = a})

instance NFData CreateUserPoolDomainResponse
