{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    CreateUserPoolDomain (..),
    mkCreateUserPoolDomain,

    -- ** Request lenses
    cupdCustomDomainConfig,
    cupdDomain,
    cupdUserPoolId,

    -- * Destructuring the response
    CreateUserPoolDomainResponse (..),
    mkCreateUserPoolDomainResponse,

    -- ** Response lenses
    cupdrsCloudFrontDomain,
    cupdrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUserPoolDomain' smart constructor.
data CreateUserPoolDomain = CreateUserPoolDomain'
  { customDomainConfig ::
      Lude.Maybe CustomDomainConfigType,
    domain :: Lude.Text,
    userPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserPoolDomain' with the minimum fields required to make a request.
--
-- * 'customDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
-- Provide this parameter only if you want to use a custom domain for your user pool. Otherwise, you can exclude this parameter and use the Amazon Cognito hosted domain instead.
-- For more information about the hosted domain and custom domains, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-assign-domain.html Configuring a User Pool Domain> .
-- * 'domain' - The domain string.
-- * 'userPoolId' - The user pool ID.
mkCreateUserPoolDomain ::
  -- | 'domain'
  Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  CreateUserPoolDomain
mkCreateUserPoolDomain pDomain_ pUserPoolId_ =
  CreateUserPoolDomain'
    { customDomainConfig = Lude.Nothing,
      domain = pDomain_,
      userPoolId = pUserPoolId_
    }

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
-- Provide this parameter only if you want to use a custom domain for your user pool. Otherwise, you can exclude this parameter and use the Amazon Cognito hosted domain instead.
-- For more information about the hosted domain and custom domains, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-assign-domain.html Configuring a User Pool Domain> .
--
-- /Note:/ Consider using 'customDomainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupdCustomDomainConfig :: Lens.Lens' CreateUserPoolDomain (Lude.Maybe CustomDomainConfigType)
cupdCustomDomainConfig = Lens.lens (customDomainConfig :: CreateUserPoolDomain -> Lude.Maybe CustomDomainConfigType) (\s a -> s {customDomainConfig = a} :: CreateUserPoolDomain)
{-# DEPRECATED cupdCustomDomainConfig "Use generic-lens or generic-optics with 'customDomainConfig' instead." #-}

-- | The domain string.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupdDomain :: Lens.Lens' CreateUserPoolDomain Lude.Text
cupdDomain = Lens.lens (domain :: CreateUserPoolDomain -> Lude.Text) (\s a -> s {domain = a} :: CreateUserPoolDomain)
{-# DEPRECATED cupdDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupdUserPoolId :: Lens.Lens' CreateUserPoolDomain Lude.Text
cupdUserPoolId = Lens.lens (userPoolId :: CreateUserPoolDomain -> Lude.Text) (\s a -> s {userPoolId = a} :: CreateUserPoolDomain)
{-# DEPRECATED cupdUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest CreateUserPoolDomain where
  type Rs CreateUserPoolDomain = CreateUserPoolDomainResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserPoolDomainResponse'
            Lude.<$> (x Lude..?> "CloudFrontDomain")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserPoolDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolDomain" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserPoolDomain where
  toJSON CreateUserPoolDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CustomDomainConfig" Lude..=) Lude.<$> customDomainConfig,
            Lude.Just ("Domain" Lude..= domain),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath CreateUserPoolDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserPoolDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserPoolDomainResponse' smart constructor.
data CreateUserPoolDomainResponse = CreateUserPoolDomainResponse'
  { cloudFrontDomain ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- * 'cloudFrontDomain' - The Amazon CloudFront endpoint that you use as the target of the alias that you set up with your Domain Name Service (DNS) provider.
-- * 'responseStatus' - The response status code.
mkCreateUserPoolDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserPoolDomainResponse
mkCreateUserPoolDomainResponse pResponseStatus_ =
  CreateUserPoolDomainResponse'
    { cloudFrontDomain = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon CloudFront endpoint that you use as the target of the alias that you set up with your Domain Name Service (DNS) provider.
--
-- /Note:/ Consider using 'cloudFrontDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupdrsCloudFrontDomain :: Lens.Lens' CreateUserPoolDomainResponse (Lude.Maybe Lude.Text)
cupdrsCloudFrontDomain = Lens.lens (cloudFrontDomain :: CreateUserPoolDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {cloudFrontDomain = a} :: CreateUserPoolDomainResponse)
{-# DEPRECATED cupdrsCloudFrontDomain "Use generic-lens or generic-optics with 'cloudFrontDomain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupdrsResponseStatus :: Lens.Lens' CreateUserPoolDomainResponse Lude.Int
cupdrsResponseStatus = Lens.lens (responseStatus :: CreateUserPoolDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserPoolDomainResponse)
{-# DEPRECATED cupdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
