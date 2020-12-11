{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Secure Sockets Layer (SSL) certificate for the custom domain for your user pool.
--
-- You can use this operation to provide the Amazon Resource Name (ARN) of a new certificate to Amazon Cognito. You cannot use it to change the domain for a user pool.
-- A custom domain is used to host the Amazon Cognito hosted UI, which provides sign-up and sign-in pages for your application. When you set up a custom domain, you provide a certificate that you manage with AWS Certificate Manager (ACM). When necessary, you can use this operation to change the certificate that you applied to your custom domain.
-- Usually, this is unnecessary following routine certificate renewal with ACM. When you renew your existing certificate in ACM, the ARN for your certificate remains the same, and your custom domain uses the new certificate automatically.
-- However, if you replace your existing certificate with a new one, ACM gives the new certificate a new ARN. To apply the new certificate to your custom domain, you must provide this ARN to Amazon Cognito.
-- When you add your new certificate in ACM, you must choose US East (N. Virginia) as the AWS Region.
-- After you submit your request, Amazon Cognito requires up to 1 hour to distribute your new certificate to your custom domain.
-- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
  ( -- * Creating a request
    UpdateUserPoolDomain (..),
    mkUpdateUserPoolDomain,

    -- ** Request lenses
    uupdDomain,
    uupdUserPoolId,
    uupdCustomDomainConfig,

    -- * Destructuring the response
    UpdateUserPoolDomainResponse (..),
    mkUpdateUserPoolDomainResponse,

    -- ** Response lenses
    uupdrsCloudFrontDomain,
    uupdrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The UpdateUserPoolDomain request input.
--
-- /See:/ 'mkUpdateUserPoolDomain' smart constructor.
data UpdateUserPoolDomain = UpdateUserPoolDomain'
  { domain ::
      Lude.Text,
    userPoolId :: Lude.Text,
    customDomainConfig :: CustomDomainConfigType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserPoolDomain' with the minimum fields required to make a request.
--
-- * 'customDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in pages for your application. Use this object to specify an SSL certificate that is managed by ACM.
-- * 'domain' - The domain name for the custom domain that hosts the sign-up and sign-in pages for your application. For example: @auth.example.com@ .
--
-- This string can include only lowercase letters, numbers, and hyphens. Do not use a hyphen for the first or last character. Use periods to separate subdomain names.
-- * 'userPoolId' - The ID of the user pool that is associated with the custom domain that you are updating the certificate for.
mkUpdateUserPoolDomain ::
  -- | 'domain'
  Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'customDomainConfig'
  CustomDomainConfigType ->
  UpdateUserPoolDomain
mkUpdateUserPoolDomain pDomain_ pUserPoolId_ pCustomDomainConfig_ =
  UpdateUserPoolDomain'
    { domain = pDomain_,
      userPoolId = pUserPoolId_,
      customDomainConfig = pCustomDomainConfig_
    }

-- | The domain name for the custom domain that hosts the sign-up and sign-in pages for your application. For example: @auth.example.com@ .
--
-- This string can include only lowercase letters, numbers, and hyphens. Do not use a hyphen for the first or last character. Use periods to separate subdomain names.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdDomain :: Lens.Lens' UpdateUserPoolDomain Lude.Text
uupdDomain = Lens.lens (domain :: UpdateUserPoolDomain -> Lude.Text) (\s a -> s {domain = a} :: UpdateUserPoolDomain)
{-# DEPRECATED uupdDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The ID of the user pool that is associated with the custom domain that you are updating the certificate for.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdUserPoolId :: Lens.Lens' UpdateUserPoolDomain Lude.Text
uupdUserPoolId = Lens.lens (userPoolId :: UpdateUserPoolDomain -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateUserPoolDomain)
{-# DEPRECATED uupdUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The configuration for a custom domain that hosts the sign-up and sign-in pages for your application. Use this object to specify an SSL certificate that is managed by ACM.
--
-- /Note:/ Consider using 'customDomainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdCustomDomainConfig :: Lens.Lens' UpdateUserPoolDomain CustomDomainConfigType
uupdCustomDomainConfig = Lens.lens (customDomainConfig :: UpdateUserPoolDomain -> CustomDomainConfigType) (\s a -> s {customDomainConfig = a} :: UpdateUserPoolDomain)
{-# DEPRECATED uupdCustomDomainConfig "Use generic-lens or generic-optics with 'customDomainConfig' instead." #-}

instance Lude.AWSRequest UpdateUserPoolDomain where
  type Rs UpdateUserPoolDomain = UpdateUserPoolDomainResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateUserPoolDomainResponse'
            Lude.<$> (x Lude..?> "CloudFrontDomain")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUserPoolDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateUserPoolDomain" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserPoolDomain where
  toJSON UpdateUserPoolDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Domain" Lude..= domain),
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("CustomDomainConfig" Lude..= customDomainConfig)
          ]
      )

instance Lude.ToPath UpdateUserPoolDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserPoolDomain where
  toQuery = Lude.const Lude.mempty

-- | The UpdateUserPoolDomain response output.
--
-- /See:/ 'mkUpdateUserPoolDomainResponse' smart constructor.
data UpdateUserPoolDomainResponse = UpdateUserPoolDomainResponse'
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

-- | Creates a value of 'UpdateUserPoolDomainResponse' with the minimum fields required to make a request.
--
-- * 'cloudFrontDomain' - The Amazon CloudFront endpoint that Amazon Cognito set up when you added the custom domain to your user pool.
-- * 'responseStatus' - The response status code.
mkUpdateUserPoolDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserPoolDomainResponse
mkUpdateUserPoolDomainResponse pResponseStatus_ =
  UpdateUserPoolDomainResponse'
    { cloudFrontDomain = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon CloudFront endpoint that Amazon Cognito set up when you added the custom domain to your user pool.
--
-- /Note:/ Consider using 'cloudFrontDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdrsCloudFrontDomain :: Lens.Lens' UpdateUserPoolDomainResponse (Lude.Maybe Lude.Text)
uupdrsCloudFrontDomain = Lens.lens (cloudFrontDomain :: UpdateUserPoolDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {cloudFrontDomain = a} :: UpdateUserPoolDomainResponse)
{-# DEPRECATED uupdrsCloudFrontDomain "Use generic-lens or generic-optics with 'cloudFrontDomain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupdrsResponseStatus :: Lens.Lens' UpdateUserPoolDomainResponse Lude.Int
uupdrsResponseStatus = Lens.lens (responseStatus :: UpdateUserPoolDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserPoolDomainResponse)
{-# DEPRECATED uupdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
