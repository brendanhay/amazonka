{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attribute verification code for the specified attribute name.
module Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
  ( -- * Creating a request
    GetUserAttributeVerificationCode (..),
    mkGetUserAttributeVerificationCode,

    -- ** Request lenses
    guavcClientMetadata,
    guavcAccessToken,
    guavcAttributeName,

    -- * Destructuring the response
    GetUserAttributeVerificationCodeResponse (..),
    mkGetUserAttributeVerificationCodeResponse,

    -- ** Response lenses
    guavcrsCodeDeliveryDetails,
    guavcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to get user attribute verification.
--
-- /See:/ 'mkGetUserAttributeVerificationCode' smart constructor.
data GetUserAttributeVerificationCode = GetUserAttributeVerificationCode'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the GetUserAttributeVerificationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your GetUserAttributeVerificationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The access token returned by the server response to get the user attribute verification code.
    accessToken :: Lude.Sensitive Lude.Text,
    -- | The attribute name returned by the server response to get the user attribute verification code.
    attributeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserAttributeVerificationCode' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the GetUserAttributeVerificationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your GetUserAttributeVerificationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'accessToken' - The access token returned by the server response to get the user attribute verification code.
-- * 'attributeName' - The attribute name returned by the server response to get the user attribute verification code.
mkGetUserAttributeVerificationCode ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  GetUserAttributeVerificationCode
mkGetUserAttributeVerificationCode pAccessToken_ pAttributeName_ =
  GetUserAttributeVerificationCode'
    { clientMetadata = Lude.Nothing,
      accessToken = pAccessToken_,
      attributeName = pAttributeName_
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the GetUserAttributeVerificationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your GetUserAttributeVerificationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcClientMetadata :: Lens.Lens' GetUserAttributeVerificationCode (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
guavcClientMetadata = Lens.lens (clientMetadata :: GetUserAttributeVerificationCode -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: GetUserAttributeVerificationCode)
{-# DEPRECATED guavcClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The access token returned by the server response to get the user attribute verification code.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcAccessToken :: Lens.Lens' GetUserAttributeVerificationCode (Lude.Sensitive Lude.Text)
guavcAccessToken = Lens.lens (accessToken :: GetUserAttributeVerificationCode -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: GetUserAttributeVerificationCode)
{-# DEPRECATED guavcAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The attribute name returned by the server response to get the user attribute verification code.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcAttributeName :: Lens.Lens' GetUserAttributeVerificationCode Lude.Text
guavcAttributeName = Lens.lens (attributeName :: GetUserAttributeVerificationCode -> Lude.Text) (\s a -> s {attributeName = a} :: GetUserAttributeVerificationCode)
{-# DEPRECATED guavcAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.AWSRequest GetUserAttributeVerificationCode where
  type
    Rs GetUserAttributeVerificationCode =
      GetUserAttributeVerificationCodeResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUserAttributeVerificationCodeResponse'
            Lude.<$> (x Lude..?> "CodeDeliveryDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUserAttributeVerificationCode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.GetUserAttributeVerificationCode" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUserAttributeVerificationCode where
  toJSON GetUserAttributeVerificationCode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("AccessToken" Lude..= accessToken),
            Lude.Just ("AttributeName" Lude..= attributeName)
          ]
      )

instance Lude.ToPath GetUserAttributeVerificationCode where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUserAttributeVerificationCode where
  toQuery = Lude.const Lude.mempty

-- | The verification code response returned by the server response to get the user attribute verification code.
--
-- /See:/ 'mkGetUserAttributeVerificationCodeResponse' smart constructor.
data GetUserAttributeVerificationCodeResponse = GetUserAttributeVerificationCodeResponse'
  { -- | The code delivery details returned by the server in response to the request to get the user attribute verification code.
    codeDeliveryDetails :: Lude.Maybe CodeDeliveryDetailsType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserAttributeVerificationCodeResponse' with the minimum fields required to make a request.
--
-- * 'codeDeliveryDetails' - The code delivery details returned by the server in response to the request to get the user attribute verification code.
-- * 'responseStatus' - The response status code.
mkGetUserAttributeVerificationCodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUserAttributeVerificationCodeResponse
mkGetUserAttributeVerificationCodeResponse pResponseStatus_ =
  GetUserAttributeVerificationCodeResponse'
    { codeDeliveryDetails =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The code delivery details returned by the server in response to the request to get the user attribute verification code.
--
-- /Note:/ Consider using 'codeDeliveryDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcrsCodeDeliveryDetails :: Lens.Lens' GetUserAttributeVerificationCodeResponse (Lude.Maybe CodeDeliveryDetailsType)
guavcrsCodeDeliveryDetails = Lens.lens (codeDeliveryDetails :: GetUserAttributeVerificationCodeResponse -> Lude.Maybe CodeDeliveryDetailsType) (\s a -> s {codeDeliveryDetails = a} :: GetUserAttributeVerificationCodeResponse)
{-# DEPRECATED guavcrsCodeDeliveryDetails "Use generic-lens or generic-optics with 'codeDeliveryDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guavcrsResponseStatus :: Lens.Lens' GetUserAttributeVerificationCodeResponse Lude.Int
guavcrsResponseStatus = Lens.lens (responseStatus :: GetUserAttributeVerificationCodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserAttributeVerificationCodeResponse)
{-# DEPRECATED guavcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
