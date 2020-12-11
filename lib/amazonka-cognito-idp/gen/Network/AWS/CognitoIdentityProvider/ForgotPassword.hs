{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ForgotPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calling this API causes a message to be sent to the end user with a confirmation code that is required to change the user's password. For the @Username@ parameter, you can use the username or user alias. The method used to send the confirmation code is sent according to the specified AccountRecoverySetting. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-recover-a-user-account.html Recovering User Accounts> in the /Amazon Cognito Developer Guide/ . If neither a verified phone number nor a verified email exists, an @InvalidParameterException@ is thrown. To use the confirmation code for resetting the password, call <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ConfirmForgotPassword.html ConfirmForgotPassword> .
module Network.AWS.CognitoIdentityProvider.ForgotPassword
  ( -- * Creating a request
    ForgotPassword (..),
    mkForgotPassword,

    -- ** Request lenses
    fpClientMetadata,
    fpAnalyticsMetadata,
    fpUserContextData,
    fpSecretHash,
    fpClientId,
    fpUsername,

    -- * Destructuring the response
    ForgotPasswordResponse (..),
    mkForgotPasswordResponse,

    -- ** Response lenses
    fprsCodeDeliveryDetails,
    fprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to reset a user's password.
--
-- /See:/ 'mkForgotPassword' smart constructor.
data ForgotPassword = ForgotPassword'
  { clientMetadata ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    userContextData :: Lude.Maybe UserContextDataType,
    secretHash :: Lude.Maybe (Lude.Sensitive Lude.Text),
    clientId :: Lude.Sensitive Lude.Text,
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ForgotPassword' with the minimum fields required to make a request.
--
-- * 'analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @ForgotPassword@ calls.
-- * 'clientId' - The ID of the client associated with the user pool.
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ForgotPassword API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , and /user migration/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'secretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
-- * 'userContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'username' - The user name of the user for whom you want to enter a code to reset a forgotten password.
mkForgotPassword ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  ForgotPassword
mkForgotPassword pClientId_ pUsername_ =
  ForgotPassword'
    { clientMetadata = Lude.Nothing,
      analyticsMetadata = Lude.Nothing,
      userContextData = Lude.Nothing,
      secretHash = Lude.Nothing,
      clientId = pClientId_,
      username = pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ForgotPassword API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , and /user migration/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpClientMetadata :: Lens.Lens' ForgotPassword (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
fpClientMetadata = Lens.lens (clientMetadata :: ForgotPassword -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: ForgotPassword)
{-# DEPRECATED fpClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ForgotPassword@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpAnalyticsMetadata :: Lens.Lens' ForgotPassword (Lude.Maybe AnalyticsMetadataType)
fpAnalyticsMetadata = Lens.lens (analyticsMetadata :: ForgotPassword -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: ForgotPassword)
{-# DEPRECATED fpAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpUserContextData :: Lens.Lens' ForgotPassword (Lude.Maybe UserContextDataType)
fpUserContextData = Lens.lens (userContextData :: ForgotPassword -> Lude.Maybe UserContextDataType) (\s a -> s {userContextData = a} :: ForgotPassword)
{-# DEPRECATED fpUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpSecretHash :: Lens.Lens' ForgotPassword (Lude.Maybe (Lude.Sensitive Lude.Text))
fpSecretHash = Lens.lens (secretHash :: ForgotPassword -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretHash = a} :: ForgotPassword)
{-# DEPRECATED fpSecretHash "Use generic-lens or generic-optics with 'secretHash' instead." #-}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpClientId :: Lens.Lens' ForgotPassword (Lude.Sensitive Lude.Text)
fpClientId = Lens.lens (clientId :: ForgotPassword -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: ForgotPassword)
{-# DEPRECATED fpClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The user name of the user for whom you want to enter a code to reset a forgotten password.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpUsername :: Lens.Lens' ForgotPassword (Lude.Sensitive Lude.Text)
fpUsername = Lens.lens (username :: ForgotPassword -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: ForgotPassword)
{-# DEPRECATED fpUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest ForgotPassword where
  type Rs ForgotPassword = ForgotPasswordResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ForgotPasswordResponse'
            Lude.<$> (x Lude..?> "CodeDeliveryDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ForgotPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ForgotPassword" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ForgotPassword where
  toJSON ForgotPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("UserContextData" Lude..=) Lude.<$> userContextData,
            ("SecretHash" Lude..=) Lude.<$> secretHash,
            Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath ForgotPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery ForgotPassword where
  toQuery = Lude.const Lude.mempty

-- | Respresents the response from the server regarding the request to reset a password.
--
-- /See:/ 'mkForgotPasswordResponse' smart constructor.
data ForgotPasswordResponse = ForgotPasswordResponse'
  { codeDeliveryDetails ::
      Lude.Maybe CodeDeliveryDetailsType,
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

-- | Creates a value of 'ForgotPasswordResponse' with the minimum fields required to make a request.
--
-- * 'codeDeliveryDetails' - The code delivery details returned by the server in response to the request to reset a password.
-- * 'responseStatus' - The response status code.
mkForgotPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ForgotPasswordResponse
mkForgotPasswordResponse pResponseStatus_ =
  ForgotPasswordResponse'
    { codeDeliveryDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The code delivery details returned by the server in response to the request to reset a password.
--
-- /Note:/ Consider using 'codeDeliveryDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fprsCodeDeliveryDetails :: Lens.Lens' ForgotPasswordResponse (Lude.Maybe CodeDeliveryDetailsType)
fprsCodeDeliveryDetails = Lens.lens (codeDeliveryDetails :: ForgotPasswordResponse -> Lude.Maybe CodeDeliveryDetailsType) (\s a -> s {codeDeliveryDetails = a} :: ForgotPasswordResponse)
{-# DEPRECATED fprsCodeDeliveryDetails "Use generic-lens or generic-optics with 'codeDeliveryDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fprsResponseStatus :: Lens.Lens' ForgotPasswordResponse Lude.Int
fprsResponseStatus = Lens.lens (responseStatus :: ForgotPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ForgotPasswordResponse)
{-# DEPRECATED fprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
