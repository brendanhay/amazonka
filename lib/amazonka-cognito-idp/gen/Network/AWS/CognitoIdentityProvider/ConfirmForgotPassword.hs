{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to enter a confirmation code to reset a forgotten password.
module Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
  ( -- * Creating a request
    ConfirmForgotPassword (..),
    mkConfirmForgotPassword,

    -- ** Request lenses
    cfpClientMetadata,
    cfpClientId,
    cfpConfirmationCode,
    cfpAnalyticsMetadata,
    cfpUserContextData,
    cfpUsername,
    cfpSecretHash,
    cfpPassword,

    -- * Destructuring the response
    ConfirmForgotPasswordResponse (..),
    mkConfirmForgotPasswordResponse,

    -- ** Response lenses
    cfprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request representing the confirmation for a password reset.
--
-- /See:/ 'mkConfirmForgotPassword' smart constructor.
data ConfirmForgotPassword = ConfirmForgotPassword'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmForgotPassword API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The app client ID of the app associated with the user pool.
    clientId :: Lude.Sensitive Lude.Text,
    -- | The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword> .
    confirmationCode :: Lude.Text,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Lude.Maybe UserContextDataType,
    -- | The user name of the user for whom you want to enter a code to retrieve a forgotten password.
    username :: Lude.Sensitive Lude.Text,
    -- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
    secretHash :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The password sent by a user's request to retrieve a forgotten password.
    password :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmForgotPassword' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmForgotPassword API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'clientId' - The app client ID of the app associated with the user pool.
-- * 'confirmationCode' - The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword> .
-- * 'analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
-- * 'userContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'username' - The user name of the user for whom you want to enter a code to retrieve a forgotten password.
-- * 'secretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
-- * 'password' - The password sent by a user's request to retrieve a forgotten password.
mkConfirmForgotPassword ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'confirmationCode'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  ConfirmForgotPassword
mkConfirmForgotPassword
  pClientId_
  pConfirmationCode_
  pUsername_
  pPassword_ =
    ConfirmForgotPassword'
      { clientMetadata = Lude.Nothing,
        clientId = pClientId_,
        confirmationCode = pConfirmationCode_,
        analyticsMetadata = Lude.Nothing,
        userContextData = Lude.Nothing,
        username = pUsername_,
        secretHash = Lude.Nothing,
        password = pPassword_
      }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmForgotPassword API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmForgotPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpClientMetadata :: Lens.Lens' ConfirmForgotPassword (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfpClientMetadata = Lens.lens (clientMetadata :: ConfirmForgotPassword -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpClientId :: Lens.Lens' ConfirmForgotPassword (Lude.Sensitive Lude.Text)
cfpClientId = Lens.lens (clientId :: ConfirmForgotPassword -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword> .
--
-- /Note:/ Consider using 'confirmationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpConfirmationCode :: Lens.Lens' ConfirmForgotPassword Lude.Text
cfpConfirmationCode = Lens.lens (confirmationCode :: ConfirmForgotPassword -> Lude.Text) (\s a -> s {confirmationCode = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpConfirmationCode "Use generic-lens or generic-optics with 'confirmationCode' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpAnalyticsMetadata :: Lens.Lens' ConfirmForgotPassword (Lude.Maybe AnalyticsMetadataType)
cfpAnalyticsMetadata = Lens.lens (analyticsMetadata :: ConfirmForgotPassword -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpUserContextData :: Lens.Lens' ConfirmForgotPassword (Lude.Maybe UserContextDataType)
cfpUserContextData = Lens.lens (userContextData :: ConfirmForgotPassword -> Lude.Maybe UserContextDataType) (\s a -> s {userContextData = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | The user name of the user for whom you want to enter a code to retrieve a forgotten password.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpUsername :: Lens.Lens' ConfirmForgotPassword (Lude.Sensitive Lude.Text)
cfpUsername = Lens.lens (username :: ConfirmForgotPassword -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpSecretHash :: Lens.Lens' ConfirmForgotPassword (Lude.Maybe (Lude.Sensitive Lude.Text))
cfpSecretHash = Lens.lens (secretHash :: ConfirmForgotPassword -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretHash = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpSecretHash "Use generic-lens or generic-optics with 'secretHash' instead." #-}

-- | The password sent by a user's request to retrieve a forgotten password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfpPassword :: Lens.Lens' ConfirmForgotPassword (Lude.Sensitive Lude.Text)
cfpPassword = Lens.lens (password :: ConfirmForgotPassword -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: ConfirmForgotPassword)
{-# DEPRECATED cfpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest ConfirmForgotPassword where
  type Rs ConfirmForgotPassword = ConfirmForgotPasswordResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ConfirmForgotPasswordResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmForgotPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ConfirmForgotPassword" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfirmForgotPassword where
  toJSON ConfirmForgotPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("ConfirmationCode" Lude..= confirmationCode),
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("UserContextData" Lude..=) Lude.<$> userContextData,
            Lude.Just ("Username" Lude..= username),
            ("SecretHash" Lude..=) Lude.<$> secretHash,
            Lude.Just ("Password" Lude..= password)
          ]
      )

instance Lude.ToPath ConfirmForgotPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmForgotPassword where
  toQuery = Lude.const Lude.mempty

-- | The response from the server that results from a user's request to retrieve a forgotten password.
--
-- /See:/ 'mkConfirmForgotPasswordResponse' smart constructor.
newtype ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmForgotPasswordResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkConfirmForgotPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmForgotPasswordResponse
mkConfirmForgotPasswordResponse pResponseStatus_ =
  ConfirmForgotPasswordResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfprsResponseStatus :: Lens.Lens' ConfirmForgotPasswordResponse Lude.Int
cfprsResponseStatus = Lens.lens (responseStatus :: ConfirmForgotPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmForgotPasswordResponse)
{-# DEPRECATED cfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
