{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the confirmation (for confirmation of registration) to a specific user in the user pool.
module Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
  ( -- * Creating a request
    ResendConfirmationCode (..),
    mkResendConfirmationCode,

    -- ** Request lenses
    rccClientMetadata,
    rccClientId,
    rccAnalyticsMetadata,
    rccUserContextData,
    rccUsername,
    rccSecretHash,

    -- * Destructuring the response
    ResendConfirmationCodeResponse (..),
    mkResendConfirmationCodeResponse,

    -- ** Response lenses
    rccrsCodeDeliveryDetails,
    rccrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to resend the confirmation code.
--
-- /See:/ 'mkResendConfirmationCode' smart constructor.
data ResendConfirmationCode = ResendConfirmationCode'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ResendConfirmationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ResendConfirmationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The ID of the client associated with the user pool.
    clientId :: Lude.Sensitive Lude.Text,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @ResendConfirmationCode@ calls.
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Lude.Maybe UserContextDataType,
    -- | The user name of the user to whom you wish to resend a confirmation code.
    username :: Lude.Sensitive Lude.Text,
    -- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
    secretHash :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResendConfirmationCode' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ResendConfirmationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ResendConfirmationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'clientId' - The ID of the client associated with the user pool.
-- * 'analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @ResendConfirmationCode@ calls.
-- * 'userContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'username' - The user name of the user to whom you wish to resend a confirmation code.
-- * 'secretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
mkResendConfirmationCode ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  ResendConfirmationCode
mkResendConfirmationCode pClientId_ pUsername_ =
  ResendConfirmationCode'
    { clientMetadata = Lude.Nothing,
      clientId = pClientId_,
      analyticsMetadata = Lude.Nothing,
      userContextData = Lude.Nothing,
      username = pUsername_,
      secretHash = Lude.Nothing
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ResendConfirmationCode API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ResendConfirmationCode request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccClientMetadata :: Lens.Lens' ResendConfirmationCode (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rccClientMetadata = Lens.lens (clientMetadata :: ResendConfirmationCode -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: ResendConfirmationCode)
{-# DEPRECATED rccClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccClientId :: Lens.Lens' ResendConfirmationCode (Lude.Sensitive Lude.Text)
rccClientId = Lens.lens (clientId :: ResendConfirmationCode -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: ResendConfirmationCode)
{-# DEPRECATED rccClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ResendConfirmationCode@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccAnalyticsMetadata :: Lens.Lens' ResendConfirmationCode (Lude.Maybe AnalyticsMetadataType)
rccAnalyticsMetadata = Lens.lens (analyticsMetadata :: ResendConfirmationCode -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: ResendConfirmationCode)
{-# DEPRECATED rccAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccUserContextData :: Lens.Lens' ResendConfirmationCode (Lude.Maybe UserContextDataType)
rccUserContextData = Lens.lens (userContextData :: ResendConfirmationCode -> Lude.Maybe UserContextDataType) (\s a -> s {userContextData = a} :: ResendConfirmationCode)
{-# DEPRECATED rccUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | The user name of the user to whom you wish to resend a confirmation code.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccUsername :: Lens.Lens' ResendConfirmationCode (Lude.Sensitive Lude.Text)
rccUsername = Lens.lens (username :: ResendConfirmationCode -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: ResendConfirmationCode)
{-# DEPRECATED rccUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccSecretHash :: Lens.Lens' ResendConfirmationCode (Lude.Maybe (Lude.Sensitive Lude.Text))
rccSecretHash = Lens.lens (secretHash :: ResendConfirmationCode -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretHash = a} :: ResendConfirmationCode)
{-# DEPRECATED rccSecretHash "Use generic-lens or generic-optics with 'secretHash' instead." #-}

instance Lude.AWSRequest ResendConfirmationCode where
  type Rs ResendConfirmationCode = ResendConfirmationCodeResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResendConfirmationCodeResponse'
            Lude.<$> (x Lude..?> "CodeDeliveryDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResendConfirmationCode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ResendConfirmationCode" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResendConfirmationCode where
  toJSON ResendConfirmationCode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("ClientId" Lude..= clientId),
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("UserContextData" Lude..=) Lude.<$> userContextData,
            Lude.Just ("Username" Lude..= username),
            ("SecretHash" Lude..=) Lude.<$> secretHash
          ]
      )

instance Lude.ToPath ResendConfirmationCode where
  toPath = Lude.const "/"

instance Lude.ToQuery ResendConfirmationCode where
  toQuery = Lude.const Lude.mempty

-- | The response from the server when the Amazon Cognito Your User Pools service makes the request to resend a confirmation code.
--
-- /See:/ 'mkResendConfirmationCodeResponse' smart constructor.
data ResendConfirmationCodeResponse = ResendConfirmationCodeResponse'
  { -- | The code delivery details returned by the server in response to the request to resend the confirmation code.
    codeDeliveryDetails :: Lude.Maybe CodeDeliveryDetailsType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResendConfirmationCodeResponse' with the minimum fields required to make a request.
--
-- * 'codeDeliveryDetails' - The code delivery details returned by the server in response to the request to resend the confirmation code.
-- * 'responseStatus' - The response status code.
mkResendConfirmationCodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResendConfirmationCodeResponse
mkResendConfirmationCodeResponse pResponseStatus_ =
  ResendConfirmationCodeResponse'
    { codeDeliveryDetails =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The code delivery details returned by the server in response to the request to resend the confirmation code.
--
-- /Note:/ Consider using 'codeDeliveryDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrsCodeDeliveryDetails :: Lens.Lens' ResendConfirmationCodeResponse (Lude.Maybe CodeDeliveryDetailsType)
rccrsCodeDeliveryDetails = Lens.lens (codeDeliveryDetails :: ResendConfirmationCodeResponse -> Lude.Maybe CodeDeliveryDetailsType) (\s a -> s {codeDeliveryDetails = a} :: ResendConfirmationCodeResponse)
{-# DEPRECATED rccrsCodeDeliveryDetails "Use generic-lens or generic-optics with 'codeDeliveryDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rccrsResponseStatus :: Lens.Lens' ResendConfirmationCodeResponse Lude.Int
rccrsResponseStatus = Lens.lens (responseStatus :: ResendConfirmationCodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResendConfirmationCodeResponse)
{-# DEPRECATED rccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
