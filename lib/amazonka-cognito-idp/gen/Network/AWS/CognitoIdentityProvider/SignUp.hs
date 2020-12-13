{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SignUp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the user in the specified user pool and creates a user name, password, and user attributes.
module Network.AWS.CognitoIdentityProvider.SignUp
  ( -- * Creating a request
    SignUp (..),
    mkSignUp,

    -- ** Request lenses
    suClientMetadata,
    suClientId,
    suAnalyticsMetadata,
    suUserContextData,
    suUserAttributes,
    suUsername,
    suSecretHash,
    suPassword,
    suValidationData,

    -- * Destructuring the response
    SignUpResponse (..),
    mkSignUpResponse,

    -- ** Response lenses
    sursUserConfirmed,
    sursUserSub,
    sursCodeDeliveryDetails,
    sursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to register a user.
--
-- /See:/ 'mkSignUp' smart constructor.
data SignUp = SignUp'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the SignUp API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , and /post confirmation/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your SignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The ID of the client associated with the user pool.
    clientId :: Lude.Sensitive Lude.Text,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @SignUp@ calls.
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Lude.Maybe UserContextDataType,
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
    userAttributes :: Lude.Maybe [AttributeType],
    -- | The user name of the user you wish to register.
    username :: Lude.Sensitive Lude.Text,
    -- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
    secretHash :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The password of the user you wish to register.
    password :: Lude.Sensitive Lude.Text,
    -- | The validation data in the request to register a user.
    validationData :: Lude.Maybe [AttributeType]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SignUp' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the SignUp API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , and /post confirmation/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your SignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'clientId' - The ID of the client associated with the user pool.
-- * 'analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @SignUp@ calls.
-- * 'userContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
-- * 'username' - The user name of the user you wish to register.
-- * 'secretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
-- * 'password' - The password of the user you wish to register.
-- * 'validationData' - The validation data in the request to register a user.
mkSignUp ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  SignUp
mkSignUp pClientId_ pUsername_ pPassword_ =
  SignUp'
    { clientMetadata = Lude.Nothing,
      clientId = pClientId_,
      analyticsMetadata = Lude.Nothing,
      userContextData = Lude.Nothing,
      userAttributes = Lude.Nothing,
      username = pUsername_,
      secretHash = Lude.Nothing,
      password = pPassword_,
      validationData = Lude.Nothing
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the SignUp API action, Amazon Cognito invokes any functions that are assigned to the following triggers: /pre sign-up/ , /custom message/ , and /post confirmation/ . When Amazon Cognito invokes any of these functions, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your SignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suClientMetadata :: Lens.Lens' SignUp (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
suClientMetadata = Lens.lens (clientMetadata :: SignUp -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: SignUp)
{-# DEPRECATED suClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suClientId :: Lens.Lens' SignUp (Lude.Sensitive Lude.Text)
suClientId = Lens.lens (clientId :: SignUp -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: SignUp)
{-# DEPRECATED suClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @SignUp@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suAnalyticsMetadata :: Lens.Lens' SignUp (Lude.Maybe AnalyticsMetadataType)
suAnalyticsMetadata = Lens.lens (analyticsMetadata :: SignUp -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: SignUp)
{-# DEPRECATED suAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suUserContextData :: Lens.Lens' SignUp (Lude.Maybe UserContextDataType)
suUserContextData = Lens.lens (userContextData :: SignUp -> Lude.Maybe UserContextDataType) (\s a -> s {userContextData = a} :: SignUp)
{-# DEPRECATED suUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suUserAttributes :: Lens.Lens' SignUp (Lude.Maybe [AttributeType])
suUserAttributes = Lens.lens (userAttributes :: SignUp -> Lude.Maybe [AttributeType]) (\s a -> s {userAttributes = a} :: SignUp)
{-# DEPRECATED suUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The user name of the user you wish to register.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suUsername :: Lens.Lens' SignUp (Lude.Sensitive Lude.Text)
suUsername = Lens.lens (username :: SignUp -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: SignUp)
{-# DEPRECATED suUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suSecretHash :: Lens.Lens' SignUp (Lude.Maybe (Lude.Sensitive Lude.Text))
suSecretHash = Lens.lens (secretHash :: SignUp -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretHash = a} :: SignUp)
{-# DEPRECATED suSecretHash "Use generic-lens or generic-optics with 'secretHash' instead." #-}

-- | The password of the user you wish to register.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suPassword :: Lens.Lens' SignUp (Lude.Sensitive Lude.Text)
suPassword = Lens.lens (password :: SignUp -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: SignUp)
{-# DEPRECATED suPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The validation data in the request to register a user.
--
-- /Note:/ Consider using 'validationData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suValidationData :: Lens.Lens' SignUp (Lude.Maybe [AttributeType])
suValidationData = Lens.lens (validationData :: SignUp -> Lude.Maybe [AttributeType]) (\s a -> s {validationData = a} :: SignUp)
{-# DEPRECATED suValidationData "Use generic-lens or generic-optics with 'validationData' instead." #-}

instance Lude.AWSRequest SignUp where
  type Rs SignUp = SignUpResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          SignUpResponse'
            Lude.<$> (x Lude..:> "UserConfirmed")
            Lude.<*> (x Lude..:> "UserSub")
            Lude.<*> (x Lude..?> "CodeDeliveryDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SignUp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityProviderService.SignUp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SignUp where
  toJSON SignUp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("ClientId" Lude..= clientId),
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("UserContextData" Lude..=) Lude.<$> userContextData,
            ("UserAttributes" Lude..=) Lude.<$> userAttributes,
            Lude.Just ("Username" Lude..= username),
            ("SecretHash" Lude..=) Lude.<$> secretHash,
            Lude.Just ("Password" Lude..= password),
            ("ValidationData" Lude..=) Lude.<$> validationData
          ]
      )

instance Lude.ToPath SignUp where
  toPath = Lude.const "/"

instance Lude.ToQuery SignUp where
  toQuery = Lude.const Lude.mempty

-- | The response from the server for a registration request.
--
-- /See:/ 'mkSignUpResponse' smart constructor.
data SignUpResponse = SignUpResponse'
  { -- | A response from the server indicating that a user registration has been confirmed.
    userConfirmed :: Lude.Bool,
    -- | The UUID of the authenticated user. This is not the same as @username@ .
    userSub :: Lude.Text,
    -- | The code delivery details returned by the server response to the user registration request.
    codeDeliveryDetails :: Lude.Maybe CodeDeliveryDetailsType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SignUpResponse' with the minimum fields required to make a request.
--
-- * 'userConfirmed' - A response from the server indicating that a user registration has been confirmed.
-- * 'userSub' - The UUID of the authenticated user. This is not the same as @username@ .
-- * 'codeDeliveryDetails' - The code delivery details returned by the server response to the user registration request.
-- * 'responseStatus' - The response status code.
mkSignUpResponse ::
  -- | 'userConfirmed'
  Lude.Bool ->
  -- | 'userSub'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  SignUpResponse
mkSignUpResponse pUserConfirmed_ pUserSub_ pResponseStatus_ =
  SignUpResponse'
    { userConfirmed = pUserConfirmed_,
      userSub = pUserSub_,
      codeDeliveryDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A response from the server indicating that a user registration has been confirmed.
--
-- /Note:/ Consider using 'userConfirmed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursUserConfirmed :: Lens.Lens' SignUpResponse Lude.Bool
sursUserConfirmed = Lens.lens (userConfirmed :: SignUpResponse -> Lude.Bool) (\s a -> s {userConfirmed = a} :: SignUpResponse)
{-# DEPRECATED sursUserConfirmed "Use generic-lens or generic-optics with 'userConfirmed' instead." #-}

-- | The UUID of the authenticated user. This is not the same as @username@ .
--
-- /Note:/ Consider using 'userSub' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursUserSub :: Lens.Lens' SignUpResponse Lude.Text
sursUserSub = Lens.lens (userSub :: SignUpResponse -> Lude.Text) (\s a -> s {userSub = a} :: SignUpResponse)
{-# DEPRECATED sursUserSub "Use generic-lens or generic-optics with 'userSub' instead." #-}

-- | The code delivery details returned by the server response to the user registration request.
--
-- /Note:/ Consider using 'codeDeliveryDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursCodeDeliveryDetails :: Lens.Lens' SignUpResponse (Lude.Maybe CodeDeliveryDetailsType)
sursCodeDeliveryDetails = Lens.lens (codeDeliveryDetails :: SignUpResponse -> Lude.Maybe CodeDeliveryDetailsType) (\s a -> s {codeDeliveryDetails = a} :: SignUpResponse)
{-# DEPRECATED sursCodeDeliveryDetails "Use generic-lens or generic-optics with 'codeDeliveryDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursResponseStatus :: Lens.Lens' SignUpResponse Lude.Int
sursResponseStatus = Lens.lens (responseStatus :: SignUpResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SignUpResponse)
{-# DEPRECATED sursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
