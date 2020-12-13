{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmSignUp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms registration of a user and handles the existing alias from a previous user.
module Network.AWS.CognitoIdentityProvider.ConfirmSignUp
  ( -- * Creating a request
    ConfirmSignUp (..),
    mkConfirmSignUp,

    -- ** Request lenses
    csuClientMetadata,
    csuClientId,
    csuForceAliasCreation,
    csuConfirmationCode,
    csuAnalyticsMetadata,
    csuUserContextData,
    csuUsername,
    csuSecretHash,

    -- * Destructuring the response
    ConfirmSignUpResponse (..),
    mkConfirmSignUpResponse,

    -- ** Response lenses
    csursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to confirm registration of a user.
--
-- /See:/ 'mkConfirmSignUp' smart constructor.
data ConfirmSignUp = ConfirmSignUp'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmSignUp API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmSignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The ID of the app client associated with the user pool.
    clientId :: Lude.Sensitive Lude.Text,
    -- | Boolean to be specified to force user confirmation irrespective of existing alias. By default set to @False@ . If this parameter is set to @True@ and the phone number/email used for sign up confirmation already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user being confirmed. If set to @False@ , the API will throw an __AliasExistsException__ error.
    forceAliasCreation :: Lude.Maybe Lude.Bool,
    -- | The confirmation code sent by a user's request to confirm registration.
    confirmationCode :: Lude.Text,
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmSignUp@ calls.
    analyticsMetadata :: Lude.Maybe AnalyticsMetadataType,
    -- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
    userContextData :: Lude.Maybe UserContextDataType,
    -- | The user name of the user whose registration you wish to confirm.
    username :: Lude.Sensitive Lude.Text,
    -- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
    secretHash :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmSignUp' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmSignUp API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmSignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'clientId' - The ID of the app client associated with the user pool.
-- * 'forceAliasCreation' - Boolean to be specified to force user confirmation irrespective of existing alias. By default set to @False@ . If this parameter is set to @True@ and the phone number/email used for sign up confirmation already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user being confirmed. If set to @False@ , the API will throw an __AliasExistsException__ error.
-- * 'confirmationCode' - The confirmation code sent by a user's request to confirm registration.
-- * 'analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmSignUp@ calls.
-- * 'userContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
-- * 'username' - The user name of the user whose registration you wish to confirm.
-- * 'secretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
mkConfirmSignUp ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'confirmationCode'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  ConfirmSignUp
mkConfirmSignUp pClientId_ pConfirmationCode_ pUsername_ =
  ConfirmSignUp'
    { clientMetadata = Lude.Nothing,
      clientId = pClientId_,
      forceAliasCreation = Lude.Nothing,
      confirmationCode = pConfirmationCode_,
      analyticsMetadata = Lude.Nothing,
      userContextData = Lude.Nothing,
      username = pUsername_,
      secretHash = Lude.Nothing
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the ConfirmSignUp API action, Amazon Cognito invokes the function that is assigned to the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your ConfirmSignUp request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuClientMetadata :: Lens.Lens' ConfirmSignUp (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
csuClientMetadata = Lens.lens (clientMetadata :: ConfirmSignUp -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: ConfirmSignUp)
{-# DEPRECATED csuClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The ID of the app client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuClientId :: Lens.Lens' ConfirmSignUp (Lude.Sensitive Lude.Text)
csuClientId = Lens.lens (clientId :: ConfirmSignUp -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: ConfirmSignUp)
{-# DEPRECATED csuClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | Boolean to be specified to force user confirmation irrespective of existing alias. By default set to @False@ . If this parameter is set to @True@ and the phone number/email used for sign up confirmation already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user being confirmed. If set to @False@ , the API will throw an __AliasExistsException__ error.
--
-- /Note:/ Consider using 'forceAliasCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuForceAliasCreation :: Lens.Lens' ConfirmSignUp (Lude.Maybe Lude.Bool)
csuForceAliasCreation = Lens.lens (forceAliasCreation :: ConfirmSignUp -> Lude.Maybe Lude.Bool) (\s a -> s {forceAliasCreation = a} :: ConfirmSignUp)
{-# DEPRECATED csuForceAliasCreation "Use generic-lens or generic-optics with 'forceAliasCreation' instead." #-}

-- | The confirmation code sent by a user's request to confirm registration.
--
-- /Note:/ Consider using 'confirmationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuConfirmationCode :: Lens.Lens' ConfirmSignUp Lude.Text
csuConfirmationCode = Lens.lens (confirmationCode :: ConfirmSignUp -> Lude.Text) (\s a -> s {confirmationCode = a} :: ConfirmSignUp)
{-# DEPRECATED csuConfirmationCode "Use generic-lens or generic-optics with 'confirmationCode' instead." #-}

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmSignUp@ calls.
--
-- /Note:/ Consider using 'analyticsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuAnalyticsMetadata :: Lens.Lens' ConfirmSignUp (Lude.Maybe AnalyticsMetadataType)
csuAnalyticsMetadata = Lens.lens (analyticsMetadata :: ConfirmSignUp -> Lude.Maybe AnalyticsMetadataType) (\s a -> s {analyticsMetadata = a} :: ConfirmSignUp)
{-# DEPRECATED csuAnalyticsMetadata "Use generic-lens or generic-optics with 'analyticsMetadata' instead." #-}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /Note:/ Consider using 'userContextData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuUserContextData :: Lens.Lens' ConfirmSignUp (Lude.Maybe UserContextDataType)
csuUserContextData = Lens.lens (userContextData :: ConfirmSignUp -> Lude.Maybe UserContextDataType) (\s a -> s {userContextData = a} :: ConfirmSignUp)
{-# DEPRECATED csuUserContextData "Use generic-lens or generic-optics with 'userContextData' instead." #-}

-- | The user name of the user whose registration you wish to confirm.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuUsername :: Lens.Lens' ConfirmSignUp (Lude.Sensitive Lude.Text)
csuUsername = Lens.lens (username :: ConfirmSignUp -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: ConfirmSignUp)
{-# DEPRECATED csuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- /Note:/ Consider using 'secretHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuSecretHash :: Lens.Lens' ConfirmSignUp (Lude.Maybe (Lude.Sensitive Lude.Text))
csuSecretHash = Lens.lens (secretHash :: ConfirmSignUp -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {secretHash = a} :: ConfirmSignUp)
{-# DEPRECATED csuSecretHash "Use generic-lens or generic-optics with 'secretHash' instead." #-}

instance Lude.AWSRequest ConfirmSignUp where
  type Rs ConfirmSignUp = ConfirmSignUpResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ConfirmSignUpResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmSignUp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.ConfirmSignUp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfirmSignUp where
  toJSON ConfirmSignUp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("ClientId" Lude..= clientId),
            ("ForceAliasCreation" Lude..=) Lude.<$> forceAliasCreation,
            Lude.Just ("ConfirmationCode" Lude..= confirmationCode),
            ("AnalyticsMetadata" Lude..=) Lude.<$> analyticsMetadata,
            ("UserContextData" Lude..=) Lude.<$> userContextData,
            Lude.Just ("Username" Lude..= username),
            ("SecretHash" Lude..=) Lude.<$> secretHash
          ]
      )

instance Lude.ToPath ConfirmSignUp where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmSignUp where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server for the registration confirmation.
--
-- /See:/ 'mkConfirmSignUpResponse' smart constructor.
newtype ConfirmSignUpResponse = ConfirmSignUpResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmSignUpResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkConfirmSignUpResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmSignUpResponse
mkConfirmSignUpResponse pResponseStatus_ =
  ConfirmSignUpResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csursResponseStatus :: Lens.Lens' ConfirmSignUpResponse Lude.Int
csursResponseStatus = Lens.lens (responseStatus :: ConfirmSignUpResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmSignUpResponse)
{-# DEPRECATED csursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
