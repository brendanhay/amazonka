{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms user registration as an admin without using a confirmation code. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
  ( -- * Creating a request
    AdminConfirmSignUp (..),
    mkAdminConfirmSignUp,

    -- ** Request lenses
    acsuClientMetadata,
    acsuUserPoolId,
    acsuUsername,

    -- * Destructuring the response
    AdminConfirmSignUpResponse (..),
    mkAdminConfirmSignUpResponse,

    -- ** Response lenses
    acsursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to confirm user registration.
--
-- /See:/ 'mkAdminConfirmSignUp' smart constructor.
data AdminConfirmSignUp = AdminConfirmSignUp'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- If your user pool configuration includes triggers, the AdminConfirmSignUp API action invokes the AWS Lambda function that is specified for the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. In this payload, the @clientMetadata@ attribute provides the data that you assigned to the ClientMetadata parameter in your AdminConfirmSignUp request. In your function code in AWS Lambda, you can process the ClientMetadata value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The user pool ID for which you want to confirm user registration.
    userPoolId :: Lude.Text,
    -- | The user name for which you want to confirm user registration.
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminConfirmSignUp' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- If your user pool configuration includes triggers, the AdminConfirmSignUp API action invokes the AWS Lambda function that is specified for the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. In this payload, the @clientMetadata@ attribute provides the data that you assigned to the ClientMetadata parameter in your AdminConfirmSignUp request. In your function code in AWS Lambda, you can process the ClientMetadata value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'userPoolId' - The user pool ID for which you want to confirm user registration.
-- * 'username' - The user name for which you want to confirm user registration.
mkAdminConfirmSignUp ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminConfirmSignUp
mkAdminConfirmSignUp pUserPoolId_ pUsername_ =
  AdminConfirmSignUp'
    { clientMetadata = Lude.Nothing,
      userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- If your user pool configuration includes triggers, the AdminConfirmSignUp API action invokes the AWS Lambda function that is specified for the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. In this payload, the @clientMetadata@ attribute provides the data that you assigned to the ClientMetadata parameter in your AdminConfirmSignUp request. In your function code in AWS Lambda, you can process the ClientMetadata value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsuClientMetadata :: Lens.Lens' AdminConfirmSignUp (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
acsuClientMetadata = Lens.lens (clientMetadata :: AdminConfirmSignUp -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: AdminConfirmSignUp)
{-# DEPRECATED acsuClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The user pool ID for which you want to confirm user registration.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsuUserPoolId :: Lens.Lens' AdminConfirmSignUp Lude.Text
acsuUserPoolId = Lens.lens (userPoolId :: AdminConfirmSignUp -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminConfirmSignUp)
{-# DEPRECATED acsuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name for which you want to confirm user registration.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsuUsername :: Lens.Lens' AdminConfirmSignUp (Lude.Sensitive Lude.Text)
acsuUsername = Lens.lens (username :: AdminConfirmSignUp -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminConfirmSignUp)
{-# DEPRECATED acsuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminConfirmSignUp where
  type Rs AdminConfirmSignUp = AdminConfirmSignUpResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminConfirmSignUpResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminConfirmSignUp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminConfirmSignUp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminConfirmSignUp where
  toJSON AdminConfirmSignUp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminConfirmSignUp where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminConfirmSignUp where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server for the request to confirm registration.
--
-- /See:/ 'mkAdminConfirmSignUpResponse' smart constructor.
newtype AdminConfirmSignUpResponse = AdminConfirmSignUpResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminConfirmSignUpResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminConfirmSignUpResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminConfirmSignUpResponse
mkAdminConfirmSignUpResponse pResponseStatus_ =
  AdminConfirmSignUpResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsursResponseStatus :: Lens.Lens' AdminConfirmSignUpResponse Lude.Int
acsursResponseStatus = Lens.lens (responseStatus :: AdminConfirmSignUpResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminConfirmSignUpResponse)
{-# DEPRECATED acsursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
