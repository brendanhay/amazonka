{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified user's password in a user pool as an administrator. Works on any user.
--
-- When a developer calls this API, the current password is invalidated, so it must be changed. If a user tries to sign in after the API is called, the app will get a PasswordResetRequiredException exception back and should direct the user down the flow to reset the password, which is the same as the forgot password flow. In addition, if the user pool has phone verification selected and a verified phone number exists for the user, or if email verification is selected and a verified email exists for the user, calling this API will also result in sending a message to the end user with the code to change their password.
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
  ( -- * Creating a request
    AdminResetUserPassword (..),
    mkAdminResetUserPassword,

    -- ** Request lenses
    arupClientMetadata,
    arupUserPoolId,
    arupUsername,

    -- * Destructuring the response
    AdminResetUserPasswordResponse (..),
    mkAdminResetUserPasswordResponse,

    -- ** Response lenses
    aruprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to reset a user's password as an administrator.
--
-- /See:/ 'mkAdminResetUserPassword' smart constructor.
data AdminResetUserPassword = AdminResetUserPassword'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminResetUserPassword API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminResetUserPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The user pool ID for the user pool where you want to reset the user's password.
    userPoolId :: Lude.Text,
    -- | The user name of the user whose password you wish to reset.
    username :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminResetUserPassword' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminResetUserPassword API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminResetUserPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'userPoolId' - The user pool ID for the user pool where you want to reset the user's password.
-- * 'username' - The user name of the user whose password you wish to reset.
mkAdminResetUserPassword ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminResetUserPassword
mkAdminResetUserPassword pUserPoolId_ pUsername_ =
  AdminResetUserPassword'
    { clientMetadata = Lude.Nothing,
      userPoolId = pUserPoolId_,
      username = pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminResetUserPassword API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminResetUserPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arupClientMetadata :: Lens.Lens' AdminResetUserPassword (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
arupClientMetadata = Lens.lens (clientMetadata :: AdminResetUserPassword -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: AdminResetUserPassword)
{-# DEPRECATED arupClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The user pool ID for the user pool where you want to reset the user's password.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arupUserPoolId :: Lens.Lens' AdminResetUserPassword Lude.Text
arupUserPoolId = Lens.lens (userPoolId :: AdminResetUserPassword -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminResetUserPassword)
{-# DEPRECATED arupUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user whose password you wish to reset.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arupUsername :: Lens.Lens' AdminResetUserPassword (Lude.Sensitive Lude.Text)
arupUsername = Lens.lens (username :: AdminResetUserPassword -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminResetUserPassword)
{-# DEPRECATED arupUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Lude.AWSRequest AdminResetUserPassword where
  type Rs AdminResetUserPassword = AdminResetUserPasswordResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminResetUserPasswordResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminResetUserPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminResetUserPassword" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminResetUserPassword where
  toJSON AdminResetUserPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username)
          ]
      )

instance Lude.ToPath AdminResetUserPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminResetUserPassword where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to reset a user password as an administrator.
--
-- /See:/ 'mkAdminResetUserPasswordResponse' smart constructor.
newtype AdminResetUserPasswordResponse = AdminResetUserPasswordResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminResetUserPasswordResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminResetUserPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminResetUserPasswordResponse
mkAdminResetUserPasswordResponse pResponseStatus_ =
  AdminResetUserPasswordResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aruprsResponseStatus :: Lens.Lens' AdminResetUserPasswordResponse Lude.Int
aruprsResponseStatus = Lens.lens (responseStatus :: AdminResetUserPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminResetUserPasswordResponse)
{-# DEPRECATED aruprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
