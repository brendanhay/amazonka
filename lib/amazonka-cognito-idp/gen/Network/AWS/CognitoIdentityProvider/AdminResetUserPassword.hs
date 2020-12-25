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
    arupUserPoolId,
    arupUsername,
    arupClientMetadata,

    -- * Destructuring the response
    AdminResetUserPasswordResponse (..),
    mkAdminResetUserPasswordResponse,

    -- ** Response lenses
    aruprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to reset a user's password as an administrator.
--
-- /See:/ 'mkAdminResetUserPassword' smart constructor.
data AdminResetUserPassword = AdminResetUserPassword'
  { -- | The user pool ID for the user pool where you want to reset the user's password.
    userPoolId :: Types.UserPoolId,
    -- | The user name of the user whose password you wish to reset.
    username :: Types.Username,
    -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminResetUserPassword API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminResetUserPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminResetUserPassword' value with any optional fields omitted.
mkAdminResetUserPassword ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'username'
  Types.Username ->
  AdminResetUserPassword
mkAdminResetUserPassword userPoolId username =
  AdminResetUserPassword'
    { userPoolId,
      username,
      clientMetadata = Core.Nothing
    }

-- | The user pool ID for the user pool where you want to reset the user's password.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arupUserPoolId :: Lens.Lens' AdminResetUserPassword Types.UserPoolId
arupUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED arupUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user whose password you wish to reset.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arupUsername :: Lens.Lens' AdminResetUserPassword Types.Username
arupUsername = Lens.field @"username"
{-# DEPRECATED arupUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminResetUserPassword API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminResetUserPassword request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arupClientMetadata :: Lens.Lens' AdminResetUserPassword (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
arupClientMetadata = Lens.field @"clientMetadata"
{-# DEPRECATED arupClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

instance Core.FromJSON AdminResetUserPassword where
  toJSON AdminResetUserPassword {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            ("ClientMetadata" Core..=) Core.<$> clientMetadata
          ]
      )

instance Core.AWSRequest AdminResetUserPassword where
  type Rs AdminResetUserPassword = AdminResetUserPasswordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminResetUserPassword"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminResetUserPasswordResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server to reset a user password as an administrator.
--
-- /See:/ 'mkAdminResetUserPasswordResponse' smart constructor.
newtype AdminResetUserPasswordResponse = AdminResetUserPasswordResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminResetUserPasswordResponse' value with any optional fields omitted.
mkAdminResetUserPasswordResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminResetUserPasswordResponse
mkAdminResetUserPasswordResponse responseStatus =
  AdminResetUserPasswordResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aruprrsResponseStatus :: Lens.Lens' AdminResetUserPasswordResponse Core.Int
aruprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aruprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
