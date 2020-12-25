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
    acsuUserPoolId,
    acsuUsername,
    acsuClientMetadata,

    -- * Destructuring the response
    AdminConfirmSignUpResponse (..),
    mkAdminConfirmSignUpResponse,

    -- ** Response lenses
    acsurrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to confirm user registration.
--
-- /See:/ 'mkAdminConfirmSignUp' smart constructor.
data AdminConfirmSignUp = AdminConfirmSignUp'
  { -- | The user pool ID for which you want to confirm user registration.
    userPoolId :: Types.UserPoolId,
    -- | The user name for which you want to confirm user registration.
    username :: Types.Username,
    -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- If your user pool configuration includes triggers, the AdminConfirmSignUp API action invokes the AWS Lambda function that is specified for the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. In this payload, the @clientMetadata@ attribute provides the data that you assigned to the ClientMetadata parameter in your AdminConfirmSignUp request. In your function code in AWS Lambda, you can process the ClientMetadata value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminConfirmSignUp' value with any optional fields omitted.
mkAdminConfirmSignUp ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'username'
  Types.Username ->
  AdminConfirmSignUp
mkAdminConfirmSignUp userPoolId username =
  AdminConfirmSignUp'
    { userPoolId,
      username,
      clientMetadata = Core.Nothing
    }

-- | The user pool ID for which you want to confirm user registration.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsuUserPoolId :: Lens.Lens' AdminConfirmSignUp Types.UserPoolId
acsuUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED acsuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name for which you want to confirm user registration.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsuUsername :: Lens.Lens' AdminConfirmSignUp Types.Username
acsuUsername = Lens.field @"username"
{-# DEPRECATED acsuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- If your user pool configuration includes triggers, the AdminConfirmSignUp API action invokes the AWS Lambda function that is specified for the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. In this payload, the @clientMetadata@ attribute provides the data that you assigned to the ClientMetadata parameter in your AdminConfirmSignUp request. In your function code in AWS Lambda, you can process the ClientMetadata value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsuClientMetadata :: Lens.Lens' AdminConfirmSignUp (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
acsuClientMetadata = Lens.field @"clientMetadata"
{-# DEPRECATED acsuClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

instance Core.FromJSON AdminConfirmSignUp where
  toJSON AdminConfirmSignUp {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            ("ClientMetadata" Core..=) Core.<$> clientMetadata
          ]
      )

instance Core.AWSRequest AdminConfirmSignUp where
  type Rs AdminConfirmSignUp = AdminConfirmSignUpResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminConfirmSignUp"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminConfirmSignUpResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server for the request to confirm registration.
--
-- /See:/ 'mkAdminConfirmSignUpResponse' smart constructor.
newtype AdminConfirmSignUpResponse = AdminConfirmSignUpResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminConfirmSignUpResponse' value with any optional fields omitted.
mkAdminConfirmSignUpResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminConfirmSignUpResponse
mkAdminConfirmSignUpResponse responseStatus =
  AdminConfirmSignUpResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsurrsResponseStatus :: Lens.Lens' AdminConfirmSignUpResponse Core.Int
acsurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED acsurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
