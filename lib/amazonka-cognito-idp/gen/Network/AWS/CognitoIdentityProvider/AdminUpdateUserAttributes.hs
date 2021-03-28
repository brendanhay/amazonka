{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user's attributes, including developer attributes, as an administrator. Works on any user.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
-- In addition to updating user attributes, this API can also be used to mark phone and email as verified.
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
    (
    -- * Creating a request
      AdminUpdateUserAttributes (..)
    , mkAdminUpdateUserAttributes
    -- ** Request lenses
    , auuaUserPoolId
    , auuaUsername
    , auuaUserAttributes
    , auuaClientMetadata

    -- * Destructuring the response
    , AdminUpdateUserAttributesResponse (..)
    , mkAdminUpdateUserAttributesResponse
    -- ** Response lenses
    , auuarrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the user's attributes as an administrator.
--
-- /See:/ 'mkAdminUpdateUserAttributes' smart constructor.
data AdminUpdateUserAttributes = AdminUpdateUserAttributes'
  { userPoolId :: Types.UserPoolIdType
    -- ^ The user pool ID for the user pool where you want to update user attributes.
  , username :: Types.UsernameType
    -- ^ The user name of the user for whom you want to update user attributes.
  , userAttributes :: [Types.AttributeType]
    -- ^ An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminUpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminUpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateUserAttributes' value with any optional fields omitted.
mkAdminUpdateUserAttributes
    :: Types.UserPoolIdType -- ^ 'userPoolId'
    -> Types.UsernameType -- ^ 'username'
    -> AdminUpdateUserAttributes
mkAdminUpdateUserAttributes userPoolId username
  = AdminUpdateUserAttributes'{userPoolId, username,
                               userAttributes = Core.mempty, clientMetadata = Core.Nothing}

-- | The user pool ID for the user pool where you want to update user attributes.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaUserPoolId :: Lens.Lens' AdminUpdateUserAttributes Types.UserPoolIdType
auuaUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE auuaUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user name of the user for whom you want to update user attributes.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaUsername :: Lens.Lens' AdminUpdateUserAttributes Types.UsernameType
auuaUsername = Lens.field @"username"
{-# INLINEABLE auuaUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaUserAttributes :: Lens.Lens' AdminUpdateUserAttributes [Types.AttributeType]
auuaUserAttributes = Lens.field @"userAttributes"
{-# INLINEABLE auuaUserAttributes #-}
{-# DEPRECATED userAttributes "Use generic-lens or generic-optics with 'userAttributes' instead"  #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminUpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminUpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaClientMetadata :: Lens.Lens' AdminUpdateUserAttributes (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
auuaClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE auuaClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

instance Core.ToQuery AdminUpdateUserAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminUpdateUserAttributes where
        toHeaders AdminUpdateUserAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminUpdateUserAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminUpdateUserAttributes where
        toJSON AdminUpdateUserAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("UserAttributes" Core..= userAttributes),
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata])

instance Core.AWSRequest AdminUpdateUserAttributes where
        type Rs AdminUpdateUserAttributes =
             AdminUpdateUserAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminUpdateUserAttributesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server for the request to update user attributes as an administrator.
--
-- /See:/ 'mkAdminUpdateUserAttributesResponse' smart constructor.
newtype AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminUpdateUserAttributesResponse' value with any optional fields omitted.
mkAdminUpdateUserAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminUpdateUserAttributesResponse
mkAdminUpdateUserAttributesResponse responseStatus
  = AdminUpdateUserAttributesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuarrsResponseStatus :: Lens.Lens' AdminUpdateUserAttributesResponse Core.Int
auuarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE auuarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
