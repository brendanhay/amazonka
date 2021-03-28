{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminCreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the specified user pool.
--
-- If @MessageAction@ is not set, the default is to send a welcome message via email or phone (SMS).
-- This message is based on a template that you configured in your call to create or update a user pool. This template includes your custom sign-up instructions and placeholders for user name and temporary password.
-- Alternatively, you can call @AdminCreateUser@ with “SUPPRESS” for the @MessageAction@ parameter, and Amazon Cognito will not send any email. 
-- In either case, the user will be in the @FORCE_CHANGE_PASSWORD@ state until they sign in and change their password.
-- @AdminCreateUser@ requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminCreateUser
    (
    -- * Creating a request
      AdminCreateUser (..)
    , mkAdminCreateUser
    -- ** Request lenses
    , acuUserPoolId
    , acuUsername
    , acuClientMetadata
    , acuDesiredDeliveryMediums
    , acuForceAliasCreation
    , acuMessageAction
    , acuTemporaryPassword
    , acuUserAttributes
    , acuValidationData

    -- * Destructuring the response
    , AdminCreateUserResponse (..)
    , mkAdminCreateUserResponse
    -- ** Response lenses
    , acurrsUser
    , acurrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create a user in the specified user pool.
--
-- /See:/ 'mkAdminCreateUser' smart constructor.
data AdminCreateUser = AdminCreateUser'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool where the user will be created.
  , username :: Types.Username
    -- ^ The username for the user. Must be unique within the user pool. Must be a UTF-8 string between 1 and 128 characters. After the user is created, the username cannot be changed.
  , clientMetadata :: Core.Maybe (Core.HashMap Types.StringType Types.StringType)
    -- ^ A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminCreateUser API action, Amazon Cognito invokes the function that is assigned to the /pre sign-up/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminCreateUser request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
  , desiredDeliveryMediums :: Core.Maybe [Types.DeliveryMediumType]
    -- ^ Specify @"EMAIL"@ if email will be used to send the welcome message. Specify @"SMS"@ if the phone number will be used. The default value is @"SMS"@ . More than one value can be specified.
  , forceAliasCreation :: Core.Maybe Core.Bool
    -- ^ This parameter is only used if the @phone_number_verified@ or @email_verified@ attribute is set to @True@ . Otherwise, it is ignored.
--
-- If this parameter is set to @True@ and the phone number or email address specified in the UserAttributes parameter already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user. The previous user will no longer be able to log in using that alias.
-- If this parameter is set to @False@ , the API throws an @AliasExistsException@ error if the alias already exists. The default value is @False@ .
  , messageAction :: Core.Maybe Types.MessageActionType
    -- ^ Set to @"RESEND"@ to resend the invitation message to a user that already exists and reset the expiration limit on the user's account. Set to @"SUPPRESS"@ to suppress sending the message. Only one value can be specified.
  , temporaryPassword :: Core.Maybe Types.PasswordType
    -- ^ The user's temporary password. This password must conform to the password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create User flow, the user must enter the temporary password in the sign-in page along with a new password to be used in all future sign-ins.
-- This parameter is not required. If you do not specify a value, Amazon Cognito generates one for you.
-- The temporary password can only be used until the user account expiration limit that you specified when you created the user pool. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter.
  , userAttributes :: Core.Maybe [Types.AttributeType]
    -- ^ An array of name-value pairs that contain user attributes and attribute values to be set for the user to be created. You can create a user without specifying any attributes other than @Username@ . However, any attributes that you specify as required (when creating a user pool or in the __Attributes__ tab of the console) must be supplied either by you (in your call to @AdminCreateUser@ ) or by the user (when he or she signs up in response to your welcome message).
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
-- To send a message inviting the user to sign up, you must specify the user's email address or phone number. This can be done in your call to AdminCreateUser or in the __Users__ tab of the Amazon Cognito console for managing your user pools.
-- In your call to @AdminCreateUser@ , you can set the @email_verified@ attribute to @True@ , and you can set the @phone_number_verified@ attribute to @True@ . (You can also do this by calling <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes> .)
--
--     * __email__ : The email address of the user to whom the message that contains the code and username will be sent. Required if the @email_verified@ attribute is set to @True@ , or if @"EMAIL"@ is specified in the @DesiredDeliveryMediums@ parameter.
--
--
--     * __phone_number__ : The phone number of the user to whom the message that contains the code and username will be sent. Required if the @phone_number_verified@ attribute is set to @True@ , or if @"SMS"@ is specified in the @DesiredDeliveryMediums@ parameter.
--
--
  , validationData :: Core.Maybe [Types.AttributeType]
    -- ^ The user's validation data. This is an array of name-value pairs that contain user attributes and attribute values that you can use for custom validation, such as restricting the types of user accounts that can be registered. For example, you might choose to allow or disallow user sign-up based on the user's domain.
--
-- To configure custom validation, you must create a Pre Sign-up Lambda trigger for the user pool as described in the Amazon Cognito Developer Guide. The Lambda trigger receives the validation data and uses it in the validation process.
-- The user's validation data is not persisted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminCreateUser' value with any optional fields omitted.
mkAdminCreateUser
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> AdminCreateUser
mkAdminCreateUser userPoolId username
  = AdminCreateUser'{userPoolId, username,
                     clientMetadata = Core.Nothing,
                     desiredDeliveryMediums = Core.Nothing,
                     forceAliasCreation = Core.Nothing, messageAction = Core.Nothing,
                     temporaryPassword = Core.Nothing, userAttributes = Core.Nothing,
                     validationData = Core.Nothing}

-- | The user pool ID for the user pool where the user will be created.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuUserPoolId :: Lens.Lens' AdminCreateUser Types.UserPoolId
acuUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE acuUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The username for the user. Must be unique within the user pool. Must be a UTF-8 string between 1 and 128 characters. After the user is created, the username cannot be changed.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuUsername :: Lens.Lens' AdminCreateUser Types.Username
acuUsername = Lens.field @"username"
{-# INLINEABLE acuUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers. 
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminCreateUser API action, Amazon Cognito invokes the function that is assigned to the /pre sign-up/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminCreateUser request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuClientMetadata :: Lens.Lens' AdminCreateUser (Core.Maybe (Core.HashMap Types.StringType Types.StringType))
acuClientMetadata = Lens.field @"clientMetadata"
{-# INLINEABLE acuClientMetadata #-}
{-# DEPRECATED clientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead"  #-}

-- | Specify @"EMAIL"@ if email will be used to send the welcome message. Specify @"SMS"@ if the phone number will be used. The default value is @"SMS"@ . More than one value can be specified.
--
-- /Note:/ Consider using 'desiredDeliveryMediums' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuDesiredDeliveryMediums :: Lens.Lens' AdminCreateUser (Core.Maybe [Types.DeliveryMediumType])
acuDesiredDeliveryMediums = Lens.field @"desiredDeliveryMediums"
{-# INLINEABLE acuDesiredDeliveryMediums #-}
{-# DEPRECATED desiredDeliveryMediums "Use generic-lens or generic-optics with 'desiredDeliveryMediums' instead"  #-}

-- | This parameter is only used if the @phone_number_verified@ or @email_verified@ attribute is set to @True@ . Otherwise, it is ignored.
--
-- If this parameter is set to @True@ and the phone number or email address specified in the UserAttributes parameter already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user. The previous user will no longer be able to log in using that alias.
-- If this parameter is set to @False@ , the API throws an @AliasExistsException@ error if the alias already exists. The default value is @False@ .
--
-- /Note:/ Consider using 'forceAliasCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuForceAliasCreation :: Lens.Lens' AdminCreateUser (Core.Maybe Core.Bool)
acuForceAliasCreation = Lens.field @"forceAliasCreation"
{-# INLINEABLE acuForceAliasCreation #-}
{-# DEPRECATED forceAliasCreation "Use generic-lens or generic-optics with 'forceAliasCreation' instead"  #-}

-- | Set to @"RESEND"@ to resend the invitation message to a user that already exists and reset the expiration limit on the user's account. Set to @"SUPPRESS"@ to suppress sending the message. Only one value can be specified.
--
-- /Note:/ Consider using 'messageAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuMessageAction :: Lens.Lens' AdminCreateUser (Core.Maybe Types.MessageActionType)
acuMessageAction = Lens.field @"messageAction"
{-# INLINEABLE acuMessageAction #-}
{-# DEPRECATED messageAction "Use generic-lens or generic-optics with 'messageAction' instead"  #-}

-- | The user's temporary password. This password must conform to the password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create User flow, the user must enter the temporary password in the sign-in page along with a new password to be used in all future sign-ins.
-- This parameter is not required. If you do not specify a value, Amazon Cognito generates one for you.
-- The temporary password can only be used until the user account expiration limit that you specified when you created the user pool. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter.
--
-- /Note:/ Consider using 'temporaryPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuTemporaryPassword :: Lens.Lens' AdminCreateUser (Core.Maybe Types.PasswordType)
acuTemporaryPassword = Lens.field @"temporaryPassword"
{-# INLINEABLE acuTemporaryPassword #-}
{-# DEPRECATED temporaryPassword "Use generic-lens or generic-optics with 'temporaryPassword' instead"  #-}

-- | An array of name-value pairs that contain user attributes and attribute values to be set for the user to be created. You can create a user without specifying any attributes other than @Username@ . However, any attributes that you specify as required (when creating a user pool or in the __Attributes__ tab of the console) must be supplied either by you (in your call to @AdminCreateUser@ ) or by the user (when he or she signs up in response to your welcome message).
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
-- To send a message inviting the user to sign up, you must specify the user's email address or phone number. This can be done in your call to AdminCreateUser or in the __Users__ tab of the Amazon Cognito console for managing your user pools.
-- In your call to @AdminCreateUser@ , you can set the @email_verified@ attribute to @True@ , and you can set the @phone_number_verified@ attribute to @True@ . (You can also do this by calling <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes> .)
--
--     * __email__ : The email address of the user to whom the message that contains the code and username will be sent. Required if the @email_verified@ attribute is set to @True@ , or if @"EMAIL"@ is specified in the @DesiredDeliveryMediums@ parameter.
--
--
--     * __phone_number__ : The phone number of the user to whom the message that contains the code and username will be sent. Required if the @phone_number_verified@ attribute is set to @True@ , or if @"SMS"@ is specified in the @DesiredDeliveryMediums@ parameter.
--
--
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuUserAttributes :: Lens.Lens' AdminCreateUser (Core.Maybe [Types.AttributeType])
acuUserAttributes = Lens.field @"userAttributes"
{-# INLINEABLE acuUserAttributes #-}
{-# DEPRECATED userAttributes "Use generic-lens or generic-optics with 'userAttributes' instead"  #-}

-- | The user's validation data. This is an array of name-value pairs that contain user attributes and attribute values that you can use for custom validation, such as restricting the types of user accounts that can be registered. For example, you might choose to allow or disallow user sign-up based on the user's domain.
--
-- To configure custom validation, you must create a Pre Sign-up Lambda trigger for the user pool as described in the Amazon Cognito Developer Guide. The Lambda trigger receives the validation data and uses it in the validation process.
-- The user's validation data is not persisted.
--
-- /Note:/ Consider using 'validationData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuValidationData :: Lens.Lens' AdminCreateUser (Core.Maybe [Types.AttributeType])
acuValidationData = Lens.field @"validationData"
{-# INLINEABLE acuValidationData #-}
{-# DEPRECATED validationData "Use generic-lens or generic-optics with 'validationData' instead"  #-}

instance Core.ToQuery AdminCreateUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminCreateUser where
        toHeaders AdminCreateUser{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminCreateUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminCreateUser where
        toJSON AdminCreateUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  ("ClientMetadata" Core..=) Core.<$> clientMetadata,
                  ("DesiredDeliveryMediums" Core..=) Core.<$> desiredDeliveryMediums,
                  ("ForceAliasCreation" Core..=) Core.<$> forceAliasCreation,
                  ("MessageAction" Core..=) Core.<$> messageAction,
                  ("TemporaryPassword" Core..=) Core.<$> temporaryPassword,
                  ("UserAttributes" Core..=) Core.<$> userAttributes,
                  ("ValidationData" Core..=) Core.<$> validationData])

instance Core.AWSRequest AdminCreateUser where
        type Rs AdminCreateUser = AdminCreateUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AdminCreateUserResponse' Core.<$>
                   (x Core..:? "User") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server to the request to create the user.
--
-- /See:/ 'mkAdminCreateUserResponse' smart constructor.
data AdminCreateUserResponse = AdminCreateUserResponse'
  { user :: Core.Maybe Types.UserType
    -- ^ The newly created user.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AdminCreateUserResponse' value with any optional fields omitted.
mkAdminCreateUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminCreateUserResponse
mkAdminCreateUserResponse responseStatus
  = AdminCreateUserResponse'{user = Core.Nothing, responseStatus}

-- | The newly created user.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acurrsUser :: Lens.Lens' AdminCreateUserResponse (Core.Maybe Types.UserType)
acurrsUser = Lens.field @"user"
{-# INLINEABLE acurrsUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acurrsResponseStatus :: Lens.Lens' AdminCreateUserResponse Core.Int
acurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
