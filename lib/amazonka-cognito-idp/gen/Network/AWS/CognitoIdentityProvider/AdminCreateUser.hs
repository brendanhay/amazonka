{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AdminCreateUser (..),
    mkAdminCreateUser,

    -- ** Request lenses
    acuClientMetadata,
    acuTemporaryPassword,
    acuForceAliasCreation,
    acuDesiredDeliveryMediums,
    acuMessageAction,
    acuUserPoolId,
    acuUserAttributes,
    acuUsername,
    acuValidationData,

    -- * Destructuring the response
    AdminCreateUserResponse (..),
    mkAdminCreateUserResponse,

    -- ** Response lenses
    acursUser,
    acursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to create a user in the specified user pool.
--
-- /See:/ 'mkAdminCreateUser' smart constructor.
data AdminCreateUser = AdminCreateUser'
  { -- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminCreateUser API action, Amazon Cognito invokes the function that is assigned to the /pre sign-up/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminCreateUser request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
    -- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
    clientMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The user's temporary password. This password must conform to the password policy that you specified when you created the user pool.
    --
    -- The temporary password is valid only once. To complete the Admin Create User flow, the user must enter the temporary password in the sign-in page along with a new password to be used in all future sign-ins.
    -- This parameter is not required. If you do not specify a value, Amazon Cognito generates one for you.
    -- The temporary password can only be used until the user account expiration limit that you specified when you created the user pool. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter.
    temporaryPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | This parameter is only used if the @phone_number_verified@ or @email_verified@ attribute is set to @True@ . Otherwise, it is ignored.
    --
    -- If this parameter is set to @True@ and the phone number or email address specified in the UserAttributes parameter already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user. The previous user will no longer be able to log in using that alias.
    -- If this parameter is set to @False@ , the API throws an @AliasExistsException@ error if the alias already exists. The default value is @False@ .
    forceAliasCreation :: Lude.Maybe Lude.Bool,
    -- | Specify @"EMAIL"@ if email will be used to send the welcome message. Specify @"SMS"@ if the phone number will be used. The default value is @"SMS"@ . More than one value can be specified.
    desiredDeliveryMediums :: Lude.Maybe [DeliveryMediumType],
    -- | Set to @"RESEND"@ to resend the invitation message to a user that already exists and reset the expiration limit on the user's account. Set to @"SUPPRESS"@ to suppress sending the message. Only one value can be specified.
    messageAction :: Lude.Maybe MessageActionType,
    -- | The user pool ID for the user pool where the user will be created.
    userPoolId :: Lude.Text,
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
    userAttributes :: Lude.Maybe [AttributeType],
    -- | The username for the user. Must be unique within the user pool. Must be a UTF-8 string between 1 and 128 characters. After the user is created, the username cannot be changed.
    username :: Lude.Sensitive Lude.Text,
    -- | The user's validation data. This is an array of name-value pairs that contain user attributes and attribute values that you can use for custom validation, such as restricting the types of user accounts that can be registered. For example, you might choose to allow or disallow user sign-up based on the user's domain.
    --
    -- To configure custom validation, you must create a Pre Sign-up Lambda trigger for the user pool as described in the Amazon Cognito Developer Guide. The Lambda trigger receives the validation data and uses it in the validation process.
    -- The user's validation data is not persisted.
    validationData :: Lude.Maybe [AttributeType]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminCreateUser' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminCreateUser API action, Amazon Cognito invokes the function that is assigned to the /pre sign-up/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminCreateUser request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'temporaryPassword' - The user's temporary password. This password must conform to the password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create User flow, the user must enter the temporary password in the sign-in page along with a new password to be used in all future sign-ins.
-- This parameter is not required. If you do not specify a value, Amazon Cognito generates one for you.
-- The temporary password can only be used until the user account expiration limit that you specified when you created the user pool. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter.
-- * 'forceAliasCreation' - This parameter is only used if the @phone_number_verified@ or @email_verified@ attribute is set to @True@ . Otherwise, it is ignored.
--
-- If this parameter is set to @True@ and the phone number or email address specified in the UserAttributes parameter already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user. The previous user will no longer be able to log in using that alias.
-- If this parameter is set to @False@ , the API throws an @AliasExistsException@ error if the alias already exists. The default value is @False@ .
-- * 'desiredDeliveryMediums' - Specify @"EMAIL"@ if email will be used to send the welcome message. Specify @"SMS"@ if the phone number will be used. The default value is @"SMS"@ . More than one value can be specified.
-- * 'messageAction' - Set to @"RESEND"@ to resend the invitation message to a user that already exists and reset the expiration limit on the user's account. Set to @"SUPPRESS"@ to suppress sending the message. Only one value can be specified.
-- * 'userPoolId' - The user pool ID for the user pool where the user will be created.
-- * 'userAttributes' - An array of name-value pairs that contain user attributes and attribute values to be set for the user to be created. You can create a user without specifying any attributes other than @Username@ . However, any attributes that you specify as required (when creating a user pool or in the __Attributes__ tab of the console) must be supplied either by you (in your call to @AdminCreateUser@ ) or by the user (when he or she signs up in response to your welcome message).
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
-- * 'username' - The username for the user. Must be unique within the user pool. Must be a UTF-8 string between 1 and 128 characters. After the user is created, the username cannot be changed.
-- * 'validationData' - The user's validation data. This is an array of name-value pairs that contain user attributes and attribute values that you can use for custom validation, such as restricting the types of user accounts that can be registered. For example, you might choose to allow or disallow user sign-up based on the user's domain.
--
-- To configure custom validation, you must create a Pre Sign-up Lambda trigger for the user pool as described in the Amazon Cognito Developer Guide. The Lambda trigger receives the validation data and uses it in the validation process.
-- The user's validation data is not persisted.
mkAdminCreateUser ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminCreateUser
mkAdminCreateUser pUserPoolId_ pUsername_ =
  AdminCreateUser'
    { clientMetadata = Lude.Nothing,
      temporaryPassword = Lude.Nothing,
      forceAliasCreation = Lude.Nothing,
      desiredDeliveryMediums = Lude.Nothing,
      messageAction = Lude.Nothing,
      userPoolId = pUserPoolId_,
      userAttributes = Lude.Nothing,
      username = pUsername_,
      validationData = Lude.Nothing
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminCreateUser API action, Amazon Cognito invokes the function that is assigned to the /pre sign-up/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminCreateUser request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuClientMetadata :: Lens.Lens' AdminCreateUser (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
acuClientMetadata = Lens.lens (clientMetadata :: AdminCreateUser -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: AdminCreateUser)
{-# DEPRECATED acuClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The user's temporary password. This password must conform to the password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create User flow, the user must enter the temporary password in the sign-in page along with a new password to be used in all future sign-ins.
-- This parameter is not required. If you do not specify a value, Amazon Cognito generates one for you.
-- The temporary password can only be used until the user account expiration limit that you specified when you created the user pool. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter.
--
-- /Note:/ Consider using 'temporaryPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuTemporaryPassword :: Lens.Lens' AdminCreateUser (Lude.Maybe (Lude.Sensitive Lude.Text))
acuTemporaryPassword = Lens.lens (temporaryPassword :: AdminCreateUser -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {temporaryPassword = a} :: AdminCreateUser)
{-# DEPRECATED acuTemporaryPassword "Use generic-lens or generic-optics with 'temporaryPassword' instead." #-}

-- | This parameter is only used if the @phone_number_verified@ or @email_verified@ attribute is set to @True@ . Otherwise, it is ignored.
--
-- If this parameter is set to @True@ and the phone number or email address specified in the UserAttributes parameter already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user. The previous user will no longer be able to log in using that alias.
-- If this parameter is set to @False@ , the API throws an @AliasExistsException@ error if the alias already exists. The default value is @False@ .
--
-- /Note:/ Consider using 'forceAliasCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuForceAliasCreation :: Lens.Lens' AdminCreateUser (Lude.Maybe Lude.Bool)
acuForceAliasCreation = Lens.lens (forceAliasCreation :: AdminCreateUser -> Lude.Maybe Lude.Bool) (\s a -> s {forceAliasCreation = a} :: AdminCreateUser)
{-# DEPRECATED acuForceAliasCreation "Use generic-lens or generic-optics with 'forceAliasCreation' instead." #-}

-- | Specify @"EMAIL"@ if email will be used to send the welcome message. Specify @"SMS"@ if the phone number will be used. The default value is @"SMS"@ . More than one value can be specified.
--
-- /Note:/ Consider using 'desiredDeliveryMediums' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuDesiredDeliveryMediums :: Lens.Lens' AdminCreateUser (Lude.Maybe [DeliveryMediumType])
acuDesiredDeliveryMediums = Lens.lens (desiredDeliveryMediums :: AdminCreateUser -> Lude.Maybe [DeliveryMediumType]) (\s a -> s {desiredDeliveryMediums = a} :: AdminCreateUser)
{-# DEPRECATED acuDesiredDeliveryMediums "Use generic-lens or generic-optics with 'desiredDeliveryMediums' instead." #-}

-- | Set to @"RESEND"@ to resend the invitation message to a user that already exists and reset the expiration limit on the user's account. Set to @"SUPPRESS"@ to suppress sending the message. Only one value can be specified.
--
-- /Note:/ Consider using 'messageAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuMessageAction :: Lens.Lens' AdminCreateUser (Lude.Maybe MessageActionType)
acuMessageAction = Lens.lens (messageAction :: AdminCreateUser -> Lude.Maybe MessageActionType) (\s a -> s {messageAction = a} :: AdminCreateUser)
{-# DEPRECATED acuMessageAction "Use generic-lens or generic-optics with 'messageAction' instead." #-}

-- | The user pool ID for the user pool where the user will be created.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuUserPoolId :: Lens.Lens' AdminCreateUser Lude.Text
acuUserPoolId = Lens.lens (userPoolId :: AdminCreateUser -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminCreateUser)
{-# DEPRECATED acuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

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
acuUserAttributes :: Lens.Lens' AdminCreateUser (Lude.Maybe [AttributeType])
acuUserAttributes = Lens.lens (userAttributes :: AdminCreateUser -> Lude.Maybe [AttributeType]) (\s a -> s {userAttributes = a} :: AdminCreateUser)
{-# DEPRECATED acuUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The username for the user. Must be unique within the user pool. Must be a UTF-8 string between 1 and 128 characters. After the user is created, the username cannot be changed.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuUsername :: Lens.Lens' AdminCreateUser (Lude.Sensitive Lude.Text)
acuUsername = Lens.lens (username :: AdminCreateUser -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminCreateUser)
{-# DEPRECATED acuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The user's validation data. This is an array of name-value pairs that contain user attributes and attribute values that you can use for custom validation, such as restricting the types of user accounts that can be registered. For example, you might choose to allow or disallow user sign-up based on the user's domain.
--
-- To configure custom validation, you must create a Pre Sign-up Lambda trigger for the user pool as described in the Amazon Cognito Developer Guide. The Lambda trigger receives the validation data and uses it in the validation process.
-- The user's validation data is not persisted.
--
-- /Note:/ Consider using 'validationData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acuValidationData :: Lens.Lens' AdminCreateUser (Lude.Maybe [AttributeType])
acuValidationData = Lens.lens (validationData :: AdminCreateUser -> Lude.Maybe [AttributeType]) (\s a -> s {validationData = a} :: AdminCreateUser)
{-# DEPRECATED acuValidationData "Use generic-lens or generic-optics with 'validationData' instead." #-}

instance Lude.AWSRequest AdminCreateUser where
  type Rs AdminCreateUser = AdminCreateUserResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          AdminCreateUserResponse'
            Lude.<$> (x Lude..?> "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminCreateUser where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminCreateUser" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminCreateUser where
  toJSON AdminCreateUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            ("TemporaryPassword" Lude..=) Lude.<$> temporaryPassword,
            ("ForceAliasCreation" Lude..=) Lude.<$> forceAliasCreation,
            ("DesiredDeliveryMediums" Lude..=) Lude.<$> desiredDeliveryMediums,
            ("MessageAction" Lude..=) Lude.<$> messageAction,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            ("UserAttributes" Lude..=) Lude.<$> userAttributes,
            Lude.Just ("Username" Lude..= username),
            ("ValidationData" Lude..=) Lude.<$> validationData
          ]
      )

instance Lude.ToPath AdminCreateUser where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminCreateUser where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to create the user.
--
-- /See:/ 'mkAdminCreateUserResponse' smart constructor.
data AdminCreateUserResponse = AdminCreateUserResponse'
  { -- | The newly created user.
    user :: Lude.Maybe UserType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminCreateUserResponse' with the minimum fields required to make a request.
--
-- * 'user' - The newly created user.
-- * 'responseStatus' - The response status code.
mkAdminCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminCreateUserResponse
mkAdminCreateUserResponse pResponseStatus_ =
  AdminCreateUserResponse'
    { user = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created user.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acursUser :: Lens.Lens' AdminCreateUserResponse (Lude.Maybe UserType)
acursUser = Lens.lens (user :: AdminCreateUserResponse -> Lude.Maybe UserType) (\s a -> s {user = a} :: AdminCreateUserResponse)
{-# DEPRECATED acursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acursResponseStatus :: Lens.Lens' AdminCreateUserResponse Lude.Int
acursResponseStatus = Lens.lens (responseStatus :: AdminCreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminCreateUserResponse)
{-# DEPRECATED acursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
