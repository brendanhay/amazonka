{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminCreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the specified user pool.
--
-- If @MessageAction@ is not set, the default is to send a welcome message
-- via email or phone (SMS).
--
-- This message is based on a template that you configured in your call to
-- create or update a user pool. This template includes your custom sign-up
-- instructions and placeholders for user name and temporary password.
--
-- Alternatively, you can call @AdminCreateUser@ with “SUPPRESS” for the
-- @MessageAction@ parameter, and Amazon Cognito will not send any email.
--
-- In either case, the user will be in the @FORCE_CHANGE_PASSWORD@ state
-- until they sign in and change their password.
--
-- @AdminCreateUser@ requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminCreateUser
  ( -- * Creating a Request
    AdminCreateUser (..),
    newAdminCreateUser,

    -- * Request Lenses
    adminCreateUser_clientMetadata,
    adminCreateUser_messageAction,
    adminCreateUser_forceAliasCreation,
    adminCreateUser_desiredDeliveryMediums,
    adminCreateUser_temporaryPassword,
    adminCreateUser_userAttributes,
    adminCreateUser_validationData,
    adminCreateUser_userPoolId,
    adminCreateUser_username,

    -- * Destructuring the Response
    AdminCreateUserResponse (..),
    newAdminCreateUserResponse,

    -- * Response Lenses
    adminCreateUserResponse_user,
    adminCreateUserResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create a user in the specified user pool.
--
-- /See:/ 'newAdminCreateUser' smart constructor.
data AdminCreateUser = AdminCreateUser'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the AdminCreateUser API action, Amazon
    -- Cognito invokes the function that is assigned to the /pre sign-up/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your AdminCreateUser request. In your
    -- function code in AWS Lambda, you can process the @clientMetadata@ value
    -- to enhance your workflow for your specific needs.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
    -- in the /Amazon Cognito Developer Guide/.
    --
    -- Take the following limitations into consideration when you use the
    -- ClientMetadata parameter:
    --
    -- -   Amazon Cognito does not store the ClientMetadata value. This data is
    --     available only to AWS Lambda triggers that are assigned to a user
    --     pool to support custom workflows. If your user pool configuration
    --     does not include triggers, the ClientMetadata parameter serves no
    --     purpose.
    --
    -- -   Amazon Cognito does not validate the ClientMetadata value.
    --
    -- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
    --     don\'t use it to provide sensitive information.
    clientMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Set to @\"RESEND\"@ to resend the invitation message to a user that
    -- already exists and reset the expiration limit on the user\'s account.
    -- Set to @\"SUPPRESS\"@ to suppress sending the message. Only one value
    -- can be specified.
    messageAction :: Prelude.Maybe MessageActionType,
    -- | This parameter is only used if the @phone_number_verified@ or
    -- @email_verified@ attribute is set to @True@. Otherwise, it is ignored.
    --
    -- If this parameter is set to @True@ and the phone number or email address
    -- specified in the UserAttributes parameter already exists as an alias
    -- with a different user, the API call will migrate the alias from the
    -- previous user to the newly created user. The previous user will no
    -- longer be able to log in using that alias.
    --
    -- If this parameter is set to @False@, the API throws an
    -- @AliasExistsException@ error if the alias already exists. The default
    -- value is @False@.
    forceAliasCreation :: Prelude.Maybe Prelude.Bool,
    -- | Specify @\"EMAIL\"@ if email will be used to send the welcome message.
    -- Specify @\"SMS\"@ if the phone number will be used. The default value is
    -- @\"SMS\"@. More than one value can be specified.
    desiredDeliveryMediums :: Prelude.Maybe [DeliveryMediumType],
    -- | The user\'s temporary password. This password must conform to the
    -- password policy that you specified when you created the user pool.
    --
    -- The temporary password is valid only once. To complete the Admin Create
    -- User flow, the user must enter the temporary password in the sign-in
    -- page along with a new password to be used in all future sign-ins.
    --
    -- This parameter is not required. If you do not specify a value, Amazon
    -- Cognito generates one for you.
    --
    -- The temporary password can only be used until the user account
    -- expiration limit that you specified when you created the user pool. To
    -- reset the account after that time limit, you must call @AdminCreateUser@
    -- again, specifying @\"RESEND\"@ for the @MessageAction@ parameter.
    temporaryPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | An array of name-value pairs that contain user attributes and attribute
    -- values to be set for the user to be created. You can create a user
    -- without specifying any attributes other than @Username@. However, any
    -- attributes that you specify as required (when creating a user pool or in
    -- the __Attributes__ tab of the console) must be supplied either by you
    -- (in your call to @AdminCreateUser@) or by the user (when he or she signs
    -- up in response to your welcome message).
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    --
    -- To send a message inviting the user to sign up, you must specify the
    -- user\'s email address or phone number. This can be done in your call to
    -- AdminCreateUser or in the __Users__ tab of the Amazon Cognito console
    -- for managing your user pools.
    --
    -- In your call to @AdminCreateUser@, you can set the @email_verified@
    -- attribute to @True@, and you can set the @phone_number_verified@
    -- attribute to @True@. (You can also do this by calling
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>.)
    --
    -- -   __email__: The email address of the user to whom the message that
    --     contains the code and username will be sent. Required if the
    --     @email_verified@ attribute is set to @True@, or if @\"EMAIL\"@ is
    --     specified in the @DesiredDeliveryMediums@ parameter.
    --
    -- -   __phone_number__: The phone number of the user to whom the message
    --     that contains the code and username will be sent. Required if the
    --     @phone_number_verified@ attribute is set to @True@, or if @\"SMS\"@
    --     is specified in the @DesiredDeliveryMediums@ parameter.
    userAttributes :: Prelude.Maybe [AttributeType],
    -- | The user\'s validation data. This is an array of name-value pairs that
    -- contain user attributes and attribute values that you can use for custom
    -- validation, such as restricting the types of user accounts that can be
    -- registered. For example, you might choose to allow or disallow user
    -- sign-up based on the user\'s domain.
    --
    -- To configure custom validation, you must create a Pre Sign-up Lambda
    -- trigger for the user pool as described in the Amazon Cognito Developer
    -- Guide. The Lambda trigger receives the validation data and uses it in
    -- the validation process.
    --
    -- The user\'s validation data is not persisted.
    validationData :: Prelude.Maybe [AttributeType],
    -- | The user pool ID for the user pool where the user will be created.
    userPoolId :: Prelude.Text,
    -- | The username for the user. Must be unique within the user pool. Must be
    -- a UTF-8 string between 1 and 128 characters. After the user is created,
    -- the username cannot be changed.
    username :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminCreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'adminCreateUser_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminCreateUser API action, Amazon
-- Cognito invokes the function that is assigned to the /pre sign-up/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your AdminCreateUser request. In your
-- function code in AWS Lambda, you can process the @clientMetadata@ value
-- to enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- Take the following limitations into consideration when you use the
-- ClientMetadata parameter:
--
-- -   Amazon Cognito does not store the ClientMetadata value. This data is
--     available only to AWS Lambda triggers that are assigned to a user
--     pool to support custom workflows. If your user pool configuration
--     does not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
--
-- 'messageAction', 'adminCreateUser_messageAction' - Set to @\"RESEND\"@ to resend the invitation message to a user that
-- already exists and reset the expiration limit on the user\'s account.
-- Set to @\"SUPPRESS\"@ to suppress sending the message. Only one value
-- can be specified.
--
-- 'forceAliasCreation', 'adminCreateUser_forceAliasCreation' - This parameter is only used if the @phone_number_verified@ or
-- @email_verified@ attribute is set to @True@. Otherwise, it is ignored.
--
-- If this parameter is set to @True@ and the phone number or email address
-- specified in the UserAttributes parameter already exists as an alias
-- with a different user, the API call will migrate the alias from the
-- previous user to the newly created user. The previous user will no
-- longer be able to log in using that alias.
--
-- If this parameter is set to @False@, the API throws an
-- @AliasExistsException@ error if the alias already exists. The default
-- value is @False@.
--
-- 'desiredDeliveryMediums', 'adminCreateUser_desiredDeliveryMediums' - Specify @\"EMAIL\"@ if email will be used to send the welcome message.
-- Specify @\"SMS\"@ if the phone number will be used. The default value is
-- @\"SMS\"@. More than one value can be specified.
--
-- 'temporaryPassword', 'adminCreateUser_temporaryPassword' - The user\'s temporary password. This password must conform to the
-- password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create
-- User flow, the user must enter the temporary password in the sign-in
-- page along with a new password to be used in all future sign-ins.
--
-- This parameter is not required. If you do not specify a value, Amazon
-- Cognito generates one for you.
--
-- The temporary password can only be used until the user account
-- expiration limit that you specified when you created the user pool. To
-- reset the account after that time limit, you must call @AdminCreateUser@
-- again, specifying @\"RESEND\"@ for the @MessageAction@ parameter.
--
-- 'userAttributes', 'adminCreateUser_userAttributes' - An array of name-value pairs that contain user attributes and attribute
-- values to be set for the user to be created. You can create a user
-- without specifying any attributes other than @Username@. However, any
-- attributes that you specify as required (when creating a user pool or in
-- the __Attributes__ tab of the console) must be supplied either by you
-- (in your call to @AdminCreateUser@) or by the user (when he or she signs
-- up in response to your welcome message).
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- To send a message inviting the user to sign up, you must specify the
-- user\'s email address or phone number. This can be done in your call to
-- AdminCreateUser or in the __Users__ tab of the Amazon Cognito console
-- for managing your user pools.
--
-- In your call to @AdminCreateUser@, you can set the @email_verified@
-- attribute to @True@, and you can set the @phone_number_verified@
-- attribute to @True@. (You can also do this by calling
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>.)
--
-- -   __email__: The email address of the user to whom the message that
--     contains the code and username will be sent. Required if the
--     @email_verified@ attribute is set to @True@, or if @\"EMAIL\"@ is
--     specified in the @DesiredDeliveryMediums@ parameter.
--
-- -   __phone_number__: The phone number of the user to whom the message
--     that contains the code and username will be sent. Required if the
--     @phone_number_verified@ attribute is set to @True@, or if @\"SMS\"@
--     is specified in the @DesiredDeliveryMediums@ parameter.
--
-- 'validationData', 'adminCreateUser_validationData' - The user\'s validation data. This is an array of name-value pairs that
-- contain user attributes and attribute values that you can use for custom
-- validation, such as restricting the types of user accounts that can be
-- registered. For example, you might choose to allow or disallow user
-- sign-up based on the user\'s domain.
--
-- To configure custom validation, you must create a Pre Sign-up Lambda
-- trigger for the user pool as described in the Amazon Cognito Developer
-- Guide. The Lambda trigger receives the validation data and uses it in
-- the validation process.
--
-- The user\'s validation data is not persisted.
--
-- 'userPoolId', 'adminCreateUser_userPoolId' - The user pool ID for the user pool where the user will be created.
--
-- 'username', 'adminCreateUser_username' - The username for the user. Must be unique within the user pool. Must be
-- a UTF-8 string between 1 and 128 characters. After the user is created,
-- the username cannot be changed.
newAdminCreateUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminCreateUser
newAdminCreateUser pUserPoolId_ pUsername_ =
  AdminCreateUser'
    { clientMetadata = Prelude.Nothing,
      messageAction = Prelude.Nothing,
      forceAliasCreation = Prelude.Nothing,
      desiredDeliveryMediums = Prelude.Nothing,
      temporaryPassword = Prelude.Nothing,
      userAttributes = Prelude.Nothing,
      validationData = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminCreateUser API action, Amazon
-- Cognito invokes the function that is assigned to the /pre sign-up/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your AdminCreateUser request. In your
-- function code in AWS Lambda, you can process the @clientMetadata@ value
-- to enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- Take the following limitations into consideration when you use the
-- ClientMetadata parameter:
--
-- -   Amazon Cognito does not store the ClientMetadata value. This data is
--     available only to AWS Lambda triggers that are assigned to a user
--     pool to support custom workflows. If your user pool configuration
--     does not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
adminCreateUser_clientMetadata :: Lens.Lens' AdminCreateUser (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminCreateUser_clientMetadata = Lens.lens (\AdminCreateUser' {clientMetadata} -> clientMetadata) (\s@AdminCreateUser' {} a -> s {clientMetadata = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens._Coerce

-- | Set to @\"RESEND\"@ to resend the invitation message to a user that
-- already exists and reset the expiration limit on the user\'s account.
-- Set to @\"SUPPRESS\"@ to suppress sending the message. Only one value
-- can be specified.
adminCreateUser_messageAction :: Lens.Lens' AdminCreateUser (Prelude.Maybe MessageActionType)
adminCreateUser_messageAction = Lens.lens (\AdminCreateUser' {messageAction} -> messageAction) (\s@AdminCreateUser' {} a -> s {messageAction = a} :: AdminCreateUser)

-- | This parameter is only used if the @phone_number_verified@ or
-- @email_verified@ attribute is set to @True@. Otherwise, it is ignored.
--
-- If this parameter is set to @True@ and the phone number or email address
-- specified in the UserAttributes parameter already exists as an alias
-- with a different user, the API call will migrate the alias from the
-- previous user to the newly created user. The previous user will no
-- longer be able to log in using that alias.
--
-- If this parameter is set to @False@, the API throws an
-- @AliasExistsException@ error if the alias already exists. The default
-- value is @False@.
adminCreateUser_forceAliasCreation :: Lens.Lens' AdminCreateUser (Prelude.Maybe Prelude.Bool)
adminCreateUser_forceAliasCreation = Lens.lens (\AdminCreateUser' {forceAliasCreation} -> forceAliasCreation) (\s@AdminCreateUser' {} a -> s {forceAliasCreation = a} :: AdminCreateUser)

-- | Specify @\"EMAIL\"@ if email will be used to send the welcome message.
-- Specify @\"SMS\"@ if the phone number will be used. The default value is
-- @\"SMS\"@. More than one value can be specified.
adminCreateUser_desiredDeliveryMediums :: Lens.Lens' AdminCreateUser (Prelude.Maybe [DeliveryMediumType])
adminCreateUser_desiredDeliveryMediums = Lens.lens (\AdminCreateUser' {desiredDeliveryMediums} -> desiredDeliveryMediums) (\s@AdminCreateUser' {} a -> s {desiredDeliveryMediums = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens._Coerce

-- | The user\'s temporary password. This password must conform to the
-- password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create
-- User flow, the user must enter the temporary password in the sign-in
-- page along with a new password to be used in all future sign-ins.
--
-- This parameter is not required. If you do not specify a value, Amazon
-- Cognito generates one for you.
--
-- The temporary password can only be used until the user account
-- expiration limit that you specified when you created the user pool. To
-- reset the account after that time limit, you must call @AdminCreateUser@
-- again, specifying @\"RESEND\"@ for the @MessageAction@ parameter.
adminCreateUser_temporaryPassword :: Lens.Lens' AdminCreateUser (Prelude.Maybe Prelude.Text)
adminCreateUser_temporaryPassword = Lens.lens (\AdminCreateUser' {temporaryPassword} -> temporaryPassword) (\s@AdminCreateUser' {} a -> s {temporaryPassword = a} :: AdminCreateUser) Prelude.. Lens.mapping Core._Sensitive

-- | An array of name-value pairs that contain user attributes and attribute
-- values to be set for the user to be created. You can create a user
-- without specifying any attributes other than @Username@. However, any
-- attributes that you specify as required (when creating a user pool or in
-- the __Attributes__ tab of the console) must be supplied either by you
-- (in your call to @AdminCreateUser@) or by the user (when he or she signs
-- up in response to your welcome message).
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- To send a message inviting the user to sign up, you must specify the
-- user\'s email address or phone number. This can be done in your call to
-- AdminCreateUser or in the __Users__ tab of the Amazon Cognito console
-- for managing your user pools.
--
-- In your call to @AdminCreateUser@, you can set the @email_verified@
-- attribute to @True@, and you can set the @phone_number_verified@
-- attribute to @True@. (You can also do this by calling
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>.)
--
-- -   __email__: The email address of the user to whom the message that
--     contains the code and username will be sent. Required if the
--     @email_verified@ attribute is set to @True@, or if @\"EMAIL\"@ is
--     specified in the @DesiredDeliveryMediums@ parameter.
--
-- -   __phone_number__: The phone number of the user to whom the message
--     that contains the code and username will be sent. Required if the
--     @phone_number_verified@ attribute is set to @True@, or if @\"SMS\"@
--     is specified in the @DesiredDeliveryMediums@ parameter.
adminCreateUser_userAttributes :: Lens.Lens' AdminCreateUser (Prelude.Maybe [AttributeType])
adminCreateUser_userAttributes = Lens.lens (\AdminCreateUser' {userAttributes} -> userAttributes) (\s@AdminCreateUser' {} a -> s {userAttributes = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens._Coerce

-- | The user\'s validation data. This is an array of name-value pairs that
-- contain user attributes and attribute values that you can use for custom
-- validation, such as restricting the types of user accounts that can be
-- registered. For example, you might choose to allow or disallow user
-- sign-up based on the user\'s domain.
--
-- To configure custom validation, you must create a Pre Sign-up Lambda
-- trigger for the user pool as described in the Amazon Cognito Developer
-- Guide. The Lambda trigger receives the validation data and uses it in
-- the validation process.
--
-- The user\'s validation data is not persisted.
adminCreateUser_validationData :: Lens.Lens' AdminCreateUser (Prelude.Maybe [AttributeType])
adminCreateUser_validationData = Lens.lens (\AdminCreateUser' {validationData} -> validationData) (\s@AdminCreateUser' {} a -> s {validationData = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens._Coerce

-- | The user pool ID for the user pool where the user will be created.
adminCreateUser_userPoolId :: Lens.Lens' AdminCreateUser Prelude.Text
adminCreateUser_userPoolId = Lens.lens (\AdminCreateUser' {userPoolId} -> userPoolId) (\s@AdminCreateUser' {} a -> s {userPoolId = a} :: AdminCreateUser)

-- | The username for the user. Must be unique within the user pool. Must be
-- a UTF-8 string between 1 and 128 characters. After the user is created,
-- the username cannot be changed.
adminCreateUser_username :: Lens.Lens' AdminCreateUser Prelude.Text
adminCreateUser_username = Lens.lens (\AdminCreateUser' {username} -> username) (\s@AdminCreateUser' {} a -> s {username = a} :: AdminCreateUser) Prelude.. Core._Sensitive

instance Core.AWSRequest AdminCreateUser where
  type
    AWSResponse AdminCreateUser =
      AdminCreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminCreateUserResponse'
            Prelude.<$> (x Core..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminCreateUser

instance Prelude.NFData AdminCreateUser

instance Core.ToHeaders AdminCreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminCreateUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AdminCreateUser where
  toJSON AdminCreateUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Core..=)
              Prelude.<$> clientMetadata,
            ("MessageAction" Core..=) Prelude.<$> messageAction,
            ("ForceAliasCreation" Core..=)
              Prelude.<$> forceAliasCreation,
            ("DesiredDeliveryMediums" Core..=)
              Prelude.<$> desiredDeliveryMediums,
            ("TemporaryPassword" Core..=)
              Prelude.<$> temporaryPassword,
            ("UserAttributes" Core..=)
              Prelude.<$> userAttributes,
            ("ValidationData" Core..=)
              Prelude.<$> validationData,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath AdminCreateUser where
  toPath = Prelude.const "/"

instance Core.ToQuery AdminCreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to create the
-- user.
--
-- /See:/ 'newAdminCreateUserResponse' smart constructor.
data AdminCreateUserResponse = AdminCreateUserResponse'
  { -- | The newly created user.
    user :: Prelude.Maybe UserType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminCreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'adminCreateUserResponse_user' - The newly created user.
--
-- 'httpStatus', 'adminCreateUserResponse_httpStatus' - The response's http status code.
newAdminCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminCreateUserResponse
newAdminCreateUserResponse pHttpStatus_ =
  AdminCreateUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created user.
adminCreateUserResponse_user :: Lens.Lens' AdminCreateUserResponse (Prelude.Maybe UserType)
adminCreateUserResponse_user = Lens.lens (\AdminCreateUserResponse' {user} -> user) (\s@AdminCreateUserResponse' {} a -> s {user = a} :: AdminCreateUserResponse)

-- | The response's http status code.
adminCreateUserResponse_httpStatus :: Lens.Lens' AdminCreateUserResponse Prelude.Int
adminCreateUserResponse_httpStatus = Lens.lens (\AdminCreateUserResponse' {httpStatus} -> httpStatus) (\s@AdminCreateUserResponse' {} a -> s {httpStatus = a} :: AdminCreateUserResponse)

instance Prelude.NFData AdminCreateUserResponse
