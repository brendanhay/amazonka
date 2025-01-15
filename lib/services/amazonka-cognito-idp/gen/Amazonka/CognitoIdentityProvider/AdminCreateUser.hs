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
-- Module      : Amazonka.CognitoIdentityProvider.AdminCreateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the specified user pool.
--
-- If @MessageAction@ isn\'t set, the default is to send a welcome message
-- via email or phone (SMS).
--
-- This action might generate an SMS text message. Starting June 1, 2021,
-- US telecom carriers require you to register an origination phone number
-- before you can send SMS messages to US phone numbers. If you use SMS
-- text messages in Amazon Cognito, you must register a phone number with
-- <https://console.aws.amazon.com/pinpoint/home/ Amazon Pinpoint>. Amazon
-- Cognito uses the registered number automatically. Otherwise, Amazon
-- Cognito users who must receive SMS messages might not be able to sign
-- up, activate their accounts, or sign in.
--
-- If you have never used SMS text messages with Amazon Cognito or any
-- other Amazon Web Service, Amazon Simple Notification Service might place
-- your account in the SMS sandbox. In
-- /<https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html sandbox mode>/
-- , you can send messages only to verified phone numbers. After you test
-- your app while in the sandbox environment, you can move out of the
-- sandbox and into production. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-sms-userpool-settings.html SMS message settings for Amazon Cognito user pools>
-- in the /Amazon Cognito Developer Guide/.
--
-- This message is based on a template that you configured in your call to
-- create or update a user pool. This template includes your custom sign-up
-- instructions and placeholders for user name and temporary password.
--
-- Alternatively, you can call @AdminCreateUser@ with @SUPPRESS@ for the
-- @MessageAction@ parameter, and Amazon Cognito won\'t send any email.
--
-- In either case, the user will be in the @FORCE_CHANGE_PASSWORD@ state
-- until they sign in and change their password.
--
-- @AdminCreateUser@ requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminCreateUser
  ( -- * Creating a Request
    AdminCreateUser (..),
    newAdminCreateUser,

    -- * Request Lenses
    adminCreateUser_clientMetadata,
    adminCreateUser_desiredDeliveryMediums,
    adminCreateUser_forceAliasCreation,
    adminCreateUser_messageAction,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to create a user in the specified user pool.
--
-- /See:/ 'newAdminCreateUser' smart constructor.
data AdminCreateUser = AdminCreateUser'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the AdminCreateUser API action, Amazon Cognito
    -- invokes the function that is assigned to the /pre sign-up/ trigger. When
    -- Amazon Cognito invokes this function, it passes a JSON payload, which
    -- the function receives as input. This payload contains a @clientMetadata@
    -- attribute, which provides the data that you assigned to the
    -- ClientMetadata parameter in your AdminCreateUser request. In your
    -- function code in Lambda, you can process the @clientMetadata@ value to
    -- enhance your workflow for your specific needs.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing user pool Workflows with Lambda Triggers>
    -- in the /Amazon Cognito Developer Guide/.
    --
    -- When you use the ClientMetadata parameter, remember that Amazon Cognito
    -- won\'t do the following:
    --
    -- -   Store the ClientMetadata value. This data is available only to
    --     Lambda triggers that are assigned to a user pool to support custom
    --     workflows. If your user pool configuration doesn\'t include
    --     triggers, the ClientMetadata parameter serves no purpose.
    --
    -- -   Validate the ClientMetadata value.
    --
    -- -   Encrypt the ClientMetadata value. Don\'t use Amazon Cognito to
    --     provide sensitive information.
    clientMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specify @\"EMAIL\"@ if email will be used to send the welcome message.
    -- Specify @\"SMS\"@ if the phone number will be used. The default value is
    -- @\"SMS\"@. You can specify more than one value.
    desiredDeliveryMediums :: Prelude.Maybe [DeliveryMediumType],
    -- | This parameter is used only if the @phone_number_verified@ or
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
    -- | Set to @RESEND@ to resend the invitation message to a user that already
    -- exists and reset the expiration limit on the user\'s account. Set to
    -- @SUPPRESS@ to suppress sending the message. You can specify only one
    -- value.
    messageAction :: Prelude.Maybe MessageActionType,
    -- | The user\'s temporary password. This password must conform to the
    -- password policy that you specified when you created the user pool.
    --
    -- The temporary password is valid only once. To complete the Admin Create
    -- User flow, the user must enter the temporary password in the sign-in
    -- page, along with a new password to be used in all future sign-ins.
    --
    -- This parameter isn\'t required. If you don\'t specify a value, Amazon
    -- Cognito generates one for you.
    --
    -- The temporary password can only be used until the user account
    -- expiration limit that you specified when you created the user pool. To
    -- reset the account after that time limit, you must call @AdminCreateUser@
    -- again, specifying @\"RESEND\"@ for the @MessageAction@ parameter.
    temporaryPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An array of name-value pairs that contain user attributes and attribute
    -- values to be set for the user to be created. You can create a user
    -- without specifying any attributes other than @Username@. However, any
    -- attributes that you specify as required (when creating a user pool or in
    -- the __Attributes__ tab of the console) either you should supply (in your
    -- call to @AdminCreateUser@) or the user should supply (when they sign up
    -- in response to your welcome message).
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    --
    -- To send a message inviting the user to sign up, you must specify the
    -- user\'s email address or phone number. You can do this in your call to
    -- AdminCreateUser or in the __Users__ tab of the Amazon Cognito console
    -- for managing your user pools.
    --
    -- In your call to @AdminCreateUser@, you can set the @email_verified@
    -- attribute to @True@, and you can set the @phone_number_verified@
    -- attribute to @True@. You can also do this by calling
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>.
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
    -- The user\'s validation data isn\'t persisted.
    validationData :: Prelude.Maybe [AttributeType],
    -- | The user pool ID for the user pool where the user will be created.
    userPoolId :: Prelude.Text,
    -- | The username for the user. Must be unique within the user pool. Must be
    -- a UTF-8 string between 1 and 128 characters. After the user is created,
    -- the username can\'t be changed.
    username :: Data.Sensitive Prelude.Text
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
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminCreateUser API action, Amazon Cognito
-- invokes the function that is assigned to the /pre sign-up/ trigger. When
-- Amazon Cognito invokes this function, it passes a JSON payload, which
-- the function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your AdminCreateUser request. In your
-- function code in Lambda, you can process the @clientMetadata@ value to
-- enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing user pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- When you use the ClientMetadata parameter, remember that Amazon Cognito
-- won\'t do the following:
--
-- -   Store the ClientMetadata value. This data is available only to
--     Lambda triggers that are assigned to a user pool to support custom
--     workflows. If your user pool configuration doesn\'t include
--     triggers, the ClientMetadata parameter serves no purpose.
--
-- -   Validate the ClientMetadata value.
--
-- -   Encrypt the ClientMetadata value. Don\'t use Amazon Cognito to
--     provide sensitive information.
--
-- 'desiredDeliveryMediums', 'adminCreateUser_desiredDeliveryMediums' - Specify @\"EMAIL\"@ if email will be used to send the welcome message.
-- Specify @\"SMS\"@ if the phone number will be used. The default value is
-- @\"SMS\"@. You can specify more than one value.
--
-- 'forceAliasCreation', 'adminCreateUser_forceAliasCreation' - This parameter is used only if the @phone_number_verified@ or
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
-- 'messageAction', 'adminCreateUser_messageAction' - Set to @RESEND@ to resend the invitation message to a user that already
-- exists and reset the expiration limit on the user\'s account. Set to
-- @SUPPRESS@ to suppress sending the message. You can specify only one
-- value.
--
-- 'temporaryPassword', 'adminCreateUser_temporaryPassword' - The user\'s temporary password. This password must conform to the
-- password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create
-- User flow, the user must enter the temporary password in the sign-in
-- page, along with a new password to be used in all future sign-ins.
--
-- This parameter isn\'t required. If you don\'t specify a value, Amazon
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
-- the __Attributes__ tab of the console) either you should supply (in your
-- call to @AdminCreateUser@) or the user should supply (when they sign up
-- in response to your welcome message).
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- To send a message inviting the user to sign up, you must specify the
-- user\'s email address or phone number. You can do this in your call to
-- AdminCreateUser or in the __Users__ tab of the Amazon Cognito console
-- for managing your user pools.
--
-- In your call to @AdminCreateUser@, you can set the @email_verified@
-- attribute to @True@, and you can set the @phone_number_verified@
-- attribute to @True@. You can also do this by calling
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>.
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
-- The user\'s validation data isn\'t persisted.
--
-- 'userPoolId', 'adminCreateUser_userPoolId' - The user pool ID for the user pool where the user will be created.
--
-- 'username', 'adminCreateUser_username' - The username for the user. Must be unique within the user pool. Must be
-- a UTF-8 string between 1 and 128 characters. After the user is created,
-- the username can\'t be changed.
newAdminCreateUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminCreateUser
newAdminCreateUser pUserPoolId_ pUsername_ =
  AdminCreateUser'
    { clientMetadata = Prelude.Nothing,
      desiredDeliveryMediums = Prelude.Nothing,
      forceAliasCreation = Prelude.Nothing,
      messageAction = Prelude.Nothing,
      temporaryPassword = Prelude.Nothing,
      userAttributes = Prelude.Nothing,
      validationData = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      username = Data._Sensitive Lens.# pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminCreateUser API action, Amazon Cognito
-- invokes the function that is assigned to the /pre sign-up/ trigger. When
-- Amazon Cognito invokes this function, it passes a JSON payload, which
-- the function receives as input. This payload contains a @clientMetadata@
-- attribute, which provides the data that you assigned to the
-- ClientMetadata parameter in your AdminCreateUser request. In your
-- function code in Lambda, you can process the @clientMetadata@ value to
-- enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing user pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- When you use the ClientMetadata parameter, remember that Amazon Cognito
-- won\'t do the following:
--
-- -   Store the ClientMetadata value. This data is available only to
--     Lambda triggers that are assigned to a user pool to support custom
--     workflows. If your user pool configuration doesn\'t include
--     triggers, the ClientMetadata parameter serves no purpose.
--
-- -   Validate the ClientMetadata value.
--
-- -   Encrypt the ClientMetadata value. Don\'t use Amazon Cognito to
--     provide sensitive information.
adminCreateUser_clientMetadata :: Lens.Lens' AdminCreateUser (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminCreateUser_clientMetadata = Lens.lens (\AdminCreateUser' {clientMetadata} -> clientMetadata) (\s@AdminCreateUser' {} a -> s {clientMetadata = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens.coerced

-- | Specify @\"EMAIL\"@ if email will be used to send the welcome message.
-- Specify @\"SMS\"@ if the phone number will be used. The default value is
-- @\"SMS\"@. You can specify more than one value.
adminCreateUser_desiredDeliveryMediums :: Lens.Lens' AdminCreateUser (Prelude.Maybe [DeliveryMediumType])
adminCreateUser_desiredDeliveryMediums = Lens.lens (\AdminCreateUser' {desiredDeliveryMediums} -> desiredDeliveryMediums) (\s@AdminCreateUser' {} a -> s {desiredDeliveryMediums = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens.coerced

-- | This parameter is used only if the @phone_number_verified@ or
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

-- | Set to @RESEND@ to resend the invitation message to a user that already
-- exists and reset the expiration limit on the user\'s account. Set to
-- @SUPPRESS@ to suppress sending the message. You can specify only one
-- value.
adminCreateUser_messageAction :: Lens.Lens' AdminCreateUser (Prelude.Maybe MessageActionType)
adminCreateUser_messageAction = Lens.lens (\AdminCreateUser' {messageAction} -> messageAction) (\s@AdminCreateUser' {} a -> s {messageAction = a} :: AdminCreateUser)

-- | The user\'s temporary password. This password must conform to the
-- password policy that you specified when you created the user pool.
--
-- The temporary password is valid only once. To complete the Admin Create
-- User flow, the user must enter the temporary password in the sign-in
-- page, along with a new password to be used in all future sign-ins.
--
-- This parameter isn\'t required. If you don\'t specify a value, Amazon
-- Cognito generates one for you.
--
-- The temporary password can only be used until the user account
-- expiration limit that you specified when you created the user pool. To
-- reset the account after that time limit, you must call @AdminCreateUser@
-- again, specifying @\"RESEND\"@ for the @MessageAction@ parameter.
adminCreateUser_temporaryPassword :: Lens.Lens' AdminCreateUser (Prelude.Maybe Prelude.Text)
adminCreateUser_temporaryPassword = Lens.lens (\AdminCreateUser' {temporaryPassword} -> temporaryPassword) (\s@AdminCreateUser' {} a -> s {temporaryPassword = a} :: AdminCreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | An array of name-value pairs that contain user attributes and attribute
-- values to be set for the user to be created. You can create a user
-- without specifying any attributes other than @Username@. However, any
-- attributes that you specify as required (when creating a user pool or in
-- the __Attributes__ tab of the console) either you should supply (in your
-- call to @AdminCreateUser@) or the user should supply (when they sign up
-- in response to your welcome message).
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- To send a message inviting the user to sign up, you must specify the
-- user\'s email address or phone number. You can do this in your call to
-- AdminCreateUser or in the __Users__ tab of the Amazon Cognito console
-- for managing your user pools.
--
-- In your call to @AdminCreateUser@, you can set the @email_verified@
-- attribute to @True@, and you can set the @phone_number_verified@
-- attribute to @True@. You can also do this by calling
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminUpdateUserAttributes.html AdminUpdateUserAttributes>.
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
adminCreateUser_userAttributes = Lens.lens (\AdminCreateUser' {userAttributes} -> userAttributes) (\s@AdminCreateUser' {} a -> s {userAttributes = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens.coerced

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
-- The user\'s validation data isn\'t persisted.
adminCreateUser_validationData :: Lens.Lens' AdminCreateUser (Prelude.Maybe [AttributeType])
adminCreateUser_validationData = Lens.lens (\AdminCreateUser' {validationData} -> validationData) (\s@AdminCreateUser' {} a -> s {validationData = a} :: AdminCreateUser) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool where the user will be created.
adminCreateUser_userPoolId :: Lens.Lens' AdminCreateUser Prelude.Text
adminCreateUser_userPoolId = Lens.lens (\AdminCreateUser' {userPoolId} -> userPoolId) (\s@AdminCreateUser' {} a -> s {userPoolId = a} :: AdminCreateUser)

-- | The username for the user. Must be unique within the user pool. Must be
-- a UTF-8 string between 1 and 128 characters. After the user is created,
-- the username can\'t be changed.
adminCreateUser_username :: Lens.Lens' AdminCreateUser Prelude.Text
adminCreateUser_username = Lens.lens (\AdminCreateUser' {username} -> username) (\s@AdminCreateUser' {} a -> s {username = a} :: AdminCreateUser) Prelude.. Data._Sensitive

instance Core.AWSRequest AdminCreateUser where
  type
    AWSResponse AdminCreateUser =
      AdminCreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminCreateUserResponse'
            Prelude.<$> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminCreateUser where
  hashWithSalt _salt AdminCreateUser' {..} =
    _salt
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` desiredDeliveryMediums
      `Prelude.hashWithSalt` forceAliasCreation
      `Prelude.hashWithSalt` messageAction
      `Prelude.hashWithSalt` temporaryPassword
      `Prelude.hashWithSalt` userAttributes
      `Prelude.hashWithSalt` validationData
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AdminCreateUser where
  rnf AdminCreateUser' {..} =
    Prelude.rnf clientMetadata `Prelude.seq`
      Prelude.rnf desiredDeliveryMediums `Prelude.seq`
        Prelude.rnf forceAliasCreation `Prelude.seq`
          Prelude.rnf messageAction `Prelude.seq`
            Prelude.rnf temporaryPassword `Prelude.seq`
              Prelude.rnf userAttributes `Prelude.seq`
                Prelude.rnf validationData `Prelude.seq`
                  Prelude.rnf userPoolId `Prelude.seq`
                    Prelude.rnf username

instance Data.ToHeaders AdminCreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminCreateUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminCreateUser where
  toJSON AdminCreateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("DesiredDeliveryMediums" Data..=)
              Prelude.<$> desiredDeliveryMediums,
            ("ForceAliasCreation" Data..=)
              Prelude.<$> forceAliasCreation,
            ("MessageAction" Data..=) Prelude.<$> messageAction,
            ("TemporaryPassword" Data..=)
              Prelude.<$> temporaryPassword,
            ("UserAttributes" Data..=)
              Prelude.<$> userAttributes,
            ("ValidationData" Data..=)
              Prelude.<$> validationData,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath AdminCreateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminCreateUser where
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

instance Prelude.NFData AdminCreateUserResponse where
  rnf AdminCreateUserResponse' {..} =
    Prelude.rnf user `Prelude.seq`
      Prelude.rnf httpStatus
