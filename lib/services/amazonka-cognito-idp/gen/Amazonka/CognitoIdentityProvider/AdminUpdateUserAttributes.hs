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
-- Module      : Amazonka.CognitoIdentityProvider.AdminUpdateUserAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user\'s attributes, including developer
-- attributes, as an administrator. Works on any user.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- In addition to updating user attributes, this API can also be used to
-- mark phone and email as verified.
--
-- This action might generate an SMS text message. Starting June 1, 2021,
-- U.S. telecom carriers require that you register an origination phone
-- number before you can send SMS messages to U.S. phone numbers. If you
-- use SMS text messages in Amazon Cognito, you must register a phone
-- number with
-- <https://console.aws.amazon.com/pinpoint/home/ Amazon Pinpoint>. Cognito
-- will use the the registered number automatically. Otherwise, Cognito
-- users that must receive SMS messages might be unable to sign up,
-- activate their accounts, or sign in.
--
-- If you have never used SMS text messages with Amazon Cognito or any
-- other Amazon Web Service, Amazon SNS might place your account in SMS
-- sandbox. In
-- /<https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html sandbox mode>/
-- , you’ll have limitations, such as sending messages to only verified
-- phone numbers. After testing in the sandbox environment, you can move
-- out of the SMS sandbox and into production. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-sms-userpool-settings.html SMS message settings for Cognito User Pools>
-- in the /Amazon Cognito Developer Guide/.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminUpdateUserAttributes
  ( -- * Creating a Request
    AdminUpdateUserAttributes (..),
    newAdminUpdateUserAttributes,

    -- * Request Lenses
    adminUpdateUserAttributes_clientMetadata,
    adminUpdateUserAttributes_userPoolId,
    adminUpdateUserAttributes_username,
    adminUpdateUserAttributes_userAttributes,

    -- * Destructuring the Response
    AdminUpdateUserAttributesResponse (..),
    newAdminUpdateUserAttributesResponse,

    -- * Response Lenses
    adminUpdateUserAttributesResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to update the user\'s attributes as an
-- administrator.
--
-- /See:/ 'newAdminUpdateUserAttributes' smart constructor.
data AdminUpdateUserAttributes = AdminUpdateUserAttributes'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the AdminUpdateUserAttributes API action, Amazon
    -- Cognito invokes the function that is assigned to the /custom message/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your AdminUpdateUserAttributes request.
    -- In your function code in Lambda, you can process the @clientMetadata@
    -- value to enhance your workflow for your specific needs.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
    -- in the /Amazon Cognito Developer Guide/.
    --
    -- Take the following limitations into consideration when you use the
    -- ClientMetadata parameter:
    --
    -- -   Amazon Cognito does not store the ClientMetadata value. This data is
    --     available only to Lambda triggers that are assigned to a user pool
    --     to support custom workflows. If your user pool configuration does
    --     not include triggers, the ClientMetadata parameter serves no
    --     purpose.
    --
    -- -   Amazon Cognito does not validate the ClientMetadata value.
    --
    -- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
    --     don\'t use it to provide sensitive information.
    clientMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The user pool ID for the user pool where you want to update user
    -- attributes.
    userPoolId :: Prelude.Text,
    -- | The user name of the user for whom you want to update user attributes.
    username :: Core.Sensitive Prelude.Text,
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: [AttributeType]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminUpdateUserAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'adminUpdateUserAttributes_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminUpdateUserAttributes API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your AdminUpdateUserAttributes request.
-- In your function code in Lambda, you can process the @clientMetadata@
-- value to enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- Take the following limitations into consideration when you use the
-- ClientMetadata parameter:
--
-- -   Amazon Cognito does not store the ClientMetadata value. This data is
--     available only to Lambda triggers that are assigned to a user pool
--     to support custom workflows. If your user pool configuration does
--     not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
--
-- 'userPoolId', 'adminUpdateUserAttributes_userPoolId' - The user pool ID for the user pool where you want to update user
-- attributes.
--
-- 'username', 'adminUpdateUserAttributes_username' - The user name of the user for whom you want to update user attributes.
--
-- 'userAttributes', 'adminUpdateUserAttributes_userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
newAdminUpdateUserAttributes ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminUpdateUserAttributes
newAdminUpdateUserAttributes pUserPoolId_ pUsername_ =
  AdminUpdateUserAttributes'
    { clientMetadata =
        Prelude.Nothing,
      userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_,
      userAttributes = Prelude.mempty
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminUpdateUserAttributes API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your AdminUpdateUserAttributes request.
-- In your function code in Lambda, you can process the @clientMetadata@
-- value to enhance your workflow for your specific needs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers>
-- in the /Amazon Cognito Developer Guide/.
--
-- Take the following limitations into consideration when you use the
-- ClientMetadata parameter:
--
-- -   Amazon Cognito does not store the ClientMetadata value. This data is
--     available only to Lambda triggers that are assigned to a user pool
--     to support custom workflows. If your user pool configuration does
--     not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
adminUpdateUserAttributes_clientMetadata :: Lens.Lens' AdminUpdateUserAttributes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminUpdateUserAttributes_clientMetadata = Lens.lens (\AdminUpdateUserAttributes' {clientMetadata} -> clientMetadata) (\s@AdminUpdateUserAttributes' {} a -> s {clientMetadata = a} :: AdminUpdateUserAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool where you want to update user
-- attributes.
adminUpdateUserAttributes_userPoolId :: Lens.Lens' AdminUpdateUserAttributes Prelude.Text
adminUpdateUserAttributes_userPoolId = Lens.lens (\AdminUpdateUserAttributes' {userPoolId} -> userPoolId) (\s@AdminUpdateUserAttributes' {} a -> s {userPoolId = a} :: AdminUpdateUserAttributes)

-- | The user name of the user for whom you want to update user attributes.
adminUpdateUserAttributes_username :: Lens.Lens' AdminUpdateUserAttributes Prelude.Text
adminUpdateUserAttributes_username = Lens.lens (\AdminUpdateUserAttributes' {username} -> username) (\s@AdminUpdateUserAttributes' {} a -> s {username = a} :: AdminUpdateUserAttributes) Prelude.. Core._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
adminUpdateUserAttributes_userAttributes :: Lens.Lens' AdminUpdateUserAttributes [AttributeType]
adminUpdateUserAttributes_userAttributes = Lens.lens (\AdminUpdateUserAttributes' {userAttributes} -> userAttributes) (\s@AdminUpdateUserAttributes' {} a -> s {userAttributes = a} :: AdminUpdateUserAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest AdminUpdateUserAttributes where
  type
    AWSResponse AdminUpdateUserAttributes =
      AdminUpdateUserAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminUpdateUserAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminUpdateUserAttributes

instance Prelude.NFData AdminUpdateUserAttributes

instance Core.ToHeaders AdminUpdateUserAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminUpdateUserAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AdminUpdateUserAttributes where
  toJSON AdminUpdateUserAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Core..=)
              Prelude.<$> clientMetadata,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("Username" Core..= username),
            Prelude.Just
              ("UserAttributes" Core..= userAttributes)
          ]
      )

instance Core.ToPath AdminUpdateUserAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery AdminUpdateUserAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server for the request to update user
-- attributes as an administrator.
--
-- /See:/ 'newAdminUpdateUserAttributesResponse' smart constructor.
data AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminUpdateUserAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminUpdateUserAttributesResponse_httpStatus' - The response's http status code.
newAdminUpdateUserAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminUpdateUserAttributesResponse
newAdminUpdateUserAttributesResponse pHttpStatus_ =
  AdminUpdateUserAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminUpdateUserAttributesResponse_httpStatus :: Lens.Lens' AdminUpdateUserAttributesResponse Prelude.Int
adminUpdateUserAttributesResponse_httpStatus = Lens.lens (\AdminUpdateUserAttributesResponse' {httpStatus} -> httpStatus) (\s@AdminUpdateUserAttributesResponse' {} a -> s {httpStatus = a} :: AdminUpdateUserAttributesResponse)

instance
  Prelude.NFData
    AdminUpdateUserAttributesResponse
