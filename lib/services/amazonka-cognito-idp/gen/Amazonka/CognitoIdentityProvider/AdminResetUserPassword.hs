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
-- Module      : Amazonka.CognitoIdentityProvider.AdminResetUserPassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified user\'s password in a user pool as an
-- administrator. Works on any user.
--
-- When a developer calls this API, the current password is invalidated, so
-- it must be changed. If a user tries to sign in after the API is called,
-- the app will get a PasswordResetRequiredException exception back and
-- should direct the user down the flow to reset the password, which is the
-- same as the forgot password flow. In addition, if the user pool has
-- phone verification selected and a verified phone number exists for the
-- user, or if email verification is selected and a verified email exists
-- for the user, calling this API will also result in sending a message to
-- the end user with the code to change their password.
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
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminResetUserPassword
  ( -- * Creating a Request
    AdminResetUserPassword (..),
    newAdminResetUserPassword,

    -- * Request Lenses
    adminResetUserPassword_clientMetadata,
    adminResetUserPassword_userPoolId,
    adminResetUserPassword_username,

    -- * Destructuring the Response
    AdminResetUserPasswordResponse (..),
    newAdminResetUserPasswordResponse,

    -- * Response Lenses
    adminResetUserPasswordResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to reset a user\'s password as an administrator.
--
-- /See:/ 'newAdminResetUserPassword' smart constructor.
data AdminResetUserPassword = AdminResetUserPassword'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the AdminResetUserPassword API action, Amazon
    -- Cognito invokes the function that is assigned to the /custom message/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your AdminResetUserPassword request. In
    -- your function code in Lambda, you can process the @clientMetadata@ value
    -- to enhance your workflow for your specific needs.
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
    -- | The user pool ID for the user pool where you want to reset the user\'s
    -- password.
    userPoolId :: Prelude.Text,
    -- | The user name of the user whose password you want to reset.
    username :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminResetUserPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'adminResetUserPassword_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminResetUserPassword API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your AdminResetUserPassword request. In
-- your function code in Lambda, you can process the @clientMetadata@ value
-- to enhance your workflow for your specific needs.
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
-- 'userPoolId', 'adminResetUserPassword_userPoolId' - The user pool ID for the user pool where you want to reset the user\'s
-- password.
--
-- 'username', 'adminResetUserPassword_username' - The user name of the user whose password you want to reset.
newAdminResetUserPassword ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminResetUserPassword
newAdminResetUserPassword pUserPoolId_ pUsername_ =
  AdminResetUserPassword'
    { clientMetadata =
        Prelude.Nothing,
      userPoolId = pUserPoolId_,
      username = Data._Sensitive Lens.# pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the AdminResetUserPassword API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your AdminResetUserPassword request. In
-- your function code in Lambda, you can process the @clientMetadata@ value
-- to enhance your workflow for your specific needs.
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
adminResetUserPassword_clientMetadata :: Lens.Lens' AdminResetUserPassword (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
adminResetUserPassword_clientMetadata = Lens.lens (\AdminResetUserPassword' {clientMetadata} -> clientMetadata) (\s@AdminResetUserPassword' {} a -> s {clientMetadata = a} :: AdminResetUserPassword) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool where you want to reset the user\'s
-- password.
adminResetUserPassword_userPoolId :: Lens.Lens' AdminResetUserPassword Prelude.Text
adminResetUserPassword_userPoolId = Lens.lens (\AdminResetUserPassword' {userPoolId} -> userPoolId) (\s@AdminResetUserPassword' {} a -> s {userPoolId = a} :: AdminResetUserPassword)

-- | The user name of the user whose password you want to reset.
adminResetUserPassword_username :: Lens.Lens' AdminResetUserPassword Prelude.Text
adminResetUserPassword_username = Lens.lens (\AdminResetUserPassword' {username} -> username) (\s@AdminResetUserPassword' {} a -> s {username = a} :: AdminResetUserPassword) Prelude.. Data._Sensitive

instance Core.AWSRequest AdminResetUserPassword where
  type
    AWSResponse AdminResetUserPassword =
      AdminResetUserPasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminResetUserPasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminResetUserPassword where
  hashWithSalt _salt AdminResetUserPassword' {..} =
    _salt
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AdminResetUserPassword where
  rnf AdminResetUserPassword' {..} =
    Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders AdminResetUserPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminResetUserPassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminResetUserPassword where
  toJSON AdminResetUserPassword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath AdminResetUserPassword where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminResetUserPassword where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to reset a user password as an
-- administrator.
--
-- /See:/ 'newAdminResetUserPasswordResponse' smart constructor.
data AdminResetUserPasswordResponse = AdminResetUserPasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminResetUserPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminResetUserPasswordResponse_httpStatus' - The response's http status code.
newAdminResetUserPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminResetUserPasswordResponse
newAdminResetUserPasswordResponse pHttpStatus_ =
  AdminResetUserPasswordResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminResetUserPasswordResponse_httpStatus :: Lens.Lens' AdminResetUserPasswordResponse Prelude.Int
adminResetUserPasswordResponse_httpStatus = Lens.lens (\AdminResetUserPasswordResponse' {httpStatus} -> httpStatus) (\s@AdminResetUserPasswordResponse' {} a -> s {httpStatus = a} :: AdminResetUserPasswordResponse)

instance
  Prelude.NFData
    AdminResetUserPasswordResponse
  where
  rnf AdminResetUserPasswordResponse' {..} =
    Prelude.rnf httpStatus
