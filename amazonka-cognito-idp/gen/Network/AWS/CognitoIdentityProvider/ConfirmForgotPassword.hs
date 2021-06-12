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
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to enter a confirmation code to reset a forgotten
-- password.
module Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
  ( -- * Creating a Request
    ConfirmForgotPassword (..),
    newConfirmForgotPassword,

    -- * Request Lenses
    confirmForgotPassword_clientMetadata,
    confirmForgotPassword_userContextData,
    confirmForgotPassword_secretHash,
    confirmForgotPassword_analyticsMetadata,
    confirmForgotPassword_clientId,
    confirmForgotPassword_username,
    confirmForgotPassword_confirmationCode,
    confirmForgotPassword_password,

    -- * Destructuring the Response
    ConfirmForgotPasswordResponse (..),
    newConfirmForgotPasswordResponse,

    -- * Response Lenses
    confirmForgotPasswordResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request representing the confirmation for a password reset.
--
-- /See:/ 'newConfirmForgotPassword' smart constructor.
data ConfirmForgotPassword = ConfirmForgotPassword'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the ConfirmForgotPassword API action, Amazon
    -- Cognito invokes the function that is assigned to the /post confirmation/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your ConfirmForgotPassword request. In
    -- your function code in AWS Lambda, you can process the @clientMetadata@
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
    --     available only to AWS Lambda triggers that are assigned to a user
    --     pool to support custom workflows. If your user pool configuration
    --     does not include triggers, the ClientMetadata parameter serves no
    --     purpose.
    --
    -- -   Amazon Cognito does not validate the ClientMetadata value.
    --
    -- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
    --     don\'t use it to provide sensitive information.
    clientMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    userContextData :: Core.Maybe UserContextDataType,
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @ConfirmForgotPassword@ calls.
    analyticsMetadata :: Core.Maybe AnalyticsMetadataType,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Core.Sensitive Core.Text,
    -- | The user name of the user for whom you want to enter a code to retrieve
    -- a forgotten password.
    username :: Core.Sensitive Core.Text,
    -- | The confirmation code sent by a user\'s request to retrieve a forgotten
    -- password. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
    confirmationCode :: Core.Text,
    -- | The password sent by a user\'s request to retrieve a forgotten password.
    password :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfirmForgotPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'confirmForgotPassword_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the ConfirmForgotPassword API action, Amazon
-- Cognito invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmForgotPassword request. In
-- your function code in AWS Lambda, you can process the @clientMetadata@
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
-- 'userContextData', 'confirmForgotPassword_userContextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'secretHash', 'confirmForgotPassword_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'analyticsMetadata', 'confirmForgotPassword_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmForgotPassword@ calls.
--
-- 'clientId', 'confirmForgotPassword_clientId' - The app client ID of the app associated with the user pool.
--
-- 'username', 'confirmForgotPassword_username' - The user name of the user for whom you want to enter a code to retrieve
-- a forgotten password.
--
-- 'confirmationCode', 'confirmForgotPassword_confirmationCode' - The confirmation code sent by a user\'s request to retrieve a forgotten
-- password. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
--
-- 'password', 'confirmForgotPassword_password' - The password sent by a user\'s request to retrieve a forgotten password.
newConfirmForgotPassword ::
  -- | 'clientId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  -- | 'confirmationCode'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  ConfirmForgotPassword
newConfirmForgotPassword
  pClientId_
  pUsername_
  pConfirmationCode_
  pPassword_ =
    ConfirmForgotPassword'
      { clientMetadata =
          Core.Nothing,
        userContextData = Core.Nothing,
        secretHash = Core.Nothing,
        analyticsMetadata = Core.Nothing,
        clientId = Core._Sensitive Lens.# pClientId_,
        username = Core._Sensitive Lens.# pUsername_,
        confirmationCode = pConfirmationCode_,
        password = Core._Sensitive Lens.# pPassword_
      }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the ConfirmForgotPassword API action, Amazon
-- Cognito invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmForgotPassword request. In
-- your function code in AWS Lambda, you can process the @clientMetadata@
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
--     available only to AWS Lambda triggers that are assigned to a user
--     pool to support custom workflows. If your user pool configuration
--     does not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
confirmForgotPassword_clientMetadata :: Lens.Lens' ConfirmForgotPassword (Core.Maybe (Core.HashMap Core.Text Core.Text))
confirmForgotPassword_clientMetadata = Lens.lens (\ConfirmForgotPassword' {clientMetadata} -> clientMetadata) (\s@ConfirmForgotPassword' {} a -> s {clientMetadata = a} :: ConfirmForgotPassword) Core.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
confirmForgotPassword_userContextData :: Lens.Lens' ConfirmForgotPassword (Core.Maybe UserContextDataType)
confirmForgotPassword_userContextData = Lens.lens (\ConfirmForgotPassword' {userContextData} -> userContextData) (\s@ConfirmForgotPassword' {} a -> s {userContextData = a} :: ConfirmForgotPassword)

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
confirmForgotPassword_secretHash :: Lens.Lens' ConfirmForgotPassword (Core.Maybe Core.Text)
confirmForgotPassword_secretHash = Lens.lens (\ConfirmForgotPassword' {secretHash} -> secretHash) (\s@ConfirmForgotPassword' {} a -> s {secretHash = a} :: ConfirmForgotPassword) Core.. Lens.mapping Core._Sensitive

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmForgotPassword@ calls.
confirmForgotPassword_analyticsMetadata :: Lens.Lens' ConfirmForgotPassword (Core.Maybe AnalyticsMetadataType)
confirmForgotPassword_analyticsMetadata = Lens.lens (\ConfirmForgotPassword' {analyticsMetadata} -> analyticsMetadata) (\s@ConfirmForgotPassword' {} a -> s {analyticsMetadata = a} :: ConfirmForgotPassword)

-- | The app client ID of the app associated with the user pool.
confirmForgotPassword_clientId :: Lens.Lens' ConfirmForgotPassword Core.Text
confirmForgotPassword_clientId = Lens.lens (\ConfirmForgotPassword' {clientId} -> clientId) (\s@ConfirmForgotPassword' {} a -> s {clientId = a} :: ConfirmForgotPassword) Core.. Core._Sensitive

-- | The user name of the user for whom you want to enter a code to retrieve
-- a forgotten password.
confirmForgotPassword_username :: Lens.Lens' ConfirmForgotPassword Core.Text
confirmForgotPassword_username = Lens.lens (\ConfirmForgotPassword' {username} -> username) (\s@ConfirmForgotPassword' {} a -> s {username = a} :: ConfirmForgotPassword) Core.. Core._Sensitive

-- | The confirmation code sent by a user\'s request to retrieve a forgotten
-- password. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
confirmForgotPassword_confirmationCode :: Lens.Lens' ConfirmForgotPassword Core.Text
confirmForgotPassword_confirmationCode = Lens.lens (\ConfirmForgotPassword' {confirmationCode} -> confirmationCode) (\s@ConfirmForgotPassword' {} a -> s {confirmationCode = a} :: ConfirmForgotPassword)

-- | The password sent by a user\'s request to retrieve a forgotten password.
confirmForgotPassword_password :: Lens.Lens' ConfirmForgotPassword Core.Text
confirmForgotPassword_password = Lens.lens (\ConfirmForgotPassword' {password} -> password) (\s@ConfirmForgotPassword' {} a -> s {password = a} :: ConfirmForgotPassword) Core.. Core._Sensitive

instance Core.AWSRequest ConfirmForgotPassword where
  type
    AWSResponse ConfirmForgotPassword =
      ConfirmForgotPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConfirmForgotPasswordResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ConfirmForgotPassword

instance Core.NFData ConfirmForgotPassword

instance Core.ToHeaders ConfirmForgotPassword where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ConfirmForgotPassword" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ConfirmForgotPassword where
  toJSON ConfirmForgotPassword' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("UserContextData" Core..=) Core.<$> userContextData,
            ("SecretHash" Core..=) Core.<$> secretHash,
            ("AnalyticsMetadata" Core..=)
              Core.<$> analyticsMetadata,
            Core.Just ("ClientId" Core..= clientId),
            Core.Just ("Username" Core..= username),
            Core.Just
              ("ConfirmationCode" Core..= confirmationCode),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath ConfirmForgotPassword where
  toPath = Core.const "/"

instance Core.ToQuery ConfirmForgotPassword where
  toQuery = Core.const Core.mempty

-- | The response from the server that results from a user\'s request to
-- retrieve a forgotten password.
--
-- /See:/ 'newConfirmForgotPasswordResponse' smart constructor.
data ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfirmForgotPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'confirmForgotPasswordResponse_httpStatus' - The response's http status code.
newConfirmForgotPasswordResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ConfirmForgotPasswordResponse
newConfirmForgotPasswordResponse pHttpStatus_ =
  ConfirmForgotPasswordResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
confirmForgotPasswordResponse_httpStatus :: Lens.Lens' ConfirmForgotPasswordResponse Core.Int
confirmForgotPasswordResponse_httpStatus = Lens.lens (\ConfirmForgotPasswordResponse' {httpStatus} -> httpStatus) (\s@ConfirmForgotPasswordResponse' {} a -> s {httpStatus = a} :: ConfirmForgotPasswordResponse)

instance Core.NFData ConfirmForgotPasswordResponse
