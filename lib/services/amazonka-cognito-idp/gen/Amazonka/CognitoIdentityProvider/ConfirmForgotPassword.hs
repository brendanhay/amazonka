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
-- Module      : Amazonka.CognitoIdentityProvider.ConfirmForgotPassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to enter a confirmation code to reset a forgotten
-- password.
module Amazonka.CognitoIdentityProvider.ConfirmForgotPassword
  ( -- * Creating a Request
    ConfirmForgotPassword (..),
    newConfirmForgotPassword,

    -- * Request Lenses
    confirmForgotPassword_analyticsMetadata,
    confirmForgotPassword_clientMetadata,
    confirmForgotPassword_secretHash,
    confirmForgotPassword_userContextData,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request representing the confirmation for a password reset.
--
-- /See:/ 'newConfirmForgotPassword' smart constructor.
data ConfirmForgotPassword = ConfirmForgotPassword'
  { -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @ConfirmForgotPassword@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the ConfirmForgotPassword API action, Amazon
    -- Cognito invokes the function that is assigned to the /post confirmation/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your ConfirmForgotPassword request. In
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
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Data.Sensitive Prelude.Text,
    -- | The user name of the user for whom you want to enter a code to retrieve
    -- a forgotten password.
    username :: Data.Sensitive Prelude.Text,
    -- | The confirmation code from your user\'s request to reset their password.
    -- For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
    confirmationCode :: Prelude.Text,
    -- | The new password that your user wants to set.
    password :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmForgotPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'confirmForgotPassword_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmForgotPassword@ calls.
--
-- 'clientMetadata', 'confirmForgotPassword_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ConfirmForgotPassword API action, Amazon
-- Cognito invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmForgotPassword request. In
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
-- 'secretHash', 'confirmForgotPassword_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'userContextData', 'confirmForgotPassword_userContextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'clientId', 'confirmForgotPassword_clientId' - The app client ID of the app associated with the user pool.
--
-- 'username', 'confirmForgotPassword_username' - The user name of the user for whom you want to enter a code to retrieve
-- a forgotten password.
--
-- 'confirmationCode', 'confirmForgotPassword_confirmationCode' - The confirmation code from your user\'s request to reset their password.
-- For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
--
-- 'password', 'confirmForgotPassword_password' - The new password that your user wants to set.
newConfirmForgotPassword ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'confirmationCode'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  ConfirmForgotPassword
newConfirmForgotPassword
  pClientId_
  pUsername_
  pConfirmationCode_
  pPassword_ =
    ConfirmForgotPassword'
      { analyticsMetadata =
          Prelude.Nothing,
        clientMetadata = Prelude.Nothing,
        secretHash = Prelude.Nothing,
        userContextData = Prelude.Nothing,
        clientId = Data._Sensitive Lens.# pClientId_,
        username = Data._Sensitive Lens.# pUsername_,
        confirmationCode = pConfirmationCode_,
        password = Data._Sensitive Lens.# pPassword_
      }

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmForgotPassword@ calls.
confirmForgotPassword_analyticsMetadata :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe AnalyticsMetadataType)
confirmForgotPassword_analyticsMetadata = Lens.lens (\ConfirmForgotPassword' {analyticsMetadata} -> analyticsMetadata) (\s@ConfirmForgotPassword' {} a -> s {analyticsMetadata = a} :: ConfirmForgotPassword)

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ConfirmForgotPassword API action, Amazon
-- Cognito invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmForgotPassword request. In
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
confirmForgotPassword_clientMetadata :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
confirmForgotPassword_clientMetadata = Lens.lens (\ConfirmForgotPassword' {clientMetadata} -> clientMetadata) (\s@ConfirmForgotPassword' {} a -> s {clientMetadata = a} :: ConfirmForgotPassword) Prelude.. Lens.mapping Lens.coerced

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
confirmForgotPassword_secretHash :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe Prelude.Text)
confirmForgotPassword_secretHash = Lens.lens (\ConfirmForgotPassword' {secretHash} -> secretHash) (\s@ConfirmForgotPassword' {} a -> s {secretHash = a} :: ConfirmForgotPassword) Prelude.. Lens.mapping Data._Sensitive

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
confirmForgotPassword_userContextData :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe UserContextDataType)
confirmForgotPassword_userContextData = Lens.lens (\ConfirmForgotPassword' {userContextData} -> userContextData) (\s@ConfirmForgotPassword' {} a -> s {userContextData = a} :: ConfirmForgotPassword)

-- | The app client ID of the app associated with the user pool.
confirmForgotPassword_clientId :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_clientId = Lens.lens (\ConfirmForgotPassword' {clientId} -> clientId) (\s@ConfirmForgotPassword' {} a -> s {clientId = a} :: ConfirmForgotPassword) Prelude.. Data._Sensitive

-- | The user name of the user for whom you want to enter a code to retrieve
-- a forgotten password.
confirmForgotPassword_username :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_username = Lens.lens (\ConfirmForgotPassword' {username} -> username) (\s@ConfirmForgotPassword' {} a -> s {username = a} :: ConfirmForgotPassword) Prelude.. Data._Sensitive

-- | The confirmation code from your user\'s request to reset their password.
-- For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
confirmForgotPassword_confirmationCode :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_confirmationCode = Lens.lens (\ConfirmForgotPassword' {confirmationCode} -> confirmationCode) (\s@ConfirmForgotPassword' {} a -> s {confirmationCode = a} :: ConfirmForgotPassword)

-- | The new password that your user wants to set.
confirmForgotPassword_password :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_password = Lens.lens (\ConfirmForgotPassword' {password} -> password) (\s@ConfirmForgotPassword' {} a -> s {password = a} :: ConfirmForgotPassword) Prelude.. Data._Sensitive

instance Core.AWSRequest ConfirmForgotPassword where
  type
    AWSResponse ConfirmForgotPassword =
      ConfirmForgotPasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConfirmForgotPasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmForgotPassword where
  hashWithSalt _salt ConfirmForgotPassword' {..} =
    _salt
      `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` secretHash
      `Prelude.hashWithSalt` userContextData
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` confirmationCode
      `Prelude.hashWithSalt` password

instance Prelude.NFData ConfirmForgotPassword where
  rnf ConfirmForgotPassword' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf secretHash
      `Prelude.seq` Prelude.rnf userContextData
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf confirmationCode
      `Prelude.seq` Prelude.rnf password

instance Data.ToHeaders ConfirmForgotPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ConfirmForgotPassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfirmForgotPassword where
  toJSON ConfirmForgotPassword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("SecretHash" Data..=) Prelude.<$> secretHash,
            ("UserContextData" Data..=)
              Prelude.<$> userContextData,
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just
              ("ConfirmationCode" Data..= confirmationCode),
            Prelude.Just ("Password" Data..= password)
          ]
      )

instance Data.ToPath ConfirmForgotPassword where
  toPath = Prelude.const "/"

instance Data.ToQuery ConfirmForgotPassword where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server that results from a user\'s request to
-- retrieve a forgotten password.
--
-- /See:/ 'newConfirmForgotPasswordResponse' smart constructor.
data ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ConfirmForgotPasswordResponse
newConfirmForgotPasswordResponse pHttpStatus_ =
  ConfirmForgotPasswordResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
confirmForgotPasswordResponse_httpStatus :: Lens.Lens' ConfirmForgotPasswordResponse Prelude.Int
confirmForgotPasswordResponse_httpStatus = Lens.lens (\ConfirmForgotPasswordResponse' {httpStatus} -> httpStatus) (\s@ConfirmForgotPasswordResponse' {} a -> s {httpStatus = a} :: ConfirmForgotPasswordResponse)

instance Prelude.NFData ConfirmForgotPasswordResponse where
  rnf ConfirmForgotPasswordResponse' {..} =
    Prelude.rnf httpStatus
