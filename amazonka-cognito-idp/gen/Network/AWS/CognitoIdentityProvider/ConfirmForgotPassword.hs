{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    clientMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @ConfirmForgotPassword@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The app client ID of the app associated with the user pool.
    clientId :: Prelude.Sensitive Prelude.Text,
    -- | The user name of the user for whom you want to enter a code to retrieve
    -- a forgotten password.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The confirmation code sent by a user\'s request to retrieve a forgotten
    -- password. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
    confirmationCode :: Prelude.Text,
    -- | The password sent by a user\'s request to retrieve a forgotten password.
    password :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      { clientMetadata =
          Prelude.Nothing,
        userContextData = Prelude.Nothing,
        secretHash = Prelude.Nothing,
        analyticsMetadata = Prelude.Nothing,
        clientId = Prelude._Sensitive Lens.# pClientId_,
        username = Prelude._Sensitive Lens.# pUsername_,
        confirmationCode = pConfirmationCode_,
        password = Prelude._Sensitive Lens.# pPassword_
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
confirmForgotPassword_clientMetadata :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
confirmForgotPassword_clientMetadata = Lens.lens (\ConfirmForgotPassword' {clientMetadata} -> clientMetadata) (\s@ConfirmForgotPassword' {} a -> s {clientMetadata = a} :: ConfirmForgotPassword) Prelude.. Lens.mapping Prelude._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
confirmForgotPassword_userContextData :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe UserContextDataType)
confirmForgotPassword_userContextData = Lens.lens (\ConfirmForgotPassword' {userContextData} -> userContextData) (\s@ConfirmForgotPassword' {} a -> s {userContextData = a} :: ConfirmForgotPassword)

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
confirmForgotPassword_secretHash :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe Prelude.Text)
confirmForgotPassword_secretHash = Lens.lens (\ConfirmForgotPassword' {secretHash} -> secretHash) (\s@ConfirmForgotPassword' {} a -> s {secretHash = a} :: ConfirmForgotPassword) Prelude.. Lens.mapping Prelude._Sensitive

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmForgotPassword@ calls.
confirmForgotPassword_analyticsMetadata :: Lens.Lens' ConfirmForgotPassword (Prelude.Maybe AnalyticsMetadataType)
confirmForgotPassword_analyticsMetadata = Lens.lens (\ConfirmForgotPassword' {analyticsMetadata} -> analyticsMetadata) (\s@ConfirmForgotPassword' {} a -> s {analyticsMetadata = a} :: ConfirmForgotPassword)

-- | The app client ID of the app associated with the user pool.
confirmForgotPassword_clientId :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_clientId = Lens.lens (\ConfirmForgotPassword' {clientId} -> clientId) (\s@ConfirmForgotPassword' {} a -> s {clientId = a} :: ConfirmForgotPassword) Prelude.. Prelude._Sensitive

-- | The user name of the user for whom you want to enter a code to retrieve
-- a forgotten password.
confirmForgotPassword_username :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_username = Lens.lens (\ConfirmForgotPassword' {username} -> username) (\s@ConfirmForgotPassword' {} a -> s {username = a} :: ConfirmForgotPassword) Prelude.. Prelude._Sensitive

-- | The confirmation code sent by a user\'s request to retrieve a forgotten
-- password. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ForgotPassword.html ForgotPassword>.
confirmForgotPassword_confirmationCode :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_confirmationCode = Lens.lens (\ConfirmForgotPassword' {confirmationCode} -> confirmationCode) (\s@ConfirmForgotPassword' {} a -> s {confirmationCode = a} :: ConfirmForgotPassword)

-- | The password sent by a user\'s request to retrieve a forgotten password.
confirmForgotPassword_password :: Lens.Lens' ConfirmForgotPassword Prelude.Text
confirmForgotPassword_password = Lens.lens (\ConfirmForgotPassword' {password} -> password) (\s@ConfirmForgotPassword' {} a -> s {password = a} :: ConfirmForgotPassword) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest ConfirmForgotPassword where
  type
    Rs ConfirmForgotPassword =
      ConfirmForgotPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConfirmForgotPasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmForgotPassword

instance Prelude.NFData ConfirmForgotPassword

instance Prelude.ToHeaders ConfirmForgotPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.ConfirmForgotPassword" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ConfirmForgotPassword where
  toJSON ConfirmForgotPassword' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Prelude..=)
              Prelude.<$> clientMetadata,
            ("UserContextData" Prelude..=)
              Prelude.<$> userContextData,
            ("SecretHash" Prelude..=) Prelude.<$> secretHash,
            ("AnalyticsMetadata" Prelude..=)
              Prelude.<$> analyticsMetadata,
            Prelude.Just ("ClientId" Prelude..= clientId),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just
              ("ConfirmationCode" Prelude..= confirmationCode),
            Prelude.Just ("Password" Prelude..= password)
          ]
      )

instance Prelude.ToPath ConfirmForgotPassword where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ConfirmForgotPassword where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server that results from a user\'s request to
-- retrieve a forgotten password.
--
-- /See:/ 'newConfirmForgotPasswordResponse' smart constructor.
data ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ConfirmForgotPasswordResponse
