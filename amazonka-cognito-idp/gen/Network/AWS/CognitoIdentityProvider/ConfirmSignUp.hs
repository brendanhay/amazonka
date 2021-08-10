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
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmSignUp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms registration of a user and handles the existing alias from a
-- previous user.
module Network.AWS.CognitoIdentityProvider.ConfirmSignUp
  ( -- * Creating a Request
    ConfirmSignUp (..),
    newConfirmSignUp,

    -- * Request Lenses
    confirmSignUp_clientMetadata,
    confirmSignUp_userContextData,
    confirmSignUp_forceAliasCreation,
    confirmSignUp_secretHash,
    confirmSignUp_analyticsMetadata,
    confirmSignUp_clientId,
    confirmSignUp_username,
    confirmSignUp_confirmationCode,

    -- * Destructuring the Response
    ConfirmSignUpResponse (..),
    newConfirmSignUpResponse,

    -- * Response Lenses
    confirmSignUpResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to confirm registration of a user.
--
-- /See:/ 'newConfirmSignUp' smart constructor.
data ConfirmSignUp = ConfirmSignUp'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the ConfirmSignUp API action, Amazon Cognito
    -- invokes the function that is assigned to the /post confirmation/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your ConfirmSignUp request. In your
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
    -- | Contextual data such as the user\'s device fingerprint, IP address, or
    -- location used for evaluating the risk of an unexpected event by Amazon
    -- Cognito advanced security.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | Boolean to be specified to force user confirmation irrespective of
    -- existing alias. By default set to @False@. If this parameter is set to
    -- @True@ and the phone number\/email used for sign up confirmation already
    -- exists as an alias with a different user, the API call will migrate the
    -- alias from the previous user to the newly created user being confirmed.
    -- If set to @False@, the API will throw an __AliasExistsException__ error.
    forceAliasCreation :: Prelude.Maybe Prelude.Bool,
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @ConfirmSignUp@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | The ID of the app client associated with the user pool.
    clientId :: Core.Sensitive Prelude.Text,
    -- | The user name of the user whose registration you wish to confirm.
    username :: Core.Sensitive Prelude.Text,
    -- | The confirmation code sent by a user\'s request to confirm registration.
    confirmationCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmSignUp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'confirmSignUp_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the ConfirmSignUp API action, Amazon Cognito
-- invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmSignUp request. In your
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
-- 'userContextData', 'confirmSignUp_userContextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'forceAliasCreation', 'confirmSignUp_forceAliasCreation' - Boolean to be specified to force user confirmation irrespective of
-- existing alias. By default set to @False@. If this parameter is set to
-- @True@ and the phone number\/email used for sign up confirmation already
-- exists as an alias with a different user, the API call will migrate the
-- alias from the previous user to the newly created user being confirmed.
-- If set to @False@, the API will throw an __AliasExistsException__ error.
--
-- 'secretHash', 'confirmSignUp_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'analyticsMetadata', 'confirmSignUp_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmSignUp@ calls.
--
-- 'clientId', 'confirmSignUp_clientId' - The ID of the app client associated with the user pool.
--
-- 'username', 'confirmSignUp_username' - The user name of the user whose registration you wish to confirm.
--
-- 'confirmationCode', 'confirmSignUp_confirmationCode' - The confirmation code sent by a user\'s request to confirm registration.
newConfirmSignUp ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'confirmationCode'
  Prelude.Text ->
  ConfirmSignUp
newConfirmSignUp
  pClientId_
  pUsername_
  pConfirmationCode_ =
    ConfirmSignUp'
      { clientMetadata = Prelude.Nothing,
        userContextData = Prelude.Nothing,
        forceAliasCreation = Prelude.Nothing,
        secretHash = Prelude.Nothing,
        analyticsMetadata = Prelude.Nothing,
        clientId = Core._Sensitive Lens.# pClientId_,
        username = Core._Sensitive Lens.# pUsername_,
        confirmationCode = pConfirmationCode_
      }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the ConfirmSignUp API action, Amazon Cognito
-- invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmSignUp request. In your
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
confirmSignUp_clientMetadata :: Lens.Lens' ConfirmSignUp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
confirmSignUp_clientMetadata = Lens.lens (\ConfirmSignUp' {clientMetadata} -> clientMetadata) (\s@ConfirmSignUp' {} a -> s {clientMetadata = a} :: ConfirmSignUp) Prelude.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
confirmSignUp_userContextData :: Lens.Lens' ConfirmSignUp (Prelude.Maybe UserContextDataType)
confirmSignUp_userContextData = Lens.lens (\ConfirmSignUp' {userContextData} -> userContextData) (\s@ConfirmSignUp' {} a -> s {userContextData = a} :: ConfirmSignUp)

-- | Boolean to be specified to force user confirmation irrespective of
-- existing alias. By default set to @False@. If this parameter is set to
-- @True@ and the phone number\/email used for sign up confirmation already
-- exists as an alias with a different user, the API call will migrate the
-- alias from the previous user to the newly created user being confirmed.
-- If set to @False@, the API will throw an __AliasExistsException__ error.
confirmSignUp_forceAliasCreation :: Lens.Lens' ConfirmSignUp (Prelude.Maybe Prelude.Bool)
confirmSignUp_forceAliasCreation = Lens.lens (\ConfirmSignUp' {forceAliasCreation} -> forceAliasCreation) (\s@ConfirmSignUp' {} a -> s {forceAliasCreation = a} :: ConfirmSignUp)

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
confirmSignUp_secretHash :: Lens.Lens' ConfirmSignUp (Prelude.Maybe Prelude.Text)
confirmSignUp_secretHash = Lens.lens (\ConfirmSignUp' {secretHash} -> secretHash) (\s@ConfirmSignUp' {} a -> s {secretHash = a} :: ConfirmSignUp) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmSignUp@ calls.
confirmSignUp_analyticsMetadata :: Lens.Lens' ConfirmSignUp (Prelude.Maybe AnalyticsMetadataType)
confirmSignUp_analyticsMetadata = Lens.lens (\ConfirmSignUp' {analyticsMetadata} -> analyticsMetadata) (\s@ConfirmSignUp' {} a -> s {analyticsMetadata = a} :: ConfirmSignUp)

-- | The ID of the app client associated with the user pool.
confirmSignUp_clientId :: Lens.Lens' ConfirmSignUp Prelude.Text
confirmSignUp_clientId = Lens.lens (\ConfirmSignUp' {clientId} -> clientId) (\s@ConfirmSignUp' {} a -> s {clientId = a} :: ConfirmSignUp) Prelude.. Core._Sensitive

-- | The user name of the user whose registration you wish to confirm.
confirmSignUp_username :: Lens.Lens' ConfirmSignUp Prelude.Text
confirmSignUp_username = Lens.lens (\ConfirmSignUp' {username} -> username) (\s@ConfirmSignUp' {} a -> s {username = a} :: ConfirmSignUp) Prelude.. Core._Sensitive

-- | The confirmation code sent by a user\'s request to confirm registration.
confirmSignUp_confirmationCode :: Lens.Lens' ConfirmSignUp Prelude.Text
confirmSignUp_confirmationCode = Lens.lens (\ConfirmSignUp' {confirmationCode} -> confirmationCode) (\s@ConfirmSignUp' {} a -> s {confirmationCode = a} :: ConfirmSignUp)

instance Core.AWSRequest ConfirmSignUp where
  type
    AWSResponse ConfirmSignUp =
      ConfirmSignUpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConfirmSignUpResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmSignUp

instance Prelude.NFData ConfirmSignUp

instance Core.ToHeaders ConfirmSignUp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ConfirmSignUp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ConfirmSignUp where
  toJSON ConfirmSignUp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Core..=)
              Prelude.<$> clientMetadata,
            ("UserContextData" Core..=)
              Prelude.<$> userContextData,
            ("ForceAliasCreation" Core..=)
              Prelude.<$> forceAliasCreation,
            ("SecretHash" Core..=) Prelude.<$> secretHash,
            ("AnalyticsMetadata" Core..=)
              Prelude.<$> analyticsMetadata,
            Prelude.Just ("ClientId" Core..= clientId),
            Prelude.Just ("Username" Core..= username),
            Prelude.Just
              ("ConfirmationCode" Core..= confirmationCode)
          ]
      )

instance Core.ToPath ConfirmSignUp where
  toPath = Prelude.const "/"

instance Core.ToQuery ConfirmSignUp where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server for the registration
-- confirmation.
--
-- /See:/ 'newConfirmSignUpResponse' smart constructor.
data ConfirmSignUpResponse = ConfirmSignUpResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmSignUpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'confirmSignUpResponse_httpStatus' - The response's http status code.
newConfirmSignUpResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfirmSignUpResponse
newConfirmSignUpResponse pHttpStatus_ =
  ConfirmSignUpResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
confirmSignUpResponse_httpStatus :: Lens.Lens' ConfirmSignUpResponse Prelude.Int
confirmSignUpResponse_httpStatus = Lens.lens (\ConfirmSignUpResponse' {httpStatus} -> httpStatus) (\s@ConfirmSignUpResponse' {} a -> s {httpStatus = a} :: ConfirmSignUpResponse)

instance Prelude.NFData ConfirmSignUpResponse
