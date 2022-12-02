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
-- Module      : Amazonka.CognitoIdentityProvider.ConfirmSignUp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms registration of a new user.
module Amazonka.CognitoIdentityProvider.ConfirmSignUp
  ( -- * Creating a Request
    ConfirmSignUp (..),
    newConfirmSignUp,

    -- * Request Lenses
    confirmSignUp_analyticsMetadata,
    confirmSignUp_clientMetadata,
    confirmSignUp_secretHash,
    confirmSignUp_userContextData,
    confirmSignUp_forceAliasCreation,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to confirm registration of a user.
--
-- /See:/ 'newConfirmSignUp' smart constructor.
data ConfirmSignUp = ConfirmSignUp'
  { -- | The Amazon Pinpoint analytics metadata for collecting metrics for
    -- @ConfirmSignUp@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the ConfirmSignUp API action, Amazon Cognito
    -- invokes the function that is assigned to the /post confirmation/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your ConfirmSignUp request. In your
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
    -- | A keyed-hash message authentication code (HMAC) calculated using the
    -- secret key of a user pool client and username plus the client ID in the
    -- message.
    secretHash :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | Boolean to be specified to force user confirmation irrespective of
    -- existing alias. By default set to @False@. If this parameter is set to
    -- @True@ and the phone number\/email used for sign up confirmation already
    -- exists as an alias with a different user, the API call will migrate the
    -- alias from the previous user to the newly created user being confirmed.
    -- If set to @False@, the API will throw an __AliasExistsException__ error.
    forceAliasCreation :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the app client associated with the user pool.
    clientId :: Data.Sensitive Prelude.Text,
    -- | The user name of the user whose registration you want to confirm.
    username :: Data.Sensitive Prelude.Text,
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
-- 'analyticsMetadata', 'confirmSignUp_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmSignUp@ calls.
--
-- 'clientMetadata', 'confirmSignUp_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ConfirmSignUp API action, Amazon Cognito
-- invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmSignUp request. In your
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
-- 'secretHash', 'confirmSignUp_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'userContextData', 'confirmSignUp_userContextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'forceAliasCreation', 'confirmSignUp_forceAliasCreation' - Boolean to be specified to force user confirmation irrespective of
-- existing alias. By default set to @False@. If this parameter is set to
-- @True@ and the phone number\/email used for sign up confirmation already
-- exists as an alias with a different user, the API call will migrate the
-- alias from the previous user to the newly created user being confirmed.
-- If set to @False@, the API will throw an __AliasExistsException__ error.
--
-- 'clientId', 'confirmSignUp_clientId' - The ID of the app client associated with the user pool.
--
-- 'username', 'confirmSignUp_username' - The user name of the user whose registration you want to confirm.
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
      { analyticsMetadata = Prelude.Nothing,
        clientMetadata = Prelude.Nothing,
        secretHash = Prelude.Nothing,
        userContextData = Prelude.Nothing,
        forceAliasCreation = Prelude.Nothing,
        clientId = Data._Sensitive Lens.# pClientId_,
        username = Data._Sensitive Lens.# pUsername_,
        confirmationCode = pConfirmationCode_
      }

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ConfirmSignUp@ calls.
confirmSignUp_analyticsMetadata :: Lens.Lens' ConfirmSignUp (Prelude.Maybe AnalyticsMetadataType)
confirmSignUp_analyticsMetadata = Lens.lens (\ConfirmSignUp' {analyticsMetadata} -> analyticsMetadata) (\s@ConfirmSignUp' {} a -> s {analyticsMetadata = a} :: ConfirmSignUp)

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ConfirmSignUp API action, Amazon Cognito
-- invokes the function that is assigned to the /post confirmation/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ConfirmSignUp request. In your
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
confirmSignUp_clientMetadata :: Lens.Lens' ConfirmSignUp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
confirmSignUp_clientMetadata = Lens.lens (\ConfirmSignUp' {clientMetadata} -> clientMetadata) (\s@ConfirmSignUp' {} a -> s {clientMetadata = a} :: ConfirmSignUp) Prelude.. Lens.mapping Lens.coerced

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
confirmSignUp_secretHash :: Lens.Lens' ConfirmSignUp (Prelude.Maybe Prelude.Text)
confirmSignUp_secretHash = Lens.lens (\ConfirmSignUp' {secretHash} -> secretHash) (\s@ConfirmSignUp' {} a -> s {secretHash = a} :: ConfirmSignUp) Prelude.. Lens.mapping Data._Sensitive

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
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

-- | The ID of the app client associated with the user pool.
confirmSignUp_clientId :: Lens.Lens' ConfirmSignUp Prelude.Text
confirmSignUp_clientId = Lens.lens (\ConfirmSignUp' {clientId} -> clientId) (\s@ConfirmSignUp' {} a -> s {clientId = a} :: ConfirmSignUp) Prelude.. Data._Sensitive

-- | The user name of the user whose registration you want to confirm.
confirmSignUp_username :: Lens.Lens' ConfirmSignUp Prelude.Text
confirmSignUp_username = Lens.lens (\ConfirmSignUp' {username} -> username) (\s@ConfirmSignUp' {} a -> s {username = a} :: ConfirmSignUp) Prelude.. Data._Sensitive

-- | The confirmation code sent by a user\'s request to confirm registration.
confirmSignUp_confirmationCode :: Lens.Lens' ConfirmSignUp Prelude.Text
confirmSignUp_confirmationCode = Lens.lens (\ConfirmSignUp' {confirmationCode} -> confirmationCode) (\s@ConfirmSignUp' {} a -> s {confirmationCode = a} :: ConfirmSignUp)

instance Core.AWSRequest ConfirmSignUp where
  type
    AWSResponse ConfirmSignUp =
      ConfirmSignUpResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConfirmSignUpResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmSignUp where
  hashWithSalt _salt ConfirmSignUp' {..} =
    _salt `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` secretHash
      `Prelude.hashWithSalt` userContextData
      `Prelude.hashWithSalt` forceAliasCreation
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` confirmationCode

instance Prelude.NFData ConfirmSignUp where
  rnf ConfirmSignUp' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf secretHash
      `Prelude.seq` Prelude.rnf userContextData
      `Prelude.seq` Prelude.rnf forceAliasCreation
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf confirmationCode

instance Data.ToHeaders ConfirmSignUp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ConfirmSignUp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfirmSignUp where
  toJSON ConfirmSignUp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Data..=)
              Prelude.<$> analyticsMetadata,
            ("ClientMetadata" Data..=)
              Prelude.<$> clientMetadata,
            ("SecretHash" Data..=) Prelude.<$> secretHash,
            ("UserContextData" Data..=)
              Prelude.<$> userContextData,
            ("ForceAliasCreation" Data..=)
              Prelude.<$> forceAliasCreation,
            Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just
              ("ConfirmationCode" Data..= confirmationCode)
          ]
      )

instance Data.ToPath ConfirmSignUp where
  toPath = Prelude.const "/"

instance Data.ToQuery ConfirmSignUp where
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

instance Prelude.NFData ConfirmSignUpResponse where
  rnf ConfirmSignUpResponse' {..} =
    Prelude.rnf httpStatus
