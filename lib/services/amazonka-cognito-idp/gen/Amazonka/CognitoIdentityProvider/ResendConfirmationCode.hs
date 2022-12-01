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
-- Module      : Amazonka.CognitoIdentityProvider.ResendConfirmationCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the confirmation (for confirmation of registration) to a
-- specific user in the user pool.
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
module Amazonka.CognitoIdentityProvider.ResendConfirmationCode
  ( -- * Creating a Request
    ResendConfirmationCode (..),
    newResendConfirmationCode,

    -- * Request Lenses
    resendConfirmationCode_analyticsMetadata,
    resendConfirmationCode_clientMetadata,
    resendConfirmationCode_secretHash,
    resendConfirmationCode_userContextData,
    resendConfirmationCode_clientId,
    resendConfirmationCode_username,

    -- * Destructuring the Response
    ResendConfirmationCodeResponse (..),
    newResendConfirmationCodeResponse,

    -- * Response Lenses
    resendConfirmationCodeResponse_codeDeliveryDetails,
    resendConfirmationCodeResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to resend the confirmation code.
--
-- /See:/ 'newResendConfirmationCode' smart constructor.
data ResendConfirmationCode = ResendConfirmationCode'
  { -- | The Amazon Pinpoint analytics metadata that contributes to your metrics
    -- for @ResendConfirmationCode@ calls.
    analyticsMetadata :: Prelude.Maybe AnalyticsMetadataType,
    -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the ResendConfirmationCode API action, Amazon
    -- Cognito invokes the function that is assigned to the /custom message/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your ResendConfirmationCode request. In
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
    secretHash :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Contextual data about your user session, such as the device fingerprint,
    -- IP address, or location. Amazon Cognito advanced security evaluates the
    -- risk of an authentication event based on the context that your app
    -- generates and passes to Amazon Cognito when it makes API requests.
    userContextData :: Prelude.Maybe UserContextDataType,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Prelude.Text,
    -- | The @username@ attribute of the user to whom you want to resend a
    -- confirmation code.
    username :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResendConfirmationCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsMetadata', 'resendConfirmationCode_analyticsMetadata' - The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @ResendConfirmationCode@ calls.
--
-- 'clientMetadata', 'resendConfirmationCode_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ResendConfirmationCode API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ResendConfirmationCode request. In
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
-- 'secretHash', 'resendConfirmationCode_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'userContextData', 'resendConfirmationCode_userContextData' - Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
--
-- 'clientId', 'resendConfirmationCode_clientId' - The ID of the client associated with the user pool.
--
-- 'username', 'resendConfirmationCode_username' - The @username@ attribute of the user to whom you want to resend a
-- confirmation code.
newResendConfirmationCode ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  ResendConfirmationCode
newResendConfirmationCode pClientId_ pUsername_ =
  ResendConfirmationCode'
    { analyticsMetadata =
        Prelude.Nothing,
      clientMetadata = Prelude.Nothing,
      secretHash = Prelude.Nothing,
      userContextData = Prelude.Nothing,
      clientId = Core._Sensitive Lens.# pClientId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | The Amazon Pinpoint analytics metadata that contributes to your metrics
-- for @ResendConfirmationCode@ calls.
resendConfirmationCode_analyticsMetadata :: Lens.Lens' ResendConfirmationCode (Prelude.Maybe AnalyticsMetadataType)
resendConfirmationCode_analyticsMetadata = Lens.lens (\ResendConfirmationCode' {analyticsMetadata} -> analyticsMetadata) (\s@ResendConfirmationCode' {} a -> s {analyticsMetadata = a} :: ResendConfirmationCode)

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the ResendConfirmationCode API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ResendConfirmationCode request. In
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
resendConfirmationCode_clientMetadata :: Lens.Lens' ResendConfirmationCode (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resendConfirmationCode_clientMetadata = Lens.lens (\ResendConfirmationCode' {clientMetadata} -> clientMetadata) (\s@ResendConfirmationCode' {} a -> s {clientMetadata = a} :: ResendConfirmationCode) Prelude.. Lens.mapping Lens.coerced

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
resendConfirmationCode_secretHash :: Lens.Lens' ResendConfirmationCode (Prelude.Maybe Prelude.Text)
resendConfirmationCode_secretHash = Lens.lens (\ResendConfirmationCode' {secretHash} -> secretHash) (\s@ResendConfirmationCode' {} a -> s {secretHash = a} :: ResendConfirmationCode) Prelude.. Lens.mapping Core._Sensitive

-- | Contextual data about your user session, such as the device fingerprint,
-- IP address, or location. Amazon Cognito advanced security evaluates the
-- risk of an authentication event based on the context that your app
-- generates and passes to Amazon Cognito when it makes API requests.
resendConfirmationCode_userContextData :: Lens.Lens' ResendConfirmationCode (Prelude.Maybe UserContextDataType)
resendConfirmationCode_userContextData = Lens.lens (\ResendConfirmationCode' {userContextData} -> userContextData) (\s@ResendConfirmationCode' {} a -> s {userContextData = a} :: ResendConfirmationCode)

-- | The ID of the client associated with the user pool.
resendConfirmationCode_clientId :: Lens.Lens' ResendConfirmationCode Prelude.Text
resendConfirmationCode_clientId = Lens.lens (\ResendConfirmationCode' {clientId} -> clientId) (\s@ResendConfirmationCode' {} a -> s {clientId = a} :: ResendConfirmationCode) Prelude.. Core._Sensitive

-- | The @username@ attribute of the user to whom you want to resend a
-- confirmation code.
resendConfirmationCode_username :: Lens.Lens' ResendConfirmationCode Prelude.Text
resendConfirmationCode_username = Lens.lens (\ResendConfirmationCode' {username} -> username) (\s@ResendConfirmationCode' {} a -> s {username = a} :: ResendConfirmationCode) Prelude.. Core._Sensitive

instance Core.AWSRequest ResendConfirmationCode where
  type
    AWSResponse ResendConfirmationCode =
      ResendConfirmationCodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResendConfirmationCodeResponse'
            Prelude.<$> (x Core..?> "CodeDeliveryDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResendConfirmationCode where
  hashWithSalt _salt ResendConfirmationCode' {..} =
    _salt `Prelude.hashWithSalt` analyticsMetadata
      `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` secretHash
      `Prelude.hashWithSalt` userContextData
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` username

instance Prelude.NFData ResendConfirmationCode where
  rnf ResendConfirmationCode' {..} =
    Prelude.rnf analyticsMetadata
      `Prelude.seq` Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf secretHash
      `Prelude.seq` Prelude.rnf userContextData
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf username

instance Core.ToHeaders ResendConfirmationCode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ResendConfirmationCode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ResendConfirmationCode where
  toJSON ResendConfirmationCode' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AnalyticsMetadata" Core..=)
              Prelude.<$> analyticsMetadata,
            ("ClientMetadata" Core..=)
              Prelude.<$> clientMetadata,
            ("SecretHash" Core..=) Prelude.<$> secretHash,
            ("UserContextData" Core..=)
              Prelude.<$> userContextData,
            Prelude.Just ("ClientId" Core..= clientId),
            Prelude.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath ResendConfirmationCode where
  toPath = Prelude.const "/"

instance Core.ToQuery ResendConfirmationCode where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server when Amazon Cognito makes the request to
-- resend a confirmation code.
--
-- /See:/ 'newResendConfirmationCodeResponse' smart constructor.
data ResendConfirmationCodeResponse = ResendConfirmationCodeResponse'
  { -- | The code delivery details returned by the server in response to the
    -- request to resend the confirmation code.
    codeDeliveryDetails :: Prelude.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResendConfirmationCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeDeliveryDetails', 'resendConfirmationCodeResponse_codeDeliveryDetails' - The code delivery details returned by the server in response to the
-- request to resend the confirmation code.
--
-- 'httpStatus', 'resendConfirmationCodeResponse_httpStatus' - The response's http status code.
newResendConfirmationCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResendConfirmationCodeResponse
newResendConfirmationCodeResponse pHttpStatus_ =
  ResendConfirmationCodeResponse'
    { codeDeliveryDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code delivery details returned by the server in response to the
-- request to resend the confirmation code.
resendConfirmationCodeResponse_codeDeliveryDetails :: Lens.Lens' ResendConfirmationCodeResponse (Prelude.Maybe CodeDeliveryDetailsType)
resendConfirmationCodeResponse_codeDeliveryDetails = Lens.lens (\ResendConfirmationCodeResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@ResendConfirmationCodeResponse' {} a -> s {codeDeliveryDetails = a} :: ResendConfirmationCodeResponse)

-- | The response's http status code.
resendConfirmationCodeResponse_httpStatus :: Lens.Lens' ResendConfirmationCodeResponse Prelude.Int
resendConfirmationCodeResponse_httpStatus = Lens.lens (\ResendConfirmationCodeResponse' {httpStatus} -> httpStatus) (\s@ResendConfirmationCodeResponse' {} a -> s {httpStatus = a} :: ResendConfirmationCodeResponse)

instance
  Prelude.NFData
    ResendConfirmationCodeResponse
  where
  rnf ResendConfirmationCodeResponse' {..} =
    Prelude.rnf codeDeliveryDetails
      `Prelude.seq` Prelude.rnf httpStatus
