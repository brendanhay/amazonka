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
-- Module      : Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the confirmation (for confirmation of registration) to a
-- specific user in the user pool.
module Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
  ( -- * Creating a Request
    ResendConfirmationCode (..),
    newResendConfirmationCode,

    -- * Request Lenses
    resendConfirmationCode_clientMetadata,
    resendConfirmationCode_userContextData,
    resendConfirmationCode_secretHash,
    resendConfirmationCode_analyticsMetadata,
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to resend the confirmation code.
--
-- /See:/ 'newResendConfirmationCode' smart constructor.
data ResendConfirmationCode = ResendConfirmationCode'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the ResendConfirmationCode API action,
    -- Amazon Cognito invokes the function that is assigned to the /custom
    -- message/ trigger. When Amazon Cognito invokes this function, it passes a
    -- JSON payload, which the function receives as input. This payload
    -- contains a @clientMetadata@ attribute, which provides the data that you
    -- assigned to the ClientMetadata parameter in your ResendConfirmationCode
    -- request. In your function code in AWS Lambda, you can process the
    -- @clientMetadata@ value to enhance your workflow for your specific needs.
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
    -- @ResendConfirmationCode@ calls.
    analyticsMetadata :: Core.Maybe AnalyticsMetadataType,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Core.Text,
    -- | The user name of the user to whom you wish to resend a confirmation
    -- code.
    username :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResendConfirmationCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'resendConfirmationCode_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the ResendConfirmationCode API action,
-- Amazon Cognito invokes the function that is assigned to the /custom
-- message/ trigger. When Amazon Cognito invokes this function, it passes a
-- JSON payload, which the function receives as input. This payload
-- contains a @clientMetadata@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your ResendConfirmationCode
-- request. In your function code in AWS Lambda, you can process the
-- @clientMetadata@ value to enhance your workflow for your specific needs.
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
-- 'userContextData', 'resendConfirmationCode_userContextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'secretHash', 'resendConfirmationCode_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'analyticsMetadata', 'resendConfirmationCode_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ResendConfirmationCode@ calls.
--
-- 'clientId', 'resendConfirmationCode_clientId' - The ID of the client associated with the user pool.
--
-- 'username', 'resendConfirmationCode_username' - The user name of the user to whom you wish to resend a confirmation
-- code.
newResendConfirmationCode ::
  -- | 'clientId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  ResendConfirmationCode
newResendConfirmationCode pClientId_ pUsername_ =
  ResendConfirmationCode'
    { clientMetadata =
        Core.Nothing,
      userContextData = Core.Nothing,
      secretHash = Core.Nothing,
      analyticsMetadata = Core.Nothing,
      clientId = Core._Sensitive Lens.# pClientId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the ResendConfirmationCode API action,
-- Amazon Cognito invokes the function that is assigned to the /custom
-- message/ trigger. When Amazon Cognito invokes this function, it passes a
-- JSON payload, which the function receives as input. This payload
-- contains a @clientMetadata@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your ResendConfirmationCode
-- request. In your function code in AWS Lambda, you can process the
-- @clientMetadata@ value to enhance your workflow for your specific needs.
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
resendConfirmationCode_clientMetadata :: Lens.Lens' ResendConfirmationCode (Core.Maybe (Core.HashMap Core.Text Core.Text))
resendConfirmationCode_clientMetadata = Lens.lens (\ResendConfirmationCode' {clientMetadata} -> clientMetadata) (\s@ResendConfirmationCode' {} a -> s {clientMetadata = a} :: ResendConfirmationCode) Core.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
resendConfirmationCode_userContextData :: Lens.Lens' ResendConfirmationCode (Core.Maybe UserContextDataType)
resendConfirmationCode_userContextData = Lens.lens (\ResendConfirmationCode' {userContextData} -> userContextData) (\s@ResendConfirmationCode' {} a -> s {userContextData = a} :: ResendConfirmationCode)

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
resendConfirmationCode_secretHash :: Lens.Lens' ResendConfirmationCode (Core.Maybe Core.Text)
resendConfirmationCode_secretHash = Lens.lens (\ResendConfirmationCode' {secretHash} -> secretHash) (\s@ResendConfirmationCode' {} a -> s {secretHash = a} :: ResendConfirmationCode) Core.. Lens.mapping Core._Sensitive

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ResendConfirmationCode@ calls.
resendConfirmationCode_analyticsMetadata :: Lens.Lens' ResendConfirmationCode (Core.Maybe AnalyticsMetadataType)
resendConfirmationCode_analyticsMetadata = Lens.lens (\ResendConfirmationCode' {analyticsMetadata} -> analyticsMetadata) (\s@ResendConfirmationCode' {} a -> s {analyticsMetadata = a} :: ResendConfirmationCode)

-- | The ID of the client associated with the user pool.
resendConfirmationCode_clientId :: Lens.Lens' ResendConfirmationCode Core.Text
resendConfirmationCode_clientId = Lens.lens (\ResendConfirmationCode' {clientId} -> clientId) (\s@ResendConfirmationCode' {} a -> s {clientId = a} :: ResendConfirmationCode) Core.. Core._Sensitive

-- | The user name of the user to whom you wish to resend a confirmation
-- code.
resendConfirmationCode_username :: Lens.Lens' ResendConfirmationCode Core.Text
resendConfirmationCode_username = Lens.lens (\ResendConfirmationCode' {username} -> username) (\s@ResendConfirmationCode' {} a -> s {username = a} :: ResendConfirmationCode) Core.. Core._Sensitive

instance Core.AWSRequest ResendConfirmationCode where
  type
    AWSResponse ResendConfirmationCode =
      ResendConfirmationCodeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResendConfirmationCodeResponse'
            Core.<$> (x Core..?> "CodeDeliveryDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResendConfirmationCode

instance Core.NFData ResendConfirmationCode

instance Core.ToHeaders ResendConfirmationCode where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ResendConfirmationCode" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResendConfirmationCode where
  toJSON ResendConfirmationCode' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            ("UserContextData" Core..=) Core.<$> userContextData,
            ("SecretHash" Core..=) Core.<$> secretHash,
            ("AnalyticsMetadata" Core..=)
              Core.<$> analyticsMetadata,
            Core.Just ("ClientId" Core..= clientId),
            Core.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath ResendConfirmationCode where
  toPath = Core.const "/"

instance Core.ToQuery ResendConfirmationCode where
  toQuery = Core.const Core.mempty

-- | The response from the server when the Amazon Cognito Your User Pools
-- service makes the request to resend a confirmation code.
--
-- /See:/ 'newResendConfirmationCodeResponse' smart constructor.
data ResendConfirmationCodeResponse = ResendConfirmationCodeResponse'
  { -- | The code delivery details returned by the server in response to the
    -- request to resend the confirmation code.
    codeDeliveryDetails :: Core.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ResendConfirmationCodeResponse
newResendConfirmationCodeResponse pHttpStatus_ =
  ResendConfirmationCodeResponse'
    { codeDeliveryDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code delivery details returned by the server in response to the
-- request to resend the confirmation code.
resendConfirmationCodeResponse_codeDeliveryDetails :: Lens.Lens' ResendConfirmationCodeResponse (Core.Maybe CodeDeliveryDetailsType)
resendConfirmationCodeResponse_codeDeliveryDetails = Lens.lens (\ResendConfirmationCodeResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@ResendConfirmationCodeResponse' {} a -> s {codeDeliveryDetails = a} :: ResendConfirmationCodeResponse)

-- | The response's http status code.
resendConfirmationCodeResponse_httpStatus :: Lens.Lens' ResendConfirmationCodeResponse Core.Int
resendConfirmationCodeResponse_httpStatus = Lens.lens (\ResendConfirmationCodeResponse' {httpStatus} -> httpStatus) (\s@ResendConfirmationCodeResponse' {} a -> s {httpStatus = a} :: ResendConfirmationCodeResponse)

instance Core.NFData ResendConfirmationCodeResponse
