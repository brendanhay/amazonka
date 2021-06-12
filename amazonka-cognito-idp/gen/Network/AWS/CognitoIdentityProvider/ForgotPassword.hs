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
-- Module      : Network.AWS.CognitoIdentityProvider.ForgotPassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calling this API causes a message to be sent to the end user with a
-- confirmation code that is required to change the user\'s password. For
-- the @Username@ parameter, you can use the username or user alias. The
-- method used to send the confirmation code is sent according to the
-- specified AccountRecoverySetting. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/how-to-recover-a-user-account.html Recovering User Accounts>
-- in the /Amazon Cognito Developer Guide/. If neither a verified phone
-- number nor a verified email exists, an @InvalidParameterException@ is
-- thrown. To use the confirmation code for resetting the password, call
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_ConfirmForgotPassword.html ConfirmForgotPassword>.
module Network.AWS.CognitoIdentityProvider.ForgotPassword
  ( -- * Creating a Request
    ForgotPassword (..),
    newForgotPassword,

    -- * Request Lenses
    forgotPassword_clientMetadata,
    forgotPassword_userContextData,
    forgotPassword_secretHash,
    forgotPassword_analyticsMetadata,
    forgotPassword_clientId,
    forgotPassword_username,

    -- * Destructuring the Response
    ForgotPasswordResponse (..),
    newForgotPasswordResponse,

    -- * Response Lenses
    forgotPasswordResponse_codeDeliveryDetails,
    forgotPasswordResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to reset a user\'s password.
--
-- /See:/ 'newForgotPassword' smart constructor.
data ForgotPassword = ForgotPassword'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the ForgotPassword API action, Amazon
    -- Cognito invokes any functions that are assigned to the following
    -- triggers: /pre sign-up/, /custom message/, and /user migration/. When
    -- Amazon Cognito invokes any of these functions, it passes a JSON payload,
    -- which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your ForgotPassword request. In your
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
    -- @ForgotPassword@ calls.
    analyticsMetadata :: Core.Maybe AnalyticsMetadataType,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Core.Text,
    -- | The user name of the user for whom you want to enter a code to reset a
    -- forgotten password.
    username :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ForgotPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'forgotPassword_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the ForgotPassword API action, Amazon
-- Cognito invokes any functions that are assigned to the following
-- triggers: /pre sign-up/, /custom message/, and /user migration/. When
-- Amazon Cognito invokes any of these functions, it passes a JSON payload,
-- which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ForgotPassword request. In your
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
-- 'userContextData', 'forgotPassword_userContextData' - Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
--
-- 'secretHash', 'forgotPassword_secretHash' - A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
--
-- 'analyticsMetadata', 'forgotPassword_analyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ForgotPassword@ calls.
--
-- 'clientId', 'forgotPassword_clientId' - The ID of the client associated with the user pool.
--
-- 'username', 'forgotPassword_username' - The user name of the user for whom you want to enter a code to reset a
-- forgotten password.
newForgotPassword ::
  -- | 'clientId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  ForgotPassword
newForgotPassword pClientId_ pUsername_ =
  ForgotPassword'
    { clientMetadata = Core.Nothing,
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
-- pool triggers. When you use the ForgotPassword API action, Amazon
-- Cognito invokes any functions that are assigned to the following
-- triggers: /pre sign-up/, /custom message/, and /user migration/. When
-- Amazon Cognito invokes any of these functions, it passes a JSON payload,
-- which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your ForgotPassword request. In your
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
forgotPassword_clientMetadata :: Lens.Lens' ForgotPassword (Core.Maybe (Core.HashMap Core.Text Core.Text))
forgotPassword_clientMetadata = Lens.lens (\ForgotPassword' {clientMetadata} -> clientMetadata) (\s@ForgotPassword' {} a -> s {clientMetadata = a} :: ForgotPassword) Core.. Lens.mapping Lens._Coerce

-- | Contextual data such as the user\'s device fingerprint, IP address, or
-- location used for evaluating the risk of an unexpected event by Amazon
-- Cognito advanced security.
forgotPassword_userContextData :: Lens.Lens' ForgotPassword (Core.Maybe UserContextDataType)
forgotPassword_userContextData = Lens.lens (\ForgotPassword' {userContextData} -> userContextData) (\s@ForgotPassword' {} a -> s {userContextData = a} :: ForgotPassword)

-- | A keyed-hash message authentication code (HMAC) calculated using the
-- secret key of a user pool client and username plus the client ID in the
-- message.
forgotPassword_secretHash :: Lens.Lens' ForgotPassword (Core.Maybe Core.Text)
forgotPassword_secretHash = Lens.lens (\ForgotPassword' {secretHash} -> secretHash) (\s@ForgotPassword' {} a -> s {secretHash = a} :: ForgotPassword) Core.. Lens.mapping Core._Sensitive

-- | The Amazon Pinpoint analytics metadata for collecting metrics for
-- @ForgotPassword@ calls.
forgotPassword_analyticsMetadata :: Lens.Lens' ForgotPassword (Core.Maybe AnalyticsMetadataType)
forgotPassword_analyticsMetadata = Lens.lens (\ForgotPassword' {analyticsMetadata} -> analyticsMetadata) (\s@ForgotPassword' {} a -> s {analyticsMetadata = a} :: ForgotPassword)

-- | The ID of the client associated with the user pool.
forgotPassword_clientId :: Lens.Lens' ForgotPassword Core.Text
forgotPassword_clientId = Lens.lens (\ForgotPassword' {clientId} -> clientId) (\s@ForgotPassword' {} a -> s {clientId = a} :: ForgotPassword) Core.. Core._Sensitive

-- | The user name of the user for whom you want to enter a code to reset a
-- forgotten password.
forgotPassword_username :: Lens.Lens' ForgotPassword Core.Text
forgotPassword_username = Lens.lens (\ForgotPassword' {username} -> username) (\s@ForgotPassword' {} a -> s {username = a} :: ForgotPassword) Core.. Core._Sensitive

instance Core.AWSRequest ForgotPassword where
  type
    AWSResponse ForgotPassword =
      ForgotPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ForgotPasswordResponse'
            Core.<$> (x Core..?> "CodeDeliveryDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ForgotPassword

instance Core.NFData ForgotPassword

instance Core.ToHeaders ForgotPassword where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ForgotPassword" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ForgotPassword where
  toJSON ForgotPassword' {..} =
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

instance Core.ToPath ForgotPassword where
  toPath = Core.const "/"

instance Core.ToQuery ForgotPassword where
  toQuery = Core.const Core.mempty

-- | Respresents the response from the server regarding the request to reset
-- a password.
--
-- /See:/ 'newForgotPasswordResponse' smart constructor.
data ForgotPasswordResponse = ForgotPasswordResponse'
  { -- | The code delivery details returned by the server in response to the
    -- request to reset a password.
    codeDeliveryDetails :: Core.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ForgotPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeDeliveryDetails', 'forgotPasswordResponse_codeDeliveryDetails' - The code delivery details returned by the server in response to the
-- request to reset a password.
--
-- 'httpStatus', 'forgotPasswordResponse_httpStatus' - The response's http status code.
newForgotPasswordResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ForgotPasswordResponse
newForgotPasswordResponse pHttpStatus_ =
  ForgotPasswordResponse'
    { codeDeliveryDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code delivery details returned by the server in response to the
-- request to reset a password.
forgotPasswordResponse_codeDeliveryDetails :: Lens.Lens' ForgotPasswordResponse (Core.Maybe CodeDeliveryDetailsType)
forgotPasswordResponse_codeDeliveryDetails = Lens.lens (\ForgotPasswordResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@ForgotPasswordResponse' {} a -> s {codeDeliveryDetails = a} :: ForgotPasswordResponse)

-- | The response's http status code.
forgotPasswordResponse_httpStatus :: Lens.Lens' ForgotPasswordResponse Core.Int
forgotPasswordResponse_httpStatus = Lens.lens (\ForgotPasswordResponse' {httpStatus} -> httpStatus) (\s@ForgotPasswordResponse' {} a -> s {httpStatus = a} :: ForgotPasswordResponse)

instance Core.NFData ForgotPasswordResponse
