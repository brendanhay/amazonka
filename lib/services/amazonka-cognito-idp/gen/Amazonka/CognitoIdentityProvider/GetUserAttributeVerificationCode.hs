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
-- Module      : Amazonka.CognitoIdentityProvider.GetUserAttributeVerificationCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attribute verification code for the specified attribute
-- name.
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
module Amazonka.CognitoIdentityProvider.GetUserAttributeVerificationCode
  ( -- * Creating a Request
    GetUserAttributeVerificationCode (..),
    newGetUserAttributeVerificationCode,

    -- * Request Lenses
    getUserAttributeVerificationCode_clientMetadata,
    getUserAttributeVerificationCode_accessToken,
    getUserAttributeVerificationCode_attributeName,

    -- * Destructuring the Response
    GetUserAttributeVerificationCodeResponse (..),
    newGetUserAttributeVerificationCodeResponse,

    -- * Response Lenses
    getUserAttributeVerificationCodeResponse_codeDeliveryDetails,
    getUserAttributeVerificationCodeResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to get user attribute verification.
--
-- /See:/ 'newGetUserAttributeVerificationCode' smart constructor.
data GetUserAttributeVerificationCode = GetUserAttributeVerificationCode'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the GetUserAttributeVerificationCode API action,
    -- Amazon Cognito invokes the function that is assigned to the /custom
    -- message/ trigger. When Amazon Cognito invokes this function, it passes a
    -- JSON payload, which the function receives as input. This payload
    -- contains a @clientMetadata@ attribute, which provides the data that you
    -- assigned to the ClientMetadata parameter in your
    -- GetUserAttributeVerificationCode request. In your function code in
    -- Lambda, you can process the @clientMetadata@ value to enhance your
    -- workflow for your specific needs.
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
    -- | The access token returned by the server response to get the user
    -- attribute verification code.
    accessToken :: Core.Sensitive Prelude.Text,
    -- | The attribute name returned by the server response to get the user
    -- attribute verification code.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserAttributeVerificationCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'getUserAttributeVerificationCode_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the GetUserAttributeVerificationCode API action,
-- Amazon Cognito invokes the function that is assigned to the /custom
-- message/ trigger. When Amazon Cognito invokes this function, it passes a
-- JSON payload, which the function receives as input. This payload
-- contains a @clientMetadata@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your
-- GetUserAttributeVerificationCode request. In your function code in
-- Lambda, you can process the @clientMetadata@ value to enhance your
-- workflow for your specific needs.
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
-- 'accessToken', 'getUserAttributeVerificationCode_accessToken' - The access token returned by the server response to get the user
-- attribute verification code.
--
-- 'attributeName', 'getUserAttributeVerificationCode_attributeName' - The attribute name returned by the server response to get the user
-- attribute verification code.
newGetUserAttributeVerificationCode ::
  -- | 'accessToken'
  Prelude.Text ->
  -- | 'attributeName'
  Prelude.Text ->
  GetUserAttributeVerificationCode
newGetUserAttributeVerificationCode
  pAccessToken_
  pAttributeName_ =
    GetUserAttributeVerificationCode'
      { clientMetadata =
          Prelude.Nothing,
        accessToken =
          Core._Sensitive Lens.# pAccessToken_,
        attributeName = pAttributeName_
      }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the GetUserAttributeVerificationCode API action,
-- Amazon Cognito invokes the function that is assigned to the /custom
-- message/ trigger. When Amazon Cognito invokes this function, it passes a
-- JSON payload, which the function receives as input. This payload
-- contains a @clientMetadata@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your
-- GetUserAttributeVerificationCode request. In your function code in
-- Lambda, you can process the @clientMetadata@ value to enhance your
-- workflow for your specific needs.
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
getUserAttributeVerificationCode_clientMetadata :: Lens.Lens' GetUserAttributeVerificationCode (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getUserAttributeVerificationCode_clientMetadata = Lens.lens (\GetUserAttributeVerificationCode' {clientMetadata} -> clientMetadata) (\s@GetUserAttributeVerificationCode' {} a -> s {clientMetadata = a} :: GetUserAttributeVerificationCode) Prelude.. Lens.mapping Lens.coerced

-- | The access token returned by the server response to get the user
-- attribute verification code.
getUserAttributeVerificationCode_accessToken :: Lens.Lens' GetUserAttributeVerificationCode Prelude.Text
getUserAttributeVerificationCode_accessToken = Lens.lens (\GetUserAttributeVerificationCode' {accessToken} -> accessToken) (\s@GetUserAttributeVerificationCode' {} a -> s {accessToken = a} :: GetUserAttributeVerificationCode) Prelude.. Core._Sensitive

-- | The attribute name returned by the server response to get the user
-- attribute verification code.
getUserAttributeVerificationCode_attributeName :: Lens.Lens' GetUserAttributeVerificationCode Prelude.Text
getUserAttributeVerificationCode_attributeName = Lens.lens (\GetUserAttributeVerificationCode' {attributeName} -> attributeName) (\s@GetUserAttributeVerificationCode' {} a -> s {attributeName = a} :: GetUserAttributeVerificationCode)

instance
  Core.AWSRequest
    GetUserAttributeVerificationCode
  where
  type
    AWSResponse GetUserAttributeVerificationCode =
      GetUserAttributeVerificationCodeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserAttributeVerificationCodeResponse'
            Prelude.<$> (x Core..?> "CodeDeliveryDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetUserAttributeVerificationCode
  where
  hashWithSalt
    _salt
    GetUserAttributeVerificationCode' {..} =
      _salt `Prelude.hashWithSalt` clientMetadata
        `Prelude.hashWithSalt` accessToken
        `Prelude.hashWithSalt` attributeName

instance
  Prelude.NFData
    GetUserAttributeVerificationCode
  where
  rnf GetUserAttributeVerificationCode' {..} =
    Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf attributeName

instance
  Core.ToHeaders
    GetUserAttributeVerificationCode
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetUserAttributeVerificationCode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetUserAttributeVerificationCode where
  toJSON GetUserAttributeVerificationCode' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Core..=)
              Prelude.<$> clientMetadata,
            Prelude.Just ("AccessToken" Core..= accessToken),
            Prelude.Just
              ("AttributeName" Core..= attributeName)
          ]
      )

instance Core.ToPath GetUserAttributeVerificationCode where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetUserAttributeVerificationCode
  where
  toQuery = Prelude.const Prelude.mempty

-- | The verification code response returned by the server response to get
-- the user attribute verification code.
--
-- /See:/ 'newGetUserAttributeVerificationCodeResponse' smart constructor.
data GetUserAttributeVerificationCodeResponse = GetUserAttributeVerificationCodeResponse'
  { -- | The code delivery details returned by the server in response to the
    -- request to get the user attribute verification code.
    codeDeliveryDetails :: Prelude.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserAttributeVerificationCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeDeliveryDetails', 'getUserAttributeVerificationCodeResponse_codeDeliveryDetails' - The code delivery details returned by the server in response to the
-- request to get the user attribute verification code.
--
-- 'httpStatus', 'getUserAttributeVerificationCodeResponse_httpStatus' - The response's http status code.
newGetUserAttributeVerificationCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUserAttributeVerificationCodeResponse
newGetUserAttributeVerificationCodeResponse
  pHttpStatus_ =
    GetUserAttributeVerificationCodeResponse'
      { codeDeliveryDetails =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The code delivery details returned by the server in response to the
-- request to get the user attribute verification code.
getUserAttributeVerificationCodeResponse_codeDeliveryDetails :: Lens.Lens' GetUserAttributeVerificationCodeResponse (Prelude.Maybe CodeDeliveryDetailsType)
getUserAttributeVerificationCodeResponse_codeDeliveryDetails = Lens.lens (\GetUserAttributeVerificationCodeResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@GetUserAttributeVerificationCodeResponse' {} a -> s {codeDeliveryDetails = a} :: GetUserAttributeVerificationCodeResponse)

-- | The response's http status code.
getUserAttributeVerificationCodeResponse_httpStatus :: Lens.Lens' GetUserAttributeVerificationCodeResponse Prelude.Int
getUserAttributeVerificationCodeResponse_httpStatus = Lens.lens (\GetUserAttributeVerificationCodeResponse' {httpStatus} -> httpStatus) (\s@GetUserAttributeVerificationCodeResponse' {} a -> s {httpStatus = a} :: GetUserAttributeVerificationCodeResponse)

instance
  Prelude.NFData
    GetUserAttributeVerificationCodeResponse
  where
  rnf GetUserAttributeVerificationCodeResponse' {..} =
    Prelude.rnf codeDeliveryDetails
      `Prelude.seq` Prelude.rnf httpStatus
