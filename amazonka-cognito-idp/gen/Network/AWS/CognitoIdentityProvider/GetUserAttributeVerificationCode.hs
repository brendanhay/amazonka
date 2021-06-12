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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attribute verification code for the specified attribute
-- name.
module Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get user attribute verification.
--
-- /See:/ 'newGetUserAttributeVerificationCode' smart constructor.
data GetUserAttributeVerificationCode = GetUserAttributeVerificationCode'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the GetUserAttributeVerificationCode API
    -- action, Amazon Cognito invokes the function that is assigned to the
    -- /custom message/ trigger. When Amazon Cognito invokes this function, it
    -- passes a JSON payload, which the function receives as input. This
    -- payload contains a @clientMetadata@ attribute, which provides the data
    -- that you assigned to the ClientMetadata parameter in your
    -- GetUserAttributeVerificationCode request. In your function code in AWS
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
    -- | The access token returned by the server response to get the user
    -- attribute verification code.
    accessToken :: Core.Sensitive Core.Text,
    -- | The attribute name returned by the server response to get the user
    -- attribute verification code.
    attributeName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the GetUserAttributeVerificationCode API
-- action, Amazon Cognito invokes the function that is assigned to the
-- /custom message/ trigger. When Amazon Cognito invokes this function, it
-- passes a JSON payload, which the function receives as input. This
-- payload contains a @clientMetadata@ attribute, which provides the data
-- that you assigned to the ClientMetadata parameter in your
-- GetUserAttributeVerificationCode request. In your function code in AWS
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
-- 'accessToken', 'getUserAttributeVerificationCode_accessToken' - The access token returned by the server response to get the user
-- attribute verification code.
--
-- 'attributeName', 'getUserAttributeVerificationCode_attributeName' - The attribute name returned by the server response to get the user
-- attribute verification code.
newGetUserAttributeVerificationCode ::
  -- | 'accessToken'
  Core.Text ->
  -- | 'attributeName'
  Core.Text ->
  GetUserAttributeVerificationCode
newGetUserAttributeVerificationCode
  pAccessToken_
  pAttributeName_ =
    GetUserAttributeVerificationCode'
      { clientMetadata =
          Core.Nothing,
        accessToken =
          Core._Sensitive Lens.# pAccessToken_,
        attributeName = pAttributeName_
      }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the GetUserAttributeVerificationCode API
-- action, Amazon Cognito invokes the function that is assigned to the
-- /custom message/ trigger. When Amazon Cognito invokes this function, it
-- passes a JSON payload, which the function receives as input. This
-- payload contains a @clientMetadata@ attribute, which provides the data
-- that you assigned to the ClientMetadata parameter in your
-- GetUserAttributeVerificationCode request. In your function code in AWS
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
--     available only to AWS Lambda triggers that are assigned to a user
--     pool to support custom workflows. If your user pool configuration
--     does not include triggers, the ClientMetadata parameter serves no
--     purpose.
--
-- -   Amazon Cognito does not validate the ClientMetadata value.
--
-- -   Amazon Cognito does not encrypt the the ClientMetadata value, so
--     don\'t use it to provide sensitive information.
getUserAttributeVerificationCode_clientMetadata :: Lens.Lens' GetUserAttributeVerificationCode (Core.Maybe (Core.HashMap Core.Text Core.Text))
getUserAttributeVerificationCode_clientMetadata = Lens.lens (\GetUserAttributeVerificationCode' {clientMetadata} -> clientMetadata) (\s@GetUserAttributeVerificationCode' {} a -> s {clientMetadata = a} :: GetUserAttributeVerificationCode) Core.. Lens.mapping Lens._Coerce

-- | The access token returned by the server response to get the user
-- attribute verification code.
getUserAttributeVerificationCode_accessToken :: Lens.Lens' GetUserAttributeVerificationCode Core.Text
getUserAttributeVerificationCode_accessToken = Lens.lens (\GetUserAttributeVerificationCode' {accessToken} -> accessToken) (\s@GetUserAttributeVerificationCode' {} a -> s {accessToken = a} :: GetUserAttributeVerificationCode) Core.. Core._Sensitive

-- | The attribute name returned by the server response to get the user
-- attribute verification code.
getUserAttributeVerificationCode_attributeName :: Lens.Lens' GetUserAttributeVerificationCode Core.Text
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
            Core.<$> (x Core..?> "CodeDeliveryDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetUserAttributeVerificationCode

instance Core.NFData GetUserAttributeVerificationCode

instance
  Core.ToHeaders
    GetUserAttributeVerificationCode
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetUserAttributeVerificationCode" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetUserAttributeVerificationCode where
  toJSON GetUserAttributeVerificationCode' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            Core.Just ("AccessToken" Core..= accessToken),
            Core.Just ("AttributeName" Core..= attributeName)
          ]
      )

instance Core.ToPath GetUserAttributeVerificationCode where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetUserAttributeVerificationCode
  where
  toQuery = Core.const Core.mempty

-- | The verification code response returned by the server response to get
-- the user attribute verification code.
--
-- /See:/ 'newGetUserAttributeVerificationCodeResponse' smart constructor.
data GetUserAttributeVerificationCodeResponse = GetUserAttributeVerificationCodeResponse'
  { -- | The code delivery details returned by the server in response to the
    -- request to get the user attribute verification code.
    codeDeliveryDetails :: Core.Maybe CodeDeliveryDetailsType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetUserAttributeVerificationCodeResponse
newGetUserAttributeVerificationCodeResponse
  pHttpStatus_ =
    GetUserAttributeVerificationCodeResponse'
      { codeDeliveryDetails =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The code delivery details returned by the server in response to the
-- request to get the user attribute verification code.
getUserAttributeVerificationCodeResponse_codeDeliveryDetails :: Lens.Lens' GetUserAttributeVerificationCodeResponse (Core.Maybe CodeDeliveryDetailsType)
getUserAttributeVerificationCodeResponse_codeDeliveryDetails = Lens.lens (\GetUserAttributeVerificationCodeResponse' {codeDeliveryDetails} -> codeDeliveryDetails) (\s@GetUserAttributeVerificationCodeResponse' {} a -> s {codeDeliveryDetails = a} :: GetUserAttributeVerificationCodeResponse)

-- | The response's http status code.
getUserAttributeVerificationCodeResponse_httpStatus :: Lens.Lens' GetUserAttributeVerificationCodeResponse Core.Int
getUserAttributeVerificationCodeResponse_httpStatus = Lens.lens (\GetUserAttributeVerificationCodeResponse' {httpStatus} -> httpStatus) (\s@GetUserAttributeVerificationCodeResponse' {} a -> s {httpStatus = a} :: GetUserAttributeVerificationCodeResponse)

instance
  Core.NFData
    GetUserAttributeVerificationCodeResponse
