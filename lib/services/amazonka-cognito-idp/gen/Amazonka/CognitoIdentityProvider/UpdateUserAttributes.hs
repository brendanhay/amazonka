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
-- Module      : Amazonka.CognitoIdentityProvider.UpdateUserAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to update a specific attribute (one at a time).
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
module Amazonka.CognitoIdentityProvider.UpdateUserAttributes
  ( -- * Creating a Request
    UpdateUserAttributes (..),
    newUpdateUserAttributes,

    -- * Request Lenses
    updateUserAttributes_clientMetadata,
    updateUserAttributes_userAttributes,
    updateUserAttributes_accessToken,

    -- * Destructuring the Response
    UpdateUserAttributesResponse (..),
    newUpdateUserAttributesResponse,

    -- * Response Lenses
    updateUserAttributesResponse_codeDeliveryDetailsList,
    updateUserAttributesResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to update user attributes.
--
-- /See:/ 'newUpdateUserAttributes' smart constructor.
data UpdateUserAttributes = UpdateUserAttributes'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action initiates.
    --
    -- You create custom workflows by assigning Lambda functions to user pool
    -- triggers. When you use the UpdateUserAttributes API action, Amazon
    -- Cognito invokes the function that is assigned to the /custom message/
    -- trigger. When Amazon Cognito invokes this function, it passes a JSON
    -- payload, which the function receives as input. This payload contains a
    -- @clientMetadata@ attribute, which provides the data that you assigned to
    -- the ClientMetadata parameter in your UpdateUserAttributes request. In
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
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    --
    -- If you have set an attribute to require verification before Amazon
    -- Cognito updates its value, this request doesn’t immediately update the
    -- value of that attribute. After your user receives and responds to a
    -- verification message to verify the new value, Amazon Cognito updates the
    -- attribute value. Your user can sign in and receive messages with the
    -- original attribute value until they verify the new value.
    userAttributes :: [AttributeType],
    -- | A valid access token that Amazon Cognito issued to the user whose user
    -- attributes you want to update.
    accessToken :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'updateUserAttributes_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action initiates.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the UpdateUserAttributes API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your UpdateUserAttributes request. In
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
-- 'userAttributes', 'updateUserAttributes_userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- If you have set an attribute to require verification before Amazon
-- Cognito updates its value, this request doesn’t immediately update the
-- value of that attribute. After your user receives and responds to a
-- verification message to verify the new value, Amazon Cognito updates the
-- attribute value. Your user can sign in and receive messages with the
-- original attribute value until they verify the new value.
--
-- 'accessToken', 'updateUserAttributes_accessToken' - A valid access token that Amazon Cognito issued to the user whose user
-- attributes you want to update.
newUpdateUserAttributes ::
  -- | 'accessToken'
  Prelude.Text ->
  UpdateUserAttributes
newUpdateUserAttributes pAccessToken_ =
  UpdateUserAttributes'
    { clientMetadata =
        Prelude.Nothing,
      userAttributes = Prelude.mempty,
      accessToken = Core._Sensitive Lens.# pAccessToken_
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action initiates.
--
-- You create custom workflows by assigning Lambda functions to user pool
-- triggers. When you use the UpdateUserAttributes API action, Amazon
-- Cognito invokes the function that is assigned to the /custom message/
-- trigger. When Amazon Cognito invokes this function, it passes a JSON
-- payload, which the function receives as input. This payload contains a
-- @clientMetadata@ attribute, which provides the data that you assigned to
-- the ClientMetadata parameter in your UpdateUserAttributes request. In
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
updateUserAttributes_clientMetadata :: Lens.Lens' UpdateUserAttributes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateUserAttributes_clientMetadata = Lens.lens (\UpdateUserAttributes' {clientMetadata} -> clientMetadata) (\s@UpdateUserAttributes' {} a -> s {clientMetadata = a} :: UpdateUserAttributes) Prelude.. Lens.mapping Lens.coerced

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- If you have set an attribute to require verification before Amazon
-- Cognito updates its value, this request doesn’t immediately update the
-- value of that attribute. After your user receives and responds to a
-- verification message to verify the new value, Amazon Cognito updates the
-- attribute value. Your user can sign in and receive messages with the
-- original attribute value until they verify the new value.
updateUserAttributes_userAttributes :: Lens.Lens' UpdateUserAttributes [AttributeType]
updateUserAttributes_userAttributes = Lens.lens (\UpdateUserAttributes' {userAttributes} -> userAttributes) (\s@UpdateUserAttributes' {} a -> s {userAttributes = a} :: UpdateUserAttributes) Prelude.. Lens.coerced

-- | A valid access token that Amazon Cognito issued to the user whose user
-- attributes you want to update.
updateUserAttributes_accessToken :: Lens.Lens' UpdateUserAttributes Prelude.Text
updateUserAttributes_accessToken = Lens.lens (\UpdateUserAttributes' {accessToken} -> accessToken) (\s@UpdateUserAttributes' {} a -> s {accessToken = a} :: UpdateUserAttributes) Prelude.. Core._Sensitive

instance Core.AWSRequest UpdateUserAttributes where
  type
    AWSResponse UpdateUserAttributes =
      UpdateUserAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserAttributesResponse'
            Prelude.<$> ( x Core..?> "CodeDeliveryDetailsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserAttributes where
  hashWithSalt _salt UpdateUserAttributes' {..} =
    _salt `Prelude.hashWithSalt` clientMetadata
      `Prelude.hashWithSalt` userAttributes
      `Prelude.hashWithSalt` accessToken

instance Prelude.NFData UpdateUserAttributes where
  rnf UpdateUserAttributes' {..} =
    Prelude.rnf clientMetadata
      `Prelude.seq` Prelude.rnf userAttributes
      `Prelude.seq` Prelude.rnf accessToken

instance Core.ToHeaders UpdateUserAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateUserAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUserAttributes where
  toJSON UpdateUserAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientMetadata" Core..=)
              Prelude.<$> clientMetadata,
            Prelude.Just
              ("UserAttributes" Core..= userAttributes),
            Prelude.Just ("AccessToken" Core..= accessToken)
          ]
      )

instance Core.ToPath UpdateUserAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateUserAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server for the request to update user
-- attributes.
--
-- /See:/ 'newUpdateUserAttributesResponse' smart constructor.
data UpdateUserAttributesResponse = UpdateUserAttributesResponse'
  { -- | The code delivery details list from the server for the request to update
    -- user attributes.
    codeDeliveryDetailsList :: Prelude.Maybe [CodeDeliveryDetailsType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeDeliveryDetailsList', 'updateUserAttributesResponse_codeDeliveryDetailsList' - The code delivery details list from the server for the request to update
-- user attributes.
--
-- 'httpStatus', 'updateUserAttributesResponse_httpStatus' - The response's http status code.
newUpdateUserAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserAttributesResponse
newUpdateUserAttributesResponse pHttpStatus_ =
  UpdateUserAttributesResponse'
    { codeDeliveryDetailsList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code delivery details list from the server for the request to update
-- user attributes.
updateUserAttributesResponse_codeDeliveryDetailsList :: Lens.Lens' UpdateUserAttributesResponse (Prelude.Maybe [CodeDeliveryDetailsType])
updateUserAttributesResponse_codeDeliveryDetailsList = Lens.lens (\UpdateUserAttributesResponse' {codeDeliveryDetailsList} -> codeDeliveryDetailsList) (\s@UpdateUserAttributesResponse' {} a -> s {codeDeliveryDetailsList = a} :: UpdateUserAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateUserAttributesResponse_httpStatus :: Lens.Lens' UpdateUserAttributesResponse Prelude.Int
updateUserAttributesResponse_httpStatus = Lens.lens (\UpdateUserAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateUserAttributesResponse' {} a -> s {httpStatus = a} :: UpdateUserAttributesResponse)

instance Prelude.NFData UpdateUserAttributesResponse where
  rnf UpdateUserAttributesResponse' {..} =
    Prelude.rnf codeDeliveryDetailsList
      `Prelude.seq` Prelude.rnf httpStatus
