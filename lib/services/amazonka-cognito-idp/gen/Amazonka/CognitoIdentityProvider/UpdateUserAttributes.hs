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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to update a specific attribute (one at a time).
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to update user attributes.
--
-- /See:/ 'newUpdateUserAttributes' smart constructor.
data UpdateUserAttributes = UpdateUserAttributes'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
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
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: [AttributeType],
    -- | The access token for the request to update user attributes.
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
-- custom workflows that this action triggers.
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
-- 'userAttributes', 'updateUserAttributes_userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- 'accessToken', 'updateUserAttributes_accessToken' - The access token for the request to update user attributes.
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
-- custom workflows that this action triggers.
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
updateUserAttributes_clientMetadata :: Lens.Lens' UpdateUserAttributes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateUserAttributes_clientMetadata = Lens.lens (\UpdateUserAttributes' {clientMetadata} -> clientMetadata) (\s@UpdateUserAttributes' {} a -> s {clientMetadata = a} :: UpdateUserAttributes) Prelude.. Lens.mapping Lens.coerced

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
updateUserAttributes_userAttributes :: Lens.Lens' UpdateUserAttributes [AttributeType]
updateUserAttributes_userAttributes = Lens.lens (\UpdateUserAttributes' {userAttributes} -> userAttributes) (\s@UpdateUserAttributes' {} a -> s {userAttributes = a} :: UpdateUserAttributes) Prelude.. Lens.coerced

-- | The access token for the request to update user attributes.
updateUserAttributes_accessToken :: Lens.Lens' UpdateUserAttributes Prelude.Text
updateUserAttributes_accessToken = Lens.lens (\UpdateUserAttributes' {accessToken} -> accessToken) (\s@UpdateUserAttributes' {} a -> s {accessToken = a} :: UpdateUserAttributes) Prelude.. Core._Sensitive

instance Core.AWSRequest UpdateUserAttributes where
  type
    AWSResponse UpdateUserAttributes =
      UpdateUserAttributesResponse
  request = Request.postJSON defaultService
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
