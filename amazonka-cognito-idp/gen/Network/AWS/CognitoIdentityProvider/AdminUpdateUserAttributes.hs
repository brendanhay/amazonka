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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user\'s attributes, including developer
-- attributes, as an administrator. Works on any user.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
--
-- In addition to updating user attributes, this API can also be used to
-- mark phone and email as verified.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
  ( -- * Creating a Request
    AdminUpdateUserAttributes (..),
    newAdminUpdateUserAttributes,

    -- * Request Lenses
    adminUpdateUserAttributes_clientMetadata,
    adminUpdateUserAttributes_userPoolId,
    adminUpdateUserAttributes_username,
    adminUpdateUserAttributes_userAttributes,

    -- * Destructuring the Response
    AdminUpdateUserAttributesResponse (..),
    newAdminUpdateUserAttributesResponse,

    -- * Response Lenses
    adminUpdateUserAttributesResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the user\'s attributes as an
-- administrator.
--
-- /See:/ 'newAdminUpdateUserAttributes' smart constructor.
data AdminUpdateUserAttributes = AdminUpdateUserAttributes'
  { -- | A map of custom key-value pairs that you can provide as input for any
    -- custom workflows that this action triggers.
    --
    -- You create custom workflows by assigning AWS Lambda functions to user
    -- pool triggers. When you use the AdminUpdateUserAttributes API action,
    -- Amazon Cognito invokes the function that is assigned to the /custom
    -- message/ trigger. When Amazon Cognito invokes this function, it passes a
    -- JSON payload, which the function receives as input. This payload
    -- contains a @clientMetadata@ attribute, which provides the data that you
    -- assigned to the ClientMetadata parameter in your
    -- AdminUpdateUserAttributes request. In your function code in AWS Lambda,
    -- you can process the @clientMetadata@ value to enhance your workflow for
    -- your specific needs.
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
    -- | The user pool ID for the user pool where you want to update user
    -- attributes.
    userPoolId :: Core.Text,
    -- | The user name of the user for whom you want to update user attributes.
    username :: Core.Sensitive Core.Text,
    -- | An array of name-value pairs representing user attributes.
    --
    -- For custom attributes, you must prepend the @custom:@ prefix to the
    -- attribute name.
    userAttributes :: [AttributeType]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminUpdateUserAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientMetadata', 'adminUpdateUserAttributes_clientMetadata' - A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminUpdateUserAttributes API action,
-- Amazon Cognito invokes the function that is assigned to the /custom
-- message/ trigger. When Amazon Cognito invokes this function, it passes a
-- JSON payload, which the function receives as input. This payload
-- contains a @clientMetadata@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your
-- AdminUpdateUserAttributes request. In your function code in AWS Lambda,
-- you can process the @clientMetadata@ value to enhance your workflow for
-- your specific needs.
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
-- 'userPoolId', 'adminUpdateUserAttributes_userPoolId' - The user pool ID for the user pool where you want to update user
-- attributes.
--
-- 'username', 'adminUpdateUserAttributes_username' - The user name of the user for whom you want to update user attributes.
--
-- 'userAttributes', 'adminUpdateUserAttributes_userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
newAdminUpdateUserAttributes ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  AdminUpdateUserAttributes
newAdminUpdateUserAttributes pUserPoolId_ pUsername_ =
  AdminUpdateUserAttributes'
    { clientMetadata =
        Core.Nothing,
      userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_,
      userAttributes = Core.mempty
    }

-- | A map of custom key-value pairs that you can provide as input for any
-- custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user
-- pool triggers. When you use the AdminUpdateUserAttributes API action,
-- Amazon Cognito invokes the function that is assigned to the /custom
-- message/ trigger. When Amazon Cognito invokes this function, it passes a
-- JSON payload, which the function receives as input. This payload
-- contains a @clientMetadata@ attribute, which provides the data that you
-- assigned to the ClientMetadata parameter in your
-- AdminUpdateUserAttributes request. In your function code in AWS Lambda,
-- you can process the @clientMetadata@ value to enhance your workflow for
-- your specific needs.
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
adminUpdateUserAttributes_clientMetadata :: Lens.Lens' AdminUpdateUserAttributes (Core.Maybe (Core.HashMap Core.Text Core.Text))
adminUpdateUserAttributes_clientMetadata = Lens.lens (\AdminUpdateUserAttributes' {clientMetadata} -> clientMetadata) (\s@AdminUpdateUserAttributes' {} a -> s {clientMetadata = a} :: AdminUpdateUserAttributes) Core.. Lens.mapping Lens._Coerce

-- | The user pool ID for the user pool where you want to update user
-- attributes.
adminUpdateUserAttributes_userPoolId :: Lens.Lens' AdminUpdateUserAttributes Core.Text
adminUpdateUserAttributes_userPoolId = Lens.lens (\AdminUpdateUserAttributes' {userPoolId} -> userPoolId) (\s@AdminUpdateUserAttributes' {} a -> s {userPoolId = a} :: AdminUpdateUserAttributes)

-- | The user name of the user for whom you want to update user attributes.
adminUpdateUserAttributes_username :: Lens.Lens' AdminUpdateUserAttributes Core.Text
adminUpdateUserAttributes_username = Lens.lens (\AdminUpdateUserAttributes' {username} -> username) (\s@AdminUpdateUserAttributes' {} a -> s {username = a} :: AdminUpdateUserAttributes) Core.. Core._Sensitive

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the
-- attribute name.
adminUpdateUserAttributes_userAttributes :: Lens.Lens' AdminUpdateUserAttributes [AttributeType]
adminUpdateUserAttributes_userAttributes = Lens.lens (\AdminUpdateUserAttributes' {userAttributes} -> userAttributes) (\s@AdminUpdateUserAttributes' {} a -> s {userAttributes = a} :: AdminUpdateUserAttributes) Core.. Lens._Coerce

instance Core.AWSRequest AdminUpdateUserAttributes where
  type
    AWSResponse AdminUpdateUserAttributes =
      AdminUpdateUserAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminUpdateUserAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AdminUpdateUserAttributes

instance Core.NFData AdminUpdateUserAttributes

instance Core.ToHeaders AdminUpdateUserAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminUpdateUserAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminUpdateUserAttributes where
  toJSON AdminUpdateUserAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientMetadata" Core..=) Core.<$> clientMetadata,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("UserAttributes" Core..= userAttributes)
          ]
      )

instance Core.ToPath AdminUpdateUserAttributes where
  toPath = Core.const "/"

instance Core.ToQuery AdminUpdateUserAttributes where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server for the request to update user
-- attributes as an administrator.
--
-- /See:/ 'newAdminUpdateUserAttributesResponse' smart constructor.
data AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminUpdateUserAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminUpdateUserAttributesResponse_httpStatus' - The response's http status code.
newAdminUpdateUserAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AdminUpdateUserAttributesResponse
newAdminUpdateUserAttributesResponse pHttpStatus_ =
  AdminUpdateUserAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminUpdateUserAttributesResponse_httpStatus :: Lens.Lens' AdminUpdateUserAttributesResponse Core.Int
adminUpdateUserAttributesResponse_httpStatus = Lens.lens (\AdminUpdateUserAttributesResponse' {httpStatus} -> httpStatus) (\s@AdminUpdateUserAttributesResponse' {} a -> s {httpStatus = a} :: AdminUpdateUserAttributesResponse)

instance
  Core.NFData
    AdminUpdateUserAttributesResponse
