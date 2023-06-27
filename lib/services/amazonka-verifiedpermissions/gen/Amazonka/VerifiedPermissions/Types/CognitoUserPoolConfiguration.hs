{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VerifiedPermissions.Types.CognitoUserPoolConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.CognitoUserPoolConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for an identity source that represents a connection to
-- an Amazon Cognito user pool used as an identity provider for Verified
-- Permissions.
--
-- This data type is used as a field that is part of an
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_Configuration.html Configuration>
-- structure that is used as a parameter to the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_Configuration.html Configuration>.
--
-- Example:@\"CognitoUserPoolConfiguration\":{\"UserPoolArn\":\"arn:aws:cognito-idp:us-east-1:123456789012:userpool\/us-east-1_1a2b3c4d5\",\"ClientIds\": [\"a1b2c3d4e5f6g7h8i9j0kalbmc\"]}@
--
-- /See:/ 'newCognitoUserPoolConfiguration' smart constructor.
data CognitoUserPoolConfiguration = CognitoUserPoolConfiguration'
  { -- | The unique application client IDs that are associated with the specified
    -- Amazon Cognito user pool.
    --
    -- Example: @\"ClientIds\": [\"&ExampleCogClientId;\"]@
    clientIds :: Prelude.Maybe [Prelude.Text],
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the Amazon Cognito user pool that contains the identities to be
    -- authorized.
    --
    -- Example:
    -- @\"UserPoolArn\": \"arn:aws:cognito-idp:us-east-1:123456789012:userpool\/us-east-1_1a2b3c4d5\"@
    userPoolArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CognitoUserPoolConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIds', 'cognitoUserPoolConfiguration_clientIds' - The unique application client IDs that are associated with the specified
-- Amazon Cognito user pool.
--
-- Example: @\"ClientIds\": [\"&ExampleCogClientId;\"]@
--
-- 'userPoolArn', 'cognitoUserPoolConfiguration_userPoolArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the Amazon Cognito user pool that contains the identities to be
-- authorized.
--
-- Example:
-- @\"UserPoolArn\": \"arn:aws:cognito-idp:us-east-1:123456789012:userpool\/us-east-1_1a2b3c4d5\"@
newCognitoUserPoolConfiguration ::
  -- | 'userPoolArn'
  Prelude.Text ->
  CognitoUserPoolConfiguration
newCognitoUserPoolConfiguration pUserPoolArn_ =
  CognitoUserPoolConfiguration'
    { clientIds =
        Prelude.Nothing,
      userPoolArn = pUserPoolArn_
    }

-- | The unique application client IDs that are associated with the specified
-- Amazon Cognito user pool.
--
-- Example: @\"ClientIds\": [\"&ExampleCogClientId;\"]@
cognitoUserPoolConfiguration_clientIds :: Lens.Lens' CognitoUserPoolConfiguration (Prelude.Maybe [Prelude.Text])
cognitoUserPoolConfiguration_clientIds = Lens.lens (\CognitoUserPoolConfiguration' {clientIds} -> clientIds) (\s@CognitoUserPoolConfiguration' {} a -> s {clientIds = a} :: CognitoUserPoolConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the Amazon Cognito user pool that contains the identities to be
-- authorized.
--
-- Example:
-- @\"UserPoolArn\": \"arn:aws:cognito-idp:us-east-1:123456789012:userpool\/us-east-1_1a2b3c4d5\"@
cognitoUserPoolConfiguration_userPoolArn :: Lens.Lens' CognitoUserPoolConfiguration Prelude.Text
cognitoUserPoolConfiguration_userPoolArn = Lens.lens (\CognitoUserPoolConfiguration' {userPoolArn} -> userPoolArn) (\s@CognitoUserPoolConfiguration' {} a -> s {userPoolArn = a} :: CognitoUserPoolConfiguration)

instance
  Prelude.Hashable
    CognitoUserPoolConfiguration
  where
  hashWithSalt _salt CognitoUserPoolConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` clientIds
      `Prelude.hashWithSalt` userPoolArn

instance Prelude.NFData CognitoUserPoolConfiguration where
  rnf CognitoUserPoolConfiguration' {..} =
    Prelude.rnf clientIds
      `Prelude.seq` Prelude.rnf userPoolArn

instance Data.ToJSON CognitoUserPoolConfiguration where
  toJSON CognitoUserPoolConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientIds" Data..=) Prelude.<$> clientIds,
            Prelude.Just ("userPoolArn" Data..= userPoolArn)
          ]
      )
