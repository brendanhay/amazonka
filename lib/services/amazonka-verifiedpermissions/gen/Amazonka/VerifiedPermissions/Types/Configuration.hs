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
-- Module      : Amazonka.VerifiedPermissions.Types.Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.CognitoUserPoolConfiguration

-- | Contains configuration information used when creating a new identity
-- source.
--
-- At this time, the only valid member of this structure is a Amazon
-- Cognito user pool configuration.
--
-- You must specify a @userPoolArn@, and optionally, a @ClientId@.
--
-- This data type is used as a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreateIdentitySource.html CreateIdentitySource>
-- operation.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | Contains configuration details of a Amazon Cognito user pool that
    -- Verified Permissions can use as a source of authenticated identities as
    -- entities. It specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of a Amazon Cognito user pool and one or more application client IDs.
    --
    -- Example:
    -- @\"configuration\":{\"cognitoUserPoolConfiguration\":{\"userPoolArn\":\"arn:aws:cognito-idp:us-east-1:123456789012:userpool\/us-east-1_1a2b3c4d5\",\"clientIds\": [\"a1b2c3d4e5f6g7h8i9j0kalbmc\"]}}@
    cognitoUserPoolConfiguration :: Prelude.Maybe CognitoUserPoolConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cognitoUserPoolConfiguration', 'configuration_cognitoUserPoolConfiguration' - Contains configuration details of a Amazon Cognito user pool that
-- Verified Permissions can use as a source of authenticated identities as
-- entities. It specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of a Amazon Cognito user pool and one or more application client IDs.
--
-- Example:
-- @\"configuration\":{\"cognitoUserPoolConfiguration\":{\"userPoolArn\":\"arn:aws:cognito-idp:us-east-1:123456789012:userpool\/us-east-1_1a2b3c4d5\",\"clientIds\": [\"a1b2c3d4e5f6g7h8i9j0kalbmc\"]}}@
newConfiguration ::
  Configuration
newConfiguration =
  Configuration'
    { cognitoUserPoolConfiguration =
        Prelude.Nothing
    }

-- | Contains configuration details of a Amazon Cognito user pool that
-- Verified Permissions can use as a source of authenticated identities as
-- entities. It specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of a Amazon Cognito user pool and one or more application client IDs.
--
-- Example:
-- @\"configuration\":{\"cognitoUserPoolConfiguration\":{\"userPoolArn\":\"arn:aws:cognito-idp:us-east-1:123456789012:userpool\/us-east-1_1a2b3c4d5\",\"clientIds\": [\"a1b2c3d4e5f6g7h8i9j0kalbmc\"]}}@
configuration_cognitoUserPoolConfiguration :: Lens.Lens' Configuration (Prelude.Maybe CognitoUserPoolConfiguration)
configuration_cognitoUserPoolConfiguration = Lens.lens (\Configuration' {cognitoUserPoolConfiguration} -> cognitoUserPoolConfiguration) (\s@Configuration' {} a -> s {cognitoUserPoolConfiguration = a} :: Configuration)

instance Prelude.Hashable Configuration where
  hashWithSalt _salt Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` cognitoUserPoolConfiguration

instance Prelude.NFData Configuration where
  rnf Configuration' {..} =
    Prelude.rnf cognitoUserPoolConfiguration

instance Data.ToJSON Configuration where
  toJSON Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cognitoUserPoolConfiguration" Data..=)
              Prelude.<$> cognitoUserPoolConfiguration
          ]
      )
