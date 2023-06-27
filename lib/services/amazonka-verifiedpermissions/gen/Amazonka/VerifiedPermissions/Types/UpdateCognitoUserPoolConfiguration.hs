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
-- Module      : Amazonka.VerifiedPermissions.Types.UpdateCognitoUserPoolConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.UpdateCognitoUserPoolConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration details of a Amazon Cognito user pool for use
-- with an identity source.
--
-- /See:/ 'newUpdateCognitoUserPoolConfiguration' smart constructor.
data UpdateCognitoUserPoolConfiguration = UpdateCognitoUserPoolConfiguration'
  { -- | The client ID of an app client that is configured for the specified
    -- Amazon Cognito user pool.
    clientIds :: Prelude.Maybe [Prelude.Text],
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the Amazon Cognito user pool associated with this identity source.
    userPoolArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCognitoUserPoolConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIds', 'updateCognitoUserPoolConfiguration_clientIds' - The client ID of an app client that is configured for the specified
-- Amazon Cognito user pool.
--
-- 'userPoolArn', 'updateCognitoUserPoolConfiguration_userPoolArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the Amazon Cognito user pool associated with this identity source.
newUpdateCognitoUserPoolConfiguration ::
  -- | 'userPoolArn'
  Prelude.Text ->
  UpdateCognitoUserPoolConfiguration
newUpdateCognitoUserPoolConfiguration pUserPoolArn_ =
  UpdateCognitoUserPoolConfiguration'
    { clientIds =
        Prelude.Nothing,
      userPoolArn = pUserPoolArn_
    }

-- | The client ID of an app client that is configured for the specified
-- Amazon Cognito user pool.
updateCognitoUserPoolConfiguration_clientIds :: Lens.Lens' UpdateCognitoUserPoolConfiguration (Prelude.Maybe [Prelude.Text])
updateCognitoUserPoolConfiguration_clientIds = Lens.lens (\UpdateCognitoUserPoolConfiguration' {clientIds} -> clientIds) (\s@UpdateCognitoUserPoolConfiguration' {} a -> s {clientIds = a} :: UpdateCognitoUserPoolConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the Amazon Cognito user pool associated with this identity source.
updateCognitoUserPoolConfiguration_userPoolArn :: Lens.Lens' UpdateCognitoUserPoolConfiguration Prelude.Text
updateCognitoUserPoolConfiguration_userPoolArn = Lens.lens (\UpdateCognitoUserPoolConfiguration' {userPoolArn} -> userPoolArn) (\s@UpdateCognitoUserPoolConfiguration' {} a -> s {userPoolArn = a} :: UpdateCognitoUserPoolConfiguration)

instance
  Prelude.Hashable
    UpdateCognitoUserPoolConfiguration
  where
  hashWithSalt
    _salt
    UpdateCognitoUserPoolConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` clientIds
        `Prelude.hashWithSalt` userPoolArn

instance
  Prelude.NFData
    UpdateCognitoUserPoolConfiguration
  where
  rnf UpdateCognitoUserPoolConfiguration' {..} =
    Prelude.rnf clientIds
      `Prelude.seq` Prelude.rnf userPoolArn

instance
  Data.ToJSON
    UpdateCognitoUserPoolConfiguration
  where
  toJSON UpdateCognitoUserPoolConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientIds" Data..=) Prelude.<$> clientIds,
            Prelude.Just ("userPoolArn" Data..= userPoolArn)
          ]
      )
