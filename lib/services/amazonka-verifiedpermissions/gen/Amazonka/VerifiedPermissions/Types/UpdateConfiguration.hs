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
-- Module      : Amazonka.VerifiedPermissions.Types.UpdateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.UpdateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.UpdateCognitoUserPoolConfiguration

-- | Contains an updated configuration to replace the configuration in an
-- existing identity source.
--
-- At this time, the only valid member of this structure is a Amazon
-- Cognito user pool configuration.
--
-- You must specify a @userPoolArn@, and optionally, a @ClientId@.
--
-- /See:/ 'newUpdateConfiguration' smart constructor.
data UpdateConfiguration = UpdateConfiguration'
  { -- | Contains configuration details of a Amazon Cognito user pool.
    cognitoUserPoolConfiguration :: Prelude.Maybe UpdateCognitoUserPoolConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cognitoUserPoolConfiguration', 'updateConfiguration_cognitoUserPoolConfiguration' - Contains configuration details of a Amazon Cognito user pool.
newUpdateConfiguration ::
  UpdateConfiguration
newUpdateConfiguration =
  UpdateConfiguration'
    { cognitoUserPoolConfiguration =
        Prelude.Nothing
    }

-- | Contains configuration details of a Amazon Cognito user pool.
updateConfiguration_cognitoUserPoolConfiguration :: Lens.Lens' UpdateConfiguration (Prelude.Maybe UpdateCognitoUserPoolConfiguration)
updateConfiguration_cognitoUserPoolConfiguration = Lens.lens (\UpdateConfiguration' {cognitoUserPoolConfiguration} -> cognitoUserPoolConfiguration) (\s@UpdateConfiguration' {} a -> s {cognitoUserPoolConfiguration = a} :: UpdateConfiguration)

instance Prelude.Hashable UpdateConfiguration where
  hashWithSalt _salt UpdateConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cognitoUserPoolConfiguration

instance Prelude.NFData UpdateConfiguration where
  rnf UpdateConfiguration' {..} =
    Prelude.rnf cognitoUserPoolConfiguration

instance Data.ToJSON UpdateConfiguration where
  toJSON UpdateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cognitoUserPoolConfiguration" Data..=)
              Prelude.<$> cognitoUserPoolConfiguration
          ]
      )
