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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendAuthIdentityPoolConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendAuthIdentityPoolConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the authorization configuration for the Amazon Cognito
-- identity pool, provisioned as a part of your auth resource in the
-- Amplify project.
--
-- /See:/ 'newUpdateBackendAuthIdentityPoolConfig' smart constructor.
data UpdateBackendAuthIdentityPoolConfig = UpdateBackendAuthIdentityPoolConfig'
  { -- | A boolean value that can be set to allow or disallow guest-level
    -- authorization into your Amplify app.
    unauthenticatedLogin :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthIdentityPoolConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unauthenticatedLogin', 'updateBackendAuthIdentityPoolConfig_unauthenticatedLogin' - A boolean value that can be set to allow or disallow guest-level
-- authorization into your Amplify app.
newUpdateBackendAuthIdentityPoolConfig ::
  UpdateBackendAuthIdentityPoolConfig
newUpdateBackendAuthIdentityPoolConfig =
  UpdateBackendAuthIdentityPoolConfig'
    { unauthenticatedLogin =
        Prelude.Nothing
    }

-- | A boolean value that can be set to allow or disallow guest-level
-- authorization into your Amplify app.
updateBackendAuthIdentityPoolConfig_unauthenticatedLogin :: Lens.Lens' UpdateBackendAuthIdentityPoolConfig (Prelude.Maybe Prelude.Bool)
updateBackendAuthIdentityPoolConfig_unauthenticatedLogin = Lens.lens (\UpdateBackendAuthIdentityPoolConfig' {unauthenticatedLogin} -> unauthenticatedLogin) (\s@UpdateBackendAuthIdentityPoolConfig' {} a -> s {unauthenticatedLogin = a} :: UpdateBackendAuthIdentityPoolConfig)

instance
  Prelude.Hashable
    UpdateBackendAuthIdentityPoolConfig
  where
  hashWithSalt
    _salt
    UpdateBackendAuthIdentityPoolConfig' {..} =
      _salt `Prelude.hashWithSalt` unauthenticatedLogin

instance
  Prelude.NFData
    UpdateBackendAuthIdentityPoolConfig
  where
  rnf UpdateBackendAuthIdentityPoolConfig' {..} =
    Prelude.rnf unauthenticatedLogin

instance
  Data.ToJSON
    UpdateBackendAuthIdentityPoolConfig
  where
  toJSON UpdateBackendAuthIdentityPoolConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("unauthenticatedLogin" Data..=)
              Prelude.<$> unauthenticatedLogin
          ]
      )
