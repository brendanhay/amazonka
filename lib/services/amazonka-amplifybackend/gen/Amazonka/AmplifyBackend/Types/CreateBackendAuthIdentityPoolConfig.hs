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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthIdentityPoolConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthIdentityPoolConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes authorization configurations for the auth resources,
-- configured as a part of your Amplify project.
--
-- /See:/ 'newCreateBackendAuthIdentityPoolConfig' smart constructor.
data CreateBackendAuthIdentityPoolConfig = CreateBackendAuthIdentityPoolConfig'
  { -- | Set to true or false based on whether you want to enable guest
    -- authorization to your Amplify app.
    unauthenticatedLogin :: Prelude.Bool,
    -- | Name of the Amazon Cognito identity pool used for authorization.
    identityPoolName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthIdentityPoolConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unauthenticatedLogin', 'createBackendAuthIdentityPoolConfig_unauthenticatedLogin' - Set to true or false based on whether you want to enable guest
-- authorization to your Amplify app.
--
-- 'identityPoolName', 'createBackendAuthIdentityPoolConfig_identityPoolName' - Name of the Amazon Cognito identity pool used for authorization.
newCreateBackendAuthIdentityPoolConfig ::
  -- | 'unauthenticatedLogin'
  Prelude.Bool ->
  -- | 'identityPoolName'
  Prelude.Text ->
  CreateBackendAuthIdentityPoolConfig
newCreateBackendAuthIdentityPoolConfig
  pUnauthenticatedLogin_
  pIdentityPoolName_ =
    CreateBackendAuthIdentityPoolConfig'
      { unauthenticatedLogin =
          pUnauthenticatedLogin_,
        identityPoolName = pIdentityPoolName_
      }

-- | Set to true or false based on whether you want to enable guest
-- authorization to your Amplify app.
createBackendAuthIdentityPoolConfig_unauthenticatedLogin :: Lens.Lens' CreateBackendAuthIdentityPoolConfig Prelude.Bool
createBackendAuthIdentityPoolConfig_unauthenticatedLogin = Lens.lens (\CreateBackendAuthIdentityPoolConfig' {unauthenticatedLogin} -> unauthenticatedLogin) (\s@CreateBackendAuthIdentityPoolConfig' {} a -> s {unauthenticatedLogin = a} :: CreateBackendAuthIdentityPoolConfig)

-- | Name of the Amazon Cognito identity pool used for authorization.
createBackendAuthIdentityPoolConfig_identityPoolName :: Lens.Lens' CreateBackendAuthIdentityPoolConfig Prelude.Text
createBackendAuthIdentityPoolConfig_identityPoolName = Lens.lens (\CreateBackendAuthIdentityPoolConfig' {identityPoolName} -> identityPoolName) (\s@CreateBackendAuthIdentityPoolConfig' {} a -> s {identityPoolName = a} :: CreateBackendAuthIdentityPoolConfig)

instance
  Core.FromJSON
    CreateBackendAuthIdentityPoolConfig
  where
  parseJSON =
    Core.withObject
      "CreateBackendAuthIdentityPoolConfig"
      ( \x ->
          CreateBackendAuthIdentityPoolConfig'
            Prelude.<$> (x Core..: "unauthenticatedLogin")
            Prelude.<*> (x Core..: "identityPoolName")
      )

instance
  Prelude.Hashable
    CreateBackendAuthIdentityPoolConfig
  where
  hashWithSalt
    _salt
    CreateBackendAuthIdentityPoolConfig' {..} =
      _salt `Prelude.hashWithSalt` unauthenticatedLogin
        `Prelude.hashWithSalt` identityPoolName

instance
  Prelude.NFData
    CreateBackendAuthIdentityPoolConfig
  where
  rnf CreateBackendAuthIdentityPoolConfig' {..} =
    Prelude.rnf unauthenticatedLogin
      `Prelude.seq` Prelude.rnf identityPoolName

instance
  Core.ToJSON
    CreateBackendAuthIdentityPoolConfig
  where
  toJSON CreateBackendAuthIdentityPoolConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "unauthenticatedLogin"
                  Core..= unauthenticatedLogin
              ),
            Prelude.Just
              ("identityPoolName" Core..= identityPoolName)
          ]
      )
