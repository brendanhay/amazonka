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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendAuthResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendAuthResourceConfig where

import Amazonka.AmplifyBackend.Types.AuthResources
import Amazonka.AmplifyBackend.Types.Service
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthIdentityPoolConfig
import Amazonka.AmplifyBackend.Types.UpdateBackendAuthUserPoolConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the resource configuration when updating an authentication
-- resource in your Amplify project.
--
-- /See:/ 'newUpdateBackendAuthResourceConfig' smart constructor.
data UpdateBackendAuthResourceConfig = UpdateBackendAuthResourceConfig'
  { -- | Describes the authorization configuration for the Amazon Cognito
    -- identity pool, provisioned as a part of your auth resource in the
    -- Amplify project.
    identityPoolConfigs :: Prelude.Maybe UpdateBackendAuthIdentityPoolConfig,
    -- | Defines the service name to use when configuring an authentication
    -- resource in your Amplify project.
    authResources :: AuthResources,
    -- | Describes the authentication configuration for the Amazon Cognito user
    -- pool, provisioned as a part of your auth resource in the Amplify
    -- project.
    userPoolConfigs :: UpdateBackendAuthUserPoolConfig,
    -- | Defines the service name to use when configuring an authentication
    -- resource in your Amplify project.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolConfigs', 'updateBackendAuthResourceConfig_identityPoolConfigs' - Describes the authorization configuration for the Amazon Cognito
-- identity pool, provisioned as a part of your auth resource in the
-- Amplify project.
--
-- 'authResources', 'updateBackendAuthResourceConfig_authResources' - Defines the service name to use when configuring an authentication
-- resource in your Amplify project.
--
-- 'userPoolConfigs', 'updateBackendAuthResourceConfig_userPoolConfigs' - Describes the authentication configuration for the Amazon Cognito user
-- pool, provisioned as a part of your auth resource in the Amplify
-- project.
--
-- 'service', 'updateBackendAuthResourceConfig_service' - Defines the service name to use when configuring an authentication
-- resource in your Amplify project.
newUpdateBackendAuthResourceConfig ::
  -- | 'authResources'
  AuthResources ->
  -- | 'userPoolConfigs'
  UpdateBackendAuthUserPoolConfig ->
  -- | 'service'
  Service ->
  UpdateBackendAuthResourceConfig
newUpdateBackendAuthResourceConfig
  pAuthResources_
  pUserPoolConfigs_
  pService_ =
    UpdateBackendAuthResourceConfig'
      { identityPoolConfigs =
          Prelude.Nothing,
        authResources = pAuthResources_,
        userPoolConfigs = pUserPoolConfigs_,
        service = pService_
      }

-- | Describes the authorization configuration for the Amazon Cognito
-- identity pool, provisioned as a part of your auth resource in the
-- Amplify project.
updateBackendAuthResourceConfig_identityPoolConfigs :: Lens.Lens' UpdateBackendAuthResourceConfig (Prelude.Maybe UpdateBackendAuthIdentityPoolConfig)
updateBackendAuthResourceConfig_identityPoolConfigs = Lens.lens (\UpdateBackendAuthResourceConfig' {identityPoolConfigs} -> identityPoolConfigs) (\s@UpdateBackendAuthResourceConfig' {} a -> s {identityPoolConfigs = a} :: UpdateBackendAuthResourceConfig)

-- | Defines the service name to use when configuring an authentication
-- resource in your Amplify project.
updateBackendAuthResourceConfig_authResources :: Lens.Lens' UpdateBackendAuthResourceConfig AuthResources
updateBackendAuthResourceConfig_authResources = Lens.lens (\UpdateBackendAuthResourceConfig' {authResources} -> authResources) (\s@UpdateBackendAuthResourceConfig' {} a -> s {authResources = a} :: UpdateBackendAuthResourceConfig)

-- | Describes the authentication configuration for the Amazon Cognito user
-- pool, provisioned as a part of your auth resource in the Amplify
-- project.
updateBackendAuthResourceConfig_userPoolConfigs :: Lens.Lens' UpdateBackendAuthResourceConfig UpdateBackendAuthUserPoolConfig
updateBackendAuthResourceConfig_userPoolConfigs = Lens.lens (\UpdateBackendAuthResourceConfig' {userPoolConfigs} -> userPoolConfigs) (\s@UpdateBackendAuthResourceConfig' {} a -> s {userPoolConfigs = a} :: UpdateBackendAuthResourceConfig)

-- | Defines the service name to use when configuring an authentication
-- resource in your Amplify project.
updateBackendAuthResourceConfig_service :: Lens.Lens' UpdateBackendAuthResourceConfig Service
updateBackendAuthResourceConfig_service = Lens.lens (\UpdateBackendAuthResourceConfig' {service} -> service) (\s@UpdateBackendAuthResourceConfig' {} a -> s {service = a} :: UpdateBackendAuthResourceConfig)

instance
  Prelude.Hashable
    UpdateBackendAuthResourceConfig
  where
  hashWithSalt
    _salt
    UpdateBackendAuthResourceConfig' {..} =
      _salt `Prelude.hashWithSalt` identityPoolConfigs
        `Prelude.hashWithSalt` authResources
        `Prelude.hashWithSalt` userPoolConfigs
        `Prelude.hashWithSalt` service

instance
  Prelude.NFData
    UpdateBackendAuthResourceConfig
  where
  rnf UpdateBackendAuthResourceConfig' {..} =
    Prelude.rnf identityPoolConfigs
      `Prelude.seq` Prelude.rnf authResources
      `Prelude.seq` Prelude.rnf userPoolConfigs
      `Prelude.seq` Prelude.rnf service

instance Data.ToJSON UpdateBackendAuthResourceConfig where
  toJSON UpdateBackendAuthResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("identityPoolConfigs" Data..=)
              Prelude.<$> identityPoolConfigs,
            Prelude.Just ("authResources" Data..= authResources),
            Prelude.Just
              ("userPoolConfigs" Data..= userPoolConfigs),
            Prelude.Just ("service" Data..= service)
          ]
      )
