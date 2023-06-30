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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthResourceConfig where

import Amazonka.AmplifyBackend.Types.AuthResources
import Amazonka.AmplifyBackend.Types.CreateBackendAuthIdentityPoolConfig
import Amazonka.AmplifyBackend.Types.CreateBackendAuthUserPoolConfig
import Amazonka.AmplifyBackend.Types.Service
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the resource configuration when creating an auth resource in
-- your Amplify project.
--
-- /See:/ 'newCreateBackendAuthResourceConfig' smart constructor.
data CreateBackendAuthResourceConfig = CreateBackendAuthResourceConfig'
  { -- | Describes the authorization configuration for the Amazon Cognito
    -- identity pool, provisioned as a part of your auth resource in the
    -- Amplify project.
    identityPoolConfigs :: Prelude.Maybe CreateBackendAuthIdentityPoolConfig,
    -- | Defines whether you want to configure only authentication or both
    -- authentication and authorization settings.
    authResources :: AuthResources,
    -- | Describes authentication configuration for the Amazon Cognito user pool,
    -- provisioned as a part of your auth resource in the Amplify project.
    userPoolConfigs :: CreateBackendAuthUserPoolConfig,
    -- | Defines the service name to use when configuring an authentication
    -- resource in your Amplify project.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolConfigs', 'createBackendAuthResourceConfig_identityPoolConfigs' - Describes the authorization configuration for the Amazon Cognito
-- identity pool, provisioned as a part of your auth resource in the
-- Amplify project.
--
-- 'authResources', 'createBackendAuthResourceConfig_authResources' - Defines whether you want to configure only authentication or both
-- authentication and authorization settings.
--
-- 'userPoolConfigs', 'createBackendAuthResourceConfig_userPoolConfigs' - Describes authentication configuration for the Amazon Cognito user pool,
-- provisioned as a part of your auth resource in the Amplify project.
--
-- 'service', 'createBackendAuthResourceConfig_service' - Defines the service name to use when configuring an authentication
-- resource in your Amplify project.
newCreateBackendAuthResourceConfig ::
  -- | 'authResources'
  AuthResources ->
  -- | 'userPoolConfigs'
  CreateBackendAuthUserPoolConfig ->
  -- | 'service'
  Service ->
  CreateBackendAuthResourceConfig
newCreateBackendAuthResourceConfig
  pAuthResources_
  pUserPoolConfigs_
  pService_ =
    CreateBackendAuthResourceConfig'
      { identityPoolConfigs =
          Prelude.Nothing,
        authResources = pAuthResources_,
        userPoolConfigs = pUserPoolConfigs_,
        service = pService_
      }

-- | Describes the authorization configuration for the Amazon Cognito
-- identity pool, provisioned as a part of your auth resource in the
-- Amplify project.
createBackendAuthResourceConfig_identityPoolConfigs :: Lens.Lens' CreateBackendAuthResourceConfig (Prelude.Maybe CreateBackendAuthIdentityPoolConfig)
createBackendAuthResourceConfig_identityPoolConfigs = Lens.lens (\CreateBackendAuthResourceConfig' {identityPoolConfigs} -> identityPoolConfigs) (\s@CreateBackendAuthResourceConfig' {} a -> s {identityPoolConfigs = a} :: CreateBackendAuthResourceConfig)

-- | Defines whether you want to configure only authentication or both
-- authentication and authorization settings.
createBackendAuthResourceConfig_authResources :: Lens.Lens' CreateBackendAuthResourceConfig AuthResources
createBackendAuthResourceConfig_authResources = Lens.lens (\CreateBackendAuthResourceConfig' {authResources} -> authResources) (\s@CreateBackendAuthResourceConfig' {} a -> s {authResources = a} :: CreateBackendAuthResourceConfig)

-- | Describes authentication configuration for the Amazon Cognito user pool,
-- provisioned as a part of your auth resource in the Amplify project.
createBackendAuthResourceConfig_userPoolConfigs :: Lens.Lens' CreateBackendAuthResourceConfig CreateBackendAuthUserPoolConfig
createBackendAuthResourceConfig_userPoolConfigs = Lens.lens (\CreateBackendAuthResourceConfig' {userPoolConfigs} -> userPoolConfigs) (\s@CreateBackendAuthResourceConfig' {} a -> s {userPoolConfigs = a} :: CreateBackendAuthResourceConfig)

-- | Defines the service name to use when configuring an authentication
-- resource in your Amplify project.
createBackendAuthResourceConfig_service :: Lens.Lens' CreateBackendAuthResourceConfig Service
createBackendAuthResourceConfig_service = Lens.lens (\CreateBackendAuthResourceConfig' {service} -> service) (\s@CreateBackendAuthResourceConfig' {} a -> s {service = a} :: CreateBackendAuthResourceConfig)

instance
  Data.FromJSON
    CreateBackendAuthResourceConfig
  where
  parseJSON =
    Data.withObject
      "CreateBackendAuthResourceConfig"
      ( \x ->
          CreateBackendAuthResourceConfig'
            Prelude.<$> (x Data..:? "identityPoolConfigs")
            Prelude.<*> (x Data..: "authResources")
            Prelude.<*> (x Data..: "userPoolConfigs")
            Prelude.<*> (x Data..: "service")
      )

instance
  Prelude.Hashable
    CreateBackendAuthResourceConfig
  where
  hashWithSalt
    _salt
    CreateBackendAuthResourceConfig' {..} =
      _salt
        `Prelude.hashWithSalt` identityPoolConfigs
        `Prelude.hashWithSalt` authResources
        `Prelude.hashWithSalt` userPoolConfigs
        `Prelude.hashWithSalt` service

instance
  Prelude.NFData
    CreateBackendAuthResourceConfig
  where
  rnf CreateBackendAuthResourceConfig' {..} =
    Prelude.rnf identityPoolConfigs
      `Prelude.seq` Prelude.rnf authResources
      `Prelude.seq` Prelude.rnf userPoolConfigs
      `Prelude.seq` Prelude.rnf service

instance Data.ToJSON CreateBackendAuthResourceConfig where
  toJSON CreateBackendAuthResourceConfig' {..} =
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
