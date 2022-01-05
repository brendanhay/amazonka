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
-- Module      : Amazonka.AmplifyBackend.Types.BackendAPIResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendAPIResourceConfig where

import Amazonka.AmplifyBackend.Types.BackendAPIAuthType
import Amazonka.AmplifyBackend.Types.BackendAPIConflictResolution
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The resource config for the data model, configured as a part of the
-- Amplify project.
--
-- /See:/ 'newBackendAPIResourceConfig' smart constructor.
data BackendAPIResourceConfig = BackendAPIResourceConfig'
  { -- | The API name used to interact with the data model, configured as a part
    -- of your Amplify project.
    apiName :: Prelude.Maybe Prelude.Text,
    -- | The service used to provision and interact with the data model.
    service :: Prelude.Maybe Prelude.Text,
    -- | The definition of the data model in the annotated transform of the
    -- GraphQL schema.
    transformSchema :: Prelude.Maybe Prelude.Text,
    -- | Additional authentication methods used to interact with your data
    -- models.
    additionalAuthTypes :: Prelude.Maybe [BackendAPIAuthType],
    -- | The conflict resolution strategy for your data stored in the data
    -- models.
    conflictResolution :: Prelude.Maybe BackendAPIConflictResolution,
    -- | The default authentication type for interacting with the configured data
    -- models in your Amplify project.
    defaultAuthType :: Prelude.Maybe BackendAPIAuthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendAPIResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiName', 'backendAPIResourceConfig_apiName' - The API name used to interact with the data model, configured as a part
-- of your Amplify project.
--
-- 'service', 'backendAPIResourceConfig_service' - The service used to provision and interact with the data model.
--
-- 'transformSchema', 'backendAPIResourceConfig_transformSchema' - The definition of the data model in the annotated transform of the
-- GraphQL schema.
--
-- 'additionalAuthTypes', 'backendAPIResourceConfig_additionalAuthTypes' - Additional authentication methods used to interact with your data
-- models.
--
-- 'conflictResolution', 'backendAPIResourceConfig_conflictResolution' - The conflict resolution strategy for your data stored in the data
-- models.
--
-- 'defaultAuthType', 'backendAPIResourceConfig_defaultAuthType' - The default authentication type for interacting with the configured data
-- models in your Amplify project.
newBackendAPIResourceConfig ::
  BackendAPIResourceConfig
newBackendAPIResourceConfig =
  BackendAPIResourceConfig'
    { apiName =
        Prelude.Nothing,
      service = Prelude.Nothing,
      transformSchema = Prelude.Nothing,
      additionalAuthTypes = Prelude.Nothing,
      conflictResolution = Prelude.Nothing,
      defaultAuthType = Prelude.Nothing
    }

-- | The API name used to interact with the data model, configured as a part
-- of your Amplify project.
backendAPIResourceConfig_apiName :: Lens.Lens' BackendAPIResourceConfig (Prelude.Maybe Prelude.Text)
backendAPIResourceConfig_apiName = Lens.lens (\BackendAPIResourceConfig' {apiName} -> apiName) (\s@BackendAPIResourceConfig' {} a -> s {apiName = a} :: BackendAPIResourceConfig)

-- | The service used to provision and interact with the data model.
backendAPIResourceConfig_service :: Lens.Lens' BackendAPIResourceConfig (Prelude.Maybe Prelude.Text)
backendAPIResourceConfig_service = Lens.lens (\BackendAPIResourceConfig' {service} -> service) (\s@BackendAPIResourceConfig' {} a -> s {service = a} :: BackendAPIResourceConfig)

-- | The definition of the data model in the annotated transform of the
-- GraphQL schema.
backendAPIResourceConfig_transformSchema :: Lens.Lens' BackendAPIResourceConfig (Prelude.Maybe Prelude.Text)
backendAPIResourceConfig_transformSchema = Lens.lens (\BackendAPIResourceConfig' {transformSchema} -> transformSchema) (\s@BackendAPIResourceConfig' {} a -> s {transformSchema = a} :: BackendAPIResourceConfig)

-- | Additional authentication methods used to interact with your data
-- models.
backendAPIResourceConfig_additionalAuthTypes :: Lens.Lens' BackendAPIResourceConfig (Prelude.Maybe [BackendAPIAuthType])
backendAPIResourceConfig_additionalAuthTypes = Lens.lens (\BackendAPIResourceConfig' {additionalAuthTypes} -> additionalAuthTypes) (\s@BackendAPIResourceConfig' {} a -> s {additionalAuthTypes = a} :: BackendAPIResourceConfig) Prelude.. Lens.mapping Lens.coerced

-- | The conflict resolution strategy for your data stored in the data
-- models.
backendAPIResourceConfig_conflictResolution :: Lens.Lens' BackendAPIResourceConfig (Prelude.Maybe BackendAPIConflictResolution)
backendAPIResourceConfig_conflictResolution = Lens.lens (\BackendAPIResourceConfig' {conflictResolution} -> conflictResolution) (\s@BackendAPIResourceConfig' {} a -> s {conflictResolution = a} :: BackendAPIResourceConfig)

-- | The default authentication type for interacting with the configured data
-- models in your Amplify project.
backendAPIResourceConfig_defaultAuthType :: Lens.Lens' BackendAPIResourceConfig (Prelude.Maybe BackendAPIAuthType)
backendAPIResourceConfig_defaultAuthType = Lens.lens (\BackendAPIResourceConfig' {defaultAuthType} -> defaultAuthType) (\s@BackendAPIResourceConfig' {} a -> s {defaultAuthType = a} :: BackendAPIResourceConfig)

instance Core.FromJSON BackendAPIResourceConfig where
  parseJSON =
    Core.withObject
      "BackendAPIResourceConfig"
      ( \x ->
          BackendAPIResourceConfig'
            Prelude.<$> (x Core..:? "apiName")
            Prelude.<*> (x Core..:? "service")
            Prelude.<*> (x Core..:? "transformSchema")
            Prelude.<*> ( x Core..:? "additionalAuthTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "conflictResolution")
            Prelude.<*> (x Core..:? "defaultAuthType")
      )

instance Prelude.Hashable BackendAPIResourceConfig where
  hashWithSalt _salt BackendAPIResourceConfig' {..} =
    _salt `Prelude.hashWithSalt` apiName
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` transformSchema
      `Prelude.hashWithSalt` additionalAuthTypes
      `Prelude.hashWithSalt` conflictResolution
      `Prelude.hashWithSalt` defaultAuthType

instance Prelude.NFData BackendAPIResourceConfig where
  rnf BackendAPIResourceConfig' {..} =
    Prelude.rnf apiName
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf transformSchema
      `Prelude.seq` Prelude.rnf additionalAuthTypes
      `Prelude.seq` Prelude.rnf conflictResolution
      `Prelude.seq` Prelude.rnf defaultAuthType

instance Core.ToJSON BackendAPIResourceConfig where
  toJSON BackendAPIResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("apiName" Core..=) Prelude.<$> apiName,
            ("service" Core..=) Prelude.<$> service,
            ("transformSchema" Core..=)
              Prelude.<$> transformSchema,
            ("additionalAuthTypes" Core..=)
              Prelude.<$> additionalAuthTypes,
            ("conflictResolution" Core..=)
              Prelude.<$> conflictResolution,
            ("defaultAuthType" Core..=)
              Prelude.<$> defaultAuthType
          ]
      )
