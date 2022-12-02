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
-- Module      : Amazonka.AppSync.Types.Resolver
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.Resolver where

import Amazonka.AppSync.Types.AppSyncRuntime
import Amazonka.AppSync.Types.CachingConfig
import Amazonka.AppSync.Types.PipelineConfig
import Amazonka.AppSync.Types.ResolverKind
import Amazonka.AppSync.Types.SyncConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a resolver.
--
-- /See:/ 'newResolver' smart constructor.
data Resolver = Resolver'
  { -- | The @resolver@ code that contains the request and response functions.
    -- When code is used, the @runtime@ is required. The @runtime@ value must
    -- be @APPSYNC_JS@.
    code :: Prelude.Maybe Prelude.Text,
    -- | The maximum batching size for a resolver.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | The resolver Amazon Resource Name (ARN).
    resolverArn :: Prelude.Maybe Prelude.Text,
    -- | The resolver field name.
    fieldName :: Prelude.Maybe Prelude.Text,
    -- | The caching configuration for the resolver.
    cachingConfig :: Prelude.Maybe CachingConfig,
    -- | The @PipelineConfig@.
    pipelineConfig :: Prelude.Maybe PipelineConfig,
    -- | The resolver type.
    --
    -- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
    --     resolver type. You can use a UNIT resolver to run a GraphQL query
    --     against a single data source.
    --
    -- -   __PIPELINE__: A PIPELINE resolver type. You can use a PIPELINE
    --     resolver to invoke a series of @Function@ objects in a serial
    --     manner. You can use a pipeline resolver to run a GraphQL query
    --     against multiple data sources.
    kind :: Prelude.Maybe ResolverKind,
    runtime :: Prelude.Maybe AppSyncRuntime,
    -- | The resolver type name.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The resolver data source name.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | The response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The @SyncConfig@ for a resolver attached to a versioned data source.
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'resolver_code' - The @resolver@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
--
-- 'maxBatchSize', 'resolver_maxBatchSize' - The maximum batching size for a resolver.
--
-- 'resolverArn', 'resolver_resolverArn' - The resolver Amazon Resource Name (ARN).
--
-- 'fieldName', 'resolver_fieldName' - The resolver field name.
--
-- 'cachingConfig', 'resolver_cachingConfig' - The caching configuration for the resolver.
--
-- 'pipelineConfig', 'resolver_pipelineConfig' - The @PipelineConfig@.
--
-- 'kind', 'resolver_kind' - The resolver type.
--
-- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
--     resolver type. You can use a UNIT resolver to run a GraphQL query
--     against a single data source.
--
-- -   __PIPELINE__: A PIPELINE resolver type. You can use a PIPELINE
--     resolver to invoke a series of @Function@ objects in a serial
--     manner. You can use a pipeline resolver to run a GraphQL query
--     against multiple data sources.
--
-- 'runtime', 'resolver_runtime' - Undocumented member.
--
-- 'typeName', 'resolver_typeName' - The resolver type name.
--
-- 'dataSourceName', 'resolver_dataSourceName' - The resolver data source name.
--
-- 'responseMappingTemplate', 'resolver_responseMappingTemplate' - The response mapping template.
--
-- 'syncConfig', 'resolver_syncConfig' - The @SyncConfig@ for a resolver attached to a versioned data source.
--
-- 'requestMappingTemplate', 'resolver_requestMappingTemplate' - The request mapping template.
newResolver ::
  Resolver
newResolver =
  Resolver'
    { code = Prelude.Nothing,
      maxBatchSize = Prelude.Nothing,
      resolverArn = Prelude.Nothing,
      fieldName = Prelude.Nothing,
      cachingConfig = Prelude.Nothing,
      pipelineConfig = Prelude.Nothing,
      kind = Prelude.Nothing,
      runtime = Prelude.Nothing,
      typeName = Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      responseMappingTemplate = Prelude.Nothing,
      syncConfig = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing
    }

-- | The @resolver@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
resolver_code :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_code = Lens.lens (\Resolver' {code} -> code) (\s@Resolver' {} a -> s {code = a} :: Resolver)

-- | The maximum batching size for a resolver.
resolver_maxBatchSize :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Natural)
resolver_maxBatchSize = Lens.lens (\Resolver' {maxBatchSize} -> maxBatchSize) (\s@Resolver' {} a -> s {maxBatchSize = a} :: Resolver)

-- | The resolver Amazon Resource Name (ARN).
resolver_resolverArn :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_resolverArn = Lens.lens (\Resolver' {resolverArn} -> resolverArn) (\s@Resolver' {} a -> s {resolverArn = a} :: Resolver)

-- | The resolver field name.
resolver_fieldName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_fieldName = Lens.lens (\Resolver' {fieldName} -> fieldName) (\s@Resolver' {} a -> s {fieldName = a} :: Resolver)

-- | The caching configuration for the resolver.
resolver_cachingConfig :: Lens.Lens' Resolver (Prelude.Maybe CachingConfig)
resolver_cachingConfig = Lens.lens (\Resolver' {cachingConfig} -> cachingConfig) (\s@Resolver' {} a -> s {cachingConfig = a} :: Resolver)

-- | The @PipelineConfig@.
resolver_pipelineConfig :: Lens.Lens' Resolver (Prelude.Maybe PipelineConfig)
resolver_pipelineConfig = Lens.lens (\Resolver' {pipelineConfig} -> pipelineConfig) (\s@Resolver' {} a -> s {pipelineConfig = a} :: Resolver)

-- | The resolver type.
--
-- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
--     resolver type. You can use a UNIT resolver to run a GraphQL query
--     against a single data source.
--
-- -   __PIPELINE__: A PIPELINE resolver type. You can use a PIPELINE
--     resolver to invoke a series of @Function@ objects in a serial
--     manner. You can use a pipeline resolver to run a GraphQL query
--     against multiple data sources.
resolver_kind :: Lens.Lens' Resolver (Prelude.Maybe ResolverKind)
resolver_kind = Lens.lens (\Resolver' {kind} -> kind) (\s@Resolver' {} a -> s {kind = a} :: Resolver)

-- | Undocumented member.
resolver_runtime :: Lens.Lens' Resolver (Prelude.Maybe AppSyncRuntime)
resolver_runtime = Lens.lens (\Resolver' {runtime} -> runtime) (\s@Resolver' {} a -> s {runtime = a} :: Resolver)

-- | The resolver type name.
resolver_typeName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_typeName = Lens.lens (\Resolver' {typeName} -> typeName) (\s@Resolver' {} a -> s {typeName = a} :: Resolver)

-- | The resolver data source name.
resolver_dataSourceName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_dataSourceName = Lens.lens (\Resolver' {dataSourceName} -> dataSourceName) (\s@Resolver' {} a -> s {dataSourceName = a} :: Resolver)

-- | The response mapping template.
resolver_responseMappingTemplate :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_responseMappingTemplate = Lens.lens (\Resolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@Resolver' {} a -> s {responseMappingTemplate = a} :: Resolver)

-- | The @SyncConfig@ for a resolver attached to a versioned data source.
resolver_syncConfig :: Lens.Lens' Resolver (Prelude.Maybe SyncConfig)
resolver_syncConfig = Lens.lens (\Resolver' {syncConfig} -> syncConfig) (\s@Resolver' {} a -> s {syncConfig = a} :: Resolver)

-- | The request mapping template.
resolver_requestMappingTemplate :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_requestMappingTemplate = Lens.lens (\Resolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@Resolver' {} a -> s {requestMappingTemplate = a} :: Resolver)

instance Data.FromJSON Resolver where
  parseJSON =
    Data.withObject
      "Resolver"
      ( \x ->
          Resolver'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "maxBatchSize")
            Prelude.<*> (x Data..:? "resolverArn")
            Prelude.<*> (x Data..:? "fieldName")
            Prelude.<*> (x Data..:? "cachingConfig")
            Prelude.<*> (x Data..:? "pipelineConfig")
            Prelude.<*> (x Data..:? "kind")
            Prelude.<*> (x Data..:? "runtime")
            Prelude.<*> (x Data..:? "typeName")
            Prelude.<*> (x Data..:? "dataSourceName")
            Prelude.<*> (x Data..:? "responseMappingTemplate")
            Prelude.<*> (x Data..:? "syncConfig")
            Prelude.<*> (x Data..:? "requestMappingTemplate")
      )

instance Prelude.Hashable Resolver where
  hashWithSalt _salt Resolver' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` resolverArn
      `Prelude.hashWithSalt` fieldName
      `Prelude.hashWithSalt` cachingConfig
      `Prelude.hashWithSalt` pipelineConfig
      `Prelude.hashWithSalt` kind
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` dataSourceName
      `Prelude.hashWithSalt` responseMappingTemplate
      `Prelude.hashWithSalt` syncConfig
      `Prelude.hashWithSalt` requestMappingTemplate

instance Prelude.NFData Resolver where
  rnf Resolver' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf maxBatchSize
      `Prelude.seq` Prelude.rnf resolverArn
      `Prelude.seq` Prelude.rnf fieldName
      `Prelude.seq` Prelude.rnf cachingConfig
      `Prelude.seq` Prelude.rnf pipelineConfig
      `Prelude.seq` Prelude.rnf kind
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf dataSourceName
      `Prelude.seq` Prelude.rnf responseMappingTemplate
      `Prelude.seq` Prelude.rnf syncConfig
      `Prelude.seq` Prelude.rnf requestMappingTemplate
