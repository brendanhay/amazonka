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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.Resolver where

import Amazonka.AppSync.Types.CachingConfig
import Amazonka.AppSync.Types.PipelineConfig
import Amazonka.AppSync.Types.ResolverKind
import Amazonka.AppSync.Types.SyncConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a resolver.
--
-- /See:/ 'newResolver' smart constructor.
data Resolver = Resolver'
  { -- | The resolver type name.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The resolver data source name.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | The request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The resolver type.
    --
    -- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
    --     resolver type. A UNIT resolver enables you to execute a GraphQL
    --     query against a single data source.
    --
    -- -   __PIPELINE__: A PIPELINE resolver type. A PIPELINE resolver enables
    --     you to execute a series of @Function@ in a serial manner. You can
    --     use a pipeline resolver to execute a GraphQL query against multiple
    --     data sources.
    kind :: Prelude.Maybe ResolverKind,
    -- | The resolver ARN.
    resolverArn :: Prelude.Maybe Prelude.Text,
    -- | The caching configuration for the resolver.
    cachingConfig :: Prelude.Maybe CachingConfig,
    -- | The response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The resolver field name.
    fieldName :: Prelude.Maybe Prelude.Text,
    -- | The @SyncConfig@ for a resolver attached to a versioned datasource.
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The @PipelineConfig@.
    pipelineConfig :: Prelude.Maybe PipelineConfig
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
-- 'typeName', 'resolver_typeName' - The resolver type name.
--
-- 'dataSourceName', 'resolver_dataSourceName' - The resolver data source name.
--
-- 'requestMappingTemplate', 'resolver_requestMappingTemplate' - The request mapping template.
--
-- 'kind', 'resolver_kind' - The resolver type.
--
-- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
--     resolver type. A UNIT resolver enables you to execute a GraphQL
--     query against a single data source.
--
-- -   __PIPELINE__: A PIPELINE resolver type. A PIPELINE resolver enables
--     you to execute a series of @Function@ in a serial manner. You can
--     use a pipeline resolver to execute a GraphQL query against multiple
--     data sources.
--
-- 'resolverArn', 'resolver_resolverArn' - The resolver ARN.
--
-- 'cachingConfig', 'resolver_cachingConfig' - The caching configuration for the resolver.
--
-- 'responseMappingTemplate', 'resolver_responseMappingTemplate' - The response mapping template.
--
-- 'fieldName', 'resolver_fieldName' - The resolver field name.
--
-- 'syncConfig', 'resolver_syncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- 'pipelineConfig', 'resolver_pipelineConfig' - The @PipelineConfig@.
newResolver ::
  Resolver
newResolver =
  Resolver'
    { typeName = Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing,
      kind = Prelude.Nothing,
      resolverArn = Prelude.Nothing,
      cachingConfig = Prelude.Nothing,
      responseMappingTemplate = Prelude.Nothing,
      fieldName = Prelude.Nothing,
      syncConfig = Prelude.Nothing,
      pipelineConfig = Prelude.Nothing
    }

-- | The resolver type name.
resolver_typeName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_typeName = Lens.lens (\Resolver' {typeName} -> typeName) (\s@Resolver' {} a -> s {typeName = a} :: Resolver)

-- | The resolver data source name.
resolver_dataSourceName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_dataSourceName = Lens.lens (\Resolver' {dataSourceName} -> dataSourceName) (\s@Resolver' {} a -> s {dataSourceName = a} :: Resolver)

-- | The request mapping template.
resolver_requestMappingTemplate :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_requestMappingTemplate = Lens.lens (\Resolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@Resolver' {} a -> s {requestMappingTemplate = a} :: Resolver)

-- | The resolver type.
--
-- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
--     resolver type. A UNIT resolver enables you to execute a GraphQL
--     query against a single data source.
--
-- -   __PIPELINE__: A PIPELINE resolver type. A PIPELINE resolver enables
--     you to execute a series of @Function@ in a serial manner. You can
--     use a pipeline resolver to execute a GraphQL query against multiple
--     data sources.
resolver_kind :: Lens.Lens' Resolver (Prelude.Maybe ResolverKind)
resolver_kind = Lens.lens (\Resolver' {kind} -> kind) (\s@Resolver' {} a -> s {kind = a} :: Resolver)

-- | The resolver ARN.
resolver_resolverArn :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_resolverArn = Lens.lens (\Resolver' {resolverArn} -> resolverArn) (\s@Resolver' {} a -> s {resolverArn = a} :: Resolver)

-- | The caching configuration for the resolver.
resolver_cachingConfig :: Lens.Lens' Resolver (Prelude.Maybe CachingConfig)
resolver_cachingConfig = Lens.lens (\Resolver' {cachingConfig} -> cachingConfig) (\s@Resolver' {} a -> s {cachingConfig = a} :: Resolver)

-- | The response mapping template.
resolver_responseMappingTemplate :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_responseMappingTemplate = Lens.lens (\Resolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@Resolver' {} a -> s {responseMappingTemplate = a} :: Resolver)

-- | The resolver field name.
resolver_fieldName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_fieldName = Lens.lens (\Resolver' {fieldName} -> fieldName) (\s@Resolver' {} a -> s {fieldName = a} :: Resolver)

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
resolver_syncConfig :: Lens.Lens' Resolver (Prelude.Maybe SyncConfig)
resolver_syncConfig = Lens.lens (\Resolver' {syncConfig} -> syncConfig) (\s@Resolver' {} a -> s {syncConfig = a} :: Resolver)

-- | The @PipelineConfig@.
resolver_pipelineConfig :: Lens.Lens' Resolver (Prelude.Maybe PipelineConfig)
resolver_pipelineConfig = Lens.lens (\Resolver' {pipelineConfig} -> pipelineConfig) (\s@Resolver' {} a -> s {pipelineConfig = a} :: Resolver)

instance Core.FromJSON Resolver where
  parseJSON =
    Core.withObject
      "Resolver"
      ( \x ->
          Resolver'
            Prelude.<$> (x Core..:? "typeName")
            Prelude.<*> (x Core..:? "dataSourceName")
            Prelude.<*> (x Core..:? "requestMappingTemplate")
            Prelude.<*> (x Core..:? "kind")
            Prelude.<*> (x Core..:? "resolverArn")
            Prelude.<*> (x Core..:? "cachingConfig")
            Prelude.<*> (x Core..:? "responseMappingTemplate")
            Prelude.<*> (x Core..:? "fieldName")
            Prelude.<*> (x Core..:? "syncConfig")
            Prelude.<*> (x Core..:? "pipelineConfig")
      )

instance Prelude.Hashable Resolver

instance Prelude.NFData Resolver
