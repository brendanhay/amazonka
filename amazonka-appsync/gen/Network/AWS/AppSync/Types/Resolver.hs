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
-- Module      : Network.AWS.AppSync.Types.Resolver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Resolver where

import Network.AWS.AppSync.Types.CachingConfig
import Network.AWS.AppSync.Types.PipelineConfig
import Network.AWS.AppSync.Types.ResolverKind
import Network.AWS.AppSync.Types.SyncConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a resolver.
--
-- /See:/ 'newResolver' smart constructor.
data Resolver = Resolver'
  { -- | The response mapping template.
    responseMappingTemplate :: Core.Maybe Core.Text,
    -- | The resolver type name.
    typeName :: Core.Maybe Core.Text,
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
    kind :: Core.Maybe ResolverKind,
    -- | The @SyncConfig@ for a resolver attached to a versioned datasource.
    syncConfig :: Core.Maybe SyncConfig,
    -- | The resolver data source name.
    dataSourceName :: Core.Maybe Core.Text,
    -- | The caching configuration for the resolver.
    cachingConfig :: Core.Maybe CachingConfig,
    -- | The resolver ARN.
    resolverArn :: Core.Maybe Core.Text,
    -- | The @PipelineConfig@.
    pipelineConfig :: Core.Maybe PipelineConfig,
    -- | The resolver field name.
    fieldName :: Core.Maybe Core.Text,
    -- | The request mapping template.
    requestMappingTemplate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Resolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseMappingTemplate', 'resolver_responseMappingTemplate' - The response mapping template.
--
-- 'typeName', 'resolver_typeName' - The resolver type name.
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
-- 'syncConfig', 'resolver_syncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- 'dataSourceName', 'resolver_dataSourceName' - The resolver data source name.
--
-- 'cachingConfig', 'resolver_cachingConfig' - The caching configuration for the resolver.
--
-- 'resolverArn', 'resolver_resolverArn' - The resolver ARN.
--
-- 'pipelineConfig', 'resolver_pipelineConfig' - The @PipelineConfig@.
--
-- 'fieldName', 'resolver_fieldName' - The resolver field name.
--
-- 'requestMappingTemplate', 'resolver_requestMappingTemplate' - The request mapping template.
newResolver ::
  Resolver
newResolver =
  Resolver'
    { responseMappingTemplate = Core.Nothing,
      typeName = Core.Nothing,
      kind = Core.Nothing,
      syncConfig = Core.Nothing,
      dataSourceName = Core.Nothing,
      cachingConfig = Core.Nothing,
      resolverArn = Core.Nothing,
      pipelineConfig = Core.Nothing,
      fieldName = Core.Nothing,
      requestMappingTemplate = Core.Nothing
    }

-- | The response mapping template.
resolver_responseMappingTemplate :: Lens.Lens' Resolver (Core.Maybe Core.Text)
resolver_responseMappingTemplate = Lens.lens (\Resolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@Resolver' {} a -> s {responseMappingTemplate = a} :: Resolver)

-- | The resolver type name.
resolver_typeName :: Lens.Lens' Resolver (Core.Maybe Core.Text)
resolver_typeName = Lens.lens (\Resolver' {typeName} -> typeName) (\s@Resolver' {} a -> s {typeName = a} :: Resolver)

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
resolver_kind :: Lens.Lens' Resolver (Core.Maybe ResolverKind)
resolver_kind = Lens.lens (\Resolver' {kind} -> kind) (\s@Resolver' {} a -> s {kind = a} :: Resolver)

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
resolver_syncConfig :: Lens.Lens' Resolver (Core.Maybe SyncConfig)
resolver_syncConfig = Lens.lens (\Resolver' {syncConfig} -> syncConfig) (\s@Resolver' {} a -> s {syncConfig = a} :: Resolver)

-- | The resolver data source name.
resolver_dataSourceName :: Lens.Lens' Resolver (Core.Maybe Core.Text)
resolver_dataSourceName = Lens.lens (\Resolver' {dataSourceName} -> dataSourceName) (\s@Resolver' {} a -> s {dataSourceName = a} :: Resolver)

-- | The caching configuration for the resolver.
resolver_cachingConfig :: Lens.Lens' Resolver (Core.Maybe CachingConfig)
resolver_cachingConfig = Lens.lens (\Resolver' {cachingConfig} -> cachingConfig) (\s@Resolver' {} a -> s {cachingConfig = a} :: Resolver)

-- | The resolver ARN.
resolver_resolverArn :: Lens.Lens' Resolver (Core.Maybe Core.Text)
resolver_resolverArn = Lens.lens (\Resolver' {resolverArn} -> resolverArn) (\s@Resolver' {} a -> s {resolverArn = a} :: Resolver)

-- | The @PipelineConfig@.
resolver_pipelineConfig :: Lens.Lens' Resolver (Core.Maybe PipelineConfig)
resolver_pipelineConfig = Lens.lens (\Resolver' {pipelineConfig} -> pipelineConfig) (\s@Resolver' {} a -> s {pipelineConfig = a} :: Resolver)

-- | The resolver field name.
resolver_fieldName :: Lens.Lens' Resolver (Core.Maybe Core.Text)
resolver_fieldName = Lens.lens (\Resolver' {fieldName} -> fieldName) (\s@Resolver' {} a -> s {fieldName = a} :: Resolver)

-- | The request mapping template.
resolver_requestMappingTemplate :: Lens.Lens' Resolver (Core.Maybe Core.Text)
resolver_requestMappingTemplate = Lens.lens (\Resolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@Resolver' {} a -> s {requestMappingTemplate = a} :: Resolver)

instance Core.FromJSON Resolver where
  parseJSON =
    Core.withObject
      "Resolver"
      ( \x ->
          Resolver'
            Core.<$> (x Core..:? "responseMappingTemplate")
            Core.<*> (x Core..:? "typeName")
            Core.<*> (x Core..:? "kind")
            Core.<*> (x Core..:? "syncConfig")
            Core.<*> (x Core..:? "dataSourceName")
            Core.<*> (x Core..:? "cachingConfig")
            Core.<*> (x Core..:? "resolverArn")
            Core.<*> (x Core..:? "pipelineConfig")
            Core.<*> (x Core..:? "fieldName")
            Core.<*> (x Core..:? "requestMappingTemplate")
      )

instance Core.Hashable Resolver

instance Core.NFData Resolver
