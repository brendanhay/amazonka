{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a resolver.
--
-- /See:/ 'newResolver' smart constructor.
data Resolver = Resolver'
  { -- | The response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The resolver type name.
    typeName :: Prelude.Maybe Prelude.Text,
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
    -- | The @SyncConfig@ for a resolver attached to a versioned datasource.
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The resolver data source name.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | The caching configuration for the resolver.
    cachingConfig :: Prelude.Maybe CachingConfig,
    -- | The resolver ARN.
    resolverArn :: Prelude.Maybe Prelude.Text,
    -- | The @PipelineConfig@.
    pipelineConfig :: Prelude.Maybe PipelineConfig,
    -- | The resolver field name.
    fieldName :: Prelude.Maybe Prelude.Text,
    -- | The request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { responseMappingTemplate =
        Prelude.Nothing,
      typeName = Prelude.Nothing,
      kind = Prelude.Nothing,
      syncConfig = Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      cachingConfig = Prelude.Nothing,
      resolverArn = Prelude.Nothing,
      pipelineConfig = Prelude.Nothing,
      fieldName = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing
    }

-- | The response mapping template.
resolver_responseMappingTemplate :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_responseMappingTemplate = Lens.lens (\Resolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@Resolver' {} a -> s {responseMappingTemplate = a} :: Resolver)

-- | The resolver type name.
resolver_typeName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
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
resolver_kind :: Lens.Lens' Resolver (Prelude.Maybe ResolverKind)
resolver_kind = Lens.lens (\Resolver' {kind} -> kind) (\s@Resolver' {} a -> s {kind = a} :: Resolver)

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
resolver_syncConfig :: Lens.Lens' Resolver (Prelude.Maybe SyncConfig)
resolver_syncConfig = Lens.lens (\Resolver' {syncConfig} -> syncConfig) (\s@Resolver' {} a -> s {syncConfig = a} :: Resolver)

-- | The resolver data source name.
resolver_dataSourceName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_dataSourceName = Lens.lens (\Resolver' {dataSourceName} -> dataSourceName) (\s@Resolver' {} a -> s {dataSourceName = a} :: Resolver)

-- | The caching configuration for the resolver.
resolver_cachingConfig :: Lens.Lens' Resolver (Prelude.Maybe CachingConfig)
resolver_cachingConfig = Lens.lens (\Resolver' {cachingConfig} -> cachingConfig) (\s@Resolver' {} a -> s {cachingConfig = a} :: Resolver)

-- | The resolver ARN.
resolver_resolverArn :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_resolverArn = Lens.lens (\Resolver' {resolverArn} -> resolverArn) (\s@Resolver' {} a -> s {resolverArn = a} :: Resolver)

-- | The @PipelineConfig@.
resolver_pipelineConfig :: Lens.Lens' Resolver (Prelude.Maybe PipelineConfig)
resolver_pipelineConfig = Lens.lens (\Resolver' {pipelineConfig} -> pipelineConfig) (\s@Resolver' {} a -> s {pipelineConfig = a} :: Resolver)

-- | The resolver field name.
resolver_fieldName :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_fieldName = Lens.lens (\Resolver' {fieldName} -> fieldName) (\s@Resolver' {} a -> s {fieldName = a} :: Resolver)

-- | The request mapping template.
resolver_requestMappingTemplate :: Lens.Lens' Resolver (Prelude.Maybe Prelude.Text)
resolver_requestMappingTemplate = Lens.lens (\Resolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@Resolver' {} a -> s {requestMappingTemplate = a} :: Resolver)

instance Prelude.FromJSON Resolver where
  parseJSON =
    Prelude.withObject
      "Resolver"
      ( \x ->
          Resolver'
            Prelude.<$> (x Prelude..:? "responseMappingTemplate")
            Prelude.<*> (x Prelude..:? "typeName")
            Prelude.<*> (x Prelude..:? "kind")
            Prelude.<*> (x Prelude..:? "syncConfig")
            Prelude.<*> (x Prelude..:? "dataSourceName")
            Prelude.<*> (x Prelude..:? "cachingConfig")
            Prelude.<*> (x Prelude..:? "resolverArn")
            Prelude.<*> (x Prelude..:? "pipelineConfig")
            Prelude.<*> (x Prelude..:? "fieldName")
            Prelude.<*> (x Prelude..:? "requestMappingTemplate")
      )

instance Prelude.Hashable Resolver

instance Prelude.NFData Resolver
