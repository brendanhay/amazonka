{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Resolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Resolver
  ( Resolver (..),

    -- * Smart constructor
    mkResolver,

    -- * Lenses
    rCachingConfig,
    rDataSourceName,
    rFieldName,
    rKind,
    rPipelineConfig,
    rRequestMappingTemplate,
    rResolverArn,
    rResponseMappingTemplate,
    rSyncConfig,
    rTypeName,
  )
where

import qualified Network.AWS.AppSync.Types.CachingConfig as Types
import qualified Network.AWS.AppSync.Types.MappingTemplate as Types
import qualified Network.AWS.AppSync.Types.PipelineConfig as Types
import qualified Network.AWS.AppSync.Types.ResolverKind as Types
import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.AppSync.Types.SyncConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a resolver.
--
-- /See:/ 'mkResolver' smart constructor.
data Resolver = Resolver'
  { -- | The caching configuration for the resolver.
    cachingConfig :: Core.Maybe Types.CachingConfig,
    -- | The resolver data source name.
    dataSourceName :: Core.Maybe Types.ResourceName,
    -- | The resolver field name.
    fieldName :: Core.Maybe Types.ResourceName,
    -- | The resolver type.
    --
    --
    --     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.
    --
    --
    --     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
    kind :: Core.Maybe Types.ResolverKind,
    -- | The @PipelineConfig@ .
    pipelineConfig :: Core.Maybe Types.PipelineConfig,
    -- | The request mapping template.
    requestMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The resolver ARN.
    resolverArn :: Core.Maybe Types.String,
    -- | The response mapping template.
    responseMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The @SyncConfig@ for a resolver attached to a versioned datasource.
    syncConfig :: Core.Maybe Types.SyncConfig,
    -- | The resolver type name.
    typeName :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Resolver' value with any optional fields omitted.
mkResolver ::
  Resolver
mkResolver =
  Resolver'
    { cachingConfig = Core.Nothing,
      dataSourceName = Core.Nothing,
      fieldName = Core.Nothing,
      kind = Core.Nothing,
      pipelineConfig = Core.Nothing,
      requestMappingTemplate = Core.Nothing,
      resolverArn = Core.Nothing,
      responseMappingTemplate = Core.Nothing,
      syncConfig = Core.Nothing,
      typeName = Core.Nothing
    }

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCachingConfig :: Lens.Lens' Resolver (Core.Maybe Types.CachingConfig)
rCachingConfig = Lens.field @"cachingConfig"
{-# DEPRECATED rCachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead." #-}

-- | The resolver data source name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDataSourceName :: Lens.Lens' Resolver (Core.Maybe Types.ResourceName)
rDataSourceName = Lens.field @"dataSourceName"
{-# DEPRECATED rDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFieldName :: Lens.Lens' Resolver (Core.Maybe Types.ResourceName)
rFieldName = Lens.field @"fieldName"
{-# DEPRECATED rFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

-- | The resolver type.
--
--
--     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.
--
--
--     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
--
--
-- /Note:/ Consider using 'kind' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKind :: Lens.Lens' Resolver (Core.Maybe Types.ResolverKind)
rKind = Lens.field @"kind"
{-# DEPRECATED rKind "Use generic-lens or generic-optics with 'kind' instead." #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPipelineConfig :: Lens.Lens' Resolver (Core.Maybe Types.PipelineConfig)
rPipelineConfig = Lens.field @"pipelineConfig"
{-# DEPRECATED rPipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead." #-}

-- | The request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRequestMappingTemplate :: Lens.Lens' Resolver (Core.Maybe Types.MappingTemplate)
rRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# DEPRECATED rRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The resolver ARN.
--
-- /Note:/ Consider using 'resolverArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResolverArn :: Lens.Lens' Resolver (Core.Maybe Types.String)
rResolverArn = Lens.field @"resolverArn"
{-# DEPRECATED rResolverArn "Use generic-lens or generic-optics with 'resolverArn' instead." #-}

-- | The response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResponseMappingTemplate :: Lens.Lens' Resolver (Core.Maybe Types.MappingTemplate)
rResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# DEPRECATED rResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSyncConfig :: Lens.Lens' Resolver (Core.Maybe Types.SyncConfig)
rSyncConfig = Lens.field @"syncConfig"
{-# DEPRECATED rSyncConfig "Use generic-lens or generic-optics with 'syncConfig' instead." #-}

-- | The resolver type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTypeName :: Lens.Lens' Resolver (Core.Maybe Types.ResourceName)
rTypeName = Lens.field @"typeName"
{-# DEPRECATED rTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Core.FromJSON Resolver where
  parseJSON =
    Core.withObject "Resolver" Core.$
      \x ->
        Resolver'
          Core.<$> (x Core..:? "cachingConfig")
          Core.<*> (x Core..:? "dataSourceName")
          Core.<*> (x Core..:? "fieldName")
          Core.<*> (x Core..:? "kind")
          Core.<*> (x Core..:? "pipelineConfig")
          Core.<*> (x Core..:? "requestMappingTemplate")
          Core.<*> (x Core..:? "resolverArn")
          Core.<*> (x Core..:? "responseMappingTemplate")
          Core.<*> (x Core..:? "syncConfig")
          Core.<*> (x Core..:? "typeName")
