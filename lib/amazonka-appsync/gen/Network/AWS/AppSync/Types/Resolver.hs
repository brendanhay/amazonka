{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Resolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.Resolver
  ( Resolver (..)
  -- * Smart constructor
  , mkResolver
  -- * Lenses
  , rCachingConfig
  , rDataSourceName
  , rFieldName
  , rKind
  , rPipelineConfig
  , rRequestMappingTemplate
  , rResolverArn
  , rResponseMappingTemplate
  , rSyncConfig
  , rTypeName
  ) where

import qualified Network.AWS.AppSync.Types.CachingConfig as Types
import qualified Network.AWS.AppSync.Types.MappingTemplate as Types
import qualified Network.AWS.AppSync.Types.PipelineConfig as Types
import qualified Network.AWS.AppSync.Types.ResolverKind as Types
import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.AppSync.Types.SyncConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a resolver.
--
-- /See:/ 'mkResolver' smart constructor.
data Resolver = Resolver'
  { cachingConfig :: Core.Maybe Types.CachingConfig
    -- ^ The caching configuration for the resolver.
  , dataSourceName :: Core.Maybe Types.ResourceName
    -- ^ The resolver data source name.
  , fieldName :: Core.Maybe Types.ResourceName
    -- ^ The resolver field name.
  , kind :: Core.Maybe Types.ResolverKind
    -- ^ The resolver type.
--
--
--     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.
--
--
--     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
--
  , pipelineConfig :: Core.Maybe Types.PipelineConfig
    -- ^ The @PipelineConfig@ .
  , requestMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The request mapping template.
  , resolverArn :: Core.Maybe Core.Text
    -- ^ The resolver ARN.
  , responseMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The response mapping template.
  , syncConfig :: Core.Maybe Types.SyncConfig
    -- ^ The @SyncConfig@ for a resolver attached to a versioned datasource.
  , typeName :: Core.Maybe Types.ResourceName
    -- ^ The resolver type name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Resolver' value with any optional fields omitted.
mkResolver
    :: Resolver
mkResolver
  = Resolver'{cachingConfig = Core.Nothing,
              dataSourceName = Core.Nothing, fieldName = Core.Nothing,
              kind = Core.Nothing, pipelineConfig = Core.Nothing,
              requestMappingTemplate = Core.Nothing, resolverArn = Core.Nothing,
              responseMappingTemplate = Core.Nothing, syncConfig = Core.Nothing,
              typeName = Core.Nothing}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCachingConfig :: Lens.Lens' Resolver (Core.Maybe Types.CachingConfig)
rCachingConfig = Lens.field @"cachingConfig"
{-# INLINEABLE rCachingConfig #-}
{-# DEPRECATED cachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead"  #-}

-- | The resolver data source name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDataSourceName :: Lens.Lens' Resolver (Core.Maybe Types.ResourceName)
rDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE rDataSourceName #-}
{-# DEPRECATED dataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead"  #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFieldName :: Lens.Lens' Resolver (Core.Maybe Types.ResourceName)
rFieldName = Lens.field @"fieldName"
{-# INLINEABLE rFieldName #-}
{-# DEPRECATED fieldName "Use generic-lens or generic-optics with 'fieldName' instead"  #-}

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
{-# INLINEABLE rKind #-}
{-# DEPRECATED kind "Use generic-lens or generic-optics with 'kind' instead"  #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPipelineConfig :: Lens.Lens' Resolver (Core.Maybe Types.PipelineConfig)
rPipelineConfig = Lens.field @"pipelineConfig"
{-# INLINEABLE rPipelineConfig #-}
{-# DEPRECATED pipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead"  #-}

-- | The request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRequestMappingTemplate :: Lens.Lens' Resolver (Core.Maybe Types.MappingTemplate)
rRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# INLINEABLE rRequestMappingTemplate #-}
{-# DEPRECATED requestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead"  #-}

-- | The resolver ARN.
--
-- /Note:/ Consider using 'resolverArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResolverArn :: Lens.Lens' Resolver (Core.Maybe Core.Text)
rResolverArn = Lens.field @"resolverArn"
{-# INLINEABLE rResolverArn #-}
{-# DEPRECATED resolverArn "Use generic-lens or generic-optics with 'resolverArn' instead"  #-}

-- | The response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResponseMappingTemplate :: Lens.Lens' Resolver (Core.Maybe Types.MappingTemplate)
rResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# INLINEABLE rResponseMappingTemplate #-}
{-# DEPRECATED responseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead"  #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSyncConfig :: Lens.Lens' Resolver (Core.Maybe Types.SyncConfig)
rSyncConfig = Lens.field @"syncConfig"
{-# INLINEABLE rSyncConfig #-}
{-# DEPRECATED syncConfig "Use generic-lens or generic-optics with 'syncConfig' instead"  #-}

-- | The resolver type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTypeName :: Lens.Lens' Resolver (Core.Maybe Types.ResourceName)
rTypeName = Lens.field @"typeName"
{-# INLINEABLE rTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

instance Core.FromJSON Resolver where
        parseJSON
          = Core.withObject "Resolver" Core.$
              \ x ->
                Resolver' Core.<$>
                  (x Core..:? "cachingConfig") Core.<*> x Core..:? "dataSourceName"
                    Core.<*> x Core..:? "fieldName"
                    Core.<*> x Core..:? "kind"
                    Core.<*> x Core..:? "pipelineConfig"
                    Core.<*> x Core..:? "requestMappingTemplate"
                    Core.<*> x Core..:? "resolverArn"
                    Core.<*> x Core..:? "responseMappingTemplate"
                    Core.<*> x Core..:? "syncConfig"
                    Core.<*> x Core..:? "typeName"
