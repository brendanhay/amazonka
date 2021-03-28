{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Resolver@ object.
module Network.AWS.AppSync.UpdateResolver
    (
    -- * Creating a request
      UpdateResolver (..)
    , mkUpdateResolver
    -- ** Request lenses
    , urApiId
    , urTypeName
    , urFieldName
    , urCachingConfig
    , urDataSourceName
    , urKind
    , urPipelineConfig
    , urRequestMappingTemplate
    , urResponseMappingTemplate
    , urSyncConfig

    -- * Destructuring the response
    , UpdateResolverResponse (..)
    , mkUpdateResolverResponse
    -- ** Response lenses
    , urrrsResolver
    , urrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateResolver' smart constructor.
data UpdateResolver = UpdateResolver'
  { apiId :: Core.Text
    -- ^ The API ID.
  , typeName :: Types.ResourceName
    -- ^ The new type name.
  , fieldName :: Types.ResourceName
    -- ^ The new field name.
  , cachingConfig :: Core.Maybe Types.CachingConfig
    -- ^ The caching configuration for the resolver.
  , dataSourceName :: Core.Maybe Types.ResourceName
    -- ^ The new data source name.
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
    -- ^ The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
  , responseMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The new response mapping template.
  , syncConfig :: Core.Maybe Types.SyncConfig
    -- ^ The @SyncConfig@ for a resolver attached to a versioned datasource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResolver' value with any optional fields omitted.
mkUpdateResolver
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'typeName'
    -> Types.ResourceName -- ^ 'fieldName'
    -> UpdateResolver
mkUpdateResolver apiId typeName fieldName
  = UpdateResolver'{apiId, typeName, fieldName,
                    cachingConfig = Core.Nothing, dataSourceName = Core.Nothing,
                    kind = Core.Nothing, pipelineConfig = Core.Nothing,
                    requestMappingTemplate = Core.Nothing,
                    responseMappingTemplate = Core.Nothing, syncConfig = Core.Nothing}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urApiId :: Lens.Lens' UpdateResolver Core.Text
urApiId = Lens.field @"apiId"
{-# INLINEABLE urApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The new type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTypeName :: Lens.Lens' UpdateResolver Types.ResourceName
urTypeName = Lens.field @"typeName"
{-# INLINEABLE urTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The new field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urFieldName :: Lens.Lens' UpdateResolver Types.ResourceName
urFieldName = Lens.field @"fieldName"
{-# INLINEABLE urFieldName #-}
{-# DEPRECATED fieldName "Use generic-lens or generic-optics with 'fieldName' instead"  #-}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urCachingConfig :: Lens.Lens' UpdateResolver (Core.Maybe Types.CachingConfig)
urCachingConfig = Lens.field @"cachingConfig"
{-# INLINEABLE urCachingConfig #-}
{-# DEPRECATED cachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead"  #-}

-- | The new data source name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDataSourceName :: Lens.Lens' UpdateResolver (Core.Maybe Types.ResourceName)
urDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE urDataSourceName #-}
{-# DEPRECATED dataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead"  #-}

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
urKind :: Lens.Lens' UpdateResolver (Core.Maybe Types.ResolverKind)
urKind = Lens.field @"kind"
{-# INLINEABLE urKind #-}
{-# DEPRECATED kind "Use generic-lens or generic-optics with 'kind' instead"  #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urPipelineConfig :: Lens.Lens' UpdateResolver (Core.Maybe Types.PipelineConfig)
urPipelineConfig = Lens.field @"pipelineConfig"
{-# INLINEABLE urPipelineConfig #-}
{-# DEPRECATED pipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead"  #-}

-- | The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRequestMappingTemplate :: Lens.Lens' UpdateResolver (Core.Maybe Types.MappingTemplate)
urRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# INLINEABLE urRequestMappingTemplate #-}
{-# DEPRECATED requestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead"  #-}

-- | The new response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResponseMappingTemplate :: Lens.Lens' UpdateResolver (Core.Maybe Types.MappingTemplate)
urResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# INLINEABLE urResponseMappingTemplate #-}
{-# DEPRECATED responseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead"  #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urSyncConfig :: Lens.Lens' UpdateResolver (Core.Maybe Types.SyncConfig)
urSyncConfig = Lens.field @"syncConfig"
{-# INLINEABLE urSyncConfig #-}
{-# DEPRECATED syncConfig "Use generic-lens or generic-optics with 'syncConfig' instead"  #-}

instance Core.ToQuery UpdateResolver where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateResolver where
        toHeaders UpdateResolver{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateResolver where
        toJSON UpdateResolver{..}
          = Core.object
              (Core.catMaybes
                 [("cachingConfig" Core..=) Core.<$> cachingConfig,
                  ("dataSourceName" Core..=) Core.<$> dataSourceName,
                  ("kind" Core..=) Core.<$> kind,
                  ("pipelineConfig" Core..=) Core.<$> pipelineConfig,
                  ("requestMappingTemplate" Core..=) Core.<$> requestMappingTemplate,
                  ("responseMappingTemplate" Core..=) Core.<$>
                    responseMappingTemplate,
                  ("syncConfig" Core..=) Core.<$> syncConfig])

instance Core.AWSRequest UpdateResolver where
        type Rs UpdateResolver = UpdateResolverResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/types/" Core.<>
                             Core.toText typeName
                             Core.<> "/resolvers/"
                             Core.<> Core.toText fieldName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateResolverResponse' Core.<$>
                   (x Core..:? "resolver") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateResolverResponse' smart constructor.
data UpdateResolverResponse = UpdateResolverResponse'
  { resolver :: Core.Maybe Types.Resolver
    -- ^ The updated @Resolver@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResolverResponse' value with any optional fields omitted.
mkUpdateResolverResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateResolverResponse
mkUpdateResolverResponse responseStatus
  = UpdateResolverResponse'{resolver = Core.Nothing, responseStatus}

-- | The updated @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResolver :: Lens.Lens' UpdateResolverResponse (Core.Maybe Types.Resolver)
urrrsResolver = Lens.field @"resolver"
{-# INLINEABLE urrrsResolver #-}
{-# DEPRECATED resolver "Use generic-lens or generic-optics with 'resolver' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateResolverResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
