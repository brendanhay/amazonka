{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Resolver@ object.
--
-- A resolver converts incoming requests into a format that a data source can understand and converts the data source's responses into GraphQL.
module Network.AWS.AppSync.CreateResolver
    (
    -- * Creating a request
      CreateResolver (..)
    , mkCreateResolver
    -- ** Request lenses
    , crApiId
    , crTypeName
    , crFieldName
    , crCachingConfig
    , crDataSourceName
    , crKind
    , crPipelineConfig
    , crRequestMappingTemplate
    , crResponseMappingTemplate
    , crSyncConfig

    -- * Destructuring the response
    , CreateResolverResponse (..)
    , mkCreateResolverResponse
    -- ** Response lenses
    , crrrsResolver
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateResolver' smart constructor.
data CreateResolver = CreateResolver'
  { apiId :: Core.Text
    -- ^ The ID for the GraphQL API for which the resolver is being created.
  , typeName :: Types.ResourceName
    -- ^ The name of the @Type@ .
  , fieldName :: Types.ResourceName
    -- ^ The name of the field to attach the resolver to.
  , cachingConfig :: Core.Maybe Types.CachingConfig
    -- ^ The caching configuration for the resolver.
  , dataSourceName :: Core.Maybe Types.ResourceName
    -- ^ The name of the data source for which the resolver is being created.
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
    -- ^ The mapping template to be used for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
  , responseMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The mapping template to be used for responses from the data source.
  , syncConfig :: Core.Maybe Types.SyncConfig
    -- ^ The @SyncConfig@ for a resolver attached to a versioned datasource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResolver' value with any optional fields omitted.
mkCreateResolver
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'typeName'
    -> Types.ResourceName -- ^ 'fieldName'
    -> CreateResolver
mkCreateResolver apiId typeName fieldName
  = CreateResolver'{apiId, typeName, fieldName,
                    cachingConfig = Core.Nothing, dataSourceName = Core.Nothing,
                    kind = Core.Nothing, pipelineConfig = Core.Nothing,
                    requestMappingTemplate = Core.Nothing,
                    responseMappingTemplate = Core.Nothing, syncConfig = Core.Nothing}

-- | The ID for the GraphQL API for which the resolver is being created.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crApiId :: Lens.Lens' CreateResolver Core.Text
crApiId = Lens.field @"apiId"
{-# INLINEABLE crApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The name of the @Type@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTypeName :: Lens.Lens' CreateResolver Types.ResourceName
crTypeName = Lens.field @"typeName"
{-# INLINEABLE crTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The name of the field to attach the resolver to.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crFieldName :: Lens.Lens' CreateResolver Types.ResourceName
crFieldName = Lens.field @"fieldName"
{-# INLINEABLE crFieldName #-}
{-# DEPRECATED fieldName "Use generic-lens or generic-optics with 'fieldName' instead"  #-}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCachingConfig :: Lens.Lens' CreateResolver (Core.Maybe Types.CachingConfig)
crCachingConfig = Lens.field @"cachingConfig"
{-# INLINEABLE crCachingConfig #-}
{-# DEPRECATED cachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead"  #-}

-- | The name of the data source for which the resolver is being created.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDataSourceName :: Lens.Lens' CreateResolver (Core.Maybe Types.ResourceName)
crDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE crDataSourceName #-}
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
crKind :: Lens.Lens' CreateResolver (Core.Maybe Types.ResolverKind)
crKind = Lens.field @"kind"
{-# INLINEABLE crKind #-}
{-# DEPRECATED kind "Use generic-lens or generic-optics with 'kind' instead"  #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPipelineConfig :: Lens.Lens' CreateResolver (Core.Maybe Types.PipelineConfig)
crPipelineConfig = Lens.field @"pipelineConfig"
{-# INLINEABLE crPipelineConfig #-}
{-# DEPRECATED pipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead"  #-}

-- | The mapping template to be used for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRequestMappingTemplate :: Lens.Lens' CreateResolver (Core.Maybe Types.MappingTemplate)
crRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# INLINEABLE crRequestMappingTemplate #-}
{-# DEPRECATED requestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead"  #-}

-- | The mapping template to be used for responses from the data source.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crResponseMappingTemplate :: Lens.Lens' CreateResolver (Core.Maybe Types.MappingTemplate)
crResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# INLINEABLE crResponseMappingTemplate #-}
{-# DEPRECATED responseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead"  #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSyncConfig :: Lens.Lens' CreateResolver (Core.Maybe Types.SyncConfig)
crSyncConfig = Lens.field @"syncConfig"
{-# INLINEABLE crSyncConfig #-}
{-# DEPRECATED syncConfig "Use generic-lens or generic-optics with 'syncConfig' instead"  #-}

instance Core.ToQuery CreateResolver where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateResolver where
        toHeaders CreateResolver{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateResolver where
        toJSON CreateResolver{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("fieldName" Core..= fieldName),
                  ("cachingConfig" Core..=) Core.<$> cachingConfig,
                  ("dataSourceName" Core..=) Core.<$> dataSourceName,
                  ("kind" Core..=) Core.<$> kind,
                  ("pipelineConfig" Core..=) Core.<$> pipelineConfig,
                  ("requestMappingTemplate" Core..=) Core.<$> requestMappingTemplate,
                  ("responseMappingTemplate" Core..=) Core.<$>
                    responseMappingTemplate,
                  ("syncConfig" Core..=) Core.<$> syncConfig])

instance Core.AWSRequest CreateResolver where
        type Rs CreateResolver = CreateResolverResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/types/" Core.<>
                             Core.toText typeName
                             Core.<> "/resolvers",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateResolverResponse' Core.<$>
                   (x Core..:? "resolver") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateResolverResponse' smart constructor.
data CreateResolverResponse = CreateResolverResponse'
  { resolver :: Core.Maybe Types.Resolver
    -- ^ The @Resolver@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResolverResponse' value with any optional fields omitted.
mkCreateResolverResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateResolverResponse
mkCreateResolverResponse responseStatus
  = CreateResolverResponse'{resolver = Core.Nothing, responseStatus}

-- | The @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResolver :: Lens.Lens' CreateResolverResponse (Core.Maybe Types.Resolver)
crrrsResolver = Lens.field @"resolver"
{-# INLINEABLE crrrsResolver #-}
{-# DEPRECATED resolver "Use generic-lens or generic-optics with 'resolver' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateResolverResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
