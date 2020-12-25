{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateResolver (..),
    mkCreateResolver,

    -- ** Request lenses
    crApiId,
    crTypeName,
    crFieldName,
    crCachingConfig,
    crDataSourceName,
    crKind,
    crPipelineConfig,
    crRequestMappingTemplate,
    crResponseMappingTemplate,
    crSyncConfig,

    -- * Destructuring the response
    CreateResolverResponse (..),
    mkCreateResolverResponse,

    -- ** Response lenses
    crrrsResolver,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateResolver' smart constructor.
data CreateResolver = CreateResolver'
  { -- | The ID for the GraphQL API for which the resolver is being created.
    apiId :: Types.String,
    -- | The name of the @Type@ .
    typeName :: Types.ResourceName,
    -- | The name of the field to attach the resolver to.
    fieldName :: Types.ResourceName,
    -- | The caching configuration for the resolver.
    cachingConfig :: Core.Maybe Types.CachingConfig,
    -- | The name of the data source for which the resolver is being created.
    dataSourceName :: Core.Maybe Types.ResourceName,
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
    -- | The mapping template to be used for requests.
    --
    -- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
    -- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
    requestMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The mapping template to be used for responses from the data source.
    responseMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The @SyncConfig@ for a resolver attached to a versioned datasource.
    syncConfig :: Core.Maybe Types.SyncConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResolver' value with any optional fields omitted.
mkCreateResolver ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.ResourceName ->
  -- | 'fieldName'
  Types.ResourceName ->
  CreateResolver
mkCreateResolver apiId typeName fieldName =
  CreateResolver'
    { apiId,
      typeName,
      fieldName,
      cachingConfig = Core.Nothing,
      dataSourceName = Core.Nothing,
      kind = Core.Nothing,
      pipelineConfig = Core.Nothing,
      requestMappingTemplate = Core.Nothing,
      responseMappingTemplate = Core.Nothing,
      syncConfig = Core.Nothing
    }

-- | The ID for the GraphQL API for which the resolver is being created.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crApiId :: Lens.Lens' CreateResolver Types.String
crApiId = Lens.field @"apiId"
{-# DEPRECATED crApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The name of the @Type@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTypeName :: Lens.Lens' CreateResolver Types.ResourceName
crTypeName = Lens.field @"typeName"
{-# DEPRECATED crTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The name of the field to attach the resolver to.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crFieldName :: Lens.Lens' CreateResolver Types.ResourceName
crFieldName = Lens.field @"fieldName"
{-# DEPRECATED crFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCachingConfig :: Lens.Lens' CreateResolver (Core.Maybe Types.CachingConfig)
crCachingConfig = Lens.field @"cachingConfig"
{-# DEPRECATED crCachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead." #-}

-- | The name of the data source for which the resolver is being created.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDataSourceName :: Lens.Lens' CreateResolver (Core.Maybe Types.ResourceName)
crDataSourceName = Lens.field @"dataSourceName"
{-# DEPRECATED crDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

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
{-# DEPRECATED crKind "Use generic-lens or generic-optics with 'kind' instead." #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPipelineConfig :: Lens.Lens' CreateResolver (Core.Maybe Types.PipelineConfig)
crPipelineConfig = Lens.field @"pipelineConfig"
{-# DEPRECATED crPipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead." #-}

-- | The mapping template to be used for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRequestMappingTemplate :: Lens.Lens' CreateResolver (Core.Maybe Types.MappingTemplate)
crRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# DEPRECATED crRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The mapping template to be used for responses from the data source.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crResponseMappingTemplate :: Lens.Lens' CreateResolver (Core.Maybe Types.MappingTemplate)
crResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# DEPRECATED crResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSyncConfig :: Lens.Lens' CreateResolver (Core.Maybe Types.SyncConfig)
crSyncConfig = Lens.field @"syncConfig"
{-# DEPRECATED crSyncConfig "Use generic-lens or generic-optics with 'syncConfig' instead." #-}

instance Core.FromJSON CreateResolver where
  toJSON CreateResolver {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("fieldName" Core..= fieldName),
            ("cachingConfig" Core..=) Core.<$> cachingConfig,
            ("dataSourceName" Core..=) Core.<$> dataSourceName,
            ("kind" Core..=) Core.<$> kind,
            ("pipelineConfig" Core..=) Core.<$> pipelineConfig,
            ("requestMappingTemplate" Core..=) Core.<$> requestMappingTemplate,
            ("responseMappingTemplate" Core..=)
              Core.<$> responseMappingTemplate,
            ("syncConfig" Core..=) Core.<$> syncConfig
          ]
      )

instance Core.AWSRequest CreateResolver where
  type Rs CreateResolver = CreateResolverResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types/")
                Core.<> (Core.toText typeName)
                Core.<> ("/resolvers")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResolverResponse'
            Core.<$> (x Core..:? "resolver") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateResolverResponse' smart constructor.
data CreateResolverResponse = CreateResolverResponse'
  { -- | The @Resolver@ object.
    resolver :: Core.Maybe Types.Resolver,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResolverResponse' value with any optional fields omitted.
mkCreateResolverResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateResolverResponse
mkCreateResolverResponse responseStatus =
  CreateResolverResponse' {resolver = Core.Nothing, responseStatus}

-- | The @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResolver :: Lens.Lens' CreateResolverResponse (Core.Maybe Types.Resolver)
crrrsResolver = Lens.field @"resolver"
{-# DEPRECATED crrrsResolver "Use generic-lens or generic-optics with 'resolver' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateResolverResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
