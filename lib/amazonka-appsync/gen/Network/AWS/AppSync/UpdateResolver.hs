{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateResolver (..),
    mkUpdateResolver,

    -- ** Request lenses
    urApiId,
    urTypeName,
    urFieldName,
    urCachingConfig,
    urDataSourceName,
    urKind,
    urPipelineConfig,
    urRequestMappingTemplate,
    urResponseMappingTemplate,
    urSyncConfig,

    -- * Destructuring the response
    UpdateResolverResponse (..),
    mkUpdateResolverResponse,

    -- ** Response lenses
    urrrsResolver,
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateResolver' smart constructor.
data UpdateResolver = UpdateResolver'
  { -- | The API ID.
    apiId :: Types.String,
    -- | The new type name.
    typeName :: Types.ResourceName,
    -- | The new field name.
    fieldName :: Types.ResourceName,
    -- | The caching configuration for the resolver.
    cachingConfig :: Core.Maybe Types.CachingConfig,
    -- | The new data source name.
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
    -- | The new request mapping template.
    --
    -- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
    -- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
    requestMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The new response mapping template.
    responseMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The @SyncConfig@ for a resolver attached to a versioned datasource.
    syncConfig :: Core.Maybe Types.SyncConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResolver' value with any optional fields omitted.
mkUpdateResolver ::
  -- | 'apiId'
  Types.String ->
  -- | 'typeName'
  Types.ResourceName ->
  -- | 'fieldName'
  Types.ResourceName ->
  UpdateResolver
mkUpdateResolver apiId typeName fieldName =
  UpdateResolver'
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

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urApiId :: Lens.Lens' UpdateResolver Types.String
urApiId = Lens.field @"apiId"
{-# DEPRECATED urApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The new type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTypeName :: Lens.Lens' UpdateResolver Types.ResourceName
urTypeName = Lens.field @"typeName"
{-# DEPRECATED urTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The new field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urFieldName :: Lens.Lens' UpdateResolver Types.ResourceName
urFieldName = Lens.field @"fieldName"
{-# DEPRECATED urFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urCachingConfig :: Lens.Lens' UpdateResolver (Core.Maybe Types.CachingConfig)
urCachingConfig = Lens.field @"cachingConfig"
{-# DEPRECATED urCachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead." #-}

-- | The new data source name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDataSourceName :: Lens.Lens' UpdateResolver (Core.Maybe Types.ResourceName)
urDataSourceName = Lens.field @"dataSourceName"
{-# DEPRECATED urDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

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
{-# DEPRECATED urKind "Use generic-lens or generic-optics with 'kind' instead." #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urPipelineConfig :: Lens.Lens' UpdateResolver (Core.Maybe Types.PipelineConfig)
urPipelineConfig = Lens.field @"pipelineConfig"
{-# DEPRECATED urPipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead." #-}

-- | The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRequestMappingTemplate :: Lens.Lens' UpdateResolver (Core.Maybe Types.MappingTemplate)
urRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# DEPRECATED urRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The new response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResponseMappingTemplate :: Lens.Lens' UpdateResolver (Core.Maybe Types.MappingTemplate)
urResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# DEPRECATED urResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urSyncConfig :: Lens.Lens' UpdateResolver (Core.Maybe Types.SyncConfig)
urSyncConfig = Lens.field @"syncConfig"
{-# DEPRECATED urSyncConfig "Use generic-lens or generic-optics with 'syncConfig' instead." #-}

instance Core.FromJSON UpdateResolver where
  toJSON UpdateResolver {..} =
    Core.object
      ( Core.catMaybes
          [ ("cachingConfig" Core..=) Core.<$> cachingConfig,
            ("dataSourceName" Core..=) Core.<$> dataSourceName,
            ("kind" Core..=) Core.<$> kind,
            ("pipelineConfig" Core..=) Core.<$> pipelineConfig,
            ("requestMappingTemplate" Core..=) Core.<$> requestMappingTemplate,
            ("responseMappingTemplate" Core..=)
              Core.<$> responseMappingTemplate,
            ("syncConfig" Core..=) Core.<$> syncConfig
          ]
      )

instance Core.AWSRequest UpdateResolver where
  type Rs UpdateResolver = UpdateResolverResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apis/" Core.<> (Core.toText apiId) Core.<> ("/types/")
                Core.<> (Core.toText typeName)
                Core.<> ("/resolvers/")
                Core.<> (Core.toText fieldName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResolverResponse'
            Core.<$> (x Core..:? "resolver") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateResolverResponse' smart constructor.
data UpdateResolverResponse = UpdateResolverResponse'
  { -- | The updated @Resolver@ object.
    resolver :: Core.Maybe Types.Resolver,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResolverResponse' value with any optional fields omitted.
mkUpdateResolverResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateResolverResponse
mkUpdateResolverResponse responseStatus =
  UpdateResolverResponse' {resolver = Core.Nothing, responseStatus}

-- | The updated @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResolver :: Lens.Lens' UpdateResolverResponse (Core.Maybe Types.Resolver)
urrrsResolver = Lens.field @"resolver"
{-# DEPRECATED urrrsResolver "Use generic-lens or generic-optics with 'resolver' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateResolverResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
