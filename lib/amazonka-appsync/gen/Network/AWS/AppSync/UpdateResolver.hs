{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    urDataSourceName,
    urRequestMappingTemplate,
    urKind,
    urCachingConfig,
    urResponseMappingTemplate,
    urSyncConfig,
    urPipelineConfig,
    urApiId,
    urTypeName,
    urFieldName,

    -- * Destructuring the response
    UpdateResolverResponse (..),
    mkUpdateResolverResponse,

    -- ** Response lenses
    urrsResolver,
    urrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateResolver' smart constructor.
data UpdateResolver = UpdateResolver'
  { dataSourceName ::
      Lude.Maybe Lude.Text,
    requestMappingTemplate :: Lude.Maybe Lude.Text,
    kind :: Lude.Maybe ResolverKind,
    cachingConfig :: Lude.Maybe CachingConfig,
    responseMappingTemplate :: Lude.Maybe Lude.Text,
    syncConfig :: Lude.Maybe SyncConfig,
    pipelineConfig :: Lude.Maybe PipelineConfig,
    apiId :: Lude.Text,
    typeName :: Lude.Text,
    fieldName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResolver' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'cachingConfig' - The caching configuration for the resolver.
-- * 'dataSourceName' - The new data source name.
-- * 'fieldName' - The new field name.
-- * 'kind' - The resolver type.
--
--
--     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.
--
--
--     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
--
-- * 'pipelineConfig' - The @PipelineConfig@ .
-- * 'requestMappingTemplate' - The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
-- * 'responseMappingTemplate' - The new response mapping template.
-- * 'syncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
-- * 'typeName' - The new type name.
mkUpdateResolver ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  -- | 'fieldName'
  Lude.Text ->
  UpdateResolver
mkUpdateResolver pApiId_ pTypeName_ pFieldName_ =
  UpdateResolver'
    { dataSourceName = Lude.Nothing,
      requestMappingTemplate = Lude.Nothing,
      kind = Lude.Nothing,
      cachingConfig = Lude.Nothing,
      responseMappingTemplate = Lude.Nothing,
      syncConfig = Lude.Nothing,
      pipelineConfig = Lude.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The new data source name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDataSourceName :: Lens.Lens' UpdateResolver (Lude.Maybe Lude.Text)
urDataSourceName = Lens.lens (dataSourceName :: UpdateResolver -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceName = a} :: UpdateResolver)
{-# DEPRECATED urDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRequestMappingTemplate :: Lens.Lens' UpdateResolver (Lude.Maybe Lude.Text)
urRequestMappingTemplate = Lens.lens (requestMappingTemplate :: UpdateResolver -> Lude.Maybe Lude.Text) (\s a -> s {requestMappingTemplate = a} :: UpdateResolver)
{-# DEPRECATED urRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

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
urKind :: Lens.Lens' UpdateResolver (Lude.Maybe ResolverKind)
urKind = Lens.lens (kind :: UpdateResolver -> Lude.Maybe ResolverKind) (\s a -> s {kind = a} :: UpdateResolver)
{-# DEPRECATED urKind "Use generic-lens or generic-optics with 'kind' instead." #-}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urCachingConfig :: Lens.Lens' UpdateResolver (Lude.Maybe CachingConfig)
urCachingConfig = Lens.lens (cachingConfig :: UpdateResolver -> Lude.Maybe CachingConfig) (\s a -> s {cachingConfig = a} :: UpdateResolver)
{-# DEPRECATED urCachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead." #-}

-- | The new response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResponseMappingTemplate :: Lens.Lens' UpdateResolver (Lude.Maybe Lude.Text)
urResponseMappingTemplate = Lens.lens (responseMappingTemplate :: UpdateResolver -> Lude.Maybe Lude.Text) (\s a -> s {responseMappingTemplate = a} :: UpdateResolver)
{-# DEPRECATED urResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urSyncConfig :: Lens.Lens' UpdateResolver (Lude.Maybe SyncConfig)
urSyncConfig = Lens.lens (syncConfig :: UpdateResolver -> Lude.Maybe SyncConfig) (\s a -> s {syncConfig = a} :: UpdateResolver)
{-# DEPRECATED urSyncConfig "Use generic-lens or generic-optics with 'syncConfig' instead." #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urPipelineConfig :: Lens.Lens' UpdateResolver (Lude.Maybe PipelineConfig)
urPipelineConfig = Lens.lens (pipelineConfig :: UpdateResolver -> Lude.Maybe PipelineConfig) (\s a -> s {pipelineConfig = a} :: UpdateResolver)
{-# DEPRECATED urPipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urApiId :: Lens.Lens' UpdateResolver Lude.Text
urApiId = Lens.lens (apiId :: UpdateResolver -> Lude.Text) (\s a -> s {apiId = a} :: UpdateResolver)
{-# DEPRECATED urApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The new type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTypeName :: Lens.Lens' UpdateResolver Lude.Text
urTypeName = Lens.lens (typeName :: UpdateResolver -> Lude.Text) (\s a -> s {typeName = a} :: UpdateResolver)
{-# DEPRECATED urTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The new field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urFieldName :: Lens.Lens' UpdateResolver Lude.Text
urFieldName = Lens.lens (fieldName :: UpdateResolver -> Lude.Text) (\s a -> s {fieldName = a} :: UpdateResolver)
{-# DEPRECATED urFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Lude.AWSRequest UpdateResolver where
  type Rs UpdateResolver = UpdateResolverResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateResolverResponse'
            Lude.<$> (x Lude..?> "resolver") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateResolver where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateResolver where
  toJSON UpdateResolver' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("dataSourceName" Lude..=) Lude.<$> dataSourceName,
            ("requestMappingTemplate" Lude..=) Lude.<$> requestMappingTemplate,
            ("kind" Lude..=) Lude.<$> kind,
            ("cachingConfig" Lude..=) Lude.<$> cachingConfig,
            ("responseMappingTemplate" Lude..=)
              Lude.<$> responseMappingTemplate,
            ("syncConfig" Lude..=) Lude.<$> syncConfig,
            ("pipelineConfig" Lude..=) Lude.<$> pipelineConfig
          ]
      )

instance Lude.ToPath UpdateResolver where
  toPath UpdateResolver' {..} =
    Lude.mconcat
      [ "/v1/apis/",
        Lude.toBS apiId,
        "/types/",
        Lude.toBS typeName,
        "/resolvers/",
        Lude.toBS fieldName
      ]

instance Lude.ToQuery UpdateResolver where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateResolverResponse' smart constructor.
data UpdateResolverResponse = UpdateResolverResponse'
  { resolver ::
      Lude.Maybe Resolver,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResolverResponse' with the minimum fields required to make a request.
--
-- * 'resolver' - The updated @Resolver@ object.
-- * 'responseStatus' - The response status code.
mkUpdateResolverResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateResolverResponse
mkUpdateResolverResponse pResponseStatus_ =
  UpdateResolverResponse'
    { resolver = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResolver :: Lens.Lens' UpdateResolverResponse (Lude.Maybe Resolver)
urrsResolver = Lens.lens (resolver :: UpdateResolverResponse -> Lude.Maybe Resolver) (\s a -> s {resolver = a} :: UpdateResolverResponse)
{-# DEPRECATED urrsResolver "Use generic-lens or generic-optics with 'resolver' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateResolverResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateResolverResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateResolverResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
