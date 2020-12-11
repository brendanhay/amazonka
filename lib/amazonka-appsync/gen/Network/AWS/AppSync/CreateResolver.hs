{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    crDataSourceName,
    crRequestMappingTemplate,
    crKind,
    crCachingConfig,
    crResponseMappingTemplate,
    crSyncConfig,
    crPipelineConfig,
    crApiId,
    crTypeName,
    crFieldName,

    -- * Destructuring the response
    CreateResolverResponse (..),
    mkCreateResolverResponse,

    -- ** Response lenses
    crrsResolver,
    crrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateResolver' smart constructor.
data CreateResolver = CreateResolver'
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

-- | Creates a value of 'CreateResolver' with the minimum fields required to make a request.
--
-- * 'apiId' - The ID for the GraphQL API for which the resolver is being created.
-- * 'cachingConfig' - The caching configuration for the resolver.
-- * 'dataSourceName' - The name of the data source for which the resolver is being created.
-- * 'fieldName' - The name of the field to attach the resolver to.
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
-- * 'requestMappingTemplate' - The mapping template to be used for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
-- * 'responseMappingTemplate' - The mapping template to be used for responses from the data source.
-- * 'syncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
-- * 'typeName' - The name of the @Type@ .
mkCreateResolver ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  -- | 'fieldName'
  Lude.Text ->
  CreateResolver
mkCreateResolver pApiId_ pTypeName_ pFieldName_ =
  CreateResolver'
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

-- | The name of the data source for which the resolver is being created.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDataSourceName :: Lens.Lens' CreateResolver (Lude.Maybe Lude.Text)
crDataSourceName = Lens.lens (dataSourceName :: CreateResolver -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceName = a} :: CreateResolver)
{-# DEPRECATED crDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The mapping template to be used for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
-- VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRequestMappingTemplate :: Lens.Lens' CreateResolver (Lude.Maybe Lude.Text)
crRequestMappingTemplate = Lens.lens (requestMappingTemplate :: CreateResolver -> Lude.Maybe Lude.Text) (\s a -> s {requestMappingTemplate = a} :: CreateResolver)
{-# DEPRECATED crRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

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
crKind :: Lens.Lens' CreateResolver (Lude.Maybe ResolverKind)
crKind = Lens.lens (kind :: CreateResolver -> Lude.Maybe ResolverKind) (\s a -> s {kind = a} :: CreateResolver)
{-# DEPRECATED crKind "Use generic-lens or generic-optics with 'kind' instead." #-}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCachingConfig :: Lens.Lens' CreateResolver (Lude.Maybe CachingConfig)
crCachingConfig = Lens.lens (cachingConfig :: CreateResolver -> Lude.Maybe CachingConfig) (\s a -> s {cachingConfig = a} :: CreateResolver)
{-# DEPRECATED crCachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead." #-}

-- | The mapping template to be used for responses from the data source.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crResponseMappingTemplate :: Lens.Lens' CreateResolver (Lude.Maybe Lude.Text)
crResponseMappingTemplate = Lens.lens (responseMappingTemplate :: CreateResolver -> Lude.Maybe Lude.Text) (\s a -> s {responseMappingTemplate = a} :: CreateResolver)
{-# DEPRECATED crResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSyncConfig :: Lens.Lens' CreateResolver (Lude.Maybe SyncConfig)
crSyncConfig = Lens.lens (syncConfig :: CreateResolver -> Lude.Maybe SyncConfig) (\s a -> s {syncConfig = a} :: CreateResolver)
{-# DEPRECATED crSyncConfig "Use generic-lens or generic-optics with 'syncConfig' instead." #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crPipelineConfig :: Lens.Lens' CreateResolver (Lude.Maybe PipelineConfig)
crPipelineConfig = Lens.lens (pipelineConfig :: CreateResolver -> Lude.Maybe PipelineConfig) (\s a -> s {pipelineConfig = a} :: CreateResolver)
{-# DEPRECATED crPipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead." #-}

-- | The ID for the GraphQL API for which the resolver is being created.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crApiId :: Lens.Lens' CreateResolver Lude.Text
crApiId = Lens.lens (apiId :: CreateResolver -> Lude.Text) (\s a -> s {apiId = a} :: CreateResolver)
{-# DEPRECATED crApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The name of the @Type@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTypeName :: Lens.Lens' CreateResolver Lude.Text
crTypeName = Lens.lens (typeName :: CreateResolver -> Lude.Text) (\s a -> s {typeName = a} :: CreateResolver)
{-# DEPRECATED crTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The name of the field to attach the resolver to.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crFieldName :: Lens.Lens' CreateResolver Lude.Text
crFieldName = Lens.lens (fieldName :: CreateResolver -> Lude.Text) (\s a -> s {fieldName = a} :: CreateResolver)
{-# DEPRECATED crFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

instance Lude.AWSRequest CreateResolver where
  type Rs CreateResolver = CreateResolverResponse
  request = Req.postJSON appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateResolverResponse'
            Lude.<$> (x Lude..?> "resolver") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateResolver where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateResolver where
  toJSON CreateResolver' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("dataSourceName" Lude..=) Lude.<$> dataSourceName,
            ("requestMappingTemplate" Lude..=) Lude.<$> requestMappingTemplate,
            ("kind" Lude..=) Lude.<$> kind,
            ("cachingConfig" Lude..=) Lude.<$> cachingConfig,
            ("responseMappingTemplate" Lude..=)
              Lude.<$> responseMappingTemplate,
            ("syncConfig" Lude..=) Lude.<$> syncConfig,
            ("pipelineConfig" Lude..=) Lude.<$> pipelineConfig,
            Lude.Just ("fieldName" Lude..= fieldName)
          ]
      )

instance Lude.ToPath CreateResolver where
  toPath CreateResolver' {..} =
    Lude.mconcat
      [ "/v1/apis/",
        Lude.toBS apiId,
        "/types/",
        Lude.toBS typeName,
        "/resolvers"
      ]

instance Lude.ToQuery CreateResolver where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateResolverResponse' smart constructor.
data CreateResolverResponse = CreateResolverResponse'
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

-- | Creates a value of 'CreateResolverResponse' with the minimum fields required to make a request.
--
-- * 'resolver' - The @Resolver@ object.
-- * 'responseStatus' - The response status code.
mkCreateResolverResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateResolverResponse
mkCreateResolverResponse pResponseStatus_ =
  CreateResolverResponse'
    { resolver = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @Resolver@ object.
--
-- /Note:/ Consider using 'resolver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResolver :: Lens.Lens' CreateResolverResponse (Lude.Maybe Resolver)
crrsResolver = Lens.lens (resolver :: CreateResolverResponse -> Lude.Maybe Resolver) (\s a -> s {resolver = a} :: CreateResolverResponse)
{-# DEPRECATED crrsResolver "Use generic-lens or generic-optics with 'resolver' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateResolverResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateResolverResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateResolverResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
