{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateResolver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Resolver@ object.
--
-- A resolver converts incoming requests into a format that a data source
-- can understand and converts the data source\'s responses into GraphQL.
module Network.AWS.AppSync.CreateResolver
  ( -- * Creating a Request
    CreateResolver (..),
    newCreateResolver,

    -- * Request Lenses
    createResolver_responseMappingTemplate,
    createResolver_kind,
    createResolver_syncConfig,
    createResolver_dataSourceName,
    createResolver_cachingConfig,
    createResolver_pipelineConfig,
    createResolver_requestMappingTemplate,
    createResolver_apiId,
    createResolver_typeName,
    createResolver_fieldName,

    -- * Destructuring the Response
    CreateResolverResponse (..),
    newCreateResolverResponse,

    -- * Response Lenses
    createResolverResponse_resolver,
    createResolverResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateResolver' smart constructor.
data CreateResolver = CreateResolver'
  { -- | The mapping template to be used for responses from the data source.
    responseMappingTemplate :: Core.Maybe Core.Text,
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
    -- | The name of the data source for which the resolver is being created.
    dataSourceName :: Core.Maybe Core.Text,
    -- | The caching configuration for the resolver.
    cachingConfig :: Core.Maybe CachingConfig,
    -- | The @PipelineConfig@.
    pipelineConfig :: Core.Maybe PipelineConfig,
    -- | The mapping template to be used for requests.
    --
    -- A resolver uses a request mapping template to convert a GraphQL
    -- expression into a format that a data source can understand. Mapping
    -- templates are written in Apache Velocity Template Language (VTL).
    --
    -- VTL request mapping templates are optional when using a Lambda data
    -- source. For all other data sources, VTL request and response mapping
    -- templates are required.
    requestMappingTemplate :: Core.Maybe Core.Text,
    -- | The ID for the GraphQL API for which the resolver is being created.
    apiId :: Core.Text,
    -- | The name of the @Type@.
    typeName :: Core.Text,
    -- | The name of the field to attach the resolver to.
    fieldName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseMappingTemplate', 'createResolver_responseMappingTemplate' - The mapping template to be used for responses from the data source.
--
-- 'kind', 'createResolver_kind' - The resolver type.
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
-- 'syncConfig', 'createResolver_syncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- 'dataSourceName', 'createResolver_dataSourceName' - The name of the data source for which the resolver is being created.
--
-- 'cachingConfig', 'createResolver_cachingConfig' - The caching configuration for the resolver.
--
-- 'pipelineConfig', 'createResolver_pipelineConfig' - The @PipelineConfig@.
--
-- 'requestMappingTemplate', 'createResolver_requestMappingTemplate' - The mapping template to be used for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using a Lambda data
-- source. For all other data sources, VTL request and response mapping
-- templates are required.
--
-- 'apiId', 'createResolver_apiId' - The ID for the GraphQL API for which the resolver is being created.
--
-- 'typeName', 'createResolver_typeName' - The name of the @Type@.
--
-- 'fieldName', 'createResolver_fieldName' - The name of the field to attach the resolver to.
newCreateResolver ::
  -- | 'apiId'
  Core.Text ->
  -- | 'typeName'
  Core.Text ->
  -- | 'fieldName'
  Core.Text ->
  CreateResolver
newCreateResolver pApiId_ pTypeName_ pFieldName_ =
  CreateResolver'
    { responseMappingTemplate =
        Core.Nothing,
      kind = Core.Nothing,
      syncConfig = Core.Nothing,
      dataSourceName = Core.Nothing,
      cachingConfig = Core.Nothing,
      pipelineConfig = Core.Nothing,
      requestMappingTemplate = Core.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The mapping template to be used for responses from the data source.
createResolver_responseMappingTemplate :: Lens.Lens' CreateResolver (Core.Maybe Core.Text)
createResolver_responseMappingTemplate = Lens.lens (\CreateResolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@CreateResolver' {} a -> s {responseMappingTemplate = a} :: CreateResolver)

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
createResolver_kind :: Lens.Lens' CreateResolver (Core.Maybe ResolverKind)
createResolver_kind = Lens.lens (\CreateResolver' {kind} -> kind) (\s@CreateResolver' {} a -> s {kind = a} :: CreateResolver)

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
createResolver_syncConfig :: Lens.Lens' CreateResolver (Core.Maybe SyncConfig)
createResolver_syncConfig = Lens.lens (\CreateResolver' {syncConfig} -> syncConfig) (\s@CreateResolver' {} a -> s {syncConfig = a} :: CreateResolver)

-- | The name of the data source for which the resolver is being created.
createResolver_dataSourceName :: Lens.Lens' CreateResolver (Core.Maybe Core.Text)
createResolver_dataSourceName = Lens.lens (\CreateResolver' {dataSourceName} -> dataSourceName) (\s@CreateResolver' {} a -> s {dataSourceName = a} :: CreateResolver)

-- | The caching configuration for the resolver.
createResolver_cachingConfig :: Lens.Lens' CreateResolver (Core.Maybe CachingConfig)
createResolver_cachingConfig = Lens.lens (\CreateResolver' {cachingConfig} -> cachingConfig) (\s@CreateResolver' {} a -> s {cachingConfig = a} :: CreateResolver)

-- | The @PipelineConfig@.
createResolver_pipelineConfig :: Lens.Lens' CreateResolver (Core.Maybe PipelineConfig)
createResolver_pipelineConfig = Lens.lens (\CreateResolver' {pipelineConfig} -> pipelineConfig) (\s@CreateResolver' {} a -> s {pipelineConfig = a} :: CreateResolver)

-- | The mapping template to be used for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using a Lambda data
-- source. For all other data sources, VTL request and response mapping
-- templates are required.
createResolver_requestMappingTemplate :: Lens.Lens' CreateResolver (Core.Maybe Core.Text)
createResolver_requestMappingTemplate = Lens.lens (\CreateResolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@CreateResolver' {} a -> s {requestMappingTemplate = a} :: CreateResolver)

-- | The ID for the GraphQL API for which the resolver is being created.
createResolver_apiId :: Lens.Lens' CreateResolver Core.Text
createResolver_apiId = Lens.lens (\CreateResolver' {apiId} -> apiId) (\s@CreateResolver' {} a -> s {apiId = a} :: CreateResolver)

-- | The name of the @Type@.
createResolver_typeName :: Lens.Lens' CreateResolver Core.Text
createResolver_typeName = Lens.lens (\CreateResolver' {typeName} -> typeName) (\s@CreateResolver' {} a -> s {typeName = a} :: CreateResolver)

-- | The name of the field to attach the resolver to.
createResolver_fieldName :: Lens.Lens' CreateResolver Core.Text
createResolver_fieldName = Lens.lens (\CreateResolver' {fieldName} -> fieldName) (\s@CreateResolver' {} a -> s {fieldName = a} :: CreateResolver)

instance Core.AWSRequest CreateResolver where
  type
    AWSResponse CreateResolver =
      CreateResolverResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResolverResponse'
            Core.<$> (x Core..?> "resolver")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateResolver

instance Core.NFData CreateResolver

instance Core.ToHeaders CreateResolver where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateResolver where
  toJSON CreateResolver' {..} =
    Core.object
      ( Core.catMaybes
          [ ("responseMappingTemplate" Core..=)
              Core.<$> responseMappingTemplate,
            ("kind" Core..=) Core.<$> kind,
            ("syncConfig" Core..=) Core.<$> syncConfig,
            ("dataSourceName" Core..=) Core.<$> dataSourceName,
            ("cachingConfig" Core..=) Core.<$> cachingConfig,
            ("pipelineConfig" Core..=) Core.<$> pipelineConfig,
            ("requestMappingTemplate" Core..=)
              Core.<$> requestMappingTemplate,
            Core.Just ("fieldName" Core..= fieldName)
          ]
      )

instance Core.ToPath CreateResolver where
  toPath CreateResolver' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName,
        "/resolvers"
      ]

instance Core.ToQuery CreateResolver where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateResolverResponse' smart constructor.
data CreateResolverResponse = CreateResolverResponse'
  { -- | The @Resolver@ object.
    resolver :: Core.Maybe Resolver,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResolverResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolver', 'createResolverResponse_resolver' - The @Resolver@ object.
--
-- 'httpStatus', 'createResolverResponse_httpStatus' - The response's http status code.
newCreateResolverResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateResolverResponse
newCreateResolverResponse pHttpStatus_ =
  CreateResolverResponse'
    { resolver = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Resolver@ object.
createResolverResponse_resolver :: Lens.Lens' CreateResolverResponse (Core.Maybe Resolver)
createResolverResponse_resolver = Lens.lens (\CreateResolverResponse' {resolver} -> resolver) (\s@CreateResolverResponse' {} a -> s {resolver = a} :: CreateResolverResponse)

-- | The response's http status code.
createResolverResponse_httpStatus :: Lens.Lens' CreateResolverResponse Core.Int
createResolverResponse_httpStatus = Lens.lens (\CreateResolverResponse' {httpStatus} -> httpStatus) (\s@CreateResolverResponse' {} a -> s {httpStatus = a} :: CreateResolverResponse)

instance Core.NFData CreateResolverResponse
