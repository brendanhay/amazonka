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
-- Module      : Network.AWS.AppSync.UpdateResolver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Resolver@ object.
module Network.AWS.AppSync.UpdateResolver
  ( -- * Creating a Request
    UpdateResolver (..),
    newUpdateResolver,

    -- * Request Lenses
    updateResolver_responseMappingTemplate,
    updateResolver_kind,
    updateResolver_syncConfig,
    updateResolver_dataSourceName,
    updateResolver_cachingConfig,
    updateResolver_pipelineConfig,
    updateResolver_requestMappingTemplate,
    updateResolver_apiId,
    updateResolver_typeName,
    updateResolver_fieldName,

    -- * Destructuring the Response
    UpdateResolverResponse (..),
    newUpdateResolverResponse,

    -- * Response Lenses
    updateResolverResponse_resolver,
    updateResolverResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateResolver' smart constructor.
data UpdateResolver = UpdateResolver'
  { -- | The new response mapping template.
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
    -- | The new data source name.
    dataSourceName :: Core.Maybe Core.Text,
    -- | The caching configuration for the resolver.
    cachingConfig :: Core.Maybe CachingConfig,
    -- | The @PipelineConfig@.
    pipelineConfig :: Core.Maybe PipelineConfig,
    -- | The new request mapping template.
    --
    -- A resolver uses a request mapping template to convert a GraphQL
    -- expression into a format that a data source can understand. Mapping
    -- templates are written in Apache Velocity Template Language (VTL).
    --
    -- VTL request mapping templates are optional when using a Lambda data
    -- source. For all other data sources, VTL request and response mapping
    -- templates are required.
    requestMappingTemplate :: Core.Maybe Core.Text,
    -- | The API ID.
    apiId :: Core.Text,
    -- | The new type name.
    typeName :: Core.Text,
    -- | The new field name.
    fieldName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateResolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseMappingTemplate', 'updateResolver_responseMappingTemplate' - The new response mapping template.
--
-- 'kind', 'updateResolver_kind' - The resolver type.
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
-- 'syncConfig', 'updateResolver_syncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- 'dataSourceName', 'updateResolver_dataSourceName' - The new data source name.
--
-- 'cachingConfig', 'updateResolver_cachingConfig' - The caching configuration for the resolver.
--
-- 'pipelineConfig', 'updateResolver_pipelineConfig' - The @PipelineConfig@.
--
-- 'requestMappingTemplate', 'updateResolver_requestMappingTemplate' - The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using a Lambda data
-- source. For all other data sources, VTL request and response mapping
-- templates are required.
--
-- 'apiId', 'updateResolver_apiId' - The API ID.
--
-- 'typeName', 'updateResolver_typeName' - The new type name.
--
-- 'fieldName', 'updateResolver_fieldName' - The new field name.
newUpdateResolver ::
  -- | 'apiId'
  Core.Text ->
  -- | 'typeName'
  Core.Text ->
  -- | 'fieldName'
  Core.Text ->
  UpdateResolver
newUpdateResolver pApiId_ pTypeName_ pFieldName_ =
  UpdateResolver'
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

-- | The new response mapping template.
updateResolver_responseMappingTemplate :: Lens.Lens' UpdateResolver (Core.Maybe Core.Text)
updateResolver_responseMappingTemplate = Lens.lens (\UpdateResolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@UpdateResolver' {} a -> s {responseMappingTemplate = a} :: UpdateResolver)

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
updateResolver_kind :: Lens.Lens' UpdateResolver (Core.Maybe ResolverKind)
updateResolver_kind = Lens.lens (\UpdateResolver' {kind} -> kind) (\s@UpdateResolver' {} a -> s {kind = a} :: UpdateResolver)

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
updateResolver_syncConfig :: Lens.Lens' UpdateResolver (Core.Maybe SyncConfig)
updateResolver_syncConfig = Lens.lens (\UpdateResolver' {syncConfig} -> syncConfig) (\s@UpdateResolver' {} a -> s {syncConfig = a} :: UpdateResolver)

-- | The new data source name.
updateResolver_dataSourceName :: Lens.Lens' UpdateResolver (Core.Maybe Core.Text)
updateResolver_dataSourceName = Lens.lens (\UpdateResolver' {dataSourceName} -> dataSourceName) (\s@UpdateResolver' {} a -> s {dataSourceName = a} :: UpdateResolver)

-- | The caching configuration for the resolver.
updateResolver_cachingConfig :: Lens.Lens' UpdateResolver (Core.Maybe CachingConfig)
updateResolver_cachingConfig = Lens.lens (\UpdateResolver' {cachingConfig} -> cachingConfig) (\s@UpdateResolver' {} a -> s {cachingConfig = a} :: UpdateResolver)

-- | The @PipelineConfig@.
updateResolver_pipelineConfig :: Lens.Lens' UpdateResolver (Core.Maybe PipelineConfig)
updateResolver_pipelineConfig = Lens.lens (\UpdateResolver' {pipelineConfig} -> pipelineConfig) (\s@UpdateResolver' {} a -> s {pipelineConfig = a} :: UpdateResolver)

-- | The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using a Lambda data
-- source. For all other data sources, VTL request and response mapping
-- templates are required.
updateResolver_requestMappingTemplate :: Lens.Lens' UpdateResolver (Core.Maybe Core.Text)
updateResolver_requestMappingTemplate = Lens.lens (\UpdateResolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@UpdateResolver' {} a -> s {requestMappingTemplate = a} :: UpdateResolver)

-- | The API ID.
updateResolver_apiId :: Lens.Lens' UpdateResolver Core.Text
updateResolver_apiId = Lens.lens (\UpdateResolver' {apiId} -> apiId) (\s@UpdateResolver' {} a -> s {apiId = a} :: UpdateResolver)

-- | The new type name.
updateResolver_typeName :: Lens.Lens' UpdateResolver Core.Text
updateResolver_typeName = Lens.lens (\UpdateResolver' {typeName} -> typeName) (\s@UpdateResolver' {} a -> s {typeName = a} :: UpdateResolver)

-- | The new field name.
updateResolver_fieldName :: Lens.Lens' UpdateResolver Core.Text
updateResolver_fieldName = Lens.lens (\UpdateResolver' {fieldName} -> fieldName) (\s@UpdateResolver' {} a -> s {fieldName = a} :: UpdateResolver)

instance Core.AWSRequest UpdateResolver where
  type
    AWSResponse UpdateResolver =
      UpdateResolverResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResolverResponse'
            Core.<$> (x Core..?> "resolver")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateResolver

instance Core.NFData UpdateResolver

instance Core.ToHeaders UpdateResolver where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateResolver where
  toJSON UpdateResolver' {..} =
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
              Core.<$> requestMappingTemplate
          ]
      )

instance Core.ToPath UpdateResolver where
  toPath UpdateResolver' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName,
        "/resolvers/",
        Core.toBS fieldName
      ]

instance Core.ToQuery UpdateResolver where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateResolverResponse' smart constructor.
data UpdateResolverResponse = UpdateResolverResponse'
  { -- | The updated @Resolver@ object.
    resolver :: Core.Maybe Resolver,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateResolverResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolver', 'updateResolverResponse_resolver' - The updated @Resolver@ object.
--
-- 'httpStatus', 'updateResolverResponse_httpStatus' - The response's http status code.
newUpdateResolverResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateResolverResponse
newUpdateResolverResponse pHttpStatus_ =
  UpdateResolverResponse'
    { resolver = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated @Resolver@ object.
updateResolverResponse_resolver :: Lens.Lens' UpdateResolverResponse (Core.Maybe Resolver)
updateResolverResponse_resolver = Lens.lens (\UpdateResolverResponse' {resolver} -> resolver) (\s@UpdateResolverResponse' {} a -> s {resolver = a} :: UpdateResolverResponse)

-- | The response's http status code.
updateResolverResponse_httpStatus :: Lens.Lens' UpdateResolverResponse Core.Int
updateResolverResponse_httpStatus = Lens.lens (\UpdateResolverResponse' {httpStatus} -> httpStatus) (\s@UpdateResolverResponse' {} a -> s {httpStatus = a} :: UpdateResolverResponse)

instance Core.NFData UpdateResolverResponse
