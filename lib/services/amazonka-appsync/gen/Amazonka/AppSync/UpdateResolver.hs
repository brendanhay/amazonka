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
-- Module      : Amazonka.AppSync.UpdateResolver
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Resolver@ object.
module Amazonka.AppSync.UpdateResolver
  ( -- * Creating a Request
    UpdateResolver (..),
    newUpdateResolver,

    -- * Request Lenses
    updateResolver_code,
    updateResolver_maxBatchSize,
    updateResolver_cachingConfig,
    updateResolver_pipelineConfig,
    updateResolver_kind,
    updateResolver_runtime,
    updateResolver_dataSourceName,
    updateResolver_responseMappingTemplate,
    updateResolver_syncConfig,
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResolver' smart constructor.
data UpdateResolver = UpdateResolver'
  { -- | The @resolver@ code that contains the request and response functions.
    -- When code is used, the @runtime@ is required. The @runtime@ value must
    -- be @APPSYNC_JS@.
    code :: Prelude.Maybe Prelude.Text,
    -- | The maximum batching size for a resolver.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | The caching configuration for the resolver.
    cachingConfig :: Prelude.Maybe CachingConfig,
    -- | The @PipelineConfig@.
    pipelineConfig :: Prelude.Maybe PipelineConfig,
    -- | The resolver type.
    --
    -- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
    --     resolver type. You can use a UNIT resolver to run a GraphQL query
    --     against a single data source.
    --
    -- -   __PIPELINE__: A PIPELINE resolver type. You can use a PIPELINE
    --     resolver to invoke a series of @Function@ objects in a serial
    --     manner. You can use a pipeline resolver to run a GraphQL query
    --     against multiple data sources.
    kind :: Prelude.Maybe ResolverKind,
    runtime :: Prelude.Maybe AppSyncRuntime,
    -- | The new data source name.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | The new response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The @SyncConfig@ for a resolver attached to a versioned data source.
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The new request mapping template.
    --
    -- A resolver uses a request mapping template to convert a GraphQL
    -- expression into a format that a data source can understand. Mapping
    -- templates are written in Apache Velocity Template Language (VTL).
    --
    -- VTL request mapping templates are optional when using an Lambda data
    -- source. For all other data sources, VTL request and response mapping
    -- templates are required.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Text,
    -- | The new type name.
    typeName :: Prelude.Text,
    -- | The new field name.
    fieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'updateResolver_code' - The @resolver@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
--
-- 'maxBatchSize', 'updateResolver_maxBatchSize' - The maximum batching size for a resolver.
--
-- 'cachingConfig', 'updateResolver_cachingConfig' - The caching configuration for the resolver.
--
-- 'pipelineConfig', 'updateResolver_pipelineConfig' - The @PipelineConfig@.
--
-- 'kind', 'updateResolver_kind' - The resolver type.
--
-- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
--     resolver type. You can use a UNIT resolver to run a GraphQL query
--     against a single data source.
--
-- -   __PIPELINE__: A PIPELINE resolver type. You can use a PIPELINE
--     resolver to invoke a series of @Function@ objects in a serial
--     manner. You can use a pipeline resolver to run a GraphQL query
--     against multiple data sources.
--
-- 'runtime', 'updateResolver_runtime' - Undocumented member.
--
-- 'dataSourceName', 'updateResolver_dataSourceName' - The new data source name.
--
-- 'responseMappingTemplate', 'updateResolver_responseMappingTemplate' - The new response mapping template.
--
-- 'syncConfig', 'updateResolver_syncConfig' - The @SyncConfig@ for a resolver attached to a versioned data source.
--
-- 'requestMappingTemplate', 'updateResolver_requestMappingTemplate' - The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using an Lambda data
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
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  -- | 'fieldName'
  Prelude.Text ->
  UpdateResolver
newUpdateResolver pApiId_ pTypeName_ pFieldName_ =
  UpdateResolver'
    { code = Prelude.Nothing,
      maxBatchSize = Prelude.Nothing,
      cachingConfig = Prelude.Nothing,
      pipelineConfig = Prelude.Nothing,
      kind = Prelude.Nothing,
      runtime = Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      responseMappingTemplate = Prelude.Nothing,
      syncConfig = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The @resolver@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
updateResolver_code :: Lens.Lens' UpdateResolver (Prelude.Maybe Prelude.Text)
updateResolver_code = Lens.lens (\UpdateResolver' {code} -> code) (\s@UpdateResolver' {} a -> s {code = a} :: UpdateResolver)

-- | The maximum batching size for a resolver.
updateResolver_maxBatchSize :: Lens.Lens' UpdateResolver (Prelude.Maybe Prelude.Natural)
updateResolver_maxBatchSize = Lens.lens (\UpdateResolver' {maxBatchSize} -> maxBatchSize) (\s@UpdateResolver' {} a -> s {maxBatchSize = a} :: UpdateResolver)

-- | The caching configuration for the resolver.
updateResolver_cachingConfig :: Lens.Lens' UpdateResolver (Prelude.Maybe CachingConfig)
updateResolver_cachingConfig = Lens.lens (\UpdateResolver' {cachingConfig} -> cachingConfig) (\s@UpdateResolver' {} a -> s {cachingConfig = a} :: UpdateResolver)

-- | The @PipelineConfig@.
updateResolver_pipelineConfig :: Lens.Lens' UpdateResolver (Prelude.Maybe PipelineConfig)
updateResolver_pipelineConfig = Lens.lens (\UpdateResolver' {pipelineConfig} -> pipelineConfig) (\s@UpdateResolver' {} a -> s {pipelineConfig = a} :: UpdateResolver)

-- | The resolver type.
--
-- -   __UNIT__: A UNIT resolver type. A UNIT resolver is the default
--     resolver type. You can use a UNIT resolver to run a GraphQL query
--     against a single data source.
--
-- -   __PIPELINE__: A PIPELINE resolver type. You can use a PIPELINE
--     resolver to invoke a series of @Function@ objects in a serial
--     manner. You can use a pipeline resolver to run a GraphQL query
--     against multiple data sources.
updateResolver_kind :: Lens.Lens' UpdateResolver (Prelude.Maybe ResolverKind)
updateResolver_kind = Lens.lens (\UpdateResolver' {kind} -> kind) (\s@UpdateResolver' {} a -> s {kind = a} :: UpdateResolver)

-- | Undocumented member.
updateResolver_runtime :: Lens.Lens' UpdateResolver (Prelude.Maybe AppSyncRuntime)
updateResolver_runtime = Lens.lens (\UpdateResolver' {runtime} -> runtime) (\s@UpdateResolver' {} a -> s {runtime = a} :: UpdateResolver)

-- | The new data source name.
updateResolver_dataSourceName :: Lens.Lens' UpdateResolver (Prelude.Maybe Prelude.Text)
updateResolver_dataSourceName = Lens.lens (\UpdateResolver' {dataSourceName} -> dataSourceName) (\s@UpdateResolver' {} a -> s {dataSourceName = a} :: UpdateResolver)

-- | The new response mapping template.
updateResolver_responseMappingTemplate :: Lens.Lens' UpdateResolver (Prelude.Maybe Prelude.Text)
updateResolver_responseMappingTemplate = Lens.lens (\UpdateResolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@UpdateResolver' {} a -> s {responseMappingTemplate = a} :: UpdateResolver)

-- | The @SyncConfig@ for a resolver attached to a versioned data source.
updateResolver_syncConfig :: Lens.Lens' UpdateResolver (Prelude.Maybe SyncConfig)
updateResolver_syncConfig = Lens.lens (\UpdateResolver' {syncConfig} -> syncConfig) (\s@UpdateResolver' {} a -> s {syncConfig = a} :: UpdateResolver)

-- | The new request mapping template.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using an Lambda data
-- source. For all other data sources, VTL request and response mapping
-- templates are required.
updateResolver_requestMappingTemplate :: Lens.Lens' UpdateResolver (Prelude.Maybe Prelude.Text)
updateResolver_requestMappingTemplate = Lens.lens (\UpdateResolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@UpdateResolver' {} a -> s {requestMappingTemplate = a} :: UpdateResolver)

-- | The API ID.
updateResolver_apiId :: Lens.Lens' UpdateResolver Prelude.Text
updateResolver_apiId = Lens.lens (\UpdateResolver' {apiId} -> apiId) (\s@UpdateResolver' {} a -> s {apiId = a} :: UpdateResolver)

-- | The new type name.
updateResolver_typeName :: Lens.Lens' UpdateResolver Prelude.Text
updateResolver_typeName = Lens.lens (\UpdateResolver' {typeName} -> typeName) (\s@UpdateResolver' {} a -> s {typeName = a} :: UpdateResolver)

-- | The new field name.
updateResolver_fieldName :: Lens.Lens' UpdateResolver Prelude.Text
updateResolver_fieldName = Lens.lens (\UpdateResolver' {fieldName} -> fieldName) (\s@UpdateResolver' {} a -> s {fieldName = a} :: UpdateResolver)

instance Core.AWSRequest UpdateResolver where
  type
    AWSResponse UpdateResolver =
      UpdateResolverResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResolverResponse'
            Prelude.<$> (x Data..?> "resolver")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResolver where
  hashWithSalt _salt UpdateResolver' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` cachingConfig
      `Prelude.hashWithSalt` pipelineConfig
      `Prelude.hashWithSalt` kind
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` dataSourceName
      `Prelude.hashWithSalt` responseMappingTemplate
      `Prelude.hashWithSalt` syncConfig
      `Prelude.hashWithSalt` requestMappingTemplate
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` fieldName

instance Prelude.NFData UpdateResolver where
  rnf UpdateResolver' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf maxBatchSize
      `Prelude.seq` Prelude.rnf cachingConfig
      `Prelude.seq` Prelude.rnf pipelineConfig
      `Prelude.seq` Prelude.rnf kind
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf dataSourceName
      `Prelude.seq` Prelude.rnf responseMappingTemplate
      `Prelude.seq` Prelude.rnf syncConfig
      `Prelude.seq` Prelude.rnf requestMappingTemplate
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf fieldName

instance Data.ToHeaders UpdateResolver where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResolver where
  toJSON UpdateResolver' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("code" Data..=) Prelude.<$> code,
            ("maxBatchSize" Data..=) Prelude.<$> maxBatchSize,
            ("cachingConfig" Data..=) Prelude.<$> cachingConfig,
            ("pipelineConfig" Data..=)
              Prelude.<$> pipelineConfig,
            ("kind" Data..=) Prelude.<$> kind,
            ("runtime" Data..=) Prelude.<$> runtime,
            ("dataSourceName" Data..=)
              Prelude.<$> dataSourceName,
            ("responseMappingTemplate" Data..=)
              Prelude.<$> responseMappingTemplate,
            ("syncConfig" Data..=) Prelude.<$> syncConfig,
            ("requestMappingTemplate" Data..=)
              Prelude.<$> requestMappingTemplate
          ]
      )

instance Data.ToPath UpdateResolver where
  toPath UpdateResolver' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/types/",
        Data.toBS typeName,
        "/resolvers/",
        Data.toBS fieldName
      ]

instance Data.ToQuery UpdateResolver where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResolverResponse' smart constructor.
data UpdateResolverResponse = UpdateResolverResponse'
  { -- | The updated @Resolver@ object.
    resolver :: Prelude.Maybe Resolver,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateResolverResponse
newUpdateResolverResponse pHttpStatus_ =
  UpdateResolverResponse'
    { resolver = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated @Resolver@ object.
updateResolverResponse_resolver :: Lens.Lens' UpdateResolverResponse (Prelude.Maybe Resolver)
updateResolverResponse_resolver = Lens.lens (\UpdateResolverResponse' {resolver} -> resolver) (\s@UpdateResolverResponse' {} a -> s {resolver = a} :: UpdateResolverResponse)

-- | The response's http status code.
updateResolverResponse_httpStatus :: Lens.Lens' UpdateResolverResponse Prelude.Int
updateResolverResponse_httpStatus = Lens.lens (\UpdateResolverResponse' {httpStatus} -> httpStatus) (\s@UpdateResolverResponse' {} a -> s {httpStatus = a} :: UpdateResolverResponse)

instance Prelude.NFData UpdateResolverResponse where
  rnf UpdateResolverResponse' {..} =
    Prelude.rnf resolver
      `Prelude.seq` Prelude.rnf httpStatus
