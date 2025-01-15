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
-- Module      : Amazonka.AppSync.CreateResolver
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Resolver@ object.
--
-- A resolver converts incoming requests into a format that a data source
-- can understand, and converts the data source\'s responses into GraphQL.
module Amazonka.AppSync.CreateResolver
  ( -- * Creating a Request
    CreateResolver (..),
    newCreateResolver,

    -- * Request Lenses
    createResolver_cachingConfig,
    createResolver_code,
    createResolver_dataSourceName,
    createResolver_kind,
    createResolver_maxBatchSize,
    createResolver_pipelineConfig,
    createResolver_requestMappingTemplate,
    createResolver_responseMappingTemplate,
    createResolver_runtime,
    createResolver_syncConfig,
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResolver' smart constructor.
data CreateResolver = CreateResolver'
  { -- | The caching configuration for the resolver.
    cachingConfig :: Prelude.Maybe CachingConfig,
    -- | The @resolver@ code that contains the request and response functions.
    -- When code is used, the @runtime@ is required. The @runtime@ value must
    -- be @APPSYNC_JS@.
    code :: Prelude.Maybe Prelude.Text,
    -- | The name of the data source for which the resolver is being created.
    dataSourceName :: Prelude.Maybe Prelude.Text,
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
    -- | The maximum batching size for a resolver.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | The @PipelineConfig@.
    pipelineConfig :: Prelude.Maybe PipelineConfig,
    -- | The mapping template to use for requests.
    --
    -- A resolver uses a request mapping template to convert a GraphQL
    -- expression into a format that a data source can understand. Mapping
    -- templates are written in Apache Velocity Template Language (VTL).
    --
    -- VTL request mapping templates are optional when using an Lambda data
    -- source. For all other data sources, VTL request and response mapping
    -- templates are required.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The mapping template to use for responses from the data source.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    runtime :: Prelude.Maybe AppSyncRuntime,
    -- | The @SyncConfig@ for a resolver attached to a versioned data source.
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The ID for the GraphQL API for which the resolver is being created.
    apiId :: Prelude.Text,
    -- | The name of the @Type@.
    typeName :: Prelude.Text,
    -- | The name of the field to attach the resolver to.
    fieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachingConfig', 'createResolver_cachingConfig' - The caching configuration for the resolver.
--
-- 'code', 'createResolver_code' - The @resolver@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
--
-- 'dataSourceName', 'createResolver_dataSourceName' - The name of the data source for which the resolver is being created.
--
-- 'kind', 'createResolver_kind' - The resolver type.
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
-- 'maxBatchSize', 'createResolver_maxBatchSize' - The maximum batching size for a resolver.
--
-- 'pipelineConfig', 'createResolver_pipelineConfig' - The @PipelineConfig@.
--
-- 'requestMappingTemplate', 'createResolver_requestMappingTemplate' - The mapping template to use for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using an Lambda data
-- source. For all other data sources, VTL request and response mapping
-- templates are required.
--
-- 'responseMappingTemplate', 'createResolver_responseMappingTemplate' - The mapping template to use for responses from the data source.
--
-- 'runtime', 'createResolver_runtime' - Undocumented member.
--
-- 'syncConfig', 'createResolver_syncConfig' - The @SyncConfig@ for a resolver attached to a versioned data source.
--
-- 'apiId', 'createResolver_apiId' - The ID for the GraphQL API for which the resolver is being created.
--
-- 'typeName', 'createResolver_typeName' - The name of the @Type@.
--
-- 'fieldName', 'createResolver_fieldName' - The name of the field to attach the resolver to.
newCreateResolver ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  -- | 'fieldName'
  Prelude.Text ->
  CreateResolver
newCreateResolver pApiId_ pTypeName_ pFieldName_ =
  CreateResolver'
    { cachingConfig = Prelude.Nothing,
      code = Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      kind = Prelude.Nothing,
      maxBatchSize = Prelude.Nothing,
      pipelineConfig = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing,
      responseMappingTemplate = Prelude.Nothing,
      runtime = Prelude.Nothing,
      syncConfig = Prelude.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The caching configuration for the resolver.
createResolver_cachingConfig :: Lens.Lens' CreateResolver (Prelude.Maybe CachingConfig)
createResolver_cachingConfig = Lens.lens (\CreateResolver' {cachingConfig} -> cachingConfig) (\s@CreateResolver' {} a -> s {cachingConfig = a} :: CreateResolver)

-- | The @resolver@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
createResolver_code :: Lens.Lens' CreateResolver (Prelude.Maybe Prelude.Text)
createResolver_code = Lens.lens (\CreateResolver' {code} -> code) (\s@CreateResolver' {} a -> s {code = a} :: CreateResolver)

-- | The name of the data source for which the resolver is being created.
createResolver_dataSourceName :: Lens.Lens' CreateResolver (Prelude.Maybe Prelude.Text)
createResolver_dataSourceName = Lens.lens (\CreateResolver' {dataSourceName} -> dataSourceName) (\s@CreateResolver' {} a -> s {dataSourceName = a} :: CreateResolver)

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
createResolver_kind :: Lens.Lens' CreateResolver (Prelude.Maybe ResolverKind)
createResolver_kind = Lens.lens (\CreateResolver' {kind} -> kind) (\s@CreateResolver' {} a -> s {kind = a} :: CreateResolver)

-- | The maximum batching size for a resolver.
createResolver_maxBatchSize :: Lens.Lens' CreateResolver (Prelude.Maybe Prelude.Natural)
createResolver_maxBatchSize = Lens.lens (\CreateResolver' {maxBatchSize} -> maxBatchSize) (\s@CreateResolver' {} a -> s {maxBatchSize = a} :: CreateResolver)

-- | The @PipelineConfig@.
createResolver_pipelineConfig :: Lens.Lens' CreateResolver (Prelude.Maybe PipelineConfig)
createResolver_pipelineConfig = Lens.lens (\CreateResolver' {pipelineConfig} -> pipelineConfig) (\s@CreateResolver' {} a -> s {pipelineConfig = a} :: CreateResolver)

-- | The mapping template to use for requests.
--
-- A resolver uses a request mapping template to convert a GraphQL
-- expression into a format that a data source can understand. Mapping
-- templates are written in Apache Velocity Template Language (VTL).
--
-- VTL request mapping templates are optional when using an Lambda data
-- source. For all other data sources, VTL request and response mapping
-- templates are required.
createResolver_requestMappingTemplate :: Lens.Lens' CreateResolver (Prelude.Maybe Prelude.Text)
createResolver_requestMappingTemplate = Lens.lens (\CreateResolver' {requestMappingTemplate} -> requestMappingTemplate) (\s@CreateResolver' {} a -> s {requestMappingTemplate = a} :: CreateResolver)

-- | The mapping template to use for responses from the data source.
createResolver_responseMappingTemplate :: Lens.Lens' CreateResolver (Prelude.Maybe Prelude.Text)
createResolver_responseMappingTemplate = Lens.lens (\CreateResolver' {responseMappingTemplate} -> responseMappingTemplate) (\s@CreateResolver' {} a -> s {responseMappingTemplate = a} :: CreateResolver)

-- | Undocumented member.
createResolver_runtime :: Lens.Lens' CreateResolver (Prelude.Maybe AppSyncRuntime)
createResolver_runtime = Lens.lens (\CreateResolver' {runtime} -> runtime) (\s@CreateResolver' {} a -> s {runtime = a} :: CreateResolver)

-- | The @SyncConfig@ for a resolver attached to a versioned data source.
createResolver_syncConfig :: Lens.Lens' CreateResolver (Prelude.Maybe SyncConfig)
createResolver_syncConfig = Lens.lens (\CreateResolver' {syncConfig} -> syncConfig) (\s@CreateResolver' {} a -> s {syncConfig = a} :: CreateResolver)

-- | The ID for the GraphQL API for which the resolver is being created.
createResolver_apiId :: Lens.Lens' CreateResolver Prelude.Text
createResolver_apiId = Lens.lens (\CreateResolver' {apiId} -> apiId) (\s@CreateResolver' {} a -> s {apiId = a} :: CreateResolver)

-- | The name of the @Type@.
createResolver_typeName :: Lens.Lens' CreateResolver Prelude.Text
createResolver_typeName = Lens.lens (\CreateResolver' {typeName} -> typeName) (\s@CreateResolver' {} a -> s {typeName = a} :: CreateResolver)

-- | The name of the field to attach the resolver to.
createResolver_fieldName :: Lens.Lens' CreateResolver Prelude.Text
createResolver_fieldName = Lens.lens (\CreateResolver' {fieldName} -> fieldName) (\s@CreateResolver' {} a -> s {fieldName = a} :: CreateResolver)

instance Core.AWSRequest CreateResolver where
  type
    AWSResponse CreateResolver =
      CreateResolverResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResolverResponse'
            Prelude.<$> (x Data..?> "resolver")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResolver where
  hashWithSalt _salt CreateResolver' {..} =
    _salt
      `Prelude.hashWithSalt` cachingConfig
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` dataSourceName
      `Prelude.hashWithSalt` kind
      `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` pipelineConfig
      `Prelude.hashWithSalt` requestMappingTemplate
      `Prelude.hashWithSalt` responseMappingTemplate
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` syncConfig
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` fieldName

instance Prelude.NFData CreateResolver where
  rnf CreateResolver' {..} =
    Prelude.rnf cachingConfig `Prelude.seq`
      Prelude.rnf code `Prelude.seq`
        Prelude.rnf dataSourceName `Prelude.seq`
          Prelude.rnf kind `Prelude.seq`
            Prelude.rnf maxBatchSize `Prelude.seq`
              Prelude.rnf pipelineConfig `Prelude.seq`
                Prelude.rnf requestMappingTemplate `Prelude.seq`
                  Prelude.rnf responseMappingTemplate `Prelude.seq`
                    Prelude.rnf runtime `Prelude.seq`
                      Prelude.rnf syncConfig `Prelude.seq`
                        Prelude.rnf apiId `Prelude.seq`
                          Prelude.rnf typeName `Prelude.seq`
                            Prelude.rnf fieldName

instance Data.ToHeaders CreateResolver where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResolver where
  toJSON CreateResolver' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cachingConfig" Data..=) Prelude.<$> cachingConfig,
            ("code" Data..=) Prelude.<$> code,
            ("dataSourceName" Data..=)
              Prelude.<$> dataSourceName,
            ("kind" Data..=) Prelude.<$> kind,
            ("maxBatchSize" Data..=) Prelude.<$> maxBatchSize,
            ("pipelineConfig" Data..=)
              Prelude.<$> pipelineConfig,
            ("requestMappingTemplate" Data..=)
              Prelude.<$> requestMappingTemplate,
            ("responseMappingTemplate" Data..=)
              Prelude.<$> responseMappingTemplate,
            ("runtime" Data..=) Prelude.<$> runtime,
            ("syncConfig" Data..=) Prelude.<$> syncConfig,
            Prelude.Just ("fieldName" Data..= fieldName)
          ]
      )

instance Data.ToPath CreateResolver where
  toPath CreateResolver' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/types/",
        Data.toBS typeName,
        "/resolvers"
      ]

instance Data.ToQuery CreateResolver where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResolverResponse' smart constructor.
data CreateResolverResponse = CreateResolverResponse'
  { -- | The @Resolver@ object.
    resolver :: Prelude.Maybe Resolver,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateResolverResponse
newCreateResolverResponse pHttpStatus_ =
  CreateResolverResponse'
    { resolver = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Resolver@ object.
createResolverResponse_resolver :: Lens.Lens' CreateResolverResponse (Prelude.Maybe Resolver)
createResolverResponse_resolver = Lens.lens (\CreateResolverResponse' {resolver} -> resolver) (\s@CreateResolverResponse' {} a -> s {resolver = a} :: CreateResolverResponse)

-- | The response's http status code.
createResolverResponse_httpStatus :: Lens.Lens' CreateResolverResponse Prelude.Int
createResolverResponse_httpStatus = Lens.lens (\CreateResolverResponse' {httpStatus} -> httpStatus) (\s@CreateResolverResponse' {} a -> s {httpStatus = a} :: CreateResolverResponse)

instance Prelude.NFData CreateResolverResponse where
  rnf CreateResolverResponse' {..} =
    Prelude.rnf resolver `Prelude.seq`
      Prelude.rnf httpStatus
