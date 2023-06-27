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
-- Module      : Amazonka.AppSync.CreateFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Function@ object.
--
-- A function is a reusable entity. You can use multiple functions to
-- compose the resolver logic.
module Amazonka.AppSync.CreateFunction
  ( -- * Creating a Request
    CreateFunction (..),
    newCreateFunction,

    -- * Request Lenses
    createFunction_code,
    createFunction_description,
    createFunction_functionVersion,
    createFunction_maxBatchSize,
    createFunction_requestMappingTemplate,
    createFunction_responseMappingTemplate,
    createFunction_runtime,
    createFunction_syncConfig,
    createFunction_apiId,
    createFunction_name,
    createFunction_dataSourceName,

    -- * Destructuring the Response
    CreateFunctionResponse (..),
    newCreateFunctionResponse,

    -- * Response Lenses
    createFunctionResponse_functionConfiguration,
    createFunctionResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | The @function@ code that contains the request and response functions.
    -- When code is used, the @runtime@ is required. The @runtime@ value must
    -- be @APPSYNC_JS@.
    code :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The @version@ of the request mapping template. Currently, the supported
    -- value is 2018-05-29. Note that when using VTL and mapping templates, the
    -- @functionVersion@ is required.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | The maximum batching size for a resolver.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    runtime :: Prelude.Maybe AppSyncRuntime,
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The GraphQL API ID.
    apiId :: Prelude.Text,
    -- | The @Function@ name. The function name does not have to be unique.
    name :: Prelude.Text,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'createFunction_code' - The @function@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
--
-- 'description', 'createFunction_description' - The @Function@ description.
--
-- 'functionVersion', 'createFunction_functionVersion' - The @version@ of the request mapping template. Currently, the supported
-- value is 2018-05-29. Note that when using VTL and mapping templates, the
-- @functionVersion@ is required.
--
-- 'maxBatchSize', 'createFunction_maxBatchSize' - The maximum batching size for a resolver.
--
-- 'requestMappingTemplate', 'createFunction_requestMappingTemplate' - The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
--
-- 'responseMappingTemplate', 'createFunction_responseMappingTemplate' - The @Function@ response mapping template.
--
-- 'runtime', 'createFunction_runtime' - Undocumented member.
--
-- 'syncConfig', 'createFunction_syncConfig' - Undocumented member.
--
-- 'apiId', 'createFunction_apiId' - The GraphQL API ID.
--
-- 'name', 'createFunction_name' - The @Function@ name. The function name does not have to be unique.
--
-- 'dataSourceName', 'createFunction_dataSourceName' - The @Function@ @DataSource@ name.
newCreateFunction ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'dataSourceName'
  Prelude.Text ->
  CreateFunction
newCreateFunction pApiId_ pName_ pDataSourceName_ =
  CreateFunction'
    { code = Prelude.Nothing,
      description = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      maxBatchSize = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing,
      responseMappingTemplate = Prelude.Nothing,
      runtime = Prelude.Nothing,
      syncConfig = Prelude.Nothing,
      apiId = pApiId_,
      name = pName_,
      dataSourceName = pDataSourceName_
    }

-- | The @function@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
createFunction_code :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_code = Lens.lens (\CreateFunction' {code} -> code) (\s@CreateFunction' {} a -> s {code = a} :: CreateFunction)

-- | The @Function@ description.
createFunction_description :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_description = Lens.lens (\CreateFunction' {description} -> description) (\s@CreateFunction' {} a -> s {description = a} :: CreateFunction)

-- | The @version@ of the request mapping template. Currently, the supported
-- value is 2018-05-29. Note that when using VTL and mapping templates, the
-- @functionVersion@ is required.
createFunction_functionVersion :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_functionVersion = Lens.lens (\CreateFunction' {functionVersion} -> functionVersion) (\s@CreateFunction' {} a -> s {functionVersion = a} :: CreateFunction)

-- | The maximum batching size for a resolver.
createFunction_maxBatchSize :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_maxBatchSize = Lens.lens (\CreateFunction' {maxBatchSize} -> maxBatchSize) (\s@CreateFunction' {} a -> s {maxBatchSize = a} :: CreateFunction)

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
createFunction_requestMappingTemplate :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_requestMappingTemplate = Lens.lens (\CreateFunction' {requestMappingTemplate} -> requestMappingTemplate) (\s@CreateFunction' {} a -> s {requestMappingTemplate = a} :: CreateFunction)

-- | The @Function@ response mapping template.
createFunction_responseMappingTemplate :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_responseMappingTemplate = Lens.lens (\CreateFunction' {responseMappingTemplate} -> responseMappingTemplate) (\s@CreateFunction' {} a -> s {responseMappingTemplate = a} :: CreateFunction)

-- | Undocumented member.
createFunction_runtime :: Lens.Lens' CreateFunction (Prelude.Maybe AppSyncRuntime)
createFunction_runtime = Lens.lens (\CreateFunction' {runtime} -> runtime) (\s@CreateFunction' {} a -> s {runtime = a} :: CreateFunction)

-- | Undocumented member.
createFunction_syncConfig :: Lens.Lens' CreateFunction (Prelude.Maybe SyncConfig)
createFunction_syncConfig = Lens.lens (\CreateFunction' {syncConfig} -> syncConfig) (\s@CreateFunction' {} a -> s {syncConfig = a} :: CreateFunction)

-- | The GraphQL API ID.
createFunction_apiId :: Lens.Lens' CreateFunction Prelude.Text
createFunction_apiId = Lens.lens (\CreateFunction' {apiId} -> apiId) (\s@CreateFunction' {} a -> s {apiId = a} :: CreateFunction)

-- | The @Function@ name. The function name does not have to be unique.
createFunction_name :: Lens.Lens' CreateFunction Prelude.Text
createFunction_name = Lens.lens (\CreateFunction' {name} -> name) (\s@CreateFunction' {} a -> s {name = a} :: CreateFunction)

-- | The @Function@ @DataSource@ name.
createFunction_dataSourceName :: Lens.Lens' CreateFunction Prelude.Text
createFunction_dataSourceName = Lens.lens (\CreateFunction' {dataSourceName} -> dataSourceName) (\s@CreateFunction' {} a -> s {dataSourceName = a} :: CreateFunction)

instance Core.AWSRequest CreateFunction where
  type
    AWSResponse CreateFunction =
      CreateFunctionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionResponse'
            Prelude.<$> (x Data..?> "functionConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFunction where
  hashWithSalt _salt CreateFunction' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` requestMappingTemplate
      `Prelude.hashWithSalt` responseMappingTemplate
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` syncConfig
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataSourceName

instance Prelude.NFData CreateFunction where
  rnf CreateFunction' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf maxBatchSize
      `Prelude.seq` Prelude.rnf requestMappingTemplate
      `Prelude.seq` Prelude.rnf responseMappingTemplate
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf syncConfig
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataSourceName

instance Data.ToHeaders CreateFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFunction where
  toJSON CreateFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("code" Data..=) Prelude.<$> code,
            ("description" Data..=) Prelude.<$> description,
            ("functionVersion" Data..=)
              Prelude.<$> functionVersion,
            ("maxBatchSize" Data..=) Prelude.<$> maxBatchSize,
            ("requestMappingTemplate" Data..=)
              Prelude.<$> requestMappingTemplate,
            ("responseMappingTemplate" Data..=)
              Prelude.<$> responseMappingTemplate,
            ("runtime" Data..=) Prelude.<$> runtime,
            ("syncConfig" Data..=) Prelude.<$> syncConfig,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("dataSourceName" Data..= dataSourceName)
          ]
      )

instance Data.ToPath CreateFunction where
  toPath CreateFunction' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/functions"]

instance Data.ToQuery CreateFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFunctionResponse' smart constructor.
data CreateFunctionResponse = CreateFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Prelude.Maybe FunctionConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionConfiguration', 'createFunctionResponse_functionConfiguration' - The @Function@ object.
--
-- 'httpStatus', 'createFunctionResponse_httpStatus' - The response's http status code.
newCreateFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFunctionResponse
newCreateFunctionResponse pHttpStatus_ =
  CreateFunctionResponse'
    { functionConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Function@ object.
createFunctionResponse_functionConfiguration :: Lens.Lens' CreateFunctionResponse (Prelude.Maybe FunctionConfiguration)
createFunctionResponse_functionConfiguration = Lens.lens (\CreateFunctionResponse' {functionConfiguration} -> functionConfiguration) (\s@CreateFunctionResponse' {} a -> s {functionConfiguration = a} :: CreateFunctionResponse)

-- | The response's http status code.
createFunctionResponse_httpStatus :: Lens.Lens' CreateFunctionResponse Prelude.Int
createFunctionResponse_httpStatus = Lens.lens (\CreateFunctionResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionResponse' {} a -> s {httpStatus = a} :: CreateFunctionResponse)

instance Prelude.NFData CreateFunctionResponse where
  rnf CreateFunctionResponse' {..} =
    Prelude.rnf functionConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
