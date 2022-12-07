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
-- Module      : Amazonka.AppSync.UpdateFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Function@ object.
module Amazonka.AppSync.UpdateFunction
  ( -- * Creating a Request
    UpdateFunction (..),
    newUpdateFunction,

    -- * Request Lenses
    updateFunction_code,
    updateFunction_maxBatchSize,
    updateFunction_functionVersion,
    updateFunction_runtime,
    updateFunction_description,
    updateFunction_responseMappingTemplate,
    updateFunction_syncConfig,
    updateFunction_requestMappingTemplate,
    updateFunction_apiId,
    updateFunction_name,
    updateFunction_functionId,
    updateFunction_dataSourceName,

    -- * Destructuring the Response
    UpdateFunctionResponse (..),
    newUpdateFunctionResponse,

    -- * Response Lenses
    updateFunctionResponse_functionConfiguration,
    updateFunctionResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { -- | The @function@ code that contains the request and response functions.
    -- When code is used, the @runtime@ is required. The @runtime@ value must
    -- be @APPSYNC_JS@.
    code :: Prelude.Maybe Prelude.Text,
    -- | The maximum batching size for a resolver.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | The @version@ of the request mapping template. Currently, the supported
    -- value is 2018-05-29. Note that when using VTL and mapping templates, the
    -- @functionVersion@ is required.
    functionVersion :: Prelude.Maybe Prelude.Text,
    runtime :: Prelude.Maybe AppSyncRuntime,
    -- | The @Function@ description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ request mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The GraphQL API ID.
    apiId :: Prelude.Text,
    -- | The @Function@ name.
    name :: Prelude.Text,
    -- | The function ID.
    functionId :: Prelude.Text,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'updateFunction_code' - The @function@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
--
-- 'maxBatchSize', 'updateFunction_maxBatchSize' - The maximum batching size for a resolver.
--
-- 'functionVersion', 'updateFunction_functionVersion' - The @version@ of the request mapping template. Currently, the supported
-- value is 2018-05-29. Note that when using VTL and mapping templates, the
-- @functionVersion@ is required.
--
-- 'runtime', 'updateFunction_runtime' - Undocumented member.
--
-- 'description', 'updateFunction_description' - The @Function@ description.
--
-- 'responseMappingTemplate', 'updateFunction_responseMappingTemplate' - The @Function@ request mapping template.
--
-- 'syncConfig', 'updateFunction_syncConfig' - Undocumented member.
--
-- 'requestMappingTemplate', 'updateFunction_requestMappingTemplate' - The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
--
-- 'apiId', 'updateFunction_apiId' - The GraphQL API ID.
--
-- 'name', 'updateFunction_name' - The @Function@ name.
--
-- 'functionId', 'updateFunction_functionId' - The function ID.
--
-- 'dataSourceName', 'updateFunction_dataSourceName' - The @Function@ @DataSource@ name.
newUpdateFunction ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'functionId'
  Prelude.Text ->
  -- | 'dataSourceName'
  Prelude.Text ->
  UpdateFunction
newUpdateFunction
  pApiId_
  pName_
  pFunctionId_
  pDataSourceName_ =
    UpdateFunction'
      { code = Prelude.Nothing,
        maxBatchSize = Prelude.Nothing,
        functionVersion = Prelude.Nothing,
        runtime = Prelude.Nothing,
        description = Prelude.Nothing,
        responseMappingTemplate = Prelude.Nothing,
        syncConfig = Prelude.Nothing,
        requestMappingTemplate = Prelude.Nothing,
        apiId = pApiId_,
        name = pName_,
        functionId = pFunctionId_,
        dataSourceName = pDataSourceName_
      }

-- | The @function@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
updateFunction_code :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_code = Lens.lens (\UpdateFunction' {code} -> code) (\s@UpdateFunction' {} a -> s {code = a} :: UpdateFunction)

-- | The maximum batching size for a resolver.
updateFunction_maxBatchSize :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Natural)
updateFunction_maxBatchSize = Lens.lens (\UpdateFunction' {maxBatchSize} -> maxBatchSize) (\s@UpdateFunction' {} a -> s {maxBatchSize = a} :: UpdateFunction)

-- | The @version@ of the request mapping template. Currently, the supported
-- value is 2018-05-29. Note that when using VTL and mapping templates, the
-- @functionVersion@ is required.
updateFunction_functionVersion :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_functionVersion = Lens.lens (\UpdateFunction' {functionVersion} -> functionVersion) (\s@UpdateFunction' {} a -> s {functionVersion = a} :: UpdateFunction)

-- | Undocumented member.
updateFunction_runtime :: Lens.Lens' UpdateFunction (Prelude.Maybe AppSyncRuntime)
updateFunction_runtime = Lens.lens (\UpdateFunction' {runtime} -> runtime) (\s@UpdateFunction' {} a -> s {runtime = a} :: UpdateFunction)

-- | The @Function@ description.
updateFunction_description :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_description = Lens.lens (\UpdateFunction' {description} -> description) (\s@UpdateFunction' {} a -> s {description = a} :: UpdateFunction)

-- | The @Function@ request mapping template.
updateFunction_responseMappingTemplate :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_responseMappingTemplate = Lens.lens (\UpdateFunction' {responseMappingTemplate} -> responseMappingTemplate) (\s@UpdateFunction' {} a -> s {responseMappingTemplate = a} :: UpdateFunction)

-- | Undocumented member.
updateFunction_syncConfig :: Lens.Lens' UpdateFunction (Prelude.Maybe SyncConfig)
updateFunction_syncConfig = Lens.lens (\UpdateFunction' {syncConfig} -> syncConfig) (\s@UpdateFunction' {} a -> s {syncConfig = a} :: UpdateFunction)

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
updateFunction_requestMappingTemplate :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_requestMappingTemplate = Lens.lens (\UpdateFunction' {requestMappingTemplate} -> requestMappingTemplate) (\s@UpdateFunction' {} a -> s {requestMappingTemplate = a} :: UpdateFunction)

-- | The GraphQL API ID.
updateFunction_apiId :: Lens.Lens' UpdateFunction Prelude.Text
updateFunction_apiId = Lens.lens (\UpdateFunction' {apiId} -> apiId) (\s@UpdateFunction' {} a -> s {apiId = a} :: UpdateFunction)

-- | The @Function@ name.
updateFunction_name :: Lens.Lens' UpdateFunction Prelude.Text
updateFunction_name = Lens.lens (\UpdateFunction' {name} -> name) (\s@UpdateFunction' {} a -> s {name = a} :: UpdateFunction)

-- | The function ID.
updateFunction_functionId :: Lens.Lens' UpdateFunction Prelude.Text
updateFunction_functionId = Lens.lens (\UpdateFunction' {functionId} -> functionId) (\s@UpdateFunction' {} a -> s {functionId = a} :: UpdateFunction)

-- | The @Function@ @DataSource@ name.
updateFunction_dataSourceName :: Lens.Lens' UpdateFunction Prelude.Text
updateFunction_dataSourceName = Lens.lens (\UpdateFunction' {dataSourceName} -> dataSourceName) (\s@UpdateFunction' {} a -> s {dataSourceName = a} :: UpdateFunction)

instance Core.AWSRequest UpdateFunction where
  type
    AWSResponse UpdateFunction =
      UpdateFunctionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFunctionResponse'
            Prelude.<$> (x Data..?> "functionConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFunction where
  hashWithSalt _salt UpdateFunction' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` responseMappingTemplate
      `Prelude.hashWithSalt` syncConfig
      `Prelude.hashWithSalt` requestMappingTemplate
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` functionId
      `Prelude.hashWithSalt` dataSourceName

instance Prelude.NFData UpdateFunction where
  rnf UpdateFunction' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf maxBatchSize
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf responseMappingTemplate
      `Prelude.seq` Prelude.rnf syncConfig
      `Prelude.seq` Prelude.rnf requestMappingTemplate
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf functionId
      `Prelude.seq` Prelude.rnf dataSourceName

instance Data.ToHeaders UpdateFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFunction where
  toJSON UpdateFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("code" Data..=) Prelude.<$> code,
            ("maxBatchSize" Data..=) Prelude.<$> maxBatchSize,
            ("functionVersion" Data..=)
              Prelude.<$> functionVersion,
            ("runtime" Data..=) Prelude.<$> runtime,
            ("description" Data..=) Prelude.<$> description,
            ("responseMappingTemplate" Data..=)
              Prelude.<$> responseMappingTemplate,
            ("syncConfig" Data..=) Prelude.<$> syncConfig,
            ("requestMappingTemplate" Data..=)
              Prelude.<$> requestMappingTemplate,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("dataSourceName" Data..= dataSourceName)
          ]
      )

instance Data.ToPath UpdateFunction where
  toPath UpdateFunction' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/functions/",
        Data.toBS functionId
      ]

instance Data.ToQuery UpdateFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFunctionResponse' smart constructor.
data UpdateFunctionResponse = UpdateFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Prelude.Maybe FunctionConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionConfiguration', 'updateFunctionResponse_functionConfiguration' - The @Function@ object.
--
-- 'httpStatus', 'updateFunctionResponse_httpStatus' - The response's http status code.
newUpdateFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFunctionResponse
newUpdateFunctionResponse pHttpStatus_ =
  UpdateFunctionResponse'
    { functionConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Function@ object.
updateFunctionResponse_functionConfiguration :: Lens.Lens' UpdateFunctionResponse (Prelude.Maybe FunctionConfiguration)
updateFunctionResponse_functionConfiguration = Lens.lens (\UpdateFunctionResponse' {functionConfiguration} -> functionConfiguration) (\s@UpdateFunctionResponse' {} a -> s {functionConfiguration = a} :: UpdateFunctionResponse)

-- | The response's http status code.
updateFunctionResponse_httpStatus :: Lens.Lens' UpdateFunctionResponse Prelude.Int
updateFunctionResponse_httpStatus = Lens.lens (\UpdateFunctionResponse' {httpStatus} -> httpStatus) (\s@UpdateFunctionResponse' {} a -> s {httpStatus = a} :: UpdateFunctionResponse)

instance Prelude.NFData UpdateFunctionResponse where
  rnf UpdateFunctionResponse' {..} =
    Prelude.rnf functionConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
