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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    updateFunction_requestMappingTemplate,
    updateFunction_responseMappingTemplate,
    updateFunction_syncConfig,
    updateFunction_description,
    updateFunction_apiId,
    updateFunction_name,
    updateFunction_functionId,
    updateFunction_dataSourceName,
    updateFunction_functionVersion,

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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ request mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The @Function@ description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The GraphQL API ID.
    apiId :: Prelude.Text,
    -- | The @Function@ name.
    name :: Prelude.Text,
    -- | The function ID.
    functionId :: Prelude.Text,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Prelude.Text,
    -- | The @version@ of the request mapping template. Currently the supported
    -- value is 2018-05-29.
    functionVersion :: Prelude.Text
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
-- 'requestMappingTemplate', 'updateFunction_requestMappingTemplate' - The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
--
-- 'responseMappingTemplate', 'updateFunction_responseMappingTemplate' - The @Function@ request mapping template.
--
-- 'syncConfig', 'updateFunction_syncConfig' - Undocumented member.
--
-- 'description', 'updateFunction_description' - The @Function@ description.
--
-- 'apiId', 'updateFunction_apiId' - The GraphQL API ID.
--
-- 'name', 'updateFunction_name' - The @Function@ name.
--
-- 'functionId', 'updateFunction_functionId' - The function ID.
--
-- 'dataSourceName', 'updateFunction_dataSourceName' - The @Function@ @DataSource@ name.
--
-- 'functionVersion', 'updateFunction_functionVersion' - The @version@ of the request mapping template. Currently the supported
-- value is 2018-05-29.
newUpdateFunction ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'functionId'
  Prelude.Text ->
  -- | 'dataSourceName'
  Prelude.Text ->
  -- | 'functionVersion'
  Prelude.Text ->
  UpdateFunction
newUpdateFunction
  pApiId_
  pName_
  pFunctionId_
  pDataSourceName_
  pFunctionVersion_ =
    UpdateFunction'
      { requestMappingTemplate =
          Prelude.Nothing,
        responseMappingTemplate = Prelude.Nothing,
        syncConfig = Prelude.Nothing,
        description = Prelude.Nothing,
        apiId = pApiId_,
        name = pName_,
        functionId = pFunctionId_,
        dataSourceName = pDataSourceName_,
        functionVersion = pFunctionVersion_
      }

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
updateFunction_requestMappingTemplate :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_requestMappingTemplate = Lens.lens (\UpdateFunction' {requestMappingTemplate} -> requestMappingTemplate) (\s@UpdateFunction' {} a -> s {requestMappingTemplate = a} :: UpdateFunction)

-- | The @Function@ request mapping template.
updateFunction_responseMappingTemplate :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_responseMappingTemplate = Lens.lens (\UpdateFunction' {responseMappingTemplate} -> responseMappingTemplate) (\s@UpdateFunction' {} a -> s {responseMappingTemplate = a} :: UpdateFunction)

-- | Undocumented member.
updateFunction_syncConfig :: Lens.Lens' UpdateFunction (Prelude.Maybe SyncConfig)
updateFunction_syncConfig = Lens.lens (\UpdateFunction' {syncConfig} -> syncConfig) (\s@UpdateFunction' {} a -> s {syncConfig = a} :: UpdateFunction)

-- | The @Function@ description.
updateFunction_description :: Lens.Lens' UpdateFunction (Prelude.Maybe Prelude.Text)
updateFunction_description = Lens.lens (\UpdateFunction' {description} -> description) (\s@UpdateFunction' {} a -> s {description = a} :: UpdateFunction)

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

-- | The @version@ of the request mapping template. Currently the supported
-- value is 2018-05-29.
updateFunction_functionVersion :: Lens.Lens' UpdateFunction Prelude.Text
updateFunction_functionVersion = Lens.lens (\UpdateFunction' {functionVersion} -> functionVersion) (\s@UpdateFunction' {} a -> s {functionVersion = a} :: UpdateFunction)

instance Core.AWSRequest UpdateFunction where
  type
    AWSResponse UpdateFunction =
      UpdateFunctionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFunctionResponse'
            Prelude.<$> (x Core..?> "functionConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFunction

instance Prelude.NFData UpdateFunction

instance Core.ToHeaders UpdateFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFunction where
  toJSON UpdateFunction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("requestMappingTemplate" Core..=)
              Prelude.<$> requestMappingTemplate,
            ("responseMappingTemplate" Core..=)
              Prelude.<$> responseMappingTemplate,
            ("syncConfig" Core..=) Prelude.<$> syncConfig,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("dataSourceName" Core..= dataSourceName),
            Prelude.Just
              ("functionVersion" Core..= functionVersion)
          ]
      )

instance Core.ToPath UpdateFunction where
  toPath UpdateFunction' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/functions/",
        Core.toBS functionId
      ]

instance Core.ToQuery UpdateFunction where
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

instance Prelude.NFData UpdateFunctionResponse
