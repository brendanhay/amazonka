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
-- Module      : Network.AWS.AppSync.UpdateFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Function@ object.
module Network.AWS.AppSync.UpdateFunction
  ( -- * Creating a Request
    UpdateFunction (..),
    newUpdateFunction,

    -- * Request Lenses
    updateFunction_responseMappingTemplate,
    updateFunction_syncConfig,
    updateFunction_description,
    updateFunction_requestMappingTemplate,
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { -- | The @Function@ request mapping template.
    responseMappingTemplate :: Core.Maybe Core.Text,
    syncConfig :: Core.Maybe SyncConfig,
    -- | The @Function@ description.
    description :: Core.Maybe Core.Text,
    -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Core.Maybe Core.Text,
    -- | The GraphQL API ID.
    apiId :: Core.Text,
    -- | The @Function@ name.
    name :: Core.Text,
    -- | The function ID.
    functionId :: Core.Text,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Core.Text,
    -- | The @version@ of the request mapping template. Currently the supported
    -- value is 2018-05-29.
    functionVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseMappingTemplate', 'updateFunction_responseMappingTemplate' - The @Function@ request mapping template.
--
-- 'syncConfig', 'updateFunction_syncConfig' - Undocumented member.
--
-- 'description', 'updateFunction_description' - The @Function@ description.
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
--
-- 'functionVersion', 'updateFunction_functionVersion' - The @version@ of the request mapping template. Currently the supported
-- value is 2018-05-29.
newUpdateFunction ::
  -- | 'apiId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'functionId'
  Core.Text ->
  -- | 'dataSourceName'
  Core.Text ->
  -- | 'functionVersion'
  Core.Text ->
  UpdateFunction
newUpdateFunction
  pApiId_
  pName_
  pFunctionId_
  pDataSourceName_
  pFunctionVersion_ =
    UpdateFunction'
      { responseMappingTemplate =
          Core.Nothing,
        syncConfig = Core.Nothing,
        description = Core.Nothing,
        requestMappingTemplate = Core.Nothing,
        apiId = pApiId_,
        name = pName_,
        functionId = pFunctionId_,
        dataSourceName = pDataSourceName_,
        functionVersion = pFunctionVersion_
      }

-- | The @Function@ request mapping template.
updateFunction_responseMappingTemplate :: Lens.Lens' UpdateFunction (Core.Maybe Core.Text)
updateFunction_responseMappingTemplate = Lens.lens (\UpdateFunction' {responseMappingTemplate} -> responseMappingTemplate) (\s@UpdateFunction' {} a -> s {responseMappingTemplate = a} :: UpdateFunction)

-- | Undocumented member.
updateFunction_syncConfig :: Lens.Lens' UpdateFunction (Core.Maybe SyncConfig)
updateFunction_syncConfig = Lens.lens (\UpdateFunction' {syncConfig} -> syncConfig) (\s@UpdateFunction' {} a -> s {syncConfig = a} :: UpdateFunction)

-- | The @Function@ description.
updateFunction_description :: Lens.Lens' UpdateFunction (Core.Maybe Core.Text)
updateFunction_description = Lens.lens (\UpdateFunction' {description} -> description) (\s@UpdateFunction' {} a -> s {description = a} :: UpdateFunction)

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
updateFunction_requestMappingTemplate :: Lens.Lens' UpdateFunction (Core.Maybe Core.Text)
updateFunction_requestMappingTemplate = Lens.lens (\UpdateFunction' {requestMappingTemplate} -> requestMappingTemplate) (\s@UpdateFunction' {} a -> s {requestMappingTemplate = a} :: UpdateFunction)

-- | The GraphQL API ID.
updateFunction_apiId :: Lens.Lens' UpdateFunction Core.Text
updateFunction_apiId = Lens.lens (\UpdateFunction' {apiId} -> apiId) (\s@UpdateFunction' {} a -> s {apiId = a} :: UpdateFunction)

-- | The @Function@ name.
updateFunction_name :: Lens.Lens' UpdateFunction Core.Text
updateFunction_name = Lens.lens (\UpdateFunction' {name} -> name) (\s@UpdateFunction' {} a -> s {name = a} :: UpdateFunction)

-- | The function ID.
updateFunction_functionId :: Lens.Lens' UpdateFunction Core.Text
updateFunction_functionId = Lens.lens (\UpdateFunction' {functionId} -> functionId) (\s@UpdateFunction' {} a -> s {functionId = a} :: UpdateFunction)

-- | The @Function@ @DataSource@ name.
updateFunction_dataSourceName :: Lens.Lens' UpdateFunction Core.Text
updateFunction_dataSourceName = Lens.lens (\UpdateFunction' {dataSourceName} -> dataSourceName) (\s@UpdateFunction' {} a -> s {dataSourceName = a} :: UpdateFunction)

-- | The @version@ of the request mapping template. Currently the supported
-- value is 2018-05-29.
updateFunction_functionVersion :: Lens.Lens' UpdateFunction Core.Text
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
            Core.<$> (x Core..?> "functionConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateFunction

instance Core.NFData UpdateFunction

instance Core.ToHeaders UpdateFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateFunction where
  toJSON UpdateFunction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("responseMappingTemplate" Core..=)
              Core.<$> responseMappingTemplate,
            ("syncConfig" Core..=) Core.<$> syncConfig,
            ("description" Core..=) Core.<$> description,
            ("requestMappingTemplate" Core..=)
              Core.<$> requestMappingTemplate,
            Core.Just ("name" Core..= name),
            Core.Just ("dataSourceName" Core..= dataSourceName),
            Core.Just
              ("functionVersion" Core..= functionVersion)
          ]
      )

instance Core.ToPath UpdateFunction where
  toPath UpdateFunction' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/functions/",
        Core.toBS functionId
      ]

instance Core.ToQuery UpdateFunction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateFunctionResponse' smart constructor.
data UpdateFunctionResponse = UpdateFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Core.Maybe FunctionConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateFunctionResponse
newUpdateFunctionResponse pHttpStatus_ =
  UpdateFunctionResponse'
    { functionConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Function@ object.
updateFunctionResponse_functionConfiguration :: Lens.Lens' UpdateFunctionResponse (Core.Maybe FunctionConfiguration)
updateFunctionResponse_functionConfiguration = Lens.lens (\UpdateFunctionResponse' {functionConfiguration} -> functionConfiguration) (\s@UpdateFunctionResponse' {} a -> s {functionConfiguration = a} :: UpdateFunctionResponse)

-- | The response's http status code.
updateFunctionResponse_httpStatus :: Lens.Lens' UpdateFunctionResponse Core.Int
updateFunctionResponse_httpStatus = Lens.lens (\UpdateFunctionResponse' {httpStatus} -> httpStatus) (\s@UpdateFunctionResponse' {} a -> s {httpStatus = a} :: UpdateFunctionResponse)

instance Core.NFData UpdateFunctionResponse
