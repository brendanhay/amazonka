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
-- Module      : Network.AWS.AppSync.CreateFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Function@ object.
--
-- A function is a reusable entity. Multiple functions can be used to
-- compose the resolver logic.
module Network.AWS.AppSync.CreateFunction
  ( -- * Creating a Request
    CreateFunction (..),
    newCreateFunction,

    -- * Request Lenses
    createFunction_responseMappingTemplate,
    createFunction_syncConfig,
    createFunction_description,
    createFunction_requestMappingTemplate,
    createFunction_apiId,
    createFunction_name,
    createFunction_dataSourceName,
    createFunction_functionVersion,

    -- * Destructuring the Response
    CreateFunctionResponse (..),
    newCreateFunctionResponse,

    -- * Response Lenses
    createFunctionResponse_functionConfiguration,
    createFunctionResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | The @Function@ response mapping template.
    responseMappingTemplate :: Core.Maybe Core.Text,
    syncConfig :: Core.Maybe SyncConfig,
    -- | The @Function@ description.
    description :: Core.Maybe Core.Text,
    -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Core.Maybe Core.Text,
    -- | The GraphQL API ID.
    apiId :: Core.Text,
    -- | The @Function@ name. The function name does not have to be unique.
    name :: Core.Text,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Core.Text,
    -- | The @version@ of the request mapping template. Currently the supported
    -- value is 2018-05-29.
    functionVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseMappingTemplate', 'createFunction_responseMappingTemplate' - The @Function@ response mapping template.
--
-- 'syncConfig', 'createFunction_syncConfig' - Undocumented member.
--
-- 'description', 'createFunction_description' - The @Function@ description.
--
-- 'requestMappingTemplate', 'createFunction_requestMappingTemplate' - The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
--
-- 'apiId', 'createFunction_apiId' - The GraphQL API ID.
--
-- 'name', 'createFunction_name' - The @Function@ name. The function name does not have to be unique.
--
-- 'dataSourceName', 'createFunction_dataSourceName' - The @Function@ @DataSource@ name.
--
-- 'functionVersion', 'createFunction_functionVersion' - The @version@ of the request mapping template. Currently the supported
-- value is 2018-05-29.
newCreateFunction ::
  -- | 'apiId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'dataSourceName'
  Core.Text ->
  -- | 'functionVersion'
  Core.Text ->
  CreateFunction
newCreateFunction
  pApiId_
  pName_
  pDataSourceName_
  pFunctionVersion_ =
    CreateFunction'
      { responseMappingTemplate =
          Core.Nothing,
        syncConfig = Core.Nothing,
        description = Core.Nothing,
        requestMappingTemplate = Core.Nothing,
        apiId = pApiId_,
        name = pName_,
        dataSourceName = pDataSourceName_,
        functionVersion = pFunctionVersion_
      }

-- | The @Function@ response mapping template.
createFunction_responseMappingTemplate :: Lens.Lens' CreateFunction (Core.Maybe Core.Text)
createFunction_responseMappingTemplate = Lens.lens (\CreateFunction' {responseMappingTemplate} -> responseMappingTemplate) (\s@CreateFunction' {} a -> s {responseMappingTemplate = a} :: CreateFunction)

-- | Undocumented member.
createFunction_syncConfig :: Lens.Lens' CreateFunction (Core.Maybe SyncConfig)
createFunction_syncConfig = Lens.lens (\CreateFunction' {syncConfig} -> syncConfig) (\s@CreateFunction' {} a -> s {syncConfig = a} :: CreateFunction)

-- | The @Function@ description.
createFunction_description :: Lens.Lens' CreateFunction (Core.Maybe Core.Text)
createFunction_description = Lens.lens (\CreateFunction' {description} -> description) (\s@CreateFunction' {} a -> s {description = a} :: CreateFunction)

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
createFunction_requestMappingTemplate :: Lens.Lens' CreateFunction (Core.Maybe Core.Text)
createFunction_requestMappingTemplate = Lens.lens (\CreateFunction' {requestMappingTemplate} -> requestMappingTemplate) (\s@CreateFunction' {} a -> s {requestMappingTemplate = a} :: CreateFunction)

-- | The GraphQL API ID.
createFunction_apiId :: Lens.Lens' CreateFunction Core.Text
createFunction_apiId = Lens.lens (\CreateFunction' {apiId} -> apiId) (\s@CreateFunction' {} a -> s {apiId = a} :: CreateFunction)

-- | The @Function@ name. The function name does not have to be unique.
createFunction_name :: Lens.Lens' CreateFunction Core.Text
createFunction_name = Lens.lens (\CreateFunction' {name} -> name) (\s@CreateFunction' {} a -> s {name = a} :: CreateFunction)

-- | The @Function@ @DataSource@ name.
createFunction_dataSourceName :: Lens.Lens' CreateFunction Core.Text
createFunction_dataSourceName = Lens.lens (\CreateFunction' {dataSourceName} -> dataSourceName) (\s@CreateFunction' {} a -> s {dataSourceName = a} :: CreateFunction)

-- | The @version@ of the request mapping template. Currently the supported
-- value is 2018-05-29.
createFunction_functionVersion :: Lens.Lens' CreateFunction Core.Text
createFunction_functionVersion = Lens.lens (\CreateFunction' {functionVersion} -> functionVersion) (\s@CreateFunction' {} a -> s {functionVersion = a} :: CreateFunction)

instance Core.AWSRequest CreateFunction where
  type
    AWSResponse CreateFunction =
      CreateFunctionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionResponse'
            Core.<$> (x Core..?> "functionConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateFunction

instance Core.NFData CreateFunction

instance Core.ToHeaders CreateFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateFunction where
  toJSON CreateFunction' {..} =
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

instance Core.ToPath CreateFunction where
  toPath CreateFunction' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/functions"]

instance Core.ToQuery CreateFunction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateFunctionResponse' smart constructor.
data CreateFunctionResponse = CreateFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Core.Maybe FunctionConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateFunctionResponse
newCreateFunctionResponse pHttpStatus_ =
  CreateFunctionResponse'
    { functionConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Function@ object.
createFunctionResponse_functionConfiguration :: Lens.Lens' CreateFunctionResponse (Core.Maybe FunctionConfiguration)
createFunctionResponse_functionConfiguration = Lens.lens (\CreateFunctionResponse' {functionConfiguration} -> functionConfiguration) (\s@CreateFunctionResponse' {} a -> s {functionConfiguration = a} :: CreateFunctionResponse)

-- | The response's http status code.
createFunctionResponse_httpStatus :: Lens.Lens' CreateFunctionResponse Core.Int
createFunctionResponse_httpStatus = Lens.lens (\CreateFunctionResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionResponse' {} a -> s {httpStatus = a} :: CreateFunctionResponse)

instance Core.NFData CreateFunctionResponse
