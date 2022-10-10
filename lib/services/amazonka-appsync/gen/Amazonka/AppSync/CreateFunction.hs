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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createFunction_maxBatchSize,
    createFunction_description,
    createFunction_responseMappingTemplate,
    createFunction_syncConfig,
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | The maximum batching size for a resolver.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | The @Function@ description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The GraphQL API ID.
    apiId :: Prelude.Text,
    -- | The @Function@ name. The function name does not have to be unique.
    name :: Prelude.Text,
    -- | The @Function@ @DataSource@ name.
    dataSourceName :: Prelude.Text,
    -- | The @version@ of the request mapping template. Currently, the supported
    -- value is 2018-05-29.
    functionVersion :: Prelude.Text
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
-- 'maxBatchSize', 'createFunction_maxBatchSize' - The maximum batching size for a resolver.
--
-- 'description', 'createFunction_description' - The @Function@ description.
--
-- 'responseMappingTemplate', 'createFunction_responseMappingTemplate' - The @Function@ response mapping template.
--
-- 'syncConfig', 'createFunction_syncConfig' - Undocumented member.
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
-- 'functionVersion', 'createFunction_functionVersion' - The @version@ of the request mapping template. Currently, the supported
-- value is 2018-05-29.
newCreateFunction ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'dataSourceName'
  Prelude.Text ->
  -- | 'functionVersion'
  Prelude.Text ->
  CreateFunction
newCreateFunction
  pApiId_
  pName_
  pDataSourceName_
  pFunctionVersion_ =
    CreateFunction'
      { maxBatchSize = Prelude.Nothing,
        description = Prelude.Nothing,
        responseMappingTemplate = Prelude.Nothing,
        syncConfig = Prelude.Nothing,
        requestMappingTemplate = Prelude.Nothing,
        apiId = pApiId_,
        name = pName_,
        dataSourceName = pDataSourceName_,
        functionVersion = pFunctionVersion_
      }

-- | The maximum batching size for a resolver.
createFunction_maxBatchSize :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Natural)
createFunction_maxBatchSize = Lens.lens (\CreateFunction' {maxBatchSize} -> maxBatchSize) (\s@CreateFunction' {} a -> s {maxBatchSize = a} :: CreateFunction)

-- | The @Function@ description.
createFunction_description :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_description = Lens.lens (\CreateFunction' {description} -> description) (\s@CreateFunction' {} a -> s {description = a} :: CreateFunction)

-- | The @Function@ response mapping template.
createFunction_responseMappingTemplate :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_responseMappingTemplate = Lens.lens (\CreateFunction' {responseMappingTemplate} -> responseMappingTemplate) (\s@CreateFunction' {} a -> s {responseMappingTemplate = a} :: CreateFunction)

-- | Undocumented member.
createFunction_syncConfig :: Lens.Lens' CreateFunction (Prelude.Maybe SyncConfig)
createFunction_syncConfig = Lens.lens (\CreateFunction' {syncConfig} -> syncConfig) (\s@CreateFunction' {} a -> s {syncConfig = a} :: CreateFunction)

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
createFunction_requestMappingTemplate :: Lens.Lens' CreateFunction (Prelude.Maybe Prelude.Text)
createFunction_requestMappingTemplate = Lens.lens (\CreateFunction' {requestMappingTemplate} -> requestMappingTemplate) (\s@CreateFunction' {} a -> s {requestMappingTemplate = a} :: CreateFunction)

-- | The GraphQL API ID.
createFunction_apiId :: Lens.Lens' CreateFunction Prelude.Text
createFunction_apiId = Lens.lens (\CreateFunction' {apiId} -> apiId) (\s@CreateFunction' {} a -> s {apiId = a} :: CreateFunction)

-- | The @Function@ name. The function name does not have to be unique.
createFunction_name :: Lens.Lens' CreateFunction Prelude.Text
createFunction_name = Lens.lens (\CreateFunction' {name} -> name) (\s@CreateFunction' {} a -> s {name = a} :: CreateFunction)

-- | The @Function@ @DataSource@ name.
createFunction_dataSourceName :: Lens.Lens' CreateFunction Prelude.Text
createFunction_dataSourceName = Lens.lens (\CreateFunction' {dataSourceName} -> dataSourceName) (\s@CreateFunction' {} a -> s {dataSourceName = a} :: CreateFunction)

-- | The @version@ of the request mapping template. Currently, the supported
-- value is 2018-05-29.
createFunction_functionVersion :: Lens.Lens' CreateFunction Prelude.Text
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
            Prelude.<$> (x Core..?> "functionConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFunction where
  hashWithSalt _salt CreateFunction' {..} =
    _salt `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` responseMappingTemplate
      `Prelude.hashWithSalt` syncConfig
      `Prelude.hashWithSalt` requestMappingTemplate
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataSourceName
      `Prelude.hashWithSalt` functionVersion

instance Prelude.NFData CreateFunction where
  rnf CreateFunction' {..} =
    Prelude.rnf maxBatchSize
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf responseMappingTemplate
      `Prelude.seq` Prelude.rnf syncConfig
      `Prelude.seq` Prelude.rnf requestMappingTemplate
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataSourceName
      `Prelude.seq` Prelude.rnf functionVersion

instance Core.ToHeaders CreateFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFunction where
  toJSON CreateFunction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxBatchSize" Core..=) Prelude.<$> maxBatchSize,
            ("description" Core..=) Prelude.<$> description,
            ("responseMappingTemplate" Core..=)
              Prelude.<$> responseMappingTemplate,
            ("syncConfig" Core..=) Prelude.<$> syncConfig,
            ("requestMappingTemplate" Core..=)
              Prelude.<$> requestMappingTemplate,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("dataSourceName" Core..= dataSourceName),
            Prelude.Just
              ("functionVersion" Core..= functionVersion)
          ]
      )

instance Core.ToPath CreateFunction where
  toPath CreateFunction' {..} =
    Prelude.mconcat
      ["/v1/apis/", Core.toBS apiId, "/functions"]

instance Core.ToQuery CreateFunction where
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
