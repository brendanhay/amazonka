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
-- Module      : Amazonka.M2.CreateApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new application with given parameters. Requires an existing
-- environment and application definition file.
module Amazonka.M2.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_clientToken,
    createApplication_description,
    createApplication_tags,
    createApplication_definition,
    createApplication_engineType,
    createApplication_name,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationArn,
    createApplicationResponse_applicationId,
    createApplicationResponse_applicationVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | Unique, case-sensitive identifier the service generates to ensure the
    -- idempotency of the request to create an application. The service
    -- generates the clientToken when the API call is triggered. The token
    -- expires after one hour, so if you retry the API within this timeframe
    -- with the same clientToken, you will get the same response. The service
    -- also handles deleting the clientToken after it expires.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to apply to the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The application definition for this application. You can specify either
    -- inline JSON or an S3 bucket location.
    definition :: Definition,
    -- | The type of the target platform for this application.
    engineType :: EngineType,
    -- | The unique identifier of the application.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createApplication_clientToken' - Unique, case-sensitive identifier the service generates to ensure the
-- idempotency of the request to create an application. The service
-- generates the clientToken when the API call is triggered. The token
-- expires after one hour, so if you retry the API within this timeframe
-- with the same clientToken, you will get the same response. The service
-- also handles deleting the clientToken after it expires.
--
-- 'description', 'createApplication_description' - The description of the application.
--
-- 'tags', 'createApplication_tags' - A list of tags to apply to the application.
--
-- 'definition', 'createApplication_definition' - The application definition for this application. You can specify either
-- inline JSON or an S3 bucket location.
--
-- 'engineType', 'createApplication_engineType' - The type of the target platform for this application.
--
-- 'name', 'createApplication_name' - The unique identifier of the application.
newCreateApplication ::
  -- | 'definition'
  Definition ->
  -- | 'engineType'
  EngineType ->
  -- | 'name'
  Prelude.Text ->
  CreateApplication
newCreateApplication pDefinition_ pEngineType_ pName_ =
  CreateApplication'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      definition = pDefinition_,
      engineType = pEngineType_,
      name = pName_
    }

-- | Unique, case-sensitive identifier the service generates to ensure the
-- idempotency of the request to create an application. The service
-- generates the clientToken when the API call is triggered. The token
-- expires after one hour, so if you retry the API within this timeframe
-- with the same clientToken, you will get the same response. The service
-- also handles deleting the clientToken after it expires.
createApplication_clientToken :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_clientToken = Lens.lens (\CreateApplication' {clientToken} -> clientToken) (\s@CreateApplication' {} a -> s {clientToken = a} :: CreateApplication)

-- | The description of the application.
createApplication_description :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | A list of tags to apply to the application.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The application definition for this application. You can specify either
-- inline JSON or an S3 bucket location.
createApplication_definition :: Lens.Lens' CreateApplication Definition
createApplication_definition = Lens.lens (\CreateApplication' {definition} -> definition) (\s@CreateApplication' {} a -> s {definition = a} :: CreateApplication)

-- | The type of the target platform for this application.
createApplication_engineType :: Lens.Lens' CreateApplication EngineType
createApplication_engineType = Lens.lens (\CreateApplication' {engineType} -> engineType) (\s@CreateApplication' {} a -> s {engineType = a} :: CreateApplication)

-- | The unique identifier of the application.
createApplication_name :: Lens.Lens' CreateApplication Prelude.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationArn")
            Prelude.<*> (x Data..:> "applicationId")
            Prelude.<*> (x Data..:> "applicationVersion")
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("definition" Data..= definition),
            Prelude.Just ("engineType" Data..= engineType),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/applications"

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Prelude.Text,
    -- | The unique application identifier.
    applicationId :: Prelude.Text,
    -- | The version number of the application.
    applicationVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationArn', 'createApplicationResponse_applicationArn' - The Amazon Resource Name (ARN) of the application.
--
-- 'applicationId', 'createApplicationResponse_applicationId' - The unique application identifier.
--
-- 'applicationVersion', 'createApplicationResponse_applicationVersion' - The version number of the application.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationArn'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  CreateApplicationResponse
newCreateApplicationResponse
  pHttpStatus_
  pApplicationArn_
  pApplicationId_
  pApplicationVersion_ =
    CreateApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        applicationArn = pApplicationArn_,
        applicationId = pApplicationId_,
        applicationVersion = pApplicationVersion_
      }

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

-- | The Amazon Resource Name (ARN) of the application.
createApplicationResponse_applicationArn :: Lens.Lens' CreateApplicationResponse Prelude.Text
createApplicationResponse_applicationArn = Lens.lens (\CreateApplicationResponse' {applicationArn} -> applicationArn) (\s@CreateApplicationResponse' {} a -> s {applicationArn = a} :: CreateApplicationResponse)

-- | The unique application identifier.
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse Prelude.Text
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | The version number of the application.
createApplicationResponse_applicationVersion :: Lens.Lens' CreateApplicationResponse Prelude.Natural
createApplicationResponse_applicationVersion = Lens.lens (\CreateApplicationResponse' {applicationVersion} -> applicationVersion) (\s@CreateApplicationResponse' {} a -> s {applicationVersion = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationVersion
