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
-- Module      : Amazonka.IoTFleetHub.CreateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Fleet Hub for AWS IoT Device Management web application.
--
-- Fleet Hub for AWS IoT Device Management is in public preview and is
-- subject to change.
module Amazonka.IoTFleetHub.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_applicationDescription,
    createApplication_clientToken,
    createApplication_tags,
    createApplication_applicationName,
    createApplication_roleArn,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationId,
    createApplicationResponse_applicationArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | An optional description of the web application.
    applicationDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A set of key\/value pairs that you can use to manage the web application
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the web application.
    applicationName :: Prelude.Text,
    -- | The ARN of the role that the web application assumes when it interacts
    -- with AWS IoT Core.
    --
    -- The name of the role must be in the form
    -- @AWSIotFleetHub_@/@random_string@/@ @.
    roleArn :: Prelude.Text
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
-- 'applicationDescription', 'createApplication_applicationDescription' - An optional description of the web application.
--
-- 'clientToken', 'createApplication_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'tags', 'createApplication_tags' - A set of key\/value pairs that you can use to manage the web application
-- resource.
--
-- 'applicationName', 'createApplication_applicationName' - The name of the web application.
--
-- 'roleArn', 'createApplication_roleArn' - The ARN of the role that the web application assumes when it interacts
-- with AWS IoT Core.
--
-- The name of the role must be in the form
-- @AWSIotFleetHub_@/@random_string@/@ @.
newCreateApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateApplication
newCreateApplication pApplicationName_ pRoleArn_ =
  CreateApplication'
    { applicationDescription =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      applicationName = pApplicationName_,
      roleArn = pRoleArn_
    }

-- | An optional description of the web application.
createApplication_applicationDescription :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_applicationDescription = Lens.lens (\CreateApplication' {applicationDescription} -> applicationDescription) (\s@CreateApplication' {} a -> s {applicationDescription = a} :: CreateApplication)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
createApplication_clientToken :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_clientToken = Lens.lens (\CreateApplication' {clientToken} -> clientToken) (\s@CreateApplication' {} a -> s {clientToken = a} :: CreateApplication)

-- | A set of key\/value pairs that you can use to manage the web application
-- resource.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the web application.
createApplication_applicationName :: Lens.Lens' CreateApplication Prelude.Text
createApplication_applicationName = Lens.lens (\CreateApplication' {applicationName} -> applicationName) (\s@CreateApplication' {} a -> s {applicationName = a} :: CreateApplication)

-- | The ARN of the role that the web application assumes when it interacts
-- with AWS IoT Core.
--
-- The name of the role must be in the form
-- @AWSIotFleetHub_@/@random_string@/@ @.
createApplication_roleArn :: Lens.Lens' CreateApplication Prelude.Text
createApplication_roleArn = Lens.lens (\CreateApplication' {roleArn} -> roleArn) (\s@CreateApplication' {} a -> s {roleArn = a} :: CreateApplication)

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
            Prelude.<*> (x Data..:> "applicationId")
            Prelude.<*> (x Data..:> "applicationArn")
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` applicationDescription
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf applicationDescription `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf applicationName `Prelude.seq`
            Prelude.rnf roleArn

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
          [ ("applicationDescription" Data..=)
              Prelude.<$> applicationDescription,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("applicationName" Data..= applicationName),
            Prelude.Just ("roleArn" Data..= roleArn)
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
    -- | The unique Id of the web application.
    applicationId :: Prelude.Text,
    -- | The ARN of the web application.
    applicationArn :: Prelude.Text
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
-- 'applicationId', 'createApplicationResponse_applicationId' - The unique Id of the web application.
--
-- 'applicationArn', 'createApplicationResponse_applicationArn' - The ARN of the web application.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationArn'
  Prelude.Text ->
  CreateApplicationResponse
newCreateApplicationResponse
  pHttpStatus_
  pApplicationId_
  pApplicationArn_ =
    CreateApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        applicationId = pApplicationId_,
        applicationArn = pApplicationArn_
      }

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

-- | The unique Id of the web application.
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse Prelude.Text
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | The ARN of the web application.
createApplicationResponse_applicationArn :: Lens.Lens' CreateApplicationResponse Prelude.Text
createApplicationResponse_applicationArn = Lens.lens (\CreateApplicationResponse' {applicationArn} -> applicationArn) (\s@CreateApplicationResponse' {} a -> s {applicationArn = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf applicationId `Prelude.seq`
        Prelude.rnf applicationArn
