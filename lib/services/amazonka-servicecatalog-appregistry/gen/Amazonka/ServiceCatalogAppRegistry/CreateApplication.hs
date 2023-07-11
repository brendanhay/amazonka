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
-- Module      : Amazonka.ServiceCatalogAppRegistry.CreateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new application that is the top-level node in a hierarchy of
-- related cloud resource abstractions.
module Amazonka.ServiceCatalogAppRegistry.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_description,
    createApplication_tags,
    createApplication_name,
    createApplication_clientToken,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_application,
    createApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs you can use to associate with the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the application. The name must be unique in the region in
    -- which you are creating the application.
    name :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If you retry
    -- a request that completed successfully using the same client token and
    -- the same parameters, the retry succeeds without performing any further
    -- actions. If you retry a successful request using the same client token,
    -- but one or more of the parameters are different, the retry fails.
    clientToken :: Prelude.Text
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
-- 'description', 'createApplication_description' - The description of the application.
--
-- 'tags', 'createApplication_tags' - Key-value pairs you can use to associate with the application.
--
-- 'name', 'createApplication_name' - The name of the application. The name must be unique in the region in
-- which you are creating the application.
--
-- 'clientToken', 'createApplication_clientToken' - A unique identifier that you provide to ensure idempotency. If you retry
-- a request that completed successfully using the same client token and
-- the same parameters, the retry succeeds without performing any further
-- actions. If you retry a successful request using the same client token,
-- but one or more of the parameters are different, the retry fails.
newCreateApplication ::
  -- | 'name'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateApplication
newCreateApplication pName_ pClientToken_ =
  CreateApplication'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      clientToken = pClientToken_
    }

-- | The description of the application.
createApplication_description :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | Key-value pairs you can use to associate with the application.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the application. The name must be unique in the region in
-- which you are creating the application.
createApplication_name :: Lens.Lens' CreateApplication Prelude.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

-- | A unique identifier that you provide to ensure idempotency. If you retry
-- a request that completed successfully using the same client token and
-- the same parameters, the retry succeeds without performing any further
-- actions. If you retry a successful request using the same client token,
-- but one or more of the parameters are different, the retry fails.
createApplication_clientToken :: Lens.Lens' CreateApplication Prelude.Text
createApplication_clientToken = Lens.lens (\CreateApplication' {clientToken} -> clientToken) (\s@CreateApplication' {} a -> s {clientToken = a} :: CreateApplication)

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
            Prelude.<$> (x Data..?> "application")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientToken

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
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/applications"

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | Information about the application.
    application :: Prelude.Maybe Application,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'application', 'createApplicationResponse_application' - Information about the application.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { application =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the application.
createApplicationResponse_application :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Application)
createApplicationResponse_application = Lens.lens (\CreateApplicationResponse' {application} -> application) (\s@CreateApplicationResponse' {} a -> s {application = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf httpStatus
