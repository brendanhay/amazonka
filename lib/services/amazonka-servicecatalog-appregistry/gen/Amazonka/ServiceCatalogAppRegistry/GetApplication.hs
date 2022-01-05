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
-- Module      : Amazonka.ServiceCatalogAppRegistry.GetApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata information about one of your applications. The
-- application can be specified either by its unique ID or by its name
-- (which is unique within one account in one region at a given point in
-- time). Specify by ID in automated workflows if you want to make sure
-- that the exact same application is returned or a
-- @ResourceNotFoundException@ is thrown, avoiding the ABA addressing
-- problem.
module Amazonka.ServiceCatalogAppRegistry.GetApplication
  ( -- * Creating a Request
    GetApplication (..),
    newGetApplication,

    -- * Request Lenses
    getApplication_application,

    -- * Destructuring the Response
    GetApplicationResponse (..),
    newGetApplicationResponse,

    -- * Response Lenses
    getApplicationResponse_creationTime,
    getApplicationResponse_arn,
    getApplicationResponse_integrations,
    getApplicationResponse_associatedResourceCount,
    getApplicationResponse_name,
    getApplicationResponse_id,
    getApplicationResponse_lastUpdateTime,
    getApplicationResponse_description,
    getApplicationResponse_tags,
    getApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The name or ID of the application.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'getApplication_application' - The name or ID of the application.
newGetApplication ::
  -- | 'application'
  Prelude.Text ->
  GetApplication
newGetApplication pApplication_ =
  GetApplication' {application = pApplication_}

-- | The name or ID of the application.
getApplication_application :: Lens.Lens' GetApplication Prelude.Text
getApplication_application = Lens.lens (\GetApplication' {application} -> application) (\s@GetApplication' {} a -> s {application = a} :: GetApplication)

instance Core.AWSRequest GetApplication where
  type
    AWSResponse GetApplication =
      GetApplicationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Prelude.<$> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "integrations")
            Prelude.<*> (x Core..?> "associatedResourceCount")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "lastUpdateTime")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplication where
  hashWithSalt _salt GetApplication' {..} =
    _salt `Prelude.hashWithSalt` application

instance Prelude.NFData GetApplication where
  rnf GetApplication' {..} = Prelude.rnf application

instance Core.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApplication where
  toPath GetApplication' {..} =
    Prelude.mconcat
      ["/applications/", Core.toBS application]

instance Core.ToQuery GetApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon resource name (ARN) that specifies the application across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The information about the integration of the application with other
    -- services, such as Resource Groups.
    integrations :: Prelude.Maybe Integrations,
    -- | The number of top-level resources that were registered as part of this
    -- application.
    associatedResourceCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the application. The name must be unique in the region in
    -- which you are creating the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the application.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment when the application was
    -- last updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs associated with the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'getApplicationResponse_creationTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- created.
--
-- 'arn', 'getApplicationResponse_arn' - The Amazon resource name (ARN) that specifies the application across
-- services.
--
-- 'integrations', 'getApplicationResponse_integrations' - The information about the integration of the application with other
-- services, such as Resource Groups.
--
-- 'associatedResourceCount', 'getApplicationResponse_associatedResourceCount' - The number of top-level resources that were registered as part of this
-- application.
--
-- 'name', 'getApplicationResponse_name' - The name of the application. The name must be unique in the region in
-- which you are creating the application.
--
-- 'id', 'getApplicationResponse_id' - The identifier of the application.
--
-- 'lastUpdateTime', 'getApplicationResponse_lastUpdateTime' - The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
--
-- 'description', 'getApplicationResponse_description' - The description of the application.
--
-- 'tags', 'getApplicationResponse_tags' - Key-value pairs associated with the application.
--
-- 'httpStatus', 'getApplicationResponse_httpStatus' - The response's http status code.
newGetApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApplicationResponse
newGetApplicationResponse pHttpStatus_ =
  GetApplicationResponse'
    { creationTime =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      integrations = Prelude.Nothing,
      associatedResourceCount = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- created.
getApplicationResponse_creationTime :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.UTCTime)
getApplicationResponse_creationTime = Lens.lens (\GetApplicationResponse' {creationTime} -> creationTime) (\s@GetApplicationResponse' {} a -> s {creationTime = a} :: GetApplicationResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon resource name (ARN) that specifies the application across
-- services.
getApplicationResponse_arn :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_arn = Lens.lens (\GetApplicationResponse' {arn} -> arn) (\s@GetApplicationResponse' {} a -> s {arn = a} :: GetApplicationResponse)

-- | The information about the integration of the application with other
-- services, such as Resource Groups.
getApplicationResponse_integrations :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Integrations)
getApplicationResponse_integrations = Lens.lens (\GetApplicationResponse' {integrations} -> integrations) (\s@GetApplicationResponse' {} a -> s {integrations = a} :: GetApplicationResponse)

-- | The number of top-level resources that were registered as part of this
-- application.
getApplicationResponse_associatedResourceCount :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Natural)
getApplicationResponse_associatedResourceCount = Lens.lens (\GetApplicationResponse' {associatedResourceCount} -> associatedResourceCount) (\s@GetApplicationResponse' {} a -> s {associatedResourceCount = a} :: GetApplicationResponse)

-- | The name of the application. The name must be unique in the region in
-- which you are creating the application.
getApplicationResponse_name :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_name = Lens.lens (\GetApplicationResponse' {name} -> name) (\s@GetApplicationResponse' {} a -> s {name = a} :: GetApplicationResponse)

-- | The identifier of the application.
getApplicationResponse_id :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_id = Lens.lens (\GetApplicationResponse' {id} -> id) (\s@GetApplicationResponse' {} a -> s {id = a} :: GetApplicationResponse)

-- | The ISO-8601 formatted timestamp of the moment when the application was
-- last updated.
getApplicationResponse_lastUpdateTime :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.UTCTime)
getApplicationResponse_lastUpdateTime = Lens.lens (\GetApplicationResponse' {lastUpdateTime} -> lastUpdateTime) (\s@GetApplicationResponse' {} a -> s {lastUpdateTime = a} :: GetApplicationResponse) Prelude.. Lens.mapping Core._Time

-- | The description of the application.
getApplicationResponse_description :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_description = Lens.lens (\GetApplicationResponse' {description} -> description) (\s@GetApplicationResponse' {} a -> s {description = a} :: GetApplicationResponse)

-- | Key-value pairs associated with the application.
getApplicationResponse_tags :: Lens.Lens' GetApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getApplicationResponse_tags = Lens.lens (\GetApplicationResponse' {tags} -> tags) (\s@GetApplicationResponse' {} a -> s {tags = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Prelude.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

instance Prelude.NFData GetApplicationResponse where
  rnf GetApplicationResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf integrations
      `Prelude.seq` Prelude.rnf associatedResourceCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
