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
-- Module      : Amazonka.M2.GetApplicationVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a specific version of a specific application.
module Amazonka.M2.GetApplicationVersion
  ( -- * Creating a Request
    GetApplicationVersion (..),
    newGetApplicationVersion,

    -- * Request Lenses
    getApplicationVersion_applicationId,
    getApplicationVersion_applicationVersion,

    -- * Destructuring the Response
    GetApplicationVersionResponse (..),
    newGetApplicationVersionResponse,

    -- * Response Lenses
    getApplicationVersionResponse_description,
    getApplicationVersionResponse_statusReason,
    getApplicationVersionResponse_httpStatus,
    getApplicationVersionResponse_applicationVersion,
    getApplicationVersionResponse_creationTime,
    getApplicationVersionResponse_definitionContent,
    getApplicationVersionResponse_name,
    getApplicationVersionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApplicationVersion' smart constructor.
data GetApplicationVersion = GetApplicationVersion'
  { -- | The unique identifier of the application.
    applicationId :: Prelude.Text,
    -- | The specific version of the application.
    applicationVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApplicationVersion_applicationId' - The unique identifier of the application.
--
-- 'applicationVersion', 'getApplicationVersion_applicationVersion' - The specific version of the application.
newGetApplicationVersion ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  GetApplicationVersion
newGetApplicationVersion
  pApplicationId_
  pApplicationVersion_ =
    GetApplicationVersion'
      { applicationId =
          pApplicationId_,
        applicationVersion = pApplicationVersion_
      }

-- | The unique identifier of the application.
getApplicationVersion_applicationId :: Lens.Lens' GetApplicationVersion Prelude.Text
getApplicationVersion_applicationId = Lens.lens (\GetApplicationVersion' {applicationId} -> applicationId) (\s@GetApplicationVersion' {} a -> s {applicationId = a} :: GetApplicationVersion)

-- | The specific version of the application.
getApplicationVersion_applicationVersion :: Lens.Lens' GetApplicationVersion Prelude.Natural
getApplicationVersion_applicationVersion = Lens.lens (\GetApplicationVersion' {applicationVersion} -> applicationVersion) (\s@GetApplicationVersion' {} a -> s {applicationVersion = a} :: GetApplicationVersion)

instance Core.AWSRequest GetApplicationVersion where
  type
    AWSResponse GetApplicationVersion =
      GetApplicationVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationVersionResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "statusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationVersion")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "definitionContent")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetApplicationVersion where
  hashWithSalt _salt GetApplicationVersion' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` applicationVersion

instance Prelude.NFData GetApplicationVersion where
  rnf GetApplicationVersion' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationVersion

instance Data.ToHeaders GetApplicationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApplicationVersion where
  toPath GetApplicationVersion' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/versions/",
        Data.toBS applicationVersion
      ]

instance Data.ToQuery GetApplicationVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationVersionResponse' smart constructor.
data GetApplicationVersionResponse = GetApplicationVersionResponse'
  { -- | The application description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The specific version of the application.
    applicationVersion :: Prelude.Natural,
    -- | The timestamp when the application version was created.
    creationTime :: Data.POSIX,
    -- | The content of the application definition. This is a JSON object that
    -- contains the resource configuration and definitions that identify an
    -- application.
    definitionContent :: Prelude.Text,
    -- | The name of the application version.
    name :: Prelude.Text,
    -- | The status of the application version.
    status :: ApplicationVersionLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getApplicationVersionResponse_description' - The application description.
--
-- 'statusReason', 'getApplicationVersionResponse_statusReason' - The reason for the reported status.
--
-- 'httpStatus', 'getApplicationVersionResponse_httpStatus' - The response's http status code.
--
-- 'applicationVersion', 'getApplicationVersionResponse_applicationVersion' - The specific version of the application.
--
-- 'creationTime', 'getApplicationVersionResponse_creationTime' - The timestamp when the application version was created.
--
-- 'definitionContent', 'getApplicationVersionResponse_definitionContent' - The content of the application definition. This is a JSON object that
-- contains the resource configuration and definitions that identify an
-- application.
--
-- 'name', 'getApplicationVersionResponse_name' - The name of the application version.
--
-- 'status', 'getApplicationVersionResponse_status' - The status of the application version.
newGetApplicationVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'definitionContent'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ApplicationVersionLifecycle ->
  GetApplicationVersionResponse
newGetApplicationVersionResponse
  pHttpStatus_
  pApplicationVersion_
  pCreationTime_
  pDefinitionContent_
  pName_
  pStatus_ =
    GetApplicationVersionResponse'
      { description =
          Prelude.Nothing,
        statusReason = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        applicationVersion = pApplicationVersion_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        definitionContent = pDefinitionContent_,
        name = pName_,
        status = pStatus_
      }

-- | The application description.
getApplicationVersionResponse_description :: Lens.Lens' GetApplicationVersionResponse (Prelude.Maybe Prelude.Text)
getApplicationVersionResponse_description = Lens.lens (\GetApplicationVersionResponse' {description} -> description) (\s@GetApplicationVersionResponse' {} a -> s {description = a} :: GetApplicationVersionResponse)

-- | The reason for the reported status.
getApplicationVersionResponse_statusReason :: Lens.Lens' GetApplicationVersionResponse (Prelude.Maybe Prelude.Text)
getApplicationVersionResponse_statusReason = Lens.lens (\GetApplicationVersionResponse' {statusReason} -> statusReason) (\s@GetApplicationVersionResponse' {} a -> s {statusReason = a} :: GetApplicationVersionResponse)

-- | The response's http status code.
getApplicationVersionResponse_httpStatus :: Lens.Lens' GetApplicationVersionResponse Prelude.Int
getApplicationVersionResponse_httpStatus = Lens.lens (\GetApplicationVersionResponse' {httpStatus} -> httpStatus) (\s@GetApplicationVersionResponse' {} a -> s {httpStatus = a} :: GetApplicationVersionResponse)

-- | The specific version of the application.
getApplicationVersionResponse_applicationVersion :: Lens.Lens' GetApplicationVersionResponse Prelude.Natural
getApplicationVersionResponse_applicationVersion = Lens.lens (\GetApplicationVersionResponse' {applicationVersion} -> applicationVersion) (\s@GetApplicationVersionResponse' {} a -> s {applicationVersion = a} :: GetApplicationVersionResponse)

-- | The timestamp when the application version was created.
getApplicationVersionResponse_creationTime :: Lens.Lens' GetApplicationVersionResponse Prelude.UTCTime
getApplicationVersionResponse_creationTime = Lens.lens (\GetApplicationVersionResponse' {creationTime} -> creationTime) (\s@GetApplicationVersionResponse' {} a -> s {creationTime = a} :: GetApplicationVersionResponse) Prelude.. Data._Time

-- | The content of the application definition. This is a JSON object that
-- contains the resource configuration and definitions that identify an
-- application.
getApplicationVersionResponse_definitionContent :: Lens.Lens' GetApplicationVersionResponse Prelude.Text
getApplicationVersionResponse_definitionContent = Lens.lens (\GetApplicationVersionResponse' {definitionContent} -> definitionContent) (\s@GetApplicationVersionResponse' {} a -> s {definitionContent = a} :: GetApplicationVersionResponse)

-- | The name of the application version.
getApplicationVersionResponse_name :: Lens.Lens' GetApplicationVersionResponse Prelude.Text
getApplicationVersionResponse_name = Lens.lens (\GetApplicationVersionResponse' {name} -> name) (\s@GetApplicationVersionResponse' {} a -> s {name = a} :: GetApplicationVersionResponse)

-- | The status of the application version.
getApplicationVersionResponse_status :: Lens.Lens' GetApplicationVersionResponse ApplicationVersionLifecycle
getApplicationVersionResponse_status = Lens.lens (\GetApplicationVersionResponse' {status} -> status) (\s@GetApplicationVersionResponse' {} a -> s {status = a} :: GetApplicationVersionResponse)

instance Prelude.NFData GetApplicationVersionResponse where
  rnf GetApplicationVersionResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf definitionContent
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
