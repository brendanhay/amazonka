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
-- Module      : Amazonka.MigrationHubReFactorSpaces.DeleteApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Web Services Migration Hub Refactor Spaces
-- application. Before you can delete an application, you must first delete
-- any services or routes within the application.
module Amazonka.MigrationHubReFactorSpaces.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_applicationIdentifier,
    deleteApplication_environmentIdentifier,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,

    -- * Response Lenses
    deleteApplicationResponse_name,
    deleteApplicationResponse_arn,
    deleteApplicationResponse_state,
    deleteApplicationResponse_lastUpdatedTime,
    deleteApplicationResponse_environmentId,
    deleteApplicationResponse_applicationId,
    deleteApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The ID of the application.
    applicationIdentifier :: Prelude.Text,
    -- | The ID of the environment.
    environmentIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIdentifier', 'deleteApplication_applicationIdentifier' - The ID of the application.
--
-- 'environmentIdentifier', 'deleteApplication_environmentIdentifier' - The ID of the environment.
newDeleteApplication ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  DeleteApplication
newDeleteApplication
  pApplicationIdentifier_
  pEnvironmentIdentifier_ =
    DeleteApplication'
      { applicationIdentifier =
          pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_
      }

-- | The ID of the application.
deleteApplication_applicationIdentifier :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_applicationIdentifier = Lens.lens (\DeleteApplication' {applicationIdentifier} -> applicationIdentifier) (\s@DeleteApplication' {} a -> s {applicationIdentifier = a} :: DeleteApplication)

-- | The ID of the environment.
deleteApplication_environmentIdentifier :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_environmentIdentifier = Lens.lens (\DeleteApplication' {environmentIdentifier} -> environmentIdentifier) (\s@DeleteApplication' {} a -> s {environmentIdentifier = a} :: DeleteApplication)

instance Core.AWSRequest DeleteApplication where
  type
    AWSResponse DeleteApplication =
      DeleteApplicationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApplicationResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "LastUpdatedTime")
            Prelude.<*> (x Core..?> "EnvironmentId")
            Prelude.<*> (x Core..?> "ApplicationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplication where
  hashWithSalt _salt DeleteApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier

instance Prelude.NFData DeleteApplication where
  rnf DeleteApplication' {..} =
    Prelude.rnf applicationIdentifier
      `Prelude.seq` Prelude.rnf environmentIdentifier

instance Core.ToHeaders DeleteApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteApplication where
  toPath DeleteApplication' {..} =
    Prelude.mconcat
      [ "/environments/",
        Core.toBS environmentIdentifier,
        "/applications/",
        Core.toBS applicationIdentifier
      ]

instance Core.ToQuery DeleteApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  { -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the application.
    state :: Prelude.Maybe ApplicationState,
    -- | A timestamp that indicates when the environment was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the application’s environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteApplicationResponse_name' - The name of the application.
--
-- 'arn', 'deleteApplicationResponse_arn' - The Amazon Resource Name (ARN) of the application.
--
-- 'state', 'deleteApplicationResponse_state' - The current state of the application.
--
-- 'lastUpdatedTime', 'deleteApplicationResponse_lastUpdatedTime' - A timestamp that indicates when the environment was last updated.
--
-- 'environmentId', 'deleteApplicationResponse_environmentId' - The unique identifier of the application’s environment.
--
-- 'applicationId', 'deleteApplicationResponse_applicationId' - The ID of the application.
--
-- 'httpStatus', 'deleteApplicationResponse_httpStatus' - The response's http status code.
newDeleteApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationResponse
newDeleteApplicationResponse pHttpStatus_ =
  DeleteApplicationResponse'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the application.
deleteApplicationResponse_name :: Lens.Lens' DeleteApplicationResponse (Prelude.Maybe Prelude.Text)
deleteApplicationResponse_name = Lens.lens (\DeleteApplicationResponse' {name} -> name) (\s@DeleteApplicationResponse' {} a -> s {name = a} :: DeleteApplicationResponse)

-- | The Amazon Resource Name (ARN) of the application.
deleteApplicationResponse_arn :: Lens.Lens' DeleteApplicationResponse (Prelude.Maybe Prelude.Text)
deleteApplicationResponse_arn = Lens.lens (\DeleteApplicationResponse' {arn} -> arn) (\s@DeleteApplicationResponse' {} a -> s {arn = a} :: DeleteApplicationResponse)

-- | The current state of the application.
deleteApplicationResponse_state :: Lens.Lens' DeleteApplicationResponse (Prelude.Maybe ApplicationState)
deleteApplicationResponse_state = Lens.lens (\DeleteApplicationResponse' {state} -> state) (\s@DeleteApplicationResponse' {} a -> s {state = a} :: DeleteApplicationResponse)

-- | A timestamp that indicates when the environment was last updated.
deleteApplicationResponse_lastUpdatedTime :: Lens.Lens' DeleteApplicationResponse (Prelude.Maybe Prelude.UTCTime)
deleteApplicationResponse_lastUpdatedTime = Lens.lens (\DeleteApplicationResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DeleteApplicationResponse' {} a -> s {lastUpdatedTime = a} :: DeleteApplicationResponse) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the application’s environment.
deleteApplicationResponse_environmentId :: Lens.Lens' DeleteApplicationResponse (Prelude.Maybe Prelude.Text)
deleteApplicationResponse_environmentId = Lens.lens (\DeleteApplicationResponse' {environmentId} -> environmentId) (\s@DeleteApplicationResponse' {} a -> s {environmentId = a} :: DeleteApplicationResponse)

-- | The ID of the application.
deleteApplicationResponse_applicationId :: Lens.Lens' DeleteApplicationResponse (Prelude.Maybe Prelude.Text)
deleteApplicationResponse_applicationId = Lens.lens (\DeleteApplicationResponse' {applicationId} -> applicationId) (\s@DeleteApplicationResponse' {} a -> s {applicationId = a} :: DeleteApplicationResponse)

-- | The response's http status code.
deleteApplicationResponse_httpStatus :: Lens.Lens' DeleteApplicationResponse Prelude.Int
deleteApplicationResponse_httpStatus = Lens.lens (\DeleteApplicationResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationResponse' {} a -> s {httpStatus = a} :: DeleteApplicationResponse)

instance Prelude.NFData DeleteApplicationResponse where
  rnf DeleteApplicationResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf httpStatus
