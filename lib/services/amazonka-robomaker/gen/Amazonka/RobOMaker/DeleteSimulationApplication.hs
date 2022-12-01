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
-- Module      : Amazonka.RobOMaker.DeleteSimulationApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a simulation application.
module Amazonka.RobOMaker.DeleteSimulationApplication
  ( -- * Creating a Request
    DeleteSimulationApplication (..),
    newDeleteSimulationApplication,

    -- * Request Lenses
    deleteSimulationApplication_applicationVersion,
    deleteSimulationApplication_application,

    -- * Destructuring the Response
    DeleteSimulationApplicationResponse (..),
    newDeleteSimulationApplicationResponse,

    -- * Response Lenses
    deleteSimulationApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDeleteSimulationApplication' smart constructor.
data DeleteSimulationApplication = DeleteSimulationApplication'
  { -- | The version of the simulation application to delete.
    applicationVersion :: Prelude.Maybe Prelude.Text,
    -- | The application information for the simulation application to delete.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSimulationApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersion', 'deleteSimulationApplication_applicationVersion' - The version of the simulation application to delete.
--
-- 'application', 'deleteSimulationApplication_application' - The application information for the simulation application to delete.
newDeleteSimulationApplication ::
  -- | 'application'
  Prelude.Text ->
  DeleteSimulationApplication
newDeleteSimulationApplication pApplication_ =
  DeleteSimulationApplication'
    { applicationVersion =
        Prelude.Nothing,
      application = pApplication_
    }

-- | The version of the simulation application to delete.
deleteSimulationApplication_applicationVersion :: Lens.Lens' DeleteSimulationApplication (Prelude.Maybe Prelude.Text)
deleteSimulationApplication_applicationVersion = Lens.lens (\DeleteSimulationApplication' {applicationVersion} -> applicationVersion) (\s@DeleteSimulationApplication' {} a -> s {applicationVersion = a} :: DeleteSimulationApplication)

-- | The application information for the simulation application to delete.
deleteSimulationApplication_application :: Lens.Lens' DeleteSimulationApplication Prelude.Text
deleteSimulationApplication_application = Lens.lens (\DeleteSimulationApplication' {application} -> application) (\s@DeleteSimulationApplication' {} a -> s {application = a} :: DeleteSimulationApplication)

instance Core.AWSRequest DeleteSimulationApplication where
  type
    AWSResponse DeleteSimulationApplication =
      DeleteSimulationApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSimulationApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSimulationApplication where
  hashWithSalt _salt DeleteSimulationApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` application

instance Prelude.NFData DeleteSimulationApplication where
  rnf DeleteSimulationApplication' {..} =
    Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf application

instance Core.ToHeaders DeleteSimulationApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteSimulationApplication where
  toJSON DeleteSimulationApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("applicationVersion" Core..=)
              Prelude.<$> applicationVersion,
            Prelude.Just ("application" Core..= application)
          ]
      )

instance Core.ToPath DeleteSimulationApplication where
  toPath = Prelude.const "/deleteSimulationApplication"

instance Core.ToQuery DeleteSimulationApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSimulationApplicationResponse' smart constructor.
data DeleteSimulationApplicationResponse = DeleteSimulationApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSimulationApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSimulationApplicationResponse_httpStatus' - The response's http status code.
newDeleteSimulationApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSimulationApplicationResponse
newDeleteSimulationApplicationResponse pHttpStatus_ =
  DeleteSimulationApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSimulationApplicationResponse_httpStatus :: Lens.Lens' DeleteSimulationApplicationResponse Prelude.Int
deleteSimulationApplicationResponse_httpStatus = Lens.lens (\DeleteSimulationApplicationResponse' {httpStatus} -> httpStatus) (\s@DeleteSimulationApplicationResponse' {} a -> s {httpStatus = a} :: DeleteSimulationApplicationResponse)

instance
  Prelude.NFData
    DeleteSimulationApplicationResponse
  where
  rnf DeleteSimulationApplicationResponse' {..} =
    Prelude.rnf httpStatus
