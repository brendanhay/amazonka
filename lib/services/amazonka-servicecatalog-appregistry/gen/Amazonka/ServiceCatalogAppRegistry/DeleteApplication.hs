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
-- Module      : Amazonka.ServiceCatalogAppRegistry.DeleteApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application that is specified either by its application ID,
-- name, or ARN. All associated attribute groups and resources must be
-- disassociated from it before deleting an application.
module Amazonka.ServiceCatalogAppRegistry.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_application,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,

    -- * Response Lenses
    deleteApplicationResponse_application,
    deleteApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The name, ID, or ARN of the application.
    application :: Prelude.Text
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
-- 'application', 'deleteApplication_application' - The name, ID, or ARN of the application.
newDeleteApplication ::
  -- | 'application'
  Prelude.Text ->
  DeleteApplication
newDeleteApplication pApplication_ =
  DeleteApplication' {application = pApplication_}

-- | The name, ID, or ARN of the application.
deleteApplication_application :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_application = Lens.lens (\DeleteApplication' {application} -> application) (\s@DeleteApplication' {} a -> s {application = a} :: DeleteApplication)

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
            Prelude.<$> (x Data..?> "application")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplication where
  hashWithSalt _salt DeleteApplication' {..} =
    _salt `Prelude.hashWithSalt` application

instance Prelude.NFData DeleteApplication where
  rnf DeleteApplication' {..} = Prelude.rnf application

instance Data.ToHeaders DeleteApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApplication where
  toPath DeleteApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS application]

instance Data.ToQuery DeleteApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  { -- | Information about the deleted application.
    application :: Prelude.Maybe ApplicationSummary,
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
-- 'application', 'deleteApplicationResponse_application' - Information about the deleted application.
--
-- 'httpStatus', 'deleteApplicationResponse_httpStatus' - The response's http status code.
newDeleteApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationResponse
newDeleteApplicationResponse pHttpStatus_ =
  DeleteApplicationResponse'
    { application =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted application.
deleteApplicationResponse_application :: Lens.Lens' DeleteApplicationResponse (Prelude.Maybe ApplicationSummary)
deleteApplicationResponse_application = Lens.lens (\DeleteApplicationResponse' {application} -> application) (\s@DeleteApplicationResponse' {} a -> s {application = a} :: DeleteApplicationResponse)

-- | The response's http status code.
deleteApplicationResponse_httpStatus :: Lens.Lens' DeleteApplicationResponse Prelude.Int
deleteApplicationResponse_httpStatus = Lens.lens (\DeleteApplicationResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationResponse' {} a -> s {httpStatus = a} :: DeleteApplicationResponse)

instance Prelude.NFData DeleteApplicationResponse where
  rnf DeleteApplicationResponse' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf httpStatus
