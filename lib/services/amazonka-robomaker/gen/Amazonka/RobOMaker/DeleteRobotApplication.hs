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
-- Module      : Amazonka.RobOMaker.DeleteRobotApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a robot application.
module Amazonka.RobOMaker.DeleteRobotApplication
  ( -- * Creating a Request
    DeleteRobotApplication (..),
    newDeleteRobotApplication,

    -- * Request Lenses
    deleteRobotApplication_applicationVersion,
    deleteRobotApplication_application,

    -- * Destructuring the Response
    DeleteRobotApplicationResponse (..),
    newDeleteRobotApplicationResponse,

    -- * Response Lenses
    deleteRobotApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDeleteRobotApplication' smart constructor.
data DeleteRobotApplication = DeleteRobotApplication'
  { -- | The version of the robot application to delete.
    applicationVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the the robot application.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRobotApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersion', 'deleteRobotApplication_applicationVersion' - The version of the robot application to delete.
--
-- 'application', 'deleteRobotApplication_application' - The Amazon Resource Name (ARN) of the the robot application.
newDeleteRobotApplication ::
  -- | 'application'
  Prelude.Text ->
  DeleteRobotApplication
newDeleteRobotApplication pApplication_ =
  DeleteRobotApplication'
    { applicationVersion =
        Prelude.Nothing,
      application = pApplication_
    }

-- | The version of the robot application to delete.
deleteRobotApplication_applicationVersion :: Lens.Lens' DeleteRobotApplication (Prelude.Maybe Prelude.Text)
deleteRobotApplication_applicationVersion = Lens.lens (\DeleteRobotApplication' {applicationVersion} -> applicationVersion) (\s@DeleteRobotApplication' {} a -> s {applicationVersion = a} :: DeleteRobotApplication)

-- | The Amazon Resource Name (ARN) of the the robot application.
deleteRobotApplication_application :: Lens.Lens' DeleteRobotApplication Prelude.Text
deleteRobotApplication_application = Lens.lens (\DeleteRobotApplication' {application} -> application) (\s@DeleteRobotApplication' {} a -> s {application = a} :: DeleteRobotApplication)

instance Core.AWSRequest DeleteRobotApplication where
  type
    AWSResponse DeleteRobotApplication =
      DeleteRobotApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRobotApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRobotApplication where
  hashWithSalt _salt DeleteRobotApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` application

instance Prelude.NFData DeleteRobotApplication where
  rnf DeleteRobotApplication' {..} =
    Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf application

instance Data.ToHeaders DeleteRobotApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRobotApplication where
  toJSON DeleteRobotApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationVersion" Data..=)
              Prelude.<$> applicationVersion,
            Prelude.Just ("application" Data..= application)
          ]
      )

instance Data.ToPath DeleteRobotApplication where
  toPath = Prelude.const "/deleteRobotApplication"

instance Data.ToQuery DeleteRobotApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRobotApplicationResponse' smart constructor.
data DeleteRobotApplicationResponse = DeleteRobotApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRobotApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRobotApplicationResponse_httpStatus' - The response's http status code.
newDeleteRobotApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRobotApplicationResponse
newDeleteRobotApplicationResponse pHttpStatus_ =
  DeleteRobotApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRobotApplicationResponse_httpStatus :: Lens.Lens' DeleteRobotApplicationResponse Prelude.Int
deleteRobotApplicationResponse_httpStatus = Lens.lens (\DeleteRobotApplicationResponse' {httpStatus} -> httpStatus) (\s@DeleteRobotApplicationResponse' {} a -> s {httpStatus = a} :: DeleteRobotApplicationResponse)

instance
  Prelude.NFData
    DeleteRobotApplicationResponse
  where
  rnf DeleteRobotApplicationResponse' {..} =
    Prelude.rnf httpStatus
