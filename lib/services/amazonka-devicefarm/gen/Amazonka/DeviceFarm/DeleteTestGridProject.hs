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
-- Module      : Amazonka.DeviceFarm.DeleteTestGridProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Selenium testing project and all content generated under it.
--
-- You cannot undo this operation.
--
-- You cannot delete a project if it has active sessions.
module Amazonka.DeviceFarm.DeleteTestGridProject
  ( -- * Creating a Request
    DeleteTestGridProject (..),
    newDeleteTestGridProject,

    -- * Request Lenses
    deleteTestGridProject_projectArn,

    -- * Destructuring the Response
    DeleteTestGridProjectResponse (..),
    newDeleteTestGridProjectResponse,

    -- * Response Lenses
    deleteTestGridProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTestGridProject' smart constructor.
data DeleteTestGridProject = DeleteTestGridProject'
  { -- | The ARN of the project to delete, from CreateTestGridProject or
    -- ListTestGridProjects.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectArn', 'deleteTestGridProject_projectArn' - The ARN of the project to delete, from CreateTestGridProject or
-- ListTestGridProjects.
newDeleteTestGridProject ::
  -- | 'projectArn'
  Prelude.Text ->
  DeleteTestGridProject
newDeleteTestGridProject pProjectArn_ =
  DeleteTestGridProject' {projectArn = pProjectArn_}

-- | The ARN of the project to delete, from CreateTestGridProject or
-- ListTestGridProjects.
deleteTestGridProject_projectArn :: Lens.Lens' DeleteTestGridProject Prelude.Text
deleteTestGridProject_projectArn = Lens.lens (\DeleteTestGridProject' {projectArn} -> projectArn) (\s@DeleteTestGridProject' {} a -> s {projectArn = a} :: DeleteTestGridProject)

instance Core.AWSRequest DeleteTestGridProject where
  type
    AWSResponse DeleteTestGridProject =
      DeleteTestGridProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTestGridProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTestGridProject where
  hashWithSalt _salt DeleteTestGridProject' {..} =
    _salt `Prelude.hashWithSalt` projectArn

instance Prelude.NFData DeleteTestGridProject where
  rnf DeleteTestGridProject' {..} =
    Prelude.rnf projectArn

instance Data.ToHeaders DeleteTestGridProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.DeleteTestGridProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTestGridProject where
  toJSON DeleteTestGridProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("projectArn" Data..= projectArn)]
      )

instance Data.ToPath DeleteTestGridProject where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTestGridProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTestGridProjectResponse' smart constructor.
data DeleteTestGridProjectResponse = DeleteTestGridProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTestGridProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTestGridProjectResponse_httpStatus' - The response's http status code.
newDeleteTestGridProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTestGridProjectResponse
newDeleteTestGridProjectResponse pHttpStatus_ =
  DeleteTestGridProjectResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTestGridProjectResponse_httpStatus :: Lens.Lens' DeleteTestGridProjectResponse Prelude.Int
deleteTestGridProjectResponse_httpStatus = Lens.lens (\DeleteTestGridProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteTestGridProjectResponse' {} a -> s {httpStatus = a} :: DeleteTestGridProjectResponse)

instance Prelude.NFData DeleteTestGridProjectResponse where
  rnf DeleteTestGridProjectResponse' {..} =
    Prelude.rnf httpStatus
