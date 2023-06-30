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
-- Module      : Amazonka.Evidently.DeleteLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Evidently launch. The feature used for the launch is not
-- deleted.
--
-- To stop a launch without deleting it, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_StopLaunch.html StopLaunch>.
module Amazonka.Evidently.DeleteLaunch
  ( -- * Creating a Request
    DeleteLaunch (..),
    newDeleteLaunch,

    -- * Request Lenses
    deleteLaunch_launch,
    deleteLaunch_project,

    -- * Destructuring the Response
    DeleteLaunchResponse (..),
    newDeleteLaunchResponse,

    -- * Response Lenses
    deleteLaunchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLaunch' smart constructor.
data DeleteLaunch = DeleteLaunch'
  { -- | The name of the launch to delete.
    launch :: Prelude.Text,
    -- | The name or ARN of the project that contains the launch to delete.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launch', 'deleteLaunch_launch' - The name of the launch to delete.
--
-- 'project', 'deleteLaunch_project' - The name or ARN of the project that contains the launch to delete.
newDeleteLaunch ::
  -- | 'launch'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  DeleteLaunch
newDeleteLaunch pLaunch_ pProject_ =
  DeleteLaunch'
    { launch = pLaunch_,
      project = pProject_
    }

-- | The name of the launch to delete.
deleteLaunch_launch :: Lens.Lens' DeleteLaunch Prelude.Text
deleteLaunch_launch = Lens.lens (\DeleteLaunch' {launch} -> launch) (\s@DeleteLaunch' {} a -> s {launch = a} :: DeleteLaunch)

-- | The name or ARN of the project that contains the launch to delete.
deleteLaunch_project :: Lens.Lens' DeleteLaunch Prelude.Text
deleteLaunch_project = Lens.lens (\DeleteLaunch' {project} -> project) (\s@DeleteLaunch' {} a -> s {project = a} :: DeleteLaunch)

instance Core.AWSRequest DeleteLaunch where
  type AWSResponse DeleteLaunch = DeleteLaunchResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLaunchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLaunch where
  hashWithSalt _salt DeleteLaunch' {..} =
    _salt
      `Prelude.hashWithSalt` launch
      `Prelude.hashWithSalt` project

instance Prelude.NFData DeleteLaunch where
  rnf DeleteLaunch' {..} =
    Prelude.rnf launch
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders DeleteLaunch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteLaunch where
  toPath DeleteLaunch' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/launches/",
        Data.toBS launch
      ]

instance Data.ToQuery DeleteLaunch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLaunchResponse' smart constructor.
data DeleteLaunchResponse = DeleteLaunchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLaunchResponse_httpStatus' - The response's http status code.
newDeleteLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLaunchResponse
newDeleteLaunchResponse pHttpStatus_ =
  DeleteLaunchResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLaunchResponse_httpStatus :: Lens.Lens' DeleteLaunchResponse Prelude.Int
deleteLaunchResponse_httpStatus = Lens.lens (\DeleteLaunchResponse' {httpStatus} -> httpStatus) (\s@DeleteLaunchResponse' {} a -> s {httpStatus = a} :: DeleteLaunchResponse)

instance Prelude.NFData DeleteLaunchResponse where
  rnf DeleteLaunchResponse' {..} =
    Prelude.rnf httpStatus
