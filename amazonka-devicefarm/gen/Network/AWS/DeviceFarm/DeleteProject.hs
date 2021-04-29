{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.DeleteProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Device Farm project, given the project ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Network.AWS.DeviceFarm.DeleteProject
  ( -- * Creating a Request
    DeleteProject (..),
    newDeleteProject,

    -- * Request Lenses
    deleteProject_arn,

    -- * Destructuring the Response
    DeleteProjectResponse (..),
    newDeleteProjectResponse,

    -- * Response Lenses
    deleteProjectResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete project operation.
--
-- /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm project to
    -- delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteProject_arn' - Represents the Amazon Resource Name (ARN) of the Device Farm project to
-- delete.
newDeleteProject ::
  -- | 'arn'
  Prelude.Text ->
  DeleteProject
newDeleteProject pArn_ = DeleteProject' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm project to
-- delete.
deleteProject_arn :: Lens.Lens' DeleteProject Prelude.Text
deleteProject_arn = Lens.lens (\DeleteProject' {arn} -> arn) (\s@DeleteProject' {} a -> s {arn = a} :: DeleteProject)

instance Prelude.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProject

instance Prelude.NFData DeleteProject

instance Prelude.ToHeaders DeleteProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.DeleteProject" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath DeleteProject where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProject where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a delete project request.
--
-- /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProjectResponse_httpStatus' - The response's http status code.
newDeleteProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Prelude.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Prelude.NFData DeleteProjectResponse
