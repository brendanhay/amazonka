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
-- Module      : Amazonka.WorkSpaces.DeleteWorkspaceBundle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified WorkSpace bundle. For more information about
-- deleting WorkSpace bundles, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/delete_bundle.html Delete a Custom WorkSpaces Bundle or Image>.
module Amazonka.WorkSpaces.DeleteWorkspaceBundle
  ( -- * Creating a Request
    DeleteWorkspaceBundle (..),
    newDeleteWorkspaceBundle,

    -- * Request Lenses
    deleteWorkspaceBundle_bundleId,

    -- * Destructuring the Response
    DeleteWorkspaceBundleResponse (..),
    newDeleteWorkspaceBundleResponse,

    -- * Response Lenses
    deleteWorkspaceBundleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDeleteWorkspaceBundle' smart constructor.
data DeleteWorkspaceBundle = DeleteWorkspaceBundle'
  { -- | The identifier of the bundle.
    bundleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'deleteWorkspaceBundle_bundleId' - The identifier of the bundle.
newDeleteWorkspaceBundle ::
  DeleteWorkspaceBundle
newDeleteWorkspaceBundle =
  DeleteWorkspaceBundle' {bundleId = Prelude.Nothing}

-- | The identifier of the bundle.
deleteWorkspaceBundle_bundleId :: Lens.Lens' DeleteWorkspaceBundle (Prelude.Maybe Prelude.Text)
deleteWorkspaceBundle_bundleId = Lens.lens (\DeleteWorkspaceBundle' {bundleId} -> bundleId) (\s@DeleteWorkspaceBundle' {} a -> s {bundleId = a} :: DeleteWorkspaceBundle)

instance Core.AWSRequest DeleteWorkspaceBundle where
  type
    AWSResponse DeleteWorkspaceBundle =
      DeleteWorkspaceBundleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkspaceBundleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkspaceBundle where
  hashWithSalt _salt DeleteWorkspaceBundle' {..} =
    _salt `Prelude.hashWithSalt` bundleId

instance Prelude.NFData DeleteWorkspaceBundle where
  rnf DeleteWorkspaceBundle' {..} = Prelude.rnf bundleId

instance Data.ToHeaders DeleteWorkspaceBundle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DeleteWorkspaceBundle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWorkspaceBundle where
  toJSON DeleteWorkspaceBundle' {..} =
    Data.object
      ( Prelude.catMaybes
          [("BundleId" Data..=) Prelude.<$> bundleId]
      )

instance Data.ToPath DeleteWorkspaceBundle where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWorkspaceBundle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkspaceBundleResponse' smart constructor.
data DeleteWorkspaceBundleResponse = DeleteWorkspaceBundleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkspaceBundleResponse_httpStatus' - The response's http status code.
newDeleteWorkspaceBundleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkspaceBundleResponse
newDeleteWorkspaceBundleResponse pHttpStatus_ =
  DeleteWorkspaceBundleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorkspaceBundleResponse_httpStatus :: Lens.Lens' DeleteWorkspaceBundleResponse Prelude.Int
deleteWorkspaceBundleResponse_httpStatus = Lens.lens (\DeleteWorkspaceBundleResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkspaceBundleResponse' {} a -> s {httpStatus = a} :: DeleteWorkspaceBundleResponse)

instance Prelude.NFData DeleteWorkspaceBundleResponse where
  rnf DeleteWorkspaceBundleResponse' {..} =
    Prelude.rnf httpStatus
