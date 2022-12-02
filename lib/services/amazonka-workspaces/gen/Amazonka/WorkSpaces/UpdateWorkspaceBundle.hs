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
-- Module      : Amazonka.WorkSpaces.UpdateWorkspaceBundle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a WorkSpace bundle with a new image. For more information about
-- updating WorkSpace bundles, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-custom-bundle.html Update a Custom WorkSpaces Bundle>.
--
-- Existing WorkSpaces aren\'t automatically updated when you update the
-- bundle that they\'re based on. To update existing WorkSpaces that are
-- based on a bundle that you\'ve updated, you must either rebuild the
-- WorkSpaces or delete and recreate them.
module Amazonka.WorkSpaces.UpdateWorkspaceBundle
  ( -- * Creating a Request
    UpdateWorkspaceBundle (..),
    newUpdateWorkspaceBundle,

    -- * Request Lenses
    updateWorkspaceBundle_bundleId,
    updateWorkspaceBundle_imageId,

    -- * Destructuring the Response
    UpdateWorkspaceBundleResponse (..),
    newUpdateWorkspaceBundleResponse,

    -- * Response Lenses
    updateWorkspaceBundleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newUpdateWorkspaceBundle' smart constructor.
data UpdateWorkspaceBundle = UpdateWorkspaceBundle'
  { -- | The identifier of the bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the image.
    imageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleId', 'updateWorkspaceBundle_bundleId' - The identifier of the bundle.
--
-- 'imageId', 'updateWorkspaceBundle_imageId' - The identifier of the image.
newUpdateWorkspaceBundle ::
  UpdateWorkspaceBundle
newUpdateWorkspaceBundle =
  UpdateWorkspaceBundle'
    { bundleId = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | The identifier of the bundle.
updateWorkspaceBundle_bundleId :: Lens.Lens' UpdateWorkspaceBundle (Prelude.Maybe Prelude.Text)
updateWorkspaceBundle_bundleId = Lens.lens (\UpdateWorkspaceBundle' {bundleId} -> bundleId) (\s@UpdateWorkspaceBundle' {} a -> s {bundleId = a} :: UpdateWorkspaceBundle)

-- | The identifier of the image.
updateWorkspaceBundle_imageId :: Lens.Lens' UpdateWorkspaceBundle (Prelude.Maybe Prelude.Text)
updateWorkspaceBundle_imageId = Lens.lens (\UpdateWorkspaceBundle' {imageId} -> imageId) (\s@UpdateWorkspaceBundle' {} a -> s {imageId = a} :: UpdateWorkspaceBundle)

instance Core.AWSRequest UpdateWorkspaceBundle where
  type
    AWSResponse UpdateWorkspaceBundle =
      UpdateWorkspaceBundleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWorkspaceBundleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkspaceBundle where
  hashWithSalt _salt UpdateWorkspaceBundle' {..} =
    _salt `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData UpdateWorkspaceBundle where
  rnf UpdateWorkspaceBundle' {..} =
    Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders UpdateWorkspaceBundle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.UpdateWorkspaceBundle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkspaceBundle where
  toJSON UpdateWorkspaceBundle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BundleId" Data..=) Prelude.<$> bundleId,
            ("ImageId" Data..=) Prelude.<$> imageId
          ]
      )

instance Data.ToPath UpdateWorkspaceBundle where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWorkspaceBundle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceBundleResponse' smart constructor.
data UpdateWorkspaceBundleResponse = UpdateWorkspaceBundleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkspaceBundleResponse_httpStatus' - The response's http status code.
newUpdateWorkspaceBundleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkspaceBundleResponse
newUpdateWorkspaceBundleResponse pHttpStatus_ =
  UpdateWorkspaceBundleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateWorkspaceBundleResponse_httpStatus :: Lens.Lens' UpdateWorkspaceBundleResponse Prelude.Int
updateWorkspaceBundleResponse_httpStatus = Lens.lens (\UpdateWorkspaceBundleResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkspaceBundleResponse' {} a -> s {httpStatus = a} :: UpdateWorkspaceBundleResponse)

instance Prelude.NFData UpdateWorkspaceBundleResponse where
  rnf UpdateWorkspaceBundleResponse' {..} =
    Prelude.rnf httpStatus
