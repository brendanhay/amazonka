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
-- Module      : Network.AWS.CodeBuild.DeleteBuildBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch build.
module Network.AWS.CodeBuild.DeleteBuildBatch
  ( -- * Creating a Request
    DeleteBuildBatch (..),
    newDeleteBuildBatch,

    -- * Request Lenses
    deleteBuildBatch_id,

    -- * Destructuring the Response
    DeleteBuildBatchResponse (..),
    newDeleteBuildBatchResponse,

    -- * Response Lenses
    deleteBuildBatchResponse_statusCode,
    deleteBuildBatchResponse_buildsDeleted,
    deleteBuildBatchResponse_buildsNotDeleted,
    deleteBuildBatchResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBuildBatch' smart constructor.
data DeleteBuildBatch = DeleteBuildBatch'
  { -- | The identifier of the batch build to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBuildBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteBuildBatch_id' - The identifier of the batch build to delete.
newDeleteBuildBatch ::
  -- | 'id'
  Prelude.Text ->
  DeleteBuildBatch
newDeleteBuildBatch pId_ =
  DeleteBuildBatch' {id = pId_}

-- | The identifier of the batch build to delete.
deleteBuildBatch_id :: Lens.Lens' DeleteBuildBatch Prelude.Text
deleteBuildBatch_id = Lens.lens (\DeleteBuildBatch' {id} -> id) (\s@DeleteBuildBatch' {} a -> s {id = a} :: DeleteBuildBatch)

instance Core.AWSRequest DeleteBuildBatch where
  type
    AWSResponse DeleteBuildBatch =
      DeleteBuildBatchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBuildBatchResponse'
            Prelude.<$> (x Core..?> "statusCode")
            Prelude.<*> (x Core..?> "buildsDeleted")
            Prelude.<*> ( x Core..?> "buildsNotDeleted"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBuildBatch

instance Prelude.NFData DeleteBuildBatch

instance Core.ToHeaders DeleteBuildBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DeleteBuildBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteBuildBatch where
  toJSON DeleteBuildBatch' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath DeleteBuildBatch where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteBuildBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBuildBatchResponse' smart constructor.
data DeleteBuildBatchResponse = DeleteBuildBatchResponse'
  { -- | The status code.
    statusCode :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that contain the identifiers of the builds that were
    -- deleted.
    buildsDeleted :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An array of @BuildNotDeleted@ objects that specify the builds that could
    -- not be deleted.
    buildsNotDeleted :: Prelude.Maybe [BuildNotDeleted],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBuildBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCode', 'deleteBuildBatchResponse_statusCode' - The status code.
--
-- 'buildsDeleted', 'deleteBuildBatchResponse_buildsDeleted' - An array of strings that contain the identifiers of the builds that were
-- deleted.
--
-- 'buildsNotDeleted', 'deleteBuildBatchResponse_buildsNotDeleted' - An array of @BuildNotDeleted@ objects that specify the builds that could
-- not be deleted.
--
-- 'httpStatus', 'deleteBuildBatchResponse_httpStatus' - The response's http status code.
newDeleteBuildBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBuildBatchResponse
newDeleteBuildBatchResponse pHttpStatus_ =
  DeleteBuildBatchResponse'
    { statusCode =
        Prelude.Nothing,
      buildsDeleted = Prelude.Nothing,
      buildsNotDeleted = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status code.
deleteBuildBatchResponse_statusCode :: Lens.Lens' DeleteBuildBatchResponse (Prelude.Maybe Prelude.Text)
deleteBuildBatchResponse_statusCode = Lens.lens (\DeleteBuildBatchResponse' {statusCode} -> statusCode) (\s@DeleteBuildBatchResponse' {} a -> s {statusCode = a} :: DeleteBuildBatchResponse)

-- | An array of strings that contain the identifiers of the builds that were
-- deleted.
deleteBuildBatchResponse_buildsDeleted :: Lens.Lens' DeleteBuildBatchResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
deleteBuildBatchResponse_buildsDeleted = Lens.lens (\DeleteBuildBatchResponse' {buildsDeleted} -> buildsDeleted) (\s@DeleteBuildBatchResponse' {} a -> s {buildsDeleted = a} :: DeleteBuildBatchResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An array of @BuildNotDeleted@ objects that specify the builds that could
-- not be deleted.
deleteBuildBatchResponse_buildsNotDeleted :: Lens.Lens' DeleteBuildBatchResponse (Prelude.Maybe [BuildNotDeleted])
deleteBuildBatchResponse_buildsNotDeleted = Lens.lens (\DeleteBuildBatchResponse' {buildsNotDeleted} -> buildsNotDeleted) (\s@DeleteBuildBatchResponse' {} a -> s {buildsNotDeleted = a} :: DeleteBuildBatchResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteBuildBatchResponse_httpStatus :: Lens.Lens' DeleteBuildBatchResponse Prelude.Int
deleteBuildBatchResponse_httpStatus = Lens.lens (\DeleteBuildBatchResponse' {httpStatus} -> httpStatus) (\s@DeleteBuildBatchResponse' {} a -> s {httpStatus = a} :: DeleteBuildBatchResponse)

instance Prelude.NFData DeleteBuildBatchResponse
