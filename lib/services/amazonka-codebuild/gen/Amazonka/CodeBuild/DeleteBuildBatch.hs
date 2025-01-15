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
-- Module      : Amazonka.CodeBuild.DeleteBuildBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch build.
module Amazonka.CodeBuild.DeleteBuildBatch
  ( -- * Creating a Request
    DeleteBuildBatch (..),
    newDeleteBuildBatch,

    -- * Request Lenses
    deleteBuildBatch_id,

    -- * Destructuring the Response
    DeleteBuildBatchResponse (..),
    newDeleteBuildBatchResponse,

    -- * Response Lenses
    deleteBuildBatchResponse_buildsDeleted,
    deleteBuildBatchResponse_buildsNotDeleted,
    deleteBuildBatchResponse_statusCode,
    deleteBuildBatchResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBuildBatchResponse'
            Prelude.<$> (x Data..?> "buildsDeleted")
            Prelude.<*> ( x
                            Data..?> "buildsNotDeleted"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "statusCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBuildBatch where
  hashWithSalt _salt DeleteBuildBatch' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteBuildBatch where
  rnf DeleteBuildBatch' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteBuildBatch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.DeleteBuildBatch" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBuildBatch where
  toJSON DeleteBuildBatch' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance Data.ToPath DeleteBuildBatch where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBuildBatch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBuildBatchResponse' smart constructor.
data DeleteBuildBatchResponse = DeleteBuildBatchResponse'
  { -- | An array of strings that contain the identifiers of the builds that were
    -- deleted.
    buildsDeleted :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An array of @BuildNotDeleted@ objects that specify the builds that could
    -- not be deleted.
    buildsNotDeleted :: Prelude.Maybe [BuildNotDeleted],
    -- | The status code.
    statusCode :: Prelude.Maybe Prelude.Text,
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
-- 'buildsDeleted', 'deleteBuildBatchResponse_buildsDeleted' - An array of strings that contain the identifiers of the builds that were
-- deleted.
--
-- 'buildsNotDeleted', 'deleteBuildBatchResponse_buildsNotDeleted' - An array of @BuildNotDeleted@ objects that specify the builds that could
-- not be deleted.
--
-- 'statusCode', 'deleteBuildBatchResponse_statusCode' - The status code.
--
-- 'httpStatus', 'deleteBuildBatchResponse_httpStatus' - The response's http status code.
newDeleteBuildBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBuildBatchResponse
newDeleteBuildBatchResponse pHttpStatus_ =
  DeleteBuildBatchResponse'
    { buildsDeleted =
        Prelude.Nothing,
      buildsNotDeleted = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of strings that contain the identifiers of the builds that were
-- deleted.
deleteBuildBatchResponse_buildsDeleted :: Lens.Lens' DeleteBuildBatchResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
deleteBuildBatchResponse_buildsDeleted = Lens.lens (\DeleteBuildBatchResponse' {buildsDeleted} -> buildsDeleted) (\s@DeleteBuildBatchResponse' {} a -> s {buildsDeleted = a} :: DeleteBuildBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of @BuildNotDeleted@ objects that specify the builds that could
-- not be deleted.
deleteBuildBatchResponse_buildsNotDeleted :: Lens.Lens' DeleteBuildBatchResponse (Prelude.Maybe [BuildNotDeleted])
deleteBuildBatchResponse_buildsNotDeleted = Lens.lens (\DeleteBuildBatchResponse' {buildsNotDeleted} -> buildsNotDeleted) (\s@DeleteBuildBatchResponse' {} a -> s {buildsNotDeleted = a} :: DeleteBuildBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status code.
deleteBuildBatchResponse_statusCode :: Lens.Lens' DeleteBuildBatchResponse (Prelude.Maybe Prelude.Text)
deleteBuildBatchResponse_statusCode = Lens.lens (\DeleteBuildBatchResponse' {statusCode} -> statusCode) (\s@DeleteBuildBatchResponse' {} a -> s {statusCode = a} :: DeleteBuildBatchResponse)

-- | The response's http status code.
deleteBuildBatchResponse_httpStatus :: Lens.Lens' DeleteBuildBatchResponse Prelude.Int
deleteBuildBatchResponse_httpStatus = Lens.lens (\DeleteBuildBatchResponse' {httpStatus} -> httpStatus) (\s@DeleteBuildBatchResponse' {} a -> s {httpStatus = a} :: DeleteBuildBatchResponse)

instance Prelude.NFData DeleteBuildBatchResponse where
  rnf DeleteBuildBatchResponse' {..} =
    Prelude.rnf buildsDeleted `Prelude.seq`
      Prelude.rnf buildsNotDeleted `Prelude.seq`
        Prelude.rnf statusCode `Prelude.seq`
          Prelude.rnf httpStatus
