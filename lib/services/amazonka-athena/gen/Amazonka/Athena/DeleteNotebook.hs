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
-- Module      : Amazonka.Athena.DeleteNotebook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notebook.
module Amazonka.Athena.DeleteNotebook
  ( -- * Creating a Request
    DeleteNotebook (..),
    newDeleteNotebook,

    -- * Request Lenses
    deleteNotebook_notebookId,

    -- * Destructuring the Response
    DeleteNotebookResponse (..),
    newDeleteNotebookResponse,

    -- * Response Lenses
    deleteNotebookResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNotebook' smart constructor.
data DeleteNotebook = DeleteNotebook'
  { -- | The ID of the notebook to delete.
    notebookId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotebook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookId', 'deleteNotebook_notebookId' - The ID of the notebook to delete.
newDeleteNotebook ::
  -- | 'notebookId'
  Prelude.Text ->
  DeleteNotebook
newDeleteNotebook pNotebookId_ =
  DeleteNotebook' {notebookId = pNotebookId_}

-- | The ID of the notebook to delete.
deleteNotebook_notebookId :: Lens.Lens' DeleteNotebook Prelude.Text
deleteNotebook_notebookId = Lens.lens (\DeleteNotebook' {notebookId} -> notebookId) (\s@DeleteNotebook' {} a -> s {notebookId = a} :: DeleteNotebook)

instance Core.AWSRequest DeleteNotebook where
  type
    AWSResponse DeleteNotebook =
      DeleteNotebookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNotebookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNotebook where
  hashWithSalt _salt DeleteNotebook' {..} =
    _salt `Prelude.hashWithSalt` notebookId

instance Prelude.NFData DeleteNotebook where
  rnf DeleteNotebook' {..} = Prelude.rnf notebookId

instance Data.ToHeaders DeleteNotebook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.DeleteNotebook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteNotebook where
  toJSON DeleteNotebook' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NotebookId" Data..= notebookId)]
      )

instance Data.ToPath DeleteNotebook where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNotebook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotebookResponse' smart constructor.
data DeleteNotebookResponse = DeleteNotebookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotebookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNotebookResponse_httpStatus' - The response's http status code.
newDeleteNotebookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNotebookResponse
newDeleteNotebookResponse pHttpStatus_ =
  DeleteNotebookResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteNotebookResponse_httpStatus :: Lens.Lens' DeleteNotebookResponse Prelude.Int
deleteNotebookResponse_httpStatus = Lens.lens (\DeleteNotebookResponse' {httpStatus} -> httpStatus) (\s@DeleteNotebookResponse' {} a -> s {httpStatus = a} :: DeleteNotebookResponse)

instance Prelude.NFData DeleteNotebookResponse where
  rnf DeleteNotebookResponse' {..} =
    Prelude.rnf httpStatus
