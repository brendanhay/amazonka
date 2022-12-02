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
-- Module      : Amazonka.IoTRoboRunner.DeleteWorker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to delete a worker
module Amazonka.IoTRoboRunner.DeleteWorker
  ( -- * Creating a Request
    DeleteWorker (..),
    newDeleteWorker,

    -- * Request Lenses
    deleteWorker_id,

    -- * Destructuring the Response
    DeleteWorkerResponse (..),
    newDeleteWorkerResponse,

    -- * Response Lenses
    deleteWorkerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorker' smart constructor.
data DeleteWorker = DeleteWorker'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteWorker_id' - Undocumented member.
newDeleteWorker ::
  -- | 'id'
  Prelude.Text ->
  DeleteWorker
newDeleteWorker pId_ = DeleteWorker' {id = pId_}

-- | Undocumented member.
deleteWorker_id :: Lens.Lens' DeleteWorker Prelude.Text
deleteWorker_id = Lens.lens (\DeleteWorker' {id} -> id) (\s@DeleteWorker' {} a -> s {id = a} :: DeleteWorker)

instance Core.AWSRequest DeleteWorker where
  type AWSResponse DeleteWorker = DeleteWorkerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorker where
  hashWithSalt _salt DeleteWorker' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteWorker where
  rnf DeleteWorker' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteWorker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWorker where
  toJSON DeleteWorker' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance Data.ToPath DeleteWorker where
  toPath = Prelude.const "/deleteWorker"

instance Data.ToQuery DeleteWorker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkerResponse' smart constructor.
data DeleteWorkerResponse = DeleteWorkerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkerResponse_httpStatus' - The response's http status code.
newDeleteWorkerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkerResponse
newDeleteWorkerResponse pHttpStatus_ =
  DeleteWorkerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWorkerResponse_httpStatus :: Lens.Lens' DeleteWorkerResponse Prelude.Int
deleteWorkerResponse_httpStatus = Lens.lens (\DeleteWorkerResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkerResponse' {} a -> s {httpStatus = a} :: DeleteWorkerResponse)

instance Prelude.NFData DeleteWorkerResponse where
  rnf DeleteWorkerResponse' {..} =
    Prelude.rnf httpStatus
