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
-- Module      : Amazonka.Translate.DeleteParallelData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a parallel data resource in Amazon Translate.
module Amazonka.Translate.DeleteParallelData
  ( -- * Creating a Request
    DeleteParallelData (..),
    newDeleteParallelData,

    -- * Request Lenses
    deleteParallelData_name,

    -- * Destructuring the Response
    DeleteParallelDataResponse (..),
    newDeleteParallelDataResponse,

    -- * Response Lenses
    deleteParallelDataResponse_name,
    deleteParallelDataResponse_status,
    deleteParallelDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newDeleteParallelData' smart constructor.
data DeleteParallelData = DeleteParallelData'
  { -- | The name of the parallel data resource that is being deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParallelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteParallelData_name' - The name of the parallel data resource that is being deleted.
newDeleteParallelData ::
  -- | 'name'
  Prelude.Text ->
  DeleteParallelData
newDeleteParallelData pName_ =
  DeleteParallelData' {name = pName_}

-- | The name of the parallel data resource that is being deleted.
deleteParallelData_name :: Lens.Lens' DeleteParallelData Prelude.Text
deleteParallelData_name = Lens.lens (\DeleteParallelData' {name} -> name) (\s@DeleteParallelData' {} a -> s {name = a} :: DeleteParallelData)

instance Core.AWSRequest DeleteParallelData where
  type
    AWSResponse DeleteParallelData =
      DeleteParallelDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteParallelDataResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteParallelData where
  hashWithSalt _salt DeleteParallelData' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteParallelData where
  rnf DeleteParallelData' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteParallelData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.DeleteParallelData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteParallelData where
  toJSON DeleteParallelData' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteParallelData where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteParallelData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteParallelDataResponse' smart constructor.
data DeleteParallelDataResponse = DeleteParallelDataResponse'
  { -- | The name of the parallel data resource that is being deleted.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the parallel data deletion.
    status :: Prelude.Maybe ParallelDataStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteParallelDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteParallelDataResponse_name' - The name of the parallel data resource that is being deleted.
--
-- 'status', 'deleteParallelDataResponse_status' - The status of the parallel data deletion.
--
-- 'httpStatus', 'deleteParallelDataResponse_httpStatus' - The response's http status code.
newDeleteParallelDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteParallelDataResponse
newDeleteParallelDataResponse pHttpStatus_ =
  DeleteParallelDataResponse'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the parallel data resource that is being deleted.
deleteParallelDataResponse_name :: Lens.Lens' DeleteParallelDataResponse (Prelude.Maybe Prelude.Text)
deleteParallelDataResponse_name = Lens.lens (\DeleteParallelDataResponse' {name} -> name) (\s@DeleteParallelDataResponse' {} a -> s {name = a} :: DeleteParallelDataResponse)

-- | The status of the parallel data deletion.
deleteParallelDataResponse_status :: Lens.Lens' DeleteParallelDataResponse (Prelude.Maybe ParallelDataStatus)
deleteParallelDataResponse_status = Lens.lens (\DeleteParallelDataResponse' {status} -> status) (\s@DeleteParallelDataResponse' {} a -> s {status = a} :: DeleteParallelDataResponse)

-- | The response's http status code.
deleteParallelDataResponse_httpStatus :: Lens.Lens' DeleteParallelDataResponse Prelude.Int
deleteParallelDataResponse_httpStatus = Lens.lens (\DeleteParallelDataResponse' {httpStatus} -> httpStatus) (\s@DeleteParallelDataResponse' {} a -> s {httpStatus = a} :: DeleteParallelDataResponse)

instance Prelude.NFData DeleteParallelDataResponse where
  rnf DeleteParallelDataResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
