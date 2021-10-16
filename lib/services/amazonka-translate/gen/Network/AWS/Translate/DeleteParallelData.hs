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
-- Module      : Network.AWS.Translate.DeleteParallelData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a parallel data resource in Amazon Translate.
module Network.AWS.Translate.DeleteParallelData
  ( -- * Creating a Request
    DeleteParallelData (..),
    newDeleteParallelData,

    -- * Request Lenses
    deleteParallelData_name,

    -- * Destructuring the Response
    DeleteParallelDataResponse (..),
    newDeleteParallelDataResponse,

    -- * Response Lenses
    deleteParallelDataResponse_status,
    deleteParallelDataResponse_name,
    deleteParallelDataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteParallelDataResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteParallelData

instance Prelude.NFData DeleteParallelData

instance Core.ToHeaders DeleteParallelData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.DeleteParallelData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteParallelData where
  toJSON DeleteParallelData' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath DeleteParallelData where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteParallelData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteParallelDataResponse' smart constructor.
data DeleteParallelDataResponse = DeleteParallelDataResponse'
  { -- | The status of the parallel data deletion.
    status :: Prelude.Maybe ParallelDataStatus,
    -- | The name of the parallel data resource that is being deleted.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'status', 'deleteParallelDataResponse_status' - The status of the parallel data deletion.
--
-- 'name', 'deleteParallelDataResponse_name' - The name of the parallel data resource that is being deleted.
--
-- 'httpStatus', 'deleteParallelDataResponse_httpStatus' - The response's http status code.
newDeleteParallelDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteParallelDataResponse
newDeleteParallelDataResponse pHttpStatus_ =
  DeleteParallelDataResponse'
    { status =
        Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the parallel data deletion.
deleteParallelDataResponse_status :: Lens.Lens' DeleteParallelDataResponse (Prelude.Maybe ParallelDataStatus)
deleteParallelDataResponse_status = Lens.lens (\DeleteParallelDataResponse' {status} -> status) (\s@DeleteParallelDataResponse' {} a -> s {status = a} :: DeleteParallelDataResponse)

-- | The name of the parallel data resource that is being deleted.
deleteParallelDataResponse_name :: Lens.Lens' DeleteParallelDataResponse (Prelude.Maybe Prelude.Text)
deleteParallelDataResponse_name = Lens.lens (\DeleteParallelDataResponse' {name} -> name) (\s@DeleteParallelDataResponse' {} a -> s {name = a} :: DeleteParallelDataResponse)

-- | The response's http status code.
deleteParallelDataResponse_httpStatus :: Lens.Lens' DeleteParallelDataResponse Prelude.Int
deleteParallelDataResponse_httpStatus = Lens.lens (\DeleteParallelDataResponse' {httpStatus} -> httpStatus) (\s@DeleteParallelDataResponse' {} a -> s {httpStatus = a} :: DeleteParallelDataResponse)

instance Prelude.NFData DeleteParallelDataResponse
