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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newDeleteParallelData' smart constructor.
data DeleteParallelData = DeleteParallelData'
  { -- | The name of the parallel data resource that is being deleted.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteParallelData
newDeleteParallelData pName_ =
  DeleteParallelData' {name = pName_}

-- | The name of the parallel data resource that is being deleted.
deleteParallelData_name :: Lens.Lens' DeleteParallelData Core.Text
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
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteParallelData

instance Core.NFData DeleteParallelData

instance Core.ToHeaders DeleteParallelData where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.DeleteParallelData" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteParallelData where
  toJSON DeleteParallelData' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteParallelData where
  toPath = Core.const "/"

instance Core.ToQuery DeleteParallelData where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteParallelDataResponse' smart constructor.
data DeleteParallelDataResponse = DeleteParallelDataResponse'
  { -- | The status of the parallel data deletion.
    status :: Core.Maybe ParallelDataStatus,
    -- | The name of the parallel data resource that is being deleted.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteParallelDataResponse
newDeleteParallelDataResponse pHttpStatus_ =
  DeleteParallelDataResponse'
    { status = Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the parallel data deletion.
deleteParallelDataResponse_status :: Lens.Lens' DeleteParallelDataResponse (Core.Maybe ParallelDataStatus)
deleteParallelDataResponse_status = Lens.lens (\DeleteParallelDataResponse' {status} -> status) (\s@DeleteParallelDataResponse' {} a -> s {status = a} :: DeleteParallelDataResponse)

-- | The name of the parallel data resource that is being deleted.
deleteParallelDataResponse_name :: Lens.Lens' DeleteParallelDataResponse (Core.Maybe Core.Text)
deleteParallelDataResponse_name = Lens.lens (\DeleteParallelDataResponse' {name} -> name) (\s@DeleteParallelDataResponse' {} a -> s {name = a} :: DeleteParallelDataResponse)

-- | The response's http status code.
deleteParallelDataResponse_httpStatus :: Lens.Lens' DeleteParallelDataResponse Core.Int
deleteParallelDataResponse_httpStatus = Lens.lens (\DeleteParallelDataResponse' {httpStatus} -> httpStatus) (\s@DeleteParallelDataResponse' {} a -> s {httpStatus = a} :: DeleteParallelDataResponse)

instance Core.NFData DeleteParallelDataResponse
