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
-- Module      : Network.AWS.Route53AutoNaming.DeleteService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service. If the service still contains one or more
-- registered instances, the request fails.
module Network.AWS.Route53AutoNaming.DeleteService
  ( -- * Creating a Request
    DeleteService (..),
    newDeleteService,

    -- * Request Lenses
    deleteService_id,

    -- * Destructuring the Response
    DeleteServiceResponse (..),
    newDeleteServiceResponse,

    -- * Response Lenses
    deleteServiceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newDeleteService' smart constructor.
data DeleteService = DeleteService'
  { -- | The ID of the service that you want to delete.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteService_id' - The ID of the service that you want to delete.
newDeleteService ::
  -- | 'id'
  Core.Text ->
  DeleteService
newDeleteService pId_ = DeleteService' {id = pId_}

-- | The ID of the service that you want to delete.
deleteService_id :: Lens.Lens' DeleteService Core.Text
deleteService_id = Lens.lens (\DeleteService' {id} -> id) (\s@DeleteService' {} a -> s {id = a} :: DeleteService)

instance Core.AWSRequest DeleteService where
  type
    AWSResponse DeleteService =
      DeleteServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteServiceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteService

instance Core.NFData DeleteService

instance Core.ToHeaders DeleteService where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.DeleteService" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteService where
  toJSON DeleteService' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.ToPath DeleteService where
  toPath = Core.const "/"

instance Core.ToQuery DeleteService where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteServiceResponse_httpStatus' - The response's http status code.
newDeleteServiceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteServiceResponse
newDeleteServiceResponse pHttpStatus_ =
  DeleteServiceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteServiceResponse_httpStatus :: Lens.Lens' DeleteServiceResponse Core.Int
deleteServiceResponse_httpStatus = Lens.lens (\DeleteServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceResponse' {} a -> s {httpStatus = a} :: DeleteServiceResponse)

instance Core.NFData DeleteServiceResponse
