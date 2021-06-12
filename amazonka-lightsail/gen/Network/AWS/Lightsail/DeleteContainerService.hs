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
-- Module      : Network.AWS.Lightsail.DeleteContainerService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerService
  ( -- * Creating a Request
    DeleteContainerService (..),
    newDeleteContainerService,

    -- * Request Lenses
    deleteContainerService_serviceName,

    -- * Destructuring the Response
    DeleteContainerServiceResponse (..),
    newDeleteContainerServiceResponse,

    -- * Response Lenses
    deleteContainerServiceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteContainerService' smart constructor.
data DeleteContainerService = DeleteContainerService'
  { -- | The name of the container service to delete.
    serviceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteContainerService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'deleteContainerService_serviceName' - The name of the container service to delete.
newDeleteContainerService ::
  -- | 'serviceName'
  Core.Text ->
  DeleteContainerService
newDeleteContainerService pServiceName_ =
  DeleteContainerService'
    { serviceName =
        pServiceName_
    }

-- | The name of the container service to delete.
deleteContainerService_serviceName :: Lens.Lens' DeleteContainerService Core.Text
deleteContainerService_serviceName = Lens.lens (\DeleteContainerService' {serviceName} -> serviceName) (\s@DeleteContainerService' {} a -> s {serviceName = a} :: DeleteContainerService)

instance Core.AWSRequest DeleteContainerService where
  type
    AWSResponse DeleteContainerService =
      DeleteContainerServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerServiceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteContainerService

instance Core.NFData DeleteContainerService

instance Core.ToHeaders DeleteContainerService where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteContainerService" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteContainerService where
  toJSON DeleteContainerService' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("serviceName" Core..= serviceName)]
      )

instance Core.ToPath DeleteContainerService where
  toPath = Core.const "/"

instance Core.ToQuery DeleteContainerService where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteContainerServiceResponse' smart constructor.
data DeleteContainerServiceResponse = DeleteContainerServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteContainerServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContainerServiceResponse_httpStatus' - The response's http status code.
newDeleteContainerServiceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteContainerServiceResponse
newDeleteContainerServiceResponse pHttpStatus_ =
  DeleteContainerServiceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteContainerServiceResponse_httpStatus :: Lens.Lens' DeleteContainerServiceResponse Core.Int
deleteContainerServiceResponse_httpStatus = Lens.lens (\DeleteContainerServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteContainerServiceResponse' {} a -> s {httpStatus = a} :: DeleteContainerServiceResponse)

instance Core.NFData DeleteContainerServiceResponse
