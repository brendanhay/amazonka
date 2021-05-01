{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteContainerService' smart constructor.
data DeleteContainerService = DeleteContainerService'
  { -- | The name of the container service to delete.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteContainerService
newDeleteContainerService pServiceName_ =
  DeleteContainerService'
    { serviceName =
        pServiceName_
    }

-- | The name of the container service to delete.
deleteContainerService_serviceName :: Lens.Lens' DeleteContainerService Prelude.Text
deleteContainerService_serviceName = Lens.lens (\DeleteContainerService' {serviceName} -> serviceName) (\s@DeleteContainerService' {} a -> s {serviceName = a} :: DeleteContainerService)

instance Prelude.AWSRequest DeleteContainerService where
  type
    Rs DeleteContainerService =
      DeleteContainerServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContainerService

instance Prelude.NFData DeleteContainerService

instance Prelude.ToHeaders DeleteContainerService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.DeleteContainerService" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteContainerService where
  toJSON DeleteContainerService' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("serviceName" Prelude..= serviceName)
          ]
      )

instance Prelude.ToPath DeleteContainerService where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteContainerService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContainerServiceResponse' smart constructor.
data DeleteContainerServiceResponse = DeleteContainerServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteContainerServiceResponse
newDeleteContainerServiceResponse pHttpStatus_ =
  DeleteContainerServiceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteContainerServiceResponse_httpStatus :: Lens.Lens' DeleteContainerServiceResponse Prelude.Int
deleteContainerServiceResponse_httpStatus = Lens.lens (\DeleteContainerServiceResponse' {httpStatus} -> httpStatus) (\s@DeleteContainerServiceResponse' {} a -> s {httpStatus = a} :: DeleteContainerServiceResponse)

instance
  Prelude.NFData
    DeleteContainerServiceResponse
