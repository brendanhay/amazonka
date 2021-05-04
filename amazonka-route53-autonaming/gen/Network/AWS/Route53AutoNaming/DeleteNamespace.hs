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
-- Module      : Network.AWS.Route53AutoNaming.DeleteNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a namespace from the current account. If the namespace still
-- contains one or more services, the request fails.
module Network.AWS.Route53AutoNaming.DeleteNamespace
  ( -- * Creating a Request
    DeleteNamespace (..),
    newDeleteNamespace,

    -- * Request Lenses
    deleteNamespace_id,

    -- * Destructuring the Response
    DeleteNamespaceResponse (..),
    newDeleteNamespaceResponse,

    -- * Response Lenses
    deleteNamespaceResponse_operationId,
    deleteNamespaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newDeleteNamespace' smart constructor.
data DeleteNamespace = DeleteNamespace'
  { -- | The ID of the namespace that you want to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteNamespace_id' - The ID of the namespace that you want to delete.
newDeleteNamespace ::
  -- | 'id'
  Prelude.Text ->
  DeleteNamespace
newDeleteNamespace pId_ = DeleteNamespace' {id = pId_}

-- | The ID of the namespace that you want to delete.
deleteNamespace_id :: Lens.Lens' DeleteNamespace Prelude.Text
deleteNamespace_id = Lens.lens (\DeleteNamespace' {id} -> id) (\s@DeleteNamespace' {} a -> s {id = a} :: DeleteNamespace)

instance Prelude.AWSRequest DeleteNamespace where
  type Rs DeleteNamespace = DeleteNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNamespaceResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNamespace

instance Prelude.NFData DeleteNamespace

instance Prelude.ToHeaders DeleteNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.DeleteNamespace" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteNamespace where
  toJSON DeleteNamespace' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Id" Prelude..= id)]
      )

instance Prelude.ToPath DeleteNamespace where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'deleteNamespaceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'deleteNamespaceResponse_httpStatus' - The response's http status code.
newDeleteNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNamespaceResponse
newDeleteNamespaceResponse pHttpStatus_ =
  DeleteNamespaceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
deleteNamespaceResponse_operationId :: Lens.Lens' DeleteNamespaceResponse (Prelude.Maybe Prelude.Text)
deleteNamespaceResponse_operationId = Lens.lens (\DeleteNamespaceResponse' {operationId} -> operationId) (\s@DeleteNamespaceResponse' {} a -> s {operationId = a} :: DeleteNamespaceResponse)

-- | The response's http status code.
deleteNamespaceResponse_httpStatus :: Lens.Lens' DeleteNamespaceResponse Prelude.Int
deleteNamespaceResponse_httpStatus = Lens.lens (\DeleteNamespaceResponse' {httpStatus} -> httpStatus) (\s@DeleteNamespaceResponse' {} a -> s {httpStatus = a} :: DeleteNamespaceResponse)

instance Prelude.NFData DeleteNamespaceResponse
