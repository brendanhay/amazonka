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
-- Module      : Amazonka.FSx.DeleteStorageVirtualMachine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Amazon FSx for ONTAP storage virtual machine (SVM).
-- Prior to deleting an SVM, you must delete all non-root volumes in the
-- SVM, otherwise the operation will fail.
module Amazonka.FSx.DeleteStorageVirtualMachine
  ( -- * Creating a Request
    DeleteStorageVirtualMachine (..),
    newDeleteStorageVirtualMachine,

    -- * Request Lenses
    deleteStorageVirtualMachine_clientRequestToken,
    deleteStorageVirtualMachine_storageVirtualMachineId,

    -- * Destructuring the Response
    DeleteStorageVirtualMachineResponse (..),
    newDeleteStorageVirtualMachineResponse,

    -- * Response Lenses
    deleteStorageVirtualMachineResponse_lifecycle,
    deleteStorageVirtualMachineResponse_storageVirtualMachineId,
    deleteStorageVirtualMachineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStorageVirtualMachine' smart constructor.
data DeleteStorageVirtualMachine = DeleteStorageVirtualMachine'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the SVM that you want to delete.
    storageVirtualMachineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStorageVirtualMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'deleteStorageVirtualMachine_clientRequestToken' - Undocumented member.
--
-- 'storageVirtualMachineId', 'deleteStorageVirtualMachine_storageVirtualMachineId' - The ID of the SVM that you want to delete.
newDeleteStorageVirtualMachine ::
  -- | 'storageVirtualMachineId'
  Prelude.Text ->
  DeleteStorageVirtualMachine
newDeleteStorageVirtualMachine
  pStorageVirtualMachineId_ =
    DeleteStorageVirtualMachine'
      { clientRequestToken =
          Prelude.Nothing,
        storageVirtualMachineId =
          pStorageVirtualMachineId_
      }

-- | Undocumented member.
deleteStorageVirtualMachine_clientRequestToken :: Lens.Lens' DeleteStorageVirtualMachine (Prelude.Maybe Prelude.Text)
deleteStorageVirtualMachine_clientRequestToken = Lens.lens (\DeleteStorageVirtualMachine' {clientRequestToken} -> clientRequestToken) (\s@DeleteStorageVirtualMachine' {} a -> s {clientRequestToken = a} :: DeleteStorageVirtualMachine)

-- | The ID of the SVM that you want to delete.
deleteStorageVirtualMachine_storageVirtualMachineId :: Lens.Lens' DeleteStorageVirtualMachine Prelude.Text
deleteStorageVirtualMachine_storageVirtualMachineId = Lens.lens (\DeleteStorageVirtualMachine' {storageVirtualMachineId} -> storageVirtualMachineId) (\s@DeleteStorageVirtualMachine' {} a -> s {storageVirtualMachineId = a} :: DeleteStorageVirtualMachine)

instance Core.AWSRequest DeleteStorageVirtualMachine where
  type
    AWSResponse DeleteStorageVirtualMachine =
      DeleteStorageVirtualMachineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteStorageVirtualMachineResponse'
            Prelude.<$> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "StorageVirtualMachineId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStorageVirtualMachine where
  hashWithSalt _salt DeleteStorageVirtualMachine' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` storageVirtualMachineId

instance Prelude.NFData DeleteStorageVirtualMachine where
  rnf DeleteStorageVirtualMachine' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf storageVirtualMachineId

instance Data.ToHeaders DeleteStorageVirtualMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DeleteStorageVirtualMachine" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStorageVirtualMachine where
  toJSON DeleteStorageVirtualMachine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just
              ( "StorageVirtualMachineId"
                  Data..= storageVirtualMachineId
              )
          ]
      )

instance Data.ToPath DeleteStorageVirtualMachine where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStorageVirtualMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStorageVirtualMachineResponse' smart constructor.
data DeleteStorageVirtualMachineResponse = DeleteStorageVirtualMachineResponse'
  { -- | Describes the lifecycle state of the SVM being deleted.
    lifecycle :: Prelude.Maybe StorageVirtualMachineLifecycle,
    -- | The ID of the SVM Amazon FSx is deleting.
    storageVirtualMachineId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStorageVirtualMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'deleteStorageVirtualMachineResponse_lifecycle' - Describes the lifecycle state of the SVM being deleted.
--
-- 'storageVirtualMachineId', 'deleteStorageVirtualMachineResponse_storageVirtualMachineId' - The ID of the SVM Amazon FSx is deleting.
--
-- 'httpStatus', 'deleteStorageVirtualMachineResponse_httpStatus' - The response's http status code.
newDeleteStorageVirtualMachineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStorageVirtualMachineResponse
newDeleteStorageVirtualMachineResponse pHttpStatus_ =
  DeleteStorageVirtualMachineResponse'
    { lifecycle =
        Prelude.Nothing,
      storageVirtualMachineId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the lifecycle state of the SVM being deleted.
deleteStorageVirtualMachineResponse_lifecycle :: Lens.Lens' DeleteStorageVirtualMachineResponse (Prelude.Maybe StorageVirtualMachineLifecycle)
deleteStorageVirtualMachineResponse_lifecycle = Lens.lens (\DeleteStorageVirtualMachineResponse' {lifecycle} -> lifecycle) (\s@DeleteStorageVirtualMachineResponse' {} a -> s {lifecycle = a} :: DeleteStorageVirtualMachineResponse)

-- | The ID of the SVM Amazon FSx is deleting.
deleteStorageVirtualMachineResponse_storageVirtualMachineId :: Lens.Lens' DeleteStorageVirtualMachineResponse (Prelude.Maybe Prelude.Text)
deleteStorageVirtualMachineResponse_storageVirtualMachineId = Lens.lens (\DeleteStorageVirtualMachineResponse' {storageVirtualMachineId} -> storageVirtualMachineId) (\s@DeleteStorageVirtualMachineResponse' {} a -> s {storageVirtualMachineId = a} :: DeleteStorageVirtualMachineResponse)

-- | The response's http status code.
deleteStorageVirtualMachineResponse_httpStatus :: Lens.Lens' DeleteStorageVirtualMachineResponse Prelude.Int
deleteStorageVirtualMachineResponse_httpStatus = Lens.lens (\DeleteStorageVirtualMachineResponse' {httpStatus} -> httpStatus) (\s@DeleteStorageVirtualMachineResponse' {} a -> s {httpStatus = a} :: DeleteStorageVirtualMachineResponse)

instance
  Prelude.NFData
    DeleteStorageVirtualMachineResponse
  where
  rnf DeleteStorageVirtualMachineResponse' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf storageVirtualMachineId
      `Prelude.seq` Prelude.rnf httpStatus
