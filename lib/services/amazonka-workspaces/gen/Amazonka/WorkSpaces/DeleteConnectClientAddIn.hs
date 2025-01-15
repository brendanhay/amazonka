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
-- Module      : Amazonka.WorkSpaces.DeleteConnectClientAddIn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a client-add-in for Amazon Connect that is configured within a
-- directory.
module Amazonka.WorkSpaces.DeleteConnectClientAddIn
  ( -- * Creating a Request
    DeleteConnectClientAddIn (..),
    newDeleteConnectClientAddIn,

    -- * Request Lenses
    deleteConnectClientAddIn_addInId,
    deleteConnectClientAddIn_resourceId,

    -- * Destructuring the Response
    DeleteConnectClientAddInResponse (..),
    newDeleteConnectClientAddInResponse,

    -- * Response Lenses
    deleteConnectClientAddInResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDeleteConnectClientAddIn' smart constructor.
data DeleteConnectClientAddIn = DeleteConnectClientAddIn'
  { -- | The identifier of the client add-in to delete.
    addInId :: Prelude.Text,
    -- | The directory identifier for which the client add-in is configured.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectClientAddIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addInId', 'deleteConnectClientAddIn_addInId' - The identifier of the client add-in to delete.
--
-- 'resourceId', 'deleteConnectClientAddIn_resourceId' - The directory identifier for which the client add-in is configured.
newDeleteConnectClientAddIn ::
  -- | 'addInId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  DeleteConnectClientAddIn
newDeleteConnectClientAddIn pAddInId_ pResourceId_ =
  DeleteConnectClientAddIn'
    { addInId = pAddInId_,
      resourceId = pResourceId_
    }

-- | The identifier of the client add-in to delete.
deleteConnectClientAddIn_addInId :: Lens.Lens' DeleteConnectClientAddIn Prelude.Text
deleteConnectClientAddIn_addInId = Lens.lens (\DeleteConnectClientAddIn' {addInId} -> addInId) (\s@DeleteConnectClientAddIn' {} a -> s {addInId = a} :: DeleteConnectClientAddIn)

-- | The directory identifier for which the client add-in is configured.
deleteConnectClientAddIn_resourceId :: Lens.Lens' DeleteConnectClientAddIn Prelude.Text
deleteConnectClientAddIn_resourceId = Lens.lens (\DeleteConnectClientAddIn' {resourceId} -> resourceId) (\s@DeleteConnectClientAddIn' {} a -> s {resourceId = a} :: DeleteConnectClientAddIn)

instance Core.AWSRequest DeleteConnectClientAddIn where
  type
    AWSResponse DeleteConnectClientAddIn =
      DeleteConnectClientAddInResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConnectClientAddInResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnectClientAddIn where
  hashWithSalt _salt DeleteConnectClientAddIn' {..} =
    _salt
      `Prelude.hashWithSalt` addInId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DeleteConnectClientAddIn where
  rnf DeleteConnectClientAddIn' {..} =
    Prelude.rnf addInId `Prelude.seq`
      Prelude.rnf resourceId

instance Data.ToHeaders DeleteConnectClientAddIn where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DeleteConnectClientAddIn" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConnectClientAddIn where
  toJSON DeleteConnectClientAddIn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AddInId" Data..= addInId),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath DeleteConnectClientAddIn where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConnectClientAddIn where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectClientAddInResponse' smart constructor.
data DeleteConnectClientAddInResponse = DeleteConnectClientAddInResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectClientAddInResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConnectClientAddInResponse_httpStatus' - The response's http status code.
newDeleteConnectClientAddInResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConnectClientAddInResponse
newDeleteConnectClientAddInResponse pHttpStatus_ =
  DeleteConnectClientAddInResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConnectClientAddInResponse_httpStatus :: Lens.Lens' DeleteConnectClientAddInResponse Prelude.Int
deleteConnectClientAddInResponse_httpStatus = Lens.lens (\DeleteConnectClientAddInResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectClientAddInResponse' {} a -> s {httpStatus = a} :: DeleteConnectClientAddInResponse)

instance
  Prelude.NFData
    DeleteConnectClientAddInResponse
  where
  rnf DeleteConnectClientAddInResponse' {..} =
    Prelude.rnf httpStatus
