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
-- Module      : Amazonka.FSx.DeleteDataRepositoryAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a data repository association on an Amazon FSx for Lustre file
-- system. Deleting the data repository association unlinks the file system
-- from the Amazon S3 bucket. When deleting a data repository association,
-- you have the option of deleting the data in the file system that
-- corresponds to the data repository association. Data repository
-- associations are supported only for file systems with the @Persistent_2@
-- deployment type.
module Amazonka.FSx.DeleteDataRepositoryAssociation
  ( -- * Creating a Request
    DeleteDataRepositoryAssociation (..),
    newDeleteDataRepositoryAssociation,

    -- * Request Lenses
    deleteDataRepositoryAssociation_clientRequestToken,
    deleteDataRepositoryAssociation_deleteDataInFileSystem,
    deleteDataRepositoryAssociation_associationId,

    -- * Destructuring the Response
    DeleteDataRepositoryAssociationResponse (..),
    newDeleteDataRepositoryAssociationResponse,

    -- * Response Lenses
    deleteDataRepositoryAssociationResponse_lifecycle,
    deleteDataRepositoryAssociationResponse_deleteDataInFileSystem,
    deleteDataRepositoryAssociationResponse_associationId,
    deleteDataRepositoryAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataRepositoryAssociation' smart constructor.
data DeleteDataRepositoryAssociation = DeleteDataRepositoryAssociation'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to delete the data in the file system that corresponds to
    -- the data repository association.
    deleteDataInFileSystem :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the data repository association that you want to delete.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataRepositoryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'deleteDataRepositoryAssociation_clientRequestToken' - Undocumented member.
--
-- 'deleteDataInFileSystem', 'deleteDataRepositoryAssociation_deleteDataInFileSystem' - Set to @true@ to delete the data in the file system that corresponds to
-- the data repository association.
--
-- 'associationId', 'deleteDataRepositoryAssociation_associationId' - The ID of the data repository association that you want to delete.
newDeleteDataRepositoryAssociation ::
  -- | 'associationId'
  Prelude.Text ->
  DeleteDataRepositoryAssociation
newDeleteDataRepositoryAssociation pAssociationId_ =
  DeleteDataRepositoryAssociation'
    { clientRequestToken =
        Prelude.Nothing,
      deleteDataInFileSystem = Prelude.Nothing,
      associationId = pAssociationId_
    }

-- | Undocumented member.
deleteDataRepositoryAssociation_clientRequestToken :: Lens.Lens' DeleteDataRepositoryAssociation (Prelude.Maybe Prelude.Text)
deleteDataRepositoryAssociation_clientRequestToken = Lens.lens (\DeleteDataRepositoryAssociation' {clientRequestToken} -> clientRequestToken) (\s@DeleteDataRepositoryAssociation' {} a -> s {clientRequestToken = a} :: DeleteDataRepositoryAssociation)

-- | Set to @true@ to delete the data in the file system that corresponds to
-- the data repository association.
deleteDataRepositoryAssociation_deleteDataInFileSystem :: Lens.Lens' DeleteDataRepositoryAssociation (Prelude.Maybe Prelude.Bool)
deleteDataRepositoryAssociation_deleteDataInFileSystem = Lens.lens (\DeleteDataRepositoryAssociation' {deleteDataInFileSystem} -> deleteDataInFileSystem) (\s@DeleteDataRepositoryAssociation' {} a -> s {deleteDataInFileSystem = a} :: DeleteDataRepositoryAssociation)

-- | The ID of the data repository association that you want to delete.
deleteDataRepositoryAssociation_associationId :: Lens.Lens' DeleteDataRepositoryAssociation Prelude.Text
deleteDataRepositoryAssociation_associationId = Lens.lens (\DeleteDataRepositoryAssociation' {associationId} -> associationId) (\s@DeleteDataRepositoryAssociation' {} a -> s {associationId = a} :: DeleteDataRepositoryAssociation)

instance
  Core.AWSRequest
    DeleteDataRepositoryAssociation
  where
  type
    AWSResponse DeleteDataRepositoryAssociation =
      DeleteDataRepositoryAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDataRepositoryAssociationResponse'
            Prelude.<$> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "DeleteDataInFileSystem")
            Prelude.<*> (x Data..?> "AssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDataRepositoryAssociation
  where
  hashWithSalt
    _salt
    DeleteDataRepositoryAssociation' {..} =
      _salt `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` deleteDataInFileSystem
        `Prelude.hashWithSalt` associationId

instance
  Prelude.NFData
    DeleteDataRepositoryAssociation
  where
  rnf DeleteDataRepositoryAssociation' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf deleteDataInFileSystem
      `Prelude.seq` Prelude.rnf associationId

instance
  Data.ToHeaders
    DeleteDataRepositoryAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DeleteDataRepositoryAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDataRepositoryAssociation where
  toJSON DeleteDataRepositoryAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("DeleteDataInFileSystem" Data..=)
              Prelude.<$> deleteDataInFileSystem,
            Prelude.Just
              ("AssociationId" Data..= associationId)
          ]
      )

instance Data.ToPath DeleteDataRepositoryAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDataRepositoryAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataRepositoryAssociationResponse' smart constructor.
data DeleteDataRepositoryAssociationResponse = DeleteDataRepositoryAssociationResponse'
  { -- | Describes the lifecycle state of the data repository association being
    -- deleted.
    lifecycle :: Prelude.Maybe DataRepositoryLifecycle,
    -- | Indicates whether data in the file system that corresponds to the data
    -- repository association is being deleted. Default is @false@.
    deleteDataInFileSystem :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the data repository association being deleted.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataRepositoryAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'deleteDataRepositoryAssociationResponse_lifecycle' - Describes the lifecycle state of the data repository association being
-- deleted.
--
-- 'deleteDataInFileSystem', 'deleteDataRepositoryAssociationResponse_deleteDataInFileSystem' - Indicates whether data in the file system that corresponds to the data
-- repository association is being deleted. Default is @false@.
--
-- 'associationId', 'deleteDataRepositoryAssociationResponse_associationId' - The ID of the data repository association being deleted.
--
-- 'httpStatus', 'deleteDataRepositoryAssociationResponse_httpStatus' - The response's http status code.
newDeleteDataRepositoryAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataRepositoryAssociationResponse
newDeleteDataRepositoryAssociationResponse
  pHttpStatus_ =
    DeleteDataRepositoryAssociationResponse'
      { lifecycle =
          Prelude.Nothing,
        deleteDataInFileSystem =
          Prelude.Nothing,
        associationId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Describes the lifecycle state of the data repository association being
-- deleted.
deleteDataRepositoryAssociationResponse_lifecycle :: Lens.Lens' DeleteDataRepositoryAssociationResponse (Prelude.Maybe DataRepositoryLifecycle)
deleteDataRepositoryAssociationResponse_lifecycle = Lens.lens (\DeleteDataRepositoryAssociationResponse' {lifecycle} -> lifecycle) (\s@DeleteDataRepositoryAssociationResponse' {} a -> s {lifecycle = a} :: DeleteDataRepositoryAssociationResponse)

-- | Indicates whether data in the file system that corresponds to the data
-- repository association is being deleted. Default is @false@.
deleteDataRepositoryAssociationResponse_deleteDataInFileSystem :: Lens.Lens' DeleteDataRepositoryAssociationResponse (Prelude.Maybe Prelude.Bool)
deleteDataRepositoryAssociationResponse_deleteDataInFileSystem = Lens.lens (\DeleteDataRepositoryAssociationResponse' {deleteDataInFileSystem} -> deleteDataInFileSystem) (\s@DeleteDataRepositoryAssociationResponse' {} a -> s {deleteDataInFileSystem = a} :: DeleteDataRepositoryAssociationResponse)

-- | The ID of the data repository association being deleted.
deleteDataRepositoryAssociationResponse_associationId :: Lens.Lens' DeleteDataRepositoryAssociationResponse (Prelude.Maybe Prelude.Text)
deleteDataRepositoryAssociationResponse_associationId = Lens.lens (\DeleteDataRepositoryAssociationResponse' {associationId} -> associationId) (\s@DeleteDataRepositoryAssociationResponse' {} a -> s {associationId = a} :: DeleteDataRepositoryAssociationResponse)

-- | The response's http status code.
deleteDataRepositoryAssociationResponse_httpStatus :: Lens.Lens' DeleteDataRepositoryAssociationResponse Prelude.Int
deleteDataRepositoryAssociationResponse_httpStatus = Lens.lens (\DeleteDataRepositoryAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteDataRepositoryAssociationResponse' {} a -> s {httpStatus = a} :: DeleteDataRepositoryAssociationResponse)

instance
  Prelude.NFData
    DeleteDataRepositoryAssociationResponse
  where
  rnf DeleteDataRepositoryAssociationResponse' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf deleteDataInFileSystem
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf httpStatus
