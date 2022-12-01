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
-- Module      : Amazonka.HealthLake.DeleteFHIRDatastore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Data Store.
module Amazonka.HealthLake.DeleteFHIRDatastore
  ( -- * Creating a Request
    DeleteFHIRDatastore (..),
    newDeleteFHIRDatastore,

    -- * Request Lenses
    deleteFHIRDatastore_datastoreId,

    -- * Destructuring the Response
    DeleteFHIRDatastoreResponse (..),
    newDeleteFHIRDatastoreResponse,

    -- * Response Lenses
    deleteFHIRDatastoreResponse_httpStatus,
    deleteFHIRDatastoreResponse_datastoreId,
    deleteFHIRDatastoreResponse_datastoreArn,
    deleteFHIRDatastoreResponse_datastoreStatus,
    deleteFHIRDatastoreResponse_datastoreEndpoint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFHIRDatastore' smart constructor.
data DeleteFHIRDatastore = DeleteFHIRDatastore'
  { -- | The AWS-generated ID for the Data Store to be deleted.
    datastoreId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFHIRDatastore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreId', 'deleteFHIRDatastore_datastoreId' - The AWS-generated ID for the Data Store to be deleted.
newDeleteFHIRDatastore ::
  DeleteFHIRDatastore
newDeleteFHIRDatastore =
  DeleteFHIRDatastore' {datastoreId = Prelude.Nothing}

-- | The AWS-generated ID for the Data Store to be deleted.
deleteFHIRDatastore_datastoreId :: Lens.Lens' DeleteFHIRDatastore (Prelude.Maybe Prelude.Text)
deleteFHIRDatastore_datastoreId = Lens.lens (\DeleteFHIRDatastore' {datastoreId} -> datastoreId) (\s@DeleteFHIRDatastore' {} a -> s {datastoreId = a} :: DeleteFHIRDatastore)

instance Core.AWSRequest DeleteFHIRDatastore where
  type
    AWSResponse DeleteFHIRDatastore =
      DeleteFHIRDatastoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFHIRDatastoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DatastoreId")
            Prelude.<*> (x Core..:> "DatastoreArn")
            Prelude.<*> (x Core..:> "DatastoreStatus")
            Prelude.<*> (x Core..:> "DatastoreEndpoint")
      )

instance Prelude.Hashable DeleteFHIRDatastore where
  hashWithSalt _salt DeleteFHIRDatastore' {..} =
    _salt `Prelude.hashWithSalt` datastoreId

instance Prelude.NFData DeleteFHIRDatastore where
  rnf DeleteFHIRDatastore' {..} =
    Prelude.rnf datastoreId

instance Core.ToHeaders DeleteFHIRDatastore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "HealthLake.DeleteFHIRDatastore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteFHIRDatastore where
  toJSON DeleteFHIRDatastore' {..} =
    Core.object
      ( Prelude.catMaybes
          [("DatastoreId" Core..=) Prelude.<$> datastoreId]
      )

instance Core.ToPath DeleteFHIRDatastore where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFHIRDatastore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFHIRDatastoreResponse' smart constructor.
data DeleteFHIRDatastoreResponse = DeleteFHIRDatastoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The AWS-generated ID for the Data Store to be deleted.
    datastoreId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon HealthLake access
    -- permission.
    datastoreArn :: Prelude.Text,
    -- | The status of the Data Store that the user has requested to be deleted.
    datastoreStatus :: DatastoreStatus,
    -- | The AWS endpoint for the Data Store the user has requested to be
    -- deleted.
    datastoreEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFHIRDatastoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFHIRDatastoreResponse_httpStatus' - The response's http status code.
--
-- 'datastoreId', 'deleteFHIRDatastoreResponse_datastoreId' - The AWS-generated ID for the Data Store to be deleted.
--
-- 'datastoreArn', 'deleteFHIRDatastoreResponse_datastoreArn' - The Amazon Resource Name (ARN) that gives Amazon HealthLake access
-- permission.
--
-- 'datastoreStatus', 'deleteFHIRDatastoreResponse_datastoreStatus' - The status of the Data Store that the user has requested to be deleted.
--
-- 'datastoreEndpoint', 'deleteFHIRDatastoreResponse_datastoreEndpoint' - The AWS endpoint for the Data Store the user has requested to be
-- deleted.
newDeleteFHIRDatastoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'datastoreArn'
  Prelude.Text ->
  -- | 'datastoreStatus'
  DatastoreStatus ->
  -- | 'datastoreEndpoint'
  Prelude.Text ->
  DeleteFHIRDatastoreResponse
newDeleteFHIRDatastoreResponse
  pHttpStatus_
  pDatastoreId_
  pDatastoreArn_
  pDatastoreStatus_
  pDatastoreEndpoint_ =
    DeleteFHIRDatastoreResponse'
      { httpStatus =
          pHttpStatus_,
        datastoreId = pDatastoreId_,
        datastoreArn = pDatastoreArn_,
        datastoreStatus = pDatastoreStatus_,
        datastoreEndpoint = pDatastoreEndpoint_
      }

-- | The response's http status code.
deleteFHIRDatastoreResponse_httpStatus :: Lens.Lens' DeleteFHIRDatastoreResponse Prelude.Int
deleteFHIRDatastoreResponse_httpStatus = Lens.lens (\DeleteFHIRDatastoreResponse' {httpStatus} -> httpStatus) (\s@DeleteFHIRDatastoreResponse' {} a -> s {httpStatus = a} :: DeleteFHIRDatastoreResponse)

-- | The AWS-generated ID for the Data Store to be deleted.
deleteFHIRDatastoreResponse_datastoreId :: Lens.Lens' DeleteFHIRDatastoreResponse Prelude.Text
deleteFHIRDatastoreResponse_datastoreId = Lens.lens (\DeleteFHIRDatastoreResponse' {datastoreId} -> datastoreId) (\s@DeleteFHIRDatastoreResponse' {} a -> s {datastoreId = a} :: DeleteFHIRDatastoreResponse)

-- | The Amazon Resource Name (ARN) that gives Amazon HealthLake access
-- permission.
deleteFHIRDatastoreResponse_datastoreArn :: Lens.Lens' DeleteFHIRDatastoreResponse Prelude.Text
deleteFHIRDatastoreResponse_datastoreArn = Lens.lens (\DeleteFHIRDatastoreResponse' {datastoreArn} -> datastoreArn) (\s@DeleteFHIRDatastoreResponse' {} a -> s {datastoreArn = a} :: DeleteFHIRDatastoreResponse)

-- | The status of the Data Store that the user has requested to be deleted.
deleteFHIRDatastoreResponse_datastoreStatus :: Lens.Lens' DeleteFHIRDatastoreResponse DatastoreStatus
deleteFHIRDatastoreResponse_datastoreStatus = Lens.lens (\DeleteFHIRDatastoreResponse' {datastoreStatus} -> datastoreStatus) (\s@DeleteFHIRDatastoreResponse' {} a -> s {datastoreStatus = a} :: DeleteFHIRDatastoreResponse)

-- | The AWS endpoint for the Data Store the user has requested to be
-- deleted.
deleteFHIRDatastoreResponse_datastoreEndpoint :: Lens.Lens' DeleteFHIRDatastoreResponse Prelude.Text
deleteFHIRDatastoreResponse_datastoreEndpoint = Lens.lens (\DeleteFHIRDatastoreResponse' {datastoreEndpoint} -> datastoreEndpoint) (\s@DeleteFHIRDatastoreResponse' {} a -> s {datastoreEndpoint = a} :: DeleteFHIRDatastoreResponse)

instance Prelude.NFData DeleteFHIRDatastoreResponse where
  rnf DeleteFHIRDatastoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf datastoreArn
      `Prelude.seq` Prelude.rnf datastoreStatus
      `Prelude.seq` Prelude.rnf datastoreEndpoint
