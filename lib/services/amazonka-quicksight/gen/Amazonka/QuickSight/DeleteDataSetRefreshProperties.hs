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
-- Module      : Amazonka.QuickSight.DeleteDataSetRefreshProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the dataset refresh properties of the dataset.
module Amazonka.QuickSight.DeleteDataSetRefreshProperties
  ( -- * Creating a Request
    DeleteDataSetRefreshProperties (..),
    newDeleteDataSetRefreshProperties,

    -- * Request Lenses
    deleteDataSetRefreshProperties_awsAccountId,
    deleteDataSetRefreshProperties_dataSetId,

    -- * Destructuring the Response
    DeleteDataSetRefreshPropertiesResponse (..),
    newDeleteDataSetRefreshPropertiesResponse,

    -- * Response Lenses
    deleteDataSetRefreshPropertiesResponse_requestId,
    deleteDataSetRefreshPropertiesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataSetRefreshProperties' smart constructor.
data DeleteDataSetRefreshProperties = DeleteDataSetRefreshProperties'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the dataset.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSetRefreshProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteDataSetRefreshProperties_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'deleteDataSetRefreshProperties_dataSetId' - The ID of the dataset.
newDeleteDataSetRefreshProperties ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  DeleteDataSetRefreshProperties
newDeleteDataSetRefreshProperties
  pAwsAccountId_
  pDataSetId_ =
    DeleteDataSetRefreshProperties'
      { awsAccountId =
          pAwsAccountId_,
        dataSetId = pDataSetId_
      }

-- | The Amazon Web Services account ID.
deleteDataSetRefreshProperties_awsAccountId :: Lens.Lens' DeleteDataSetRefreshProperties Prelude.Text
deleteDataSetRefreshProperties_awsAccountId = Lens.lens (\DeleteDataSetRefreshProperties' {awsAccountId} -> awsAccountId) (\s@DeleteDataSetRefreshProperties' {} a -> s {awsAccountId = a} :: DeleteDataSetRefreshProperties)

-- | The ID of the dataset.
deleteDataSetRefreshProperties_dataSetId :: Lens.Lens' DeleteDataSetRefreshProperties Prelude.Text
deleteDataSetRefreshProperties_dataSetId = Lens.lens (\DeleteDataSetRefreshProperties' {dataSetId} -> dataSetId) (\s@DeleteDataSetRefreshProperties' {} a -> s {dataSetId = a} :: DeleteDataSetRefreshProperties)

instance
  Core.AWSRequest
    DeleteDataSetRefreshProperties
  where
  type
    AWSResponse DeleteDataSetRefreshProperties =
      DeleteDataSetRefreshPropertiesResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDataSetRefreshPropertiesResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDataSetRefreshProperties
  where
  hashWithSalt
    _salt
    DeleteDataSetRefreshProperties' {..} =
      _salt
        `Prelude.hashWithSalt` awsAccountId
        `Prelude.hashWithSalt` dataSetId

instance
  Prelude.NFData
    DeleteDataSetRefreshProperties
  where
  rnf DeleteDataSetRefreshProperties' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId

instance
  Data.ToHeaders
    DeleteDataSetRefreshProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDataSetRefreshProperties where
  toPath DeleteDataSetRefreshProperties' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-properties"
      ]

instance Data.ToQuery DeleteDataSetRefreshProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataSetRefreshPropertiesResponse' smart constructor.
data DeleteDataSetRefreshPropertiesResponse = DeleteDataSetRefreshPropertiesResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataSetRefreshPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteDataSetRefreshPropertiesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteDataSetRefreshPropertiesResponse_status' - The HTTP status of the request.
newDeleteDataSetRefreshPropertiesResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteDataSetRefreshPropertiesResponse
newDeleteDataSetRefreshPropertiesResponse pStatus_ =
  DeleteDataSetRefreshPropertiesResponse'
    { requestId =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteDataSetRefreshPropertiesResponse_requestId :: Lens.Lens' DeleteDataSetRefreshPropertiesResponse (Prelude.Maybe Prelude.Text)
deleteDataSetRefreshPropertiesResponse_requestId = Lens.lens (\DeleteDataSetRefreshPropertiesResponse' {requestId} -> requestId) (\s@DeleteDataSetRefreshPropertiesResponse' {} a -> s {requestId = a} :: DeleteDataSetRefreshPropertiesResponse)

-- | The HTTP status of the request.
deleteDataSetRefreshPropertiesResponse_status :: Lens.Lens' DeleteDataSetRefreshPropertiesResponse Prelude.Int
deleteDataSetRefreshPropertiesResponse_status = Lens.lens (\DeleteDataSetRefreshPropertiesResponse' {status} -> status) (\s@DeleteDataSetRefreshPropertiesResponse' {} a -> s {status = a} :: DeleteDataSetRefreshPropertiesResponse)

instance
  Prelude.NFData
    DeleteDataSetRefreshPropertiesResponse
  where
  rnf DeleteDataSetRefreshPropertiesResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
