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
-- Module      : Amazonka.QuickSight.UpdateDataSetPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the permissions on a dataset.
--
-- The permissions resource is
-- @arn:aws:quicksight:region:aws-account-id:dataset\/data-set-id@.
module Amazonka.QuickSight.UpdateDataSetPermissions
  ( -- * Creating a Request
    UpdateDataSetPermissions (..),
    newUpdateDataSetPermissions,

    -- * Request Lenses
    updateDataSetPermissions_grantPermissions,
    updateDataSetPermissions_revokePermissions,
    updateDataSetPermissions_awsAccountId,
    updateDataSetPermissions_dataSetId,

    -- * Destructuring the Response
    UpdateDataSetPermissionsResponse (..),
    newUpdateDataSetPermissionsResponse,

    -- * Response Lenses
    updateDataSetPermissionsResponse_requestId,
    updateDataSetPermissionsResponse_dataSetArn,
    updateDataSetPermissionsResponse_dataSetId,
    updateDataSetPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataSetPermissions' smart constructor.
data UpdateDataSetPermissions = UpdateDataSetPermissions'
  { -- | The resource permissions that you want to grant to the dataset.
    grantPermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The resource permissions that you want to revoke from the dataset.
    revokePermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dataset whose permissions you want to update. This ID is
    -- unique per Amazon Web Services Region for each Amazon Web Services
    -- account.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSetPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantPermissions', 'updateDataSetPermissions_grantPermissions' - The resource permissions that you want to grant to the dataset.
--
-- 'revokePermissions', 'updateDataSetPermissions_revokePermissions' - The resource permissions that you want to revoke from the dataset.
--
-- 'awsAccountId', 'updateDataSetPermissions_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'updateDataSetPermissions_dataSetId' - The ID for the dataset whose permissions you want to update. This ID is
-- unique per Amazon Web Services Region for each Amazon Web Services
-- account.
newUpdateDataSetPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  UpdateDataSetPermissions
newUpdateDataSetPermissions
  pAwsAccountId_
  pDataSetId_ =
    UpdateDataSetPermissions'
      { grantPermissions =
          Prelude.Nothing,
        revokePermissions = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dataSetId = pDataSetId_
      }

-- | The resource permissions that you want to grant to the dataset.
updateDataSetPermissions_grantPermissions :: Lens.Lens' UpdateDataSetPermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateDataSetPermissions_grantPermissions = Lens.lens (\UpdateDataSetPermissions' {grantPermissions} -> grantPermissions) (\s@UpdateDataSetPermissions' {} a -> s {grantPermissions = a} :: UpdateDataSetPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The resource permissions that you want to revoke from the dataset.
updateDataSetPermissions_revokePermissions :: Lens.Lens' UpdateDataSetPermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateDataSetPermissions_revokePermissions = Lens.lens (\UpdateDataSetPermissions' {revokePermissions} -> revokePermissions) (\s@UpdateDataSetPermissions' {} a -> s {revokePermissions = a} :: UpdateDataSetPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID.
updateDataSetPermissions_awsAccountId :: Lens.Lens' UpdateDataSetPermissions Prelude.Text
updateDataSetPermissions_awsAccountId = Lens.lens (\UpdateDataSetPermissions' {awsAccountId} -> awsAccountId) (\s@UpdateDataSetPermissions' {} a -> s {awsAccountId = a} :: UpdateDataSetPermissions)

-- | The ID for the dataset whose permissions you want to update. This ID is
-- unique per Amazon Web Services Region for each Amazon Web Services
-- account.
updateDataSetPermissions_dataSetId :: Lens.Lens' UpdateDataSetPermissions Prelude.Text
updateDataSetPermissions_dataSetId = Lens.lens (\UpdateDataSetPermissions' {dataSetId} -> dataSetId) (\s@UpdateDataSetPermissions' {} a -> s {dataSetId = a} :: UpdateDataSetPermissions)

instance Core.AWSRequest UpdateDataSetPermissions where
  type
    AWSResponse UpdateDataSetPermissions =
      UpdateDataSetPermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataSetPermissionsResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "DataSetArn")
            Prelude.<*> (x Data..?> "DataSetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataSetPermissions where
  hashWithSalt _salt UpdateDataSetPermissions' {..} =
    _salt `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData UpdateDataSetPermissions where
  rnf UpdateDataSetPermissions' {..} =
    Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId

instance Data.ToHeaders UpdateDataSetPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataSetPermissions where
  toJSON UpdateDataSetPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantPermissions" Data..=)
              Prelude.<$> grantPermissions,
            ("RevokePermissions" Data..=)
              Prelude.<$> revokePermissions
          ]
      )

instance Data.ToPath UpdateDataSetPermissions where
  toPath UpdateDataSetPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/permissions"
      ]

instance Data.ToQuery UpdateDataSetPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataSetPermissionsResponse' smart constructor.
data UpdateDataSetPermissionsResponse = UpdateDataSetPermissionsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset.
    dataSetArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the dataset whose permissions you want to update. This ID is
    -- unique per Amazon Web Services Region for each Amazon Web Services
    -- account.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSetPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateDataSetPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'dataSetArn', 'updateDataSetPermissionsResponse_dataSetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'dataSetId', 'updateDataSetPermissionsResponse_dataSetId' - The ID for the dataset whose permissions you want to update. This ID is
-- unique per Amazon Web Services Region for each Amazon Web Services
-- account.
--
-- 'status', 'updateDataSetPermissionsResponse_status' - The HTTP status of the request.
newUpdateDataSetPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateDataSetPermissionsResponse
newUpdateDataSetPermissionsResponse pStatus_ =
  UpdateDataSetPermissionsResponse'
    { requestId =
        Prelude.Nothing,
      dataSetArn = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updateDataSetPermissionsResponse_requestId :: Lens.Lens' UpdateDataSetPermissionsResponse (Prelude.Maybe Prelude.Text)
updateDataSetPermissionsResponse_requestId = Lens.lens (\UpdateDataSetPermissionsResponse' {requestId} -> requestId) (\s@UpdateDataSetPermissionsResponse' {} a -> s {requestId = a} :: UpdateDataSetPermissionsResponse)

-- | The Amazon Resource Name (ARN) of the dataset.
updateDataSetPermissionsResponse_dataSetArn :: Lens.Lens' UpdateDataSetPermissionsResponse (Prelude.Maybe Prelude.Text)
updateDataSetPermissionsResponse_dataSetArn = Lens.lens (\UpdateDataSetPermissionsResponse' {dataSetArn} -> dataSetArn) (\s@UpdateDataSetPermissionsResponse' {} a -> s {dataSetArn = a} :: UpdateDataSetPermissionsResponse)

-- | The ID for the dataset whose permissions you want to update. This ID is
-- unique per Amazon Web Services Region for each Amazon Web Services
-- account.
updateDataSetPermissionsResponse_dataSetId :: Lens.Lens' UpdateDataSetPermissionsResponse (Prelude.Maybe Prelude.Text)
updateDataSetPermissionsResponse_dataSetId = Lens.lens (\UpdateDataSetPermissionsResponse' {dataSetId} -> dataSetId) (\s@UpdateDataSetPermissionsResponse' {} a -> s {dataSetId = a} :: UpdateDataSetPermissionsResponse)

-- | The HTTP status of the request.
updateDataSetPermissionsResponse_status :: Lens.Lens' UpdateDataSetPermissionsResponse Prelude.Int
updateDataSetPermissionsResponse_status = Lens.lens (\UpdateDataSetPermissionsResponse' {status} -> status) (\s@UpdateDataSetPermissionsResponse' {} a -> s {status = a} :: UpdateDataSetPermissionsResponse)

instance
  Prelude.NFData
    UpdateDataSetPermissionsResponse
  where
  rnf UpdateDataSetPermissionsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf dataSetArn
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf status
