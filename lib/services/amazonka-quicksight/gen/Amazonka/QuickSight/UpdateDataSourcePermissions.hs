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
-- Module      : Amazonka.QuickSight.UpdateDataSourcePermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the permissions to a data source.
module Amazonka.QuickSight.UpdateDataSourcePermissions
  ( -- * Creating a Request
    UpdateDataSourcePermissions (..),
    newUpdateDataSourcePermissions,

    -- * Request Lenses
    updateDataSourcePermissions_grantPermissions,
    updateDataSourcePermissions_revokePermissions,
    updateDataSourcePermissions_awsAccountId,
    updateDataSourcePermissions_dataSourceId,

    -- * Destructuring the Response
    UpdateDataSourcePermissionsResponse (..),
    newUpdateDataSourcePermissionsResponse,

    -- * Response Lenses
    updateDataSourcePermissionsResponse_dataSourceArn,
    updateDataSourcePermissionsResponse_dataSourceId,
    updateDataSourcePermissionsResponse_requestId,
    updateDataSourcePermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataSourcePermissions' smart constructor.
data UpdateDataSourcePermissions = UpdateDataSourcePermissions'
  { -- | A list of resource permissions that you want to grant on the data
    -- source.
    grantPermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | A list of resource permissions that you want to revoke on the data
    -- source.
    revokePermissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSourcePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantPermissions', 'updateDataSourcePermissions_grantPermissions' - A list of resource permissions that you want to grant on the data
-- source.
--
-- 'revokePermissions', 'updateDataSourcePermissions_revokePermissions' - A list of resource permissions that you want to revoke on the data
-- source.
--
-- 'awsAccountId', 'updateDataSourcePermissions_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSourceId', 'updateDataSourcePermissions_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
newUpdateDataSourcePermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSourceId'
  Prelude.Text ->
  UpdateDataSourcePermissions
newUpdateDataSourcePermissions
  pAwsAccountId_
  pDataSourceId_ =
    UpdateDataSourcePermissions'
      { grantPermissions =
          Prelude.Nothing,
        revokePermissions = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dataSourceId = pDataSourceId_
      }

-- | A list of resource permissions that you want to grant on the data
-- source.
updateDataSourcePermissions_grantPermissions :: Lens.Lens' UpdateDataSourcePermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateDataSourcePermissions_grantPermissions = Lens.lens (\UpdateDataSourcePermissions' {grantPermissions} -> grantPermissions) (\s@UpdateDataSourcePermissions' {} a -> s {grantPermissions = a} :: UpdateDataSourcePermissions) Prelude.. Lens.mapping Lens.coerced

-- | A list of resource permissions that you want to revoke on the data
-- source.
updateDataSourcePermissions_revokePermissions :: Lens.Lens' UpdateDataSourcePermissions (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateDataSourcePermissions_revokePermissions = Lens.lens (\UpdateDataSourcePermissions' {revokePermissions} -> revokePermissions) (\s@UpdateDataSourcePermissions' {} a -> s {revokePermissions = a} :: UpdateDataSourcePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID.
updateDataSourcePermissions_awsAccountId :: Lens.Lens' UpdateDataSourcePermissions Prelude.Text
updateDataSourcePermissions_awsAccountId = Lens.lens (\UpdateDataSourcePermissions' {awsAccountId} -> awsAccountId) (\s@UpdateDataSourcePermissions' {} a -> s {awsAccountId = a} :: UpdateDataSourcePermissions)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
updateDataSourcePermissions_dataSourceId :: Lens.Lens' UpdateDataSourcePermissions Prelude.Text
updateDataSourcePermissions_dataSourceId = Lens.lens (\UpdateDataSourcePermissions' {dataSourceId} -> dataSourceId) (\s@UpdateDataSourcePermissions' {} a -> s {dataSourceId = a} :: UpdateDataSourcePermissions)

instance Core.AWSRequest UpdateDataSourcePermissions where
  type
    AWSResponse UpdateDataSourcePermissions =
      UpdateDataSourcePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataSourcePermissionsResponse'
            Prelude.<$> (x Core..?> "DataSourceArn")
            Prelude.<*> (x Core..?> "DataSourceId")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataSourcePermissions where
  hashWithSalt _salt UpdateDataSourcePermissions' {..} =
    _salt `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSourceId

instance Prelude.NFData UpdateDataSourcePermissions where
  rnf UpdateDataSourcePermissions' {..} =
    Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSourceId

instance Core.ToHeaders UpdateDataSourcePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDataSourcePermissions where
  toJSON UpdateDataSourcePermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GrantPermissions" Core..=)
              Prelude.<$> grantPermissions,
            ("RevokePermissions" Core..=)
              Prelude.<$> revokePermissions
          ]
      )

instance Core.ToPath UpdateDataSourcePermissions where
  toPath UpdateDataSourcePermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/data-sources/",
        Core.toBS dataSourceId,
        "/permissions"
      ]

instance Core.ToQuery UpdateDataSourcePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataSourcePermissionsResponse' smart constructor.
data UpdateDataSourcePermissionsResponse = UpdateDataSourcePermissionsResponse'
  { -- | The Amazon Resource Name (ARN) of the data source.
    dataSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSourcePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceArn', 'updateDataSourcePermissionsResponse_dataSourceArn' - The Amazon Resource Name (ARN) of the data source.
--
-- 'dataSourceId', 'updateDataSourcePermissionsResponse_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'requestId', 'updateDataSourcePermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updateDataSourcePermissionsResponse_status' - The HTTP status of the request.
newUpdateDataSourcePermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateDataSourcePermissionsResponse
newUpdateDataSourcePermissionsResponse pStatus_ =
  UpdateDataSourcePermissionsResponse'
    { dataSourceArn =
        Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the data source.
updateDataSourcePermissionsResponse_dataSourceArn :: Lens.Lens' UpdateDataSourcePermissionsResponse (Prelude.Maybe Prelude.Text)
updateDataSourcePermissionsResponse_dataSourceArn = Lens.lens (\UpdateDataSourcePermissionsResponse' {dataSourceArn} -> dataSourceArn) (\s@UpdateDataSourcePermissionsResponse' {} a -> s {dataSourceArn = a} :: UpdateDataSourcePermissionsResponse)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
updateDataSourcePermissionsResponse_dataSourceId :: Lens.Lens' UpdateDataSourcePermissionsResponse (Prelude.Maybe Prelude.Text)
updateDataSourcePermissionsResponse_dataSourceId = Lens.lens (\UpdateDataSourcePermissionsResponse' {dataSourceId} -> dataSourceId) (\s@UpdateDataSourcePermissionsResponse' {} a -> s {dataSourceId = a} :: UpdateDataSourcePermissionsResponse)

-- | The Amazon Web Services request ID for this operation.
updateDataSourcePermissionsResponse_requestId :: Lens.Lens' UpdateDataSourcePermissionsResponse (Prelude.Maybe Prelude.Text)
updateDataSourcePermissionsResponse_requestId = Lens.lens (\UpdateDataSourcePermissionsResponse' {requestId} -> requestId) (\s@UpdateDataSourcePermissionsResponse' {} a -> s {requestId = a} :: UpdateDataSourcePermissionsResponse)

-- | The HTTP status of the request.
updateDataSourcePermissionsResponse_status :: Lens.Lens' UpdateDataSourcePermissionsResponse Prelude.Int
updateDataSourcePermissionsResponse_status = Lens.lens (\UpdateDataSourcePermissionsResponse' {status} -> status) (\s@UpdateDataSourcePermissionsResponse' {} a -> s {status = a} :: UpdateDataSourcePermissionsResponse)

instance
  Prelude.NFData
    UpdateDataSourcePermissionsResponse
  where
  rnf UpdateDataSourcePermissionsResponse' {..} =
    Prelude.rnf dataSourceArn
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
