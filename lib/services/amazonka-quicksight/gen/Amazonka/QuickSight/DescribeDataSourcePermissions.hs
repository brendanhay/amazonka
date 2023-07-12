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
-- Module      : Amazonka.QuickSight.DescribeDataSourcePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the resource permissions for a data source.
module Amazonka.QuickSight.DescribeDataSourcePermissions
  ( -- * Creating a Request
    DescribeDataSourcePermissions (..),
    newDescribeDataSourcePermissions,

    -- * Request Lenses
    describeDataSourcePermissions_awsAccountId,
    describeDataSourcePermissions_dataSourceId,

    -- * Destructuring the Response
    DescribeDataSourcePermissionsResponse (..),
    newDescribeDataSourcePermissionsResponse,

    -- * Response Lenses
    describeDataSourcePermissionsResponse_dataSourceArn,
    describeDataSourcePermissionsResponse_dataSourceId,
    describeDataSourcePermissionsResponse_permissions,
    describeDataSourcePermissionsResponse_requestId,
    describeDataSourcePermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSourcePermissions' smart constructor.
data DescribeDataSourcePermissions = DescribeDataSourcePermissions'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSourcePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeDataSourcePermissions_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSourceId', 'describeDataSourcePermissions_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
newDescribeDataSourcePermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSourceId'
  Prelude.Text ->
  DescribeDataSourcePermissions
newDescribeDataSourcePermissions
  pAwsAccountId_
  pDataSourceId_ =
    DescribeDataSourcePermissions'
      { awsAccountId =
          pAwsAccountId_,
        dataSourceId = pDataSourceId_
      }

-- | The Amazon Web Services account ID.
describeDataSourcePermissions_awsAccountId :: Lens.Lens' DescribeDataSourcePermissions Prelude.Text
describeDataSourcePermissions_awsAccountId = Lens.lens (\DescribeDataSourcePermissions' {awsAccountId} -> awsAccountId) (\s@DescribeDataSourcePermissions' {} a -> s {awsAccountId = a} :: DescribeDataSourcePermissions)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
describeDataSourcePermissions_dataSourceId :: Lens.Lens' DescribeDataSourcePermissions Prelude.Text
describeDataSourcePermissions_dataSourceId = Lens.lens (\DescribeDataSourcePermissions' {dataSourceId} -> dataSourceId) (\s@DescribeDataSourcePermissions' {} a -> s {dataSourceId = a} :: DescribeDataSourcePermissions)

instance
  Core.AWSRequest
    DescribeDataSourcePermissions
  where
  type
    AWSResponse DescribeDataSourcePermissions =
      DescribeDataSourcePermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSourcePermissionsResponse'
            Prelude.<$> (x Data..?> "DataSourceArn")
            Prelude.<*> (x Data..?> "DataSourceId")
            Prelude.<*> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDataSourcePermissions
  where
  hashWithSalt _salt DescribeDataSourcePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSourceId

instance Prelude.NFData DescribeDataSourcePermissions where
  rnf DescribeDataSourcePermissions' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSourceId

instance Data.ToHeaders DescribeDataSourcePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDataSourcePermissions where
  toPath DescribeDataSourcePermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sources/",
        Data.toBS dataSourceId,
        "/permissions"
      ]

instance Data.ToQuery DescribeDataSourcePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataSourcePermissionsResponse' smart constructor.
data DescribeDataSourcePermissionsResponse = DescribeDataSourcePermissionsResponse'
  { -- | The Amazon Resource Name (ARN) of the data source.
    dataSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the data source. This ID is unique per Amazon Web Services
    -- Region for each Amazon Web Services account.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | A list of resource permissions on the data source.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSourcePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceArn', 'describeDataSourcePermissionsResponse_dataSourceArn' - The Amazon Resource Name (ARN) of the data source.
--
-- 'dataSourceId', 'describeDataSourcePermissionsResponse_dataSourceId' - The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
--
-- 'permissions', 'describeDataSourcePermissionsResponse_permissions' - A list of resource permissions on the data source.
--
-- 'requestId', 'describeDataSourcePermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeDataSourcePermissionsResponse_status' - The HTTP status of the request.
newDescribeDataSourcePermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeDataSourcePermissionsResponse
newDescribeDataSourcePermissionsResponse pStatus_ =
  DescribeDataSourcePermissionsResponse'
    { dataSourceArn =
        Prelude.Nothing,
      dataSourceId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the data source.
describeDataSourcePermissionsResponse_dataSourceArn :: Lens.Lens' DescribeDataSourcePermissionsResponse (Prelude.Maybe Prelude.Text)
describeDataSourcePermissionsResponse_dataSourceArn = Lens.lens (\DescribeDataSourcePermissionsResponse' {dataSourceArn} -> dataSourceArn) (\s@DescribeDataSourcePermissionsResponse' {} a -> s {dataSourceArn = a} :: DescribeDataSourcePermissionsResponse)

-- | The ID of the data source. This ID is unique per Amazon Web Services
-- Region for each Amazon Web Services account.
describeDataSourcePermissionsResponse_dataSourceId :: Lens.Lens' DescribeDataSourcePermissionsResponse (Prelude.Maybe Prelude.Text)
describeDataSourcePermissionsResponse_dataSourceId = Lens.lens (\DescribeDataSourcePermissionsResponse' {dataSourceId} -> dataSourceId) (\s@DescribeDataSourcePermissionsResponse' {} a -> s {dataSourceId = a} :: DescribeDataSourcePermissionsResponse)

-- | A list of resource permissions on the data source.
describeDataSourcePermissionsResponse_permissions :: Lens.Lens' DescribeDataSourcePermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeDataSourcePermissionsResponse_permissions = Lens.lens (\DescribeDataSourcePermissionsResponse' {permissions} -> permissions) (\s@DescribeDataSourcePermissionsResponse' {} a -> s {permissions = a} :: DescribeDataSourcePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
describeDataSourcePermissionsResponse_requestId :: Lens.Lens' DescribeDataSourcePermissionsResponse (Prelude.Maybe Prelude.Text)
describeDataSourcePermissionsResponse_requestId = Lens.lens (\DescribeDataSourcePermissionsResponse' {requestId} -> requestId) (\s@DescribeDataSourcePermissionsResponse' {} a -> s {requestId = a} :: DescribeDataSourcePermissionsResponse)

-- | The HTTP status of the request.
describeDataSourcePermissionsResponse_status :: Lens.Lens' DescribeDataSourcePermissionsResponse Prelude.Int
describeDataSourcePermissionsResponse_status = Lens.lens (\DescribeDataSourcePermissionsResponse' {status} -> status) (\s@DescribeDataSourcePermissionsResponse' {} a -> s {status = a} :: DescribeDataSourcePermissionsResponse)

instance
  Prelude.NFData
    DescribeDataSourcePermissionsResponse
  where
  rnf DescribeDataSourcePermissionsResponse' {..} =
    Prelude.rnf dataSourceArn
      `Prelude.seq` Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
