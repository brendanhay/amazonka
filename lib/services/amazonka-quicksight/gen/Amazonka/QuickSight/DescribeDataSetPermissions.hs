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
-- Module      : Amazonka.QuickSight.DescribeDataSetPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions on a dataset.
--
-- The permissions resource is
-- @arn:aws:quicksight:region:aws-account-id:dataset\/data-set-id@.
module Amazonka.QuickSight.DescribeDataSetPermissions
  ( -- * Creating a Request
    DescribeDataSetPermissions (..),
    newDescribeDataSetPermissions,

    -- * Request Lenses
    describeDataSetPermissions_awsAccountId,
    describeDataSetPermissions_dataSetId,

    -- * Destructuring the Response
    DescribeDataSetPermissionsResponse (..),
    newDescribeDataSetPermissionsResponse,

    -- * Response Lenses
    describeDataSetPermissionsResponse_dataSetArn,
    describeDataSetPermissionsResponse_dataSetId,
    describeDataSetPermissionsResponse_permissions,
    describeDataSetPermissionsResponse_requestId,
    describeDataSetPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSetPermissions' smart constructor.
data DescribeDataSetPermissions = DescribeDataSetPermissions'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dataset that you want to create. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSetPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeDataSetPermissions_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'describeDataSetPermissions_dataSetId' - The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
newDescribeDataSetPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  DescribeDataSetPermissions
newDescribeDataSetPermissions
  pAwsAccountId_
  pDataSetId_ =
    DescribeDataSetPermissions'
      { awsAccountId =
          pAwsAccountId_,
        dataSetId = pDataSetId_
      }

-- | The Amazon Web Services account ID.
describeDataSetPermissions_awsAccountId :: Lens.Lens' DescribeDataSetPermissions Prelude.Text
describeDataSetPermissions_awsAccountId = Lens.lens (\DescribeDataSetPermissions' {awsAccountId} -> awsAccountId) (\s@DescribeDataSetPermissions' {} a -> s {awsAccountId = a} :: DescribeDataSetPermissions)

-- | The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeDataSetPermissions_dataSetId :: Lens.Lens' DescribeDataSetPermissions Prelude.Text
describeDataSetPermissions_dataSetId = Lens.lens (\DescribeDataSetPermissions' {dataSetId} -> dataSetId) (\s@DescribeDataSetPermissions' {} a -> s {dataSetId = a} :: DescribeDataSetPermissions)

instance Core.AWSRequest DescribeDataSetPermissions where
  type
    AWSResponse DescribeDataSetPermissions =
      DescribeDataSetPermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSetPermissionsResponse'
            Prelude.<$> (x Data..?> "DataSetArn")
            Prelude.<*> (x Data..?> "DataSetId")
            Prelude.<*> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataSetPermissions where
  hashWithSalt _salt DescribeDataSetPermissions' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData DescribeDataSetPermissions where
  rnf DescribeDataSetPermissions' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId

instance Data.ToHeaders DescribeDataSetPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDataSetPermissions where
  toPath DescribeDataSetPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/permissions"
      ]

instance Data.ToQuery DescribeDataSetPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataSetPermissionsResponse' smart constructor.
data DescribeDataSetPermissionsResponse = DescribeDataSetPermissionsResponse'
  { -- | The Amazon Resource Name (ARN) of the dataset.
    dataSetArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the dataset that you want to create. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | A list of resource permissions on the dataset.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSetPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetArn', 'describeDataSetPermissionsResponse_dataSetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'dataSetId', 'describeDataSetPermissionsResponse_dataSetId' - The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'permissions', 'describeDataSetPermissionsResponse_permissions' - A list of resource permissions on the dataset.
--
-- 'requestId', 'describeDataSetPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeDataSetPermissionsResponse_status' - The HTTP status of the request.
newDescribeDataSetPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeDataSetPermissionsResponse
newDescribeDataSetPermissionsResponse pStatus_ =
  DescribeDataSetPermissionsResponse'
    { dataSetArn =
        Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset.
describeDataSetPermissionsResponse_dataSetArn :: Lens.Lens' DescribeDataSetPermissionsResponse (Prelude.Maybe Prelude.Text)
describeDataSetPermissionsResponse_dataSetArn = Lens.lens (\DescribeDataSetPermissionsResponse' {dataSetArn} -> dataSetArn) (\s@DescribeDataSetPermissionsResponse' {} a -> s {dataSetArn = a} :: DescribeDataSetPermissionsResponse)

-- | The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeDataSetPermissionsResponse_dataSetId :: Lens.Lens' DescribeDataSetPermissionsResponse (Prelude.Maybe Prelude.Text)
describeDataSetPermissionsResponse_dataSetId = Lens.lens (\DescribeDataSetPermissionsResponse' {dataSetId} -> dataSetId) (\s@DescribeDataSetPermissionsResponse' {} a -> s {dataSetId = a} :: DescribeDataSetPermissionsResponse)

-- | A list of resource permissions on the dataset.
describeDataSetPermissionsResponse_permissions :: Lens.Lens' DescribeDataSetPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeDataSetPermissionsResponse_permissions = Lens.lens (\DescribeDataSetPermissionsResponse' {permissions} -> permissions) (\s@DescribeDataSetPermissionsResponse' {} a -> s {permissions = a} :: DescribeDataSetPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
describeDataSetPermissionsResponse_requestId :: Lens.Lens' DescribeDataSetPermissionsResponse (Prelude.Maybe Prelude.Text)
describeDataSetPermissionsResponse_requestId = Lens.lens (\DescribeDataSetPermissionsResponse' {requestId} -> requestId) (\s@DescribeDataSetPermissionsResponse' {} a -> s {requestId = a} :: DescribeDataSetPermissionsResponse)

-- | The HTTP status of the request.
describeDataSetPermissionsResponse_status :: Lens.Lens' DescribeDataSetPermissionsResponse Prelude.Int
describeDataSetPermissionsResponse_status = Lens.lens (\DescribeDataSetPermissionsResponse' {status} -> status) (\s@DescribeDataSetPermissionsResponse' {} a -> s {status = a} :: DescribeDataSetPermissionsResponse)

instance
  Prelude.NFData
    DescribeDataSetPermissionsResponse
  where
  rnf DescribeDataSetPermissionsResponse' {..} =
    Prelude.rnf dataSetArn
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
