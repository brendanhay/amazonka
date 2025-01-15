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
-- Module      : Amazonka.EC2.DescribeSnapshotAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified snapshot. You can
-- specify only one attribute at a time.
--
-- For more information about EBS snapshots, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.DescribeSnapshotAttribute
  ( -- * Creating a Request
    DescribeSnapshotAttribute (..),
    newDescribeSnapshotAttribute,

    -- * Request Lenses
    describeSnapshotAttribute_dryRun,
    describeSnapshotAttribute_attribute,
    describeSnapshotAttribute_snapshotId,

    -- * Destructuring the Response
    DescribeSnapshotAttributeResponse (..),
    newDescribeSnapshotAttributeResponse,

    -- * Response Lenses
    describeSnapshotAttributeResponse_createVolumePermissions,
    describeSnapshotAttributeResponse_productCodes,
    describeSnapshotAttributeResponse_snapshotId,
    describeSnapshotAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSnapshotAttribute' smart constructor.
data DescribeSnapshotAttribute = DescribeSnapshotAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The snapshot attribute you would like to view.
    attribute :: SnapshotAttributeName,
    -- | The ID of the EBS snapshot.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSnapshotAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'describeSnapshotAttribute_attribute' - The snapshot attribute you would like to view.
--
-- 'snapshotId', 'describeSnapshotAttribute_snapshotId' - The ID of the EBS snapshot.
newDescribeSnapshotAttribute ::
  -- | 'attribute'
  SnapshotAttributeName ->
  -- | 'snapshotId'
  Prelude.Text ->
  DescribeSnapshotAttribute
newDescribeSnapshotAttribute pAttribute_ pSnapshotId_ =
  DescribeSnapshotAttribute'
    { dryRun =
        Prelude.Nothing,
      attribute = pAttribute_,
      snapshotId = pSnapshotId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSnapshotAttribute_dryRun :: Lens.Lens' DescribeSnapshotAttribute (Prelude.Maybe Prelude.Bool)
describeSnapshotAttribute_dryRun = Lens.lens (\DescribeSnapshotAttribute' {dryRun} -> dryRun) (\s@DescribeSnapshotAttribute' {} a -> s {dryRun = a} :: DescribeSnapshotAttribute)

-- | The snapshot attribute you would like to view.
describeSnapshotAttribute_attribute :: Lens.Lens' DescribeSnapshotAttribute SnapshotAttributeName
describeSnapshotAttribute_attribute = Lens.lens (\DescribeSnapshotAttribute' {attribute} -> attribute) (\s@DescribeSnapshotAttribute' {} a -> s {attribute = a} :: DescribeSnapshotAttribute)

-- | The ID of the EBS snapshot.
describeSnapshotAttribute_snapshotId :: Lens.Lens' DescribeSnapshotAttribute Prelude.Text
describeSnapshotAttribute_snapshotId = Lens.lens (\DescribeSnapshotAttribute' {snapshotId} -> snapshotId) (\s@DescribeSnapshotAttribute' {} a -> s {snapshotId = a} :: DescribeSnapshotAttribute)

instance Core.AWSRequest DescribeSnapshotAttribute where
  type
    AWSResponse DescribeSnapshotAttribute =
      DescribeSnapshotAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSnapshotAttributeResponse'
            Prelude.<$> ( x
                            Data..@? "createVolumePermission"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x Data..@? "productCodes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "snapshotId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshotAttribute where
  hashWithSalt _salt DescribeSnapshotAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData DescribeSnapshotAttribute where
  rnf DescribeSnapshotAttribute' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf attribute `Prelude.seq`
        Prelude.rnf snapshotId

instance Data.ToHeaders DescribeSnapshotAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSnapshotAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSnapshotAttribute where
  toQuery DescribeSnapshotAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSnapshotAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Attribute" Data.=: attribute,
        "SnapshotId" Data.=: snapshotId
      ]

-- | /See:/ 'newDescribeSnapshotAttributeResponse' smart constructor.
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'
  { -- | The users and groups that have the permissions for creating volumes from
    -- the snapshot.
    createVolumePermissions :: Prelude.Maybe [CreateVolumePermission],
    -- | The product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The ID of the EBS snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createVolumePermissions', 'describeSnapshotAttributeResponse_createVolumePermissions' - The users and groups that have the permissions for creating volumes from
-- the snapshot.
--
-- 'productCodes', 'describeSnapshotAttributeResponse_productCodes' - The product codes.
--
-- 'snapshotId', 'describeSnapshotAttributeResponse_snapshotId' - The ID of the EBS snapshot.
--
-- 'httpStatus', 'describeSnapshotAttributeResponse_httpStatus' - The response's http status code.
newDescribeSnapshotAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotAttributeResponse
newDescribeSnapshotAttributeResponse pHttpStatus_ =
  DescribeSnapshotAttributeResponse'
    { createVolumePermissions =
        Prelude.Nothing,
      productCodes = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The users and groups that have the permissions for creating volumes from
-- the snapshot.
describeSnapshotAttributeResponse_createVolumePermissions :: Lens.Lens' DescribeSnapshotAttributeResponse (Prelude.Maybe [CreateVolumePermission])
describeSnapshotAttributeResponse_createVolumePermissions = Lens.lens (\DescribeSnapshotAttributeResponse' {createVolumePermissions} -> createVolumePermissions) (\s@DescribeSnapshotAttributeResponse' {} a -> s {createVolumePermissions = a} :: DescribeSnapshotAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The product codes.
describeSnapshotAttributeResponse_productCodes :: Lens.Lens' DescribeSnapshotAttributeResponse (Prelude.Maybe [ProductCode])
describeSnapshotAttributeResponse_productCodes = Lens.lens (\DescribeSnapshotAttributeResponse' {productCodes} -> productCodes) (\s@DescribeSnapshotAttributeResponse' {} a -> s {productCodes = a} :: DescribeSnapshotAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the EBS snapshot.
describeSnapshotAttributeResponse_snapshotId :: Lens.Lens' DescribeSnapshotAttributeResponse (Prelude.Maybe Prelude.Text)
describeSnapshotAttributeResponse_snapshotId = Lens.lens (\DescribeSnapshotAttributeResponse' {snapshotId} -> snapshotId) (\s@DescribeSnapshotAttributeResponse' {} a -> s {snapshotId = a} :: DescribeSnapshotAttributeResponse)

-- | The response's http status code.
describeSnapshotAttributeResponse_httpStatus :: Lens.Lens' DescribeSnapshotAttributeResponse Prelude.Int
describeSnapshotAttributeResponse_httpStatus = Lens.lens (\DescribeSnapshotAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotAttributeResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotAttributeResponse)

instance
  Prelude.NFData
    DescribeSnapshotAttributeResponse
  where
  rnf DescribeSnapshotAttributeResponse' {..} =
    Prelude.rnf createVolumePermissions `Prelude.seq`
      Prelude.rnf productCodes `Prelude.seq`
        Prelude.rnf snapshotId `Prelude.seq`
          Prelude.rnf httpStatus
