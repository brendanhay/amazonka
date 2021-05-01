{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeSnapshotAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified snapshot. You can
-- specify only one attribute at a time.
--
-- For more information about EBS snapshots, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DescribeSnapshotAttribute
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
    describeSnapshotAttributeResponse_productCodes,
    describeSnapshotAttributeResponse_createVolumePermissions,
    describeSnapshotAttributeResponse_snapshotId,
    describeSnapshotAttributeResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeSnapshotAttribute where
  type
    Rs DescribeSnapshotAttribute =
      DescribeSnapshotAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSnapshotAttributeResponse'
            Prelude.<$> ( x Prelude..@? "productCodes"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> ( x Prelude..@? "createVolumePermission"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (x Prelude..@? "snapshotId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshotAttribute

instance Prelude.NFData DescribeSnapshotAttribute

instance Prelude.ToHeaders DescribeSnapshotAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeSnapshotAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeSnapshotAttribute where
  toQuery DescribeSnapshotAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeSnapshotAttribute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "Attribute" Prelude.=: attribute,
        "SnapshotId" Prelude.=: snapshotId
      ]

-- | /See:/ 'newDescribeSnapshotAttributeResponse' smart constructor.
data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse'
  { -- | The product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The users and groups that have the permissions for creating volumes from
    -- the snapshot.
    createVolumePermissions :: Prelude.Maybe [CreateVolumePermission],
    -- | The ID of the EBS snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productCodes', 'describeSnapshotAttributeResponse_productCodes' - The product codes.
--
-- 'createVolumePermissions', 'describeSnapshotAttributeResponse_createVolumePermissions' - The users and groups that have the permissions for creating volumes from
-- the snapshot.
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
    { productCodes =
        Prelude.Nothing,
      createVolumePermissions =
        Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The product codes.
describeSnapshotAttributeResponse_productCodes :: Lens.Lens' DescribeSnapshotAttributeResponse (Prelude.Maybe [ProductCode])
describeSnapshotAttributeResponse_productCodes = Lens.lens (\DescribeSnapshotAttributeResponse' {productCodes} -> productCodes) (\s@DescribeSnapshotAttributeResponse' {} a -> s {productCodes = a} :: DescribeSnapshotAttributeResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The users and groups that have the permissions for creating volumes from
-- the snapshot.
describeSnapshotAttributeResponse_createVolumePermissions :: Lens.Lens' DescribeSnapshotAttributeResponse (Prelude.Maybe [CreateVolumePermission])
describeSnapshotAttributeResponse_createVolumePermissions = Lens.lens (\DescribeSnapshotAttributeResponse' {createVolumePermissions} -> createVolumePermissions) (\s@DescribeSnapshotAttributeResponse' {} a -> s {createVolumePermissions = a} :: DescribeSnapshotAttributeResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the EBS snapshot.
describeSnapshotAttributeResponse_snapshotId :: Lens.Lens' DescribeSnapshotAttributeResponse (Prelude.Maybe Prelude.Text)
describeSnapshotAttributeResponse_snapshotId = Lens.lens (\DescribeSnapshotAttributeResponse' {snapshotId} -> snapshotId) (\s@DescribeSnapshotAttributeResponse' {} a -> s {snapshotId = a} :: DescribeSnapshotAttributeResponse)

-- | The response's http status code.
describeSnapshotAttributeResponse_httpStatus :: Lens.Lens' DescribeSnapshotAttributeResponse Prelude.Int
describeSnapshotAttributeResponse_httpStatus = Lens.lens (\DescribeSnapshotAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotAttributeResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotAttributeResponse)

instance
  Prelude.NFData
    DescribeSnapshotAttributeResponse
