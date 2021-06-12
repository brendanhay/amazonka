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
-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified volume. You can
-- specify only one attribute at a time.
--
-- For more information about EBS volumes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DescribeVolumeAttribute
  ( -- * Creating a Request
    DescribeVolumeAttribute (..),
    newDescribeVolumeAttribute,

    -- * Request Lenses
    describeVolumeAttribute_dryRun,
    describeVolumeAttribute_attribute,
    describeVolumeAttribute_volumeId,

    -- * Destructuring the Response
    DescribeVolumeAttributeResponse (..),
    newDescribeVolumeAttributeResponse,

    -- * Response Lenses
    describeVolumeAttributeResponse_productCodes,
    describeVolumeAttributeResponse_volumeId,
    describeVolumeAttributeResponse_autoEnableIO,
    describeVolumeAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVolumeAttribute' smart constructor.
data DescribeVolumeAttribute = DescribeVolumeAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The attribute of the volume. This parameter is required.
    attribute :: VolumeAttributeName,
    -- | The ID of the volume.
    volumeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVolumeAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVolumeAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'describeVolumeAttribute_attribute' - The attribute of the volume. This parameter is required.
--
-- 'volumeId', 'describeVolumeAttribute_volumeId' - The ID of the volume.
newDescribeVolumeAttribute ::
  -- | 'attribute'
  VolumeAttributeName ->
  -- | 'volumeId'
  Core.Text ->
  DescribeVolumeAttribute
newDescribeVolumeAttribute pAttribute_ pVolumeId_ =
  DescribeVolumeAttribute'
    { dryRun = Core.Nothing,
      attribute = pAttribute_,
      volumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumeAttribute_dryRun :: Lens.Lens' DescribeVolumeAttribute (Core.Maybe Core.Bool)
describeVolumeAttribute_dryRun = Lens.lens (\DescribeVolumeAttribute' {dryRun} -> dryRun) (\s@DescribeVolumeAttribute' {} a -> s {dryRun = a} :: DescribeVolumeAttribute)

-- | The attribute of the volume. This parameter is required.
describeVolumeAttribute_attribute :: Lens.Lens' DescribeVolumeAttribute VolumeAttributeName
describeVolumeAttribute_attribute = Lens.lens (\DescribeVolumeAttribute' {attribute} -> attribute) (\s@DescribeVolumeAttribute' {} a -> s {attribute = a} :: DescribeVolumeAttribute)

-- | The ID of the volume.
describeVolumeAttribute_volumeId :: Lens.Lens' DescribeVolumeAttribute Core.Text
describeVolumeAttribute_volumeId = Lens.lens (\DescribeVolumeAttribute' {volumeId} -> volumeId) (\s@DescribeVolumeAttribute' {} a -> s {volumeId = a} :: DescribeVolumeAttribute)

instance Core.AWSRequest DescribeVolumeAttribute where
  type
    AWSResponse DescribeVolumeAttribute =
      DescribeVolumeAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumeAttributeResponse'
            Core.<$> ( x Core..@? "productCodes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "volumeId")
            Core.<*> (x Core..@? "autoEnableIO")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVolumeAttribute

instance Core.NFData DescribeVolumeAttribute

instance Core.ToHeaders DescribeVolumeAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVolumeAttribute where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVolumeAttribute where
  toQuery DescribeVolumeAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeVolumeAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Attribute" Core.=: attribute,
        "VolumeId" Core.=: volumeId
      ]

-- | /See:/ 'newDescribeVolumeAttributeResponse' smart constructor.
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
  { -- | A list of product codes.
    productCodes :: Core.Maybe [ProductCode],
    -- | The ID of the volume.
    volumeId :: Core.Maybe Core.Text,
    -- | The state of @autoEnableIO@ attribute.
    autoEnableIO :: Core.Maybe AttributeBooleanValue,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVolumeAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productCodes', 'describeVolumeAttributeResponse_productCodes' - A list of product codes.
--
-- 'volumeId', 'describeVolumeAttributeResponse_volumeId' - The ID of the volume.
--
-- 'autoEnableIO', 'describeVolumeAttributeResponse_autoEnableIO' - The state of @autoEnableIO@ attribute.
--
-- 'httpStatus', 'describeVolumeAttributeResponse_httpStatus' - The response's http status code.
newDescribeVolumeAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVolumeAttributeResponse
newDescribeVolumeAttributeResponse pHttpStatus_ =
  DescribeVolumeAttributeResponse'
    { productCodes =
        Core.Nothing,
      volumeId = Core.Nothing,
      autoEnableIO = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of product codes.
describeVolumeAttributeResponse_productCodes :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe [ProductCode])
describeVolumeAttributeResponse_productCodes = Lens.lens (\DescribeVolumeAttributeResponse' {productCodes} -> productCodes) (\s@DescribeVolumeAttributeResponse' {} a -> s {productCodes = a} :: DescribeVolumeAttributeResponse) Core.. Lens.mapping Lens._Coerce

-- | The ID of the volume.
describeVolumeAttributeResponse_volumeId :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe Core.Text)
describeVolumeAttributeResponse_volumeId = Lens.lens (\DescribeVolumeAttributeResponse' {volumeId} -> volumeId) (\s@DescribeVolumeAttributeResponse' {} a -> s {volumeId = a} :: DescribeVolumeAttributeResponse)

-- | The state of @autoEnableIO@ attribute.
describeVolumeAttributeResponse_autoEnableIO :: Lens.Lens' DescribeVolumeAttributeResponse (Core.Maybe AttributeBooleanValue)
describeVolumeAttributeResponse_autoEnableIO = Lens.lens (\DescribeVolumeAttributeResponse' {autoEnableIO} -> autoEnableIO) (\s@DescribeVolumeAttributeResponse' {} a -> s {autoEnableIO = a} :: DescribeVolumeAttributeResponse)

-- | The response's http status code.
describeVolumeAttributeResponse_httpStatus :: Lens.Lens' DescribeVolumeAttributeResponse Core.Int
describeVolumeAttributeResponse_httpStatus = Lens.lens (\DescribeVolumeAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumeAttributeResponse' {} a -> s {httpStatus = a} :: DescribeVolumeAttributeResponse)

instance Core.NFData DescribeVolumeAttributeResponse
