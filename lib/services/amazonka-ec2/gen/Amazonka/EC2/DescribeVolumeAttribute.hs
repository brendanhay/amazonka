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
-- Module      : Amazonka.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.DescribeVolumeAttribute
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVolumeAttribute' smart constructor.
data DescribeVolumeAttribute = DescribeVolumeAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The attribute of the volume. This parameter is required.
    attribute :: VolumeAttributeName,
    -- | The ID of the volume.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeVolumeAttribute
newDescribeVolumeAttribute pAttribute_ pVolumeId_ =
  DescribeVolumeAttribute'
    { dryRun = Prelude.Nothing,
      attribute = pAttribute_,
      volumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumeAttribute_dryRun :: Lens.Lens' DescribeVolumeAttribute (Prelude.Maybe Prelude.Bool)
describeVolumeAttribute_dryRun = Lens.lens (\DescribeVolumeAttribute' {dryRun} -> dryRun) (\s@DescribeVolumeAttribute' {} a -> s {dryRun = a} :: DescribeVolumeAttribute)

-- | The attribute of the volume. This parameter is required.
describeVolumeAttribute_attribute :: Lens.Lens' DescribeVolumeAttribute VolumeAttributeName
describeVolumeAttribute_attribute = Lens.lens (\DescribeVolumeAttribute' {attribute} -> attribute) (\s@DescribeVolumeAttribute' {} a -> s {attribute = a} :: DescribeVolumeAttribute)

-- | The ID of the volume.
describeVolumeAttribute_volumeId :: Lens.Lens' DescribeVolumeAttribute Prelude.Text
describeVolumeAttribute_volumeId = Lens.lens (\DescribeVolumeAttribute' {volumeId} -> volumeId) (\s@DescribeVolumeAttribute' {} a -> s {volumeId = a} :: DescribeVolumeAttribute)

instance Core.AWSRequest DescribeVolumeAttribute where
  type
    AWSResponse DescribeVolumeAttribute =
      DescribeVolumeAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumeAttributeResponse'
            Prelude.<$> ( x Core..@? "productCodes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "volumeId")
            Prelude.<*> (x Core..@? "autoEnableIO")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVolumeAttribute where
  hashWithSalt _salt DescribeVolumeAttribute' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData DescribeVolumeAttribute where
  rnf DescribeVolumeAttribute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf volumeId

instance Core.ToHeaders DescribeVolumeAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeVolumeAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeVolumeAttribute where
  toQuery DescribeVolumeAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeVolumeAttribute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Attribute" Core.=: attribute,
        "VolumeId" Core.=: volumeId
      ]

-- | /See:/ 'newDescribeVolumeAttributeResponse' smart constructor.
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
  { -- | A list of product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The ID of the volume.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The state of @autoEnableIO@ attribute.
    autoEnableIO :: Prelude.Maybe AttributeBooleanValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeVolumeAttributeResponse
newDescribeVolumeAttributeResponse pHttpStatus_ =
  DescribeVolumeAttributeResponse'
    { productCodes =
        Prelude.Nothing,
      volumeId = Prelude.Nothing,
      autoEnableIO = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of product codes.
describeVolumeAttributeResponse_productCodes :: Lens.Lens' DescribeVolumeAttributeResponse (Prelude.Maybe [ProductCode])
describeVolumeAttributeResponse_productCodes = Lens.lens (\DescribeVolumeAttributeResponse' {productCodes} -> productCodes) (\s@DescribeVolumeAttributeResponse' {} a -> s {productCodes = a} :: DescribeVolumeAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the volume.
describeVolumeAttributeResponse_volumeId :: Lens.Lens' DescribeVolumeAttributeResponse (Prelude.Maybe Prelude.Text)
describeVolumeAttributeResponse_volumeId = Lens.lens (\DescribeVolumeAttributeResponse' {volumeId} -> volumeId) (\s@DescribeVolumeAttributeResponse' {} a -> s {volumeId = a} :: DescribeVolumeAttributeResponse)

-- | The state of @autoEnableIO@ attribute.
describeVolumeAttributeResponse_autoEnableIO :: Lens.Lens' DescribeVolumeAttributeResponse (Prelude.Maybe AttributeBooleanValue)
describeVolumeAttributeResponse_autoEnableIO = Lens.lens (\DescribeVolumeAttributeResponse' {autoEnableIO} -> autoEnableIO) (\s@DescribeVolumeAttributeResponse' {} a -> s {autoEnableIO = a} :: DescribeVolumeAttributeResponse)

-- | The response's http status code.
describeVolumeAttributeResponse_httpStatus :: Lens.Lens' DescribeVolumeAttributeResponse Prelude.Int
describeVolumeAttributeResponse_httpStatus = Lens.lens (\DescribeVolumeAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumeAttributeResponse' {} a -> s {httpStatus = a} :: DescribeVolumeAttributeResponse)

instance
  Prelude.NFData
    DescribeVolumeAttributeResponse
  where
  rnf DescribeVolumeAttributeResponse' {..} =
    Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf autoEnableIO
      `Prelude.seq` Prelude.rnf httpStatus
