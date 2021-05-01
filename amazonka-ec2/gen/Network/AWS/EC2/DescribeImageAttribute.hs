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
-- Module      : Network.AWS.EC2.DescribeImageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified AMI. You can specify
-- only one attribute at a time.
module Network.AWS.EC2.DescribeImageAttribute
  ( -- * Creating a Request
    DescribeImageAttribute (..),
    newDescribeImageAttribute,

    -- * Request Lenses
    describeImageAttribute_dryRun,
    describeImageAttribute_attribute,
    describeImageAttribute_imageId,

    -- * Destructuring the Response
    DescribeImageAttributeResponse (..),
    newDescribeImageAttributeResponse,

    -- * Response Lenses
    describeImageAttributeResponse_ramdiskId,
    describeImageAttributeResponse_productCodes,
    describeImageAttributeResponse_launchPermissions,
    describeImageAttributeResponse_imageId,
    describeImageAttributeResponse_sriovNetSupport,
    describeImageAttributeResponse_blockDeviceMappings,
    describeImageAttributeResponse_kernelId,
    describeImageAttributeResponse_description,
    describeImageAttributeResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeImageAttribute.
--
-- /See:/ 'newDescribeImageAttribute' smart constructor.
data DescribeImageAttribute = DescribeImageAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The AMI attribute.
    --
    -- __Note__: Depending on your account privileges, the @blockDeviceMapping@
    -- attribute may return a @Client.AuthFailure@ error. If this happens, use
    -- DescribeImages to get information about the block device mapping for the
    -- AMI.
    attribute :: ImageAttributeName,
    -- | The ID of the AMI.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'describeImageAttribute_attribute' - The AMI attribute.
--
-- __Note__: Depending on your account privileges, the @blockDeviceMapping@
-- attribute may return a @Client.AuthFailure@ error. If this happens, use
-- DescribeImages to get information about the block device mapping for the
-- AMI.
--
-- 'imageId', 'describeImageAttribute_imageId' - The ID of the AMI.
newDescribeImageAttribute ::
  -- | 'attribute'
  ImageAttributeName ->
  -- | 'imageId'
  Prelude.Text ->
  DescribeImageAttribute
newDescribeImageAttribute pAttribute_ pImageId_ =
  DescribeImageAttribute'
    { dryRun = Prelude.Nothing,
      attribute = pAttribute_,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeImageAttribute_dryRun :: Lens.Lens' DescribeImageAttribute (Prelude.Maybe Prelude.Bool)
describeImageAttribute_dryRun = Lens.lens (\DescribeImageAttribute' {dryRun} -> dryRun) (\s@DescribeImageAttribute' {} a -> s {dryRun = a} :: DescribeImageAttribute)

-- | The AMI attribute.
--
-- __Note__: Depending on your account privileges, the @blockDeviceMapping@
-- attribute may return a @Client.AuthFailure@ error. If this happens, use
-- DescribeImages to get information about the block device mapping for the
-- AMI.
describeImageAttribute_attribute :: Lens.Lens' DescribeImageAttribute ImageAttributeName
describeImageAttribute_attribute = Lens.lens (\DescribeImageAttribute' {attribute} -> attribute) (\s@DescribeImageAttribute' {} a -> s {attribute = a} :: DescribeImageAttribute)

-- | The ID of the AMI.
describeImageAttribute_imageId :: Lens.Lens' DescribeImageAttribute Prelude.Text
describeImageAttribute_imageId = Lens.lens (\DescribeImageAttribute' {imageId} -> imageId) (\s@DescribeImageAttribute' {} a -> s {imageId = a} :: DescribeImageAttribute)

instance Prelude.AWSRequest DescribeImageAttribute where
  type
    Rs DescribeImageAttribute =
      DescribeImageAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImageAttributeResponse'
            Prelude.<$> (x Prelude..@? "ramdisk")
            Prelude.<*> ( x Prelude..@? "productCodes"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> ( x Prelude..@? "launchPermission"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (x Prelude..@? "imageId")
            Prelude.<*> (x Prelude..@? "sriovNetSupport")
            Prelude.<*> ( x Prelude..@? "blockDeviceMapping"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (x Prelude..@? "kernel")
            Prelude.<*> (x Prelude..@? "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImageAttribute

instance Prelude.NFData DescribeImageAttribute

instance Prelude.ToHeaders DescribeImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeImageAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeImageAttribute where
  toQuery DescribeImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeImageAttribute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "Attribute" Prelude.=: attribute,
        "ImageId" Prelude.=: imageId
      ]

-- | Describes an image attribute.
--
-- /See:/ 'newDescribeImageAttributeResponse' smart constructor.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
  { -- | The RAM disk ID.
    ramdiskId :: Prelude.Maybe AttributeValue,
    -- | The product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The launch permissions.
    launchPermissions :: Prelude.Maybe [LaunchPermission],
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Prelude.Maybe AttributeValue,
    -- | The block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The kernel ID.
    kernelId :: Prelude.Maybe AttributeValue,
    -- | A description for the AMI.
    description :: Prelude.Maybe AttributeValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ramdiskId', 'describeImageAttributeResponse_ramdiskId' - The RAM disk ID.
--
-- 'productCodes', 'describeImageAttributeResponse_productCodes' - The product codes.
--
-- 'launchPermissions', 'describeImageAttributeResponse_launchPermissions' - The launch permissions.
--
-- 'imageId', 'describeImageAttributeResponse_imageId' - The ID of the AMI.
--
-- 'sriovNetSupport', 'describeImageAttributeResponse_sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'blockDeviceMappings', 'describeImageAttributeResponse_blockDeviceMappings' - The block device mapping entries.
--
-- 'kernelId', 'describeImageAttributeResponse_kernelId' - The kernel ID.
--
-- 'description', 'describeImageAttributeResponse_description' - A description for the AMI.
--
-- 'httpStatus', 'describeImageAttributeResponse_httpStatus' - The response's http status code.
newDescribeImageAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageAttributeResponse
newDescribeImageAttributeResponse pHttpStatus_ =
  DescribeImageAttributeResponse'
    { ramdiskId =
        Prelude.Nothing,
      productCodes = Prelude.Nothing,
      launchPermissions = Prelude.Nothing,
      imageId = Prelude.Nothing,
      sriovNetSupport = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The RAM disk ID.
describeImageAttributeResponse_ramdiskId :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_ramdiskId = Lens.lens (\DescribeImageAttributeResponse' {ramdiskId} -> ramdiskId) (\s@DescribeImageAttributeResponse' {} a -> s {ramdiskId = a} :: DescribeImageAttributeResponse)

-- | The product codes.
describeImageAttributeResponse_productCodes :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe [ProductCode])
describeImageAttributeResponse_productCodes = Lens.lens (\DescribeImageAttributeResponse' {productCodes} -> productCodes) (\s@DescribeImageAttributeResponse' {} a -> s {productCodes = a} :: DescribeImageAttributeResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The launch permissions.
describeImageAttributeResponse_launchPermissions :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe [LaunchPermission])
describeImageAttributeResponse_launchPermissions = Lens.lens (\DescribeImageAttributeResponse' {launchPermissions} -> launchPermissions) (\s@DescribeImageAttributeResponse' {} a -> s {launchPermissions = a} :: DescribeImageAttributeResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the AMI.
describeImageAttributeResponse_imageId :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe Prelude.Text)
describeImageAttributeResponse_imageId = Lens.lens (\DescribeImageAttributeResponse' {imageId} -> imageId) (\s@DescribeImageAttributeResponse' {} a -> s {imageId = a} :: DescribeImageAttributeResponse)

-- | Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
describeImageAttributeResponse_sriovNetSupport :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_sriovNetSupport = Lens.lens (\DescribeImageAttributeResponse' {sriovNetSupport} -> sriovNetSupport) (\s@DescribeImageAttributeResponse' {} a -> s {sriovNetSupport = a} :: DescribeImageAttributeResponse)

-- | The block device mapping entries.
describeImageAttributeResponse_blockDeviceMappings :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe [BlockDeviceMapping])
describeImageAttributeResponse_blockDeviceMappings = Lens.lens (\DescribeImageAttributeResponse' {blockDeviceMappings} -> blockDeviceMappings) (\s@DescribeImageAttributeResponse' {} a -> s {blockDeviceMappings = a} :: DescribeImageAttributeResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The kernel ID.
describeImageAttributeResponse_kernelId :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_kernelId = Lens.lens (\DescribeImageAttributeResponse' {kernelId} -> kernelId) (\s@DescribeImageAttributeResponse' {} a -> s {kernelId = a} :: DescribeImageAttributeResponse)

-- | A description for the AMI.
describeImageAttributeResponse_description :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_description = Lens.lens (\DescribeImageAttributeResponse' {description} -> description) (\s@DescribeImageAttributeResponse' {} a -> s {description = a} :: DescribeImageAttributeResponse)

-- | The response's http status code.
describeImageAttributeResponse_httpStatus :: Lens.Lens' DescribeImageAttributeResponse Prelude.Int
describeImageAttributeResponse_httpStatus = Lens.lens (\DescribeImageAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeImageAttributeResponse' {} a -> s {httpStatus = a} :: DescribeImageAttributeResponse)

instance
  Prelude.NFData
    DescribeImageAttributeResponse
