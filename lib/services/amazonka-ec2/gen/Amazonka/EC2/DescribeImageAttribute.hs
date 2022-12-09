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
-- Module      : Amazonka.EC2.DescribeImageAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified AMI. You can specify
-- only one attribute at a time.
module Amazonka.EC2.DescribeImageAttribute
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
    describeImageAttributeResponse_blockDeviceMappings,
    describeImageAttributeResponse_bootMode,
    describeImageAttributeResponse_description,
    describeImageAttributeResponse_imageId,
    describeImageAttributeResponse_imdsSupport,
    describeImageAttributeResponse_kernelId,
    describeImageAttributeResponse_lastLaunchedTime,
    describeImageAttributeResponse_launchPermissions,
    describeImageAttributeResponse_productCodes,
    describeImageAttributeResponse_ramdiskId,
    describeImageAttributeResponse_sriovNetSupport,
    describeImageAttributeResponse_tpmSupport,
    describeImageAttributeResponse_uefiData,
    describeImageAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- __Note__: The @blockDeviceMapping@ attribute is deprecated. Using this
    -- attribute returns the @Client.AuthFailure@ error. To get information
    -- about the block device mappings for an AMI, use the DescribeImages
    -- action.
    attribute :: ImageAttributeName,
    -- | The ID of the AMI.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- __Note__: The @blockDeviceMapping@ attribute is deprecated. Using this
-- attribute returns the @Client.AuthFailure@ error. To get information
-- about the block device mappings for an AMI, use the DescribeImages
-- action.
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
-- __Note__: The @blockDeviceMapping@ attribute is deprecated. Using this
-- attribute returns the @Client.AuthFailure@ error. To get information
-- about the block device mappings for an AMI, use the DescribeImages
-- action.
describeImageAttribute_attribute :: Lens.Lens' DescribeImageAttribute ImageAttributeName
describeImageAttribute_attribute = Lens.lens (\DescribeImageAttribute' {attribute} -> attribute) (\s@DescribeImageAttribute' {} a -> s {attribute = a} :: DescribeImageAttribute)

-- | The ID of the AMI.
describeImageAttribute_imageId :: Lens.Lens' DescribeImageAttribute Prelude.Text
describeImageAttribute_imageId = Lens.lens (\DescribeImageAttribute' {imageId} -> imageId) (\s@DescribeImageAttribute' {} a -> s {imageId = a} :: DescribeImageAttribute)

instance Core.AWSRequest DescribeImageAttribute where
  type
    AWSResponse DescribeImageAttribute =
      DescribeImageAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImageAttributeResponse'
            Prelude.<$> ( x Data..@? "blockDeviceMapping"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "bootMode")
            Prelude.<*> (x Data..@? "description")
            Prelude.<*> (x Data..@? "imageId")
            Prelude.<*> (x Data..@? "imdsSupport")
            Prelude.<*> (x Data..@? "kernel")
            Prelude.<*> (x Data..@? "lastLaunchedTime")
            Prelude.<*> ( x Data..@? "launchPermission"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x Data..@? "productCodes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "ramdisk")
            Prelude.<*> (x Data..@? "sriovNetSupport")
            Prelude.<*> (x Data..@? "tpmSupport")
            Prelude.<*> (x Data..@? "uefiData")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImageAttribute where
  hashWithSalt _salt DescribeImageAttribute' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData DescribeImageAttribute where
  rnf DescribeImageAttribute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders DescribeImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeImageAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImageAttribute where
  toQuery DescribeImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeImageAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Attribute" Data.=: attribute,
        "ImageId" Data.=: imageId
      ]

-- | Describes an image attribute.
--
-- /See:/ 'newDescribeImageAttributeResponse' smart constructor.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
  { -- | The block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The boot mode.
    bootMode :: Prelude.Maybe AttributeValue,
    -- | A description for the AMI.
    description :: Prelude.Maybe AttributeValue,
    -- | The ID of the AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | If @v2.0@, it indicates that IMDSv2 is specified in the AMI. Instances
    -- launched from this AMI will have @HttpTokens@ automatically set to
    -- @required@ so that, by default, the instance requires that IMDSv2 is
    -- used when requesting instance metadata. In addition,
    -- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    imdsSupport :: Prelude.Maybe AttributeValue,
    -- | The kernel ID.
    kernelId :: Prelude.Maybe AttributeValue,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the AMI
    -- was last used to launch an EC2 instance. When the AMI is used to launch
    -- an instance, there is a 24-hour delay before that usage is reported.
    --
    -- @lastLaunchedTime@ data is available starting April 2017.
    lastLaunchedTime :: Prelude.Maybe AttributeValue,
    -- | The launch permissions.
    launchPermissions :: Prelude.Maybe [LaunchPermission],
    -- | The product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The RAM disk ID.
    ramdiskId :: Prelude.Maybe AttributeValue,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Prelude.Maybe AttributeValue,
    -- | If the image is configured for NitroTPM support, the value is @v2.0@.
    tpmSupport :: Prelude.Maybe AttributeValue,
    -- | Base64 representation of the non-volatile UEFI variable store. To
    -- retrieve the UEFI data, use the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceUefiData GetInstanceUefiData>
    -- command. You can inspect and modify the UEFI data by using the
    -- <https://github.com/awslabs/python-uefivars python-uefivars tool> on
    -- GitHub. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/uefi-secure-boot.html UEFI Secure Boot>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    uefiData :: Prelude.Maybe AttributeValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDeviceMappings', 'describeImageAttributeResponse_blockDeviceMappings' - The block device mapping entries.
--
-- 'bootMode', 'describeImageAttributeResponse_bootMode' - The boot mode.
--
-- 'description', 'describeImageAttributeResponse_description' - A description for the AMI.
--
-- 'imageId', 'describeImageAttributeResponse_imageId' - The ID of the AMI.
--
-- 'imdsSupport', 'describeImageAttributeResponse_imdsSupport' - If @v2.0@, it indicates that IMDSv2 is specified in the AMI. Instances
-- launched from this AMI will have @HttpTokens@ automatically set to
-- @required@ so that, by default, the instance requires that IMDSv2 is
-- used when requesting instance metadata. In addition,
-- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'kernelId', 'describeImageAttributeResponse_kernelId' - The kernel ID.
--
-- 'lastLaunchedTime', 'describeImageAttributeResponse_lastLaunchedTime' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the AMI
-- was last used to launch an EC2 instance. When the AMI is used to launch
-- an instance, there is a 24-hour delay before that usage is reported.
--
-- @lastLaunchedTime@ data is available starting April 2017.
--
-- 'launchPermissions', 'describeImageAttributeResponse_launchPermissions' - The launch permissions.
--
-- 'productCodes', 'describeImageAttributeResponse_productCodes' - The product codes.
--
-- 'ramdiskId', 'describeImageAttributeResponse_ramdiskId' - The RAM disk ID.
--
-- 'sriovNetSupport', 'describeImageAttributeResponse_sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'tpmSupport', 'describeImageAttributeResponse_tpmSupport' - If the image is configured for NitroTPM support, the value is @v2.0@.
--
-- 'uefiData', 'describeImageAttributeResponse_uefiData' - Base64 representation of the non-volatile UEFI variable store. To
-- retrieve the UEFI data, use the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceUefiData GetInstanceUefiData>
-- command. You can inspect and modify the UEFI data by using the
-- <https://github.com/awslabs/python-uefivars python-uefivars tool> on
-- GitHub. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/uefi-secure-boot.html UEFI Secure Boot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'httpStatus', 'describeImageAttributeResponse_httpStatus' - The response's http status code.
newDescribeImageAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageAttributeResponse
newDescribeImageAttributeResponse pHttpStatus_ =
  DescribeImageAttributeResponse'
    { blockDeviceMappings =
        Prelude.Nothing,
      bootMode = Prelude.Nothing,
      description = Prelude.Nothing,
      imageId = Prelude.Nothing,
      imdsSupport = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      lastLaunchedTime = Prelude.Nothing,
      launchPermissions = Prelude.Nothing,
      productCodes = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      sriovNetSupport = Prelude.Nothing,
      tpmSupport = Prelude.Nothing,
      uefiData = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The block device mapping entries.
describeImageAttributeResponse_blockDeviceMappings :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe [BlockDeviceMapping])
describeImageAttributeResponse_blockDeviceMappings = Lens.lens (\DescribeImageAttributeResponse' {blockDeviceMappings} -> blockDeviceMappings) (\s@DescribeImageAttributeResponse' {} a -> s {blockDeviceMappings = a} :: DescribeImageAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The boot mode.
describeImageAttributeResponse_bootMode :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_bootMode = Lens.lens (\DescribeImageAttributeResponse' {bootMode} -> bootMode) (\s@DescribeImageAttributeResponse' {} a -> s {bootMode = a} :: DescribeImageAttributeResponse)

-- | A description for the AMI.
describeImageAttributeResponse_description :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_description = Lens.lens (\DescribeImageAttributeResponse' {description} -> description) (\s@DescribeImageAttributeResponse' {} a -> s {description = a} :: DescribeImageAttributeResponse)

-- | The ID of the AMI.
describeImageAttributeResponse_imageId :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe Prelude.Text)
describeImageAttributeResponse_imageId = Lens.lens (\DescribeImageAttributeResponse' {imageId} -> imageId) (\s@DescribeImageAttributeResponse' {} a -> s {imageId = a} :: DescribeImageAttributeResponse)

-- | If @v2.0@, it indicates that IMDSv2 is specified in the AMI. Instances
-- launched from this AMI will have @HttpTokens@ automatically set to
-- @required@ so that, by default, the instance requires that IMDSv2 is
-- used when requesting instance metadata. In addition,
-- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
describeImageAttributeResponse_imdsSupport :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_imdsSupport = Lens.lens (\DescribeImageAttributeResponse' {imdsSupport} -> imdsSupport) (\s@DescribeImageAttributeResponse' {} a -> s {imdsSupport = a} :: DescribeImageAttributeResponse)

-- | The kernel ID.
describeImageAttributeResponse_kernelId :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_kernelId = Lens.lens (\DescribeImageAttributeResponse' {kernelId} -> kernelId) (\s@DescribeImageAttributeResponse' {} a -> s {kernelId = a} :: DescribeImageAttributeResponse)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the AMI
-- was last used to launch an EC2 instance. When the AMI is used to launch
-- an instance, there is a 24-hour delay before that usage is reported.
--
-- @lastLaunchedTime@ data is available starting April 2017.
describeImageAttributeResponse_lastLaunchedTime :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_lastLaunchedTime = Lens.lens (\DescribeImageAttributeResponse' {lastLaunchedTime} -> lastLaunchedTime) (\s@DescribeImageAttributeResponse' {} a -> s {lastLaunchedTime = a} :: DescribeImageAttributeResponse)

-- | The launch permissions.
describeImageAttributeResponse_launchPermissions :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe [LaunchPermission])
describeImageAttributeResponse_launchPermissions = Lens.lens (\DescribeImageAttributeResponse' {launchPermissions} -> launchPermissions) (\s@DescribeImageAttributeResponse' {} a -> s {launchPermissions = a} :: DescribeImageAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The product codes.
describeImageAttributeResponse_productCodes :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe [ProductCode])
describeImageAttributeResponse_productCodes = Lens.lens (\DescribeImageAttributeResponse' {productCodes} -> productCodes) (\s@DescribeImageAttributeResponse' {} a -> s {productCodes = a} :: DescribeImageAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The RAM disk ID.
describeImageAttributeResponse_ramdiskId :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_ramdiskId = Lens.lens (\DescribeImageAttributeResponse' {ramdiskId} -> ramdiskId) (\s@DescribeImageAttributeResponse' {} a -> s {ramdiskId = a} :: DescribeImageAttributeResponse)

-- | Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
describeImageAttributeResponse_sriovNetSupport :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_sriovNetSupport = Lens.lens (\DescribeImageAttributeResponse' {sriovNetSupport} -> sriovNetSupport) (\s@DescribeImageAttributeResponse' {} a -> s {sriovNetSupport = a} :: DescribeImageAttributeResponse)

-- | If the image is configured for NitroTPM support, the value is @v2.0@.
describeImageAttributeResponse_tpmSupport :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_tpmSupport = Lens.lens (\DescribeImageAttributeResponse' {tpmSupport} -> tpmSupport) (\s@DescribeImageAttributeResponse' {} a -> s {tpmSupport = a} :: DescribeImageAttributeResponse)

-- | Base64 representation of the non-volatile UEFI variable store. To
-- retrieve the UEFI data, use the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceUefiData GetInstanceUefiData>
-- command. You can inspect and modify the UEFI data by using the
-- <https://github.com/awslabs/python-uefivars python-uefivars tool> on
-- GitHub. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/uefi-secure-boot.html UEFI Secure Boot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
describeImageAttributeResponse_uefiData :: Lens.Lens' DescribeImageAttributeResponse (Prelude.Maybe AttributeValue)
describeImageAttributeResponse_uefiData = Lens.lens (\DescribeImageAttributeResponse' {uefiData} -> uefiData) (\s@DescribeImageAttributeResponse' {} a -> s {uefiData = a} :: DescribeImageAttributeResponse)

-- | The response's http status code.
describeImageAttributeResponse_httpStatus :: Lens.Lens' DescribeImageAttributeResponse Prelude.Int
describeImageAttributeResponse_httpStatus = Lens.lens (\DescribeImageAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeImageAttributeResponse' {} a -> s {httpStatus = a} :: DescribeImageAttributeResponse)

instance
  Prelude.NFData
    DescribeImageAttributeResponse
  where
  rnf DescribeImageAttributeResponse' {..} =
    Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf imdsSupport
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf lastLaunchedTime
      `Prelude.seq` Prelude.rnf launchPermissions
      `Prelude.seq` Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf sriovNetSupport
      `Prelude.seq` Prelude.rnf tpmSupport
      `Prelude.seq` Prelude.rnf uefiData
      `Prelude.seq` Prelude.rnf httpStatus
