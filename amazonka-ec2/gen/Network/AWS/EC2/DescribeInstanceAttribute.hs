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
-- Module      : Network.AWS.EC2.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified instance. You can
-- specify only one attribute at a time. Valid attribute values are:
-- @instanceType@ | @kernel@ | @ramdisk@ | @userData@ |
-- @disableApiTermination@ | @instanceInitiatedShutdownBehavior@ |
-- @rootDeviceName@ | @blockDeviceMapping@ | @productCodes@ |
-- @sourceDestCheck@ | @groupSet@ | @ebsOptimized@ | @sriovNetSupport@
module Network.AWS.EC2.DescribeInstanceAttribute
  ( -- * Creating a Request
    DescribeInstanceAttribute (..),
    newDescribeInstanceAttribute,

    -- * Request Lenses
    describeInstanceAttribute_dryRun,
    describeInstanceAttribute_attribute,
    describeInstanceAttribute_instanceId,

    -- * Destructuring the Response
    DescribeInstanceAttributeResponse (..),
    newDescribeInstanceAttributeResponse,

    -- * Response Lenses
    describeInstanceAttributeResponse_groups,
    describeInstanceAttributeResponse_instanceId,
    describeInstanceAttributeResponse_instanceType,
    describeInstanceAttributeResponse_rootDeviceName,
    describeInstanceAttributeResponse_ebsOptimized,
    describeInstanceAttributeResponse_userData,
    describeInstanceAttributeResponse_ramdiskId,
    describeInstanceAttributeResponse_sourceDestCheck,
    describeInstanceAttributeResponse_productCodes,
    describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior,
    describeInstanceAttributeResponse_sriovNetSupport,
    describeInstanceAttributeResponse_blockDeviceMappings,
    describeInstanceAttributeResponse_enclaveOptions,
    describeInstanceAttributeResponse_kernelId,
    describeInstanceAttributeResponse_disableApiTermination,
    describeInstanceAttributeResponse_enaSupport,
    describeInstanceAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The instance attribute.
    --
    -- Note: The @enaSupport@ attribute is not supported at this time.
    attribute :: InstanceAttributeName,
    -- | The ID of the instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeInstanceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'describeInstanceAttribute_attribute' - The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
--
-- 'instanceId', 'describeInstanceAttribute_instanceId' - The ID of the instance.
newDescribeInstanceAttribute ::
  -- | 'attribute'
  InstanceAttributeName ->
  -- | 'instanceId'
  Core.Text ->
  DescribeInstanceAttribute
newDescribeInstanceAttribute pAttribute_ pInstanceId_ =
  DescribeInstanceAttribute'
    { dryRun = Core.Nothing,
      attribute = pAttribute_,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceAttribute_dryRun :: Lens.Lens' DescribeInstanceAttribute (Core.Maybe Core.Bool)
describeInstanceAttribute_dryRun = Lens.lens (\DescribeInstanceAttribute' {dryRun} -> dryRun) (\s@DescribeInstanceAttribute' {} a -> s {dryRun = a} :: DescribeInstanceAttribute)

-- | The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
describeInstanceAttribute_attribute :: Lens.Lens' DescribeInstanceAttribute InstanceAttributeName
describeInstanceAttribute_attribute = Lens.lens (\DescribeInstanceAttribute' {attribute} -> attribute) (\s@DescribeInstanceAttribute' {} a -> s {attribute = a} :: DescribeInstanceAttribute)

-- | The ID of the instance.
describeInstanceAttribute_instanceId :: Lens.Lens' DescribeInstanceAttribute Core.Text
describeInstanceAttribute_instanceId = Lens.lens (\DescribeInstanceAttribute' {instanceId} -> instanceId) (\s@DescribeInstanceAttribute' {} a -> s {instanceId = a} :: DescribeInstanceAttribute)

instance Core.AWSRequest DescribeInstanceAttribute where
  type
    AWSResponse DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            Core.<$> ( x Core..@? "groupSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "instanceId")
            Core.<*> (x Core..@? "instanceType")
            Core.<*> (x Core..@? "rootDeviceName")
            Core.<*> (x Core..@? "ebsOptimized")
            Core.<*> (x Core..@? "userData")
            Core.<*> (x Core..@? "ramdisk")
            Core.<*> (x Core..@? "sourceDestCheck")
            Core.<*> ( x Core..@? "productCodes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "instanceInitiatedShutdownBehavior")
            Core.<*> (x Core..@? "sriovNetSupport")
            Core.<*> ( x Core..@? "blockDeviceMapping" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "enclaveOptions")
            Core.<*> (x Core..@? "kernel")
            Core.<*> (x Core..@? "disableApiTermination")
            Core.<*> (x Core..@? "enaSupport")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstanceAttribute

instance Core.NFData DescribeInstanceAttribute

instance Core.ToHeaders DescribeInstanceAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeInstanceAttribute where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInstanceAttribute where
  toQuery DescribeInstanceAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Attribute" Core.=: attribute,
        "InstanceId" Core.=: instanceId
      ]

-- | Describes an instance attribute.
--
-- /See:/ 'newDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The security groups associated with the instance.
    groups :: Core.Maybe [GroupIdentifier],
    -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The instance type.
    instanceType :: Core.Maybe AttributeValue,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Core.Maybe AttributeValue,
    -- | Indicates whether the instance is optimized for Amazon EBS I\/O.
    ebsOptimized :: Core.Maybe AttributeBooleanValue,
    -- | The user data.
    userData :: Core.Maybe AttributeValue,
    -- | The RAM disk ID.
    ramdiskId :: Core.Maybe AttributeValue,
    -- | Indicates whether source\/destination checking is enabled. A value of
    -- @true@ means that checking is enabled, and @false@ means that checking
    -- is disabled. This value must be @false@ for a NAT instance to perform
    -- NAT.
    sourceDestCheck :: Core.Maybe AttributeBooleanValue,
    -- | A list of product codes.
    productCodes :: Core.Maybe [ProductCode],
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Core.Maybe AttributeValue,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Core.Maybe AttributeValue,
    -- | The block device mapping of the instance.
    blockDeviceMappings :: Core.Maybe [InstanceBlockDeviceMapping],
    -- | To enable the instance for AWS Nitro Enclaves, set this parameter to
    -- @true@; otherwise, set it to @false@.
    enclaveOptions :: Core.Maybe EnclaveOptions,
    -- | The kernel ID.
    kernelId :: Core.Maybe AttributeValue,
    -- | If the value is @true@, you can\'t terminate the instance through the
    -- Amazon EC2 console, CLI, or API; otherwise, you can.
    disableApiTermination :: Core.Maybe AttributeBooleanValue,
    -- | Indicates whether enhanced networking with ENA is enabled.
    enaSupport :: Core.Maybe AttributeBooleanValue,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'describeInstanceAttributeResponse_groups' - The security groups associated with the instance.
--
-- 'instanceId', 'describeInstanceAttributeResponse_instanceId' - The ID of the instance.
--
-- 'instanceType', 'describeInstanceAttributeResponse_instanceType' - The instance type.
--
-- 'rootDeviceName', 'describeInstanceAttributeResponse_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'ebsOptimized', 'describeInstanceAttributeResponse_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O.
--
-- 'userData', 'describeInstanceAttributeResponse_userData' - The user data.
--
-- 'ramdiskId', 'describeInstanceAttributeResponse_ramdiskId' - The RAM disk ID.
--
-- 'sourceDestCheck', 'describeInstanceAttributeResponse_sourceDestCheck' - Indicates whether source\/destination checking is enabled. A value of
-- @true@ means that checking is enabled, and @false@ means that checking
-- is disabled. This value must be @false@ for a NAT instance to perform
-- NAT.
--
-- 'productCodes', 'describeInstanceAttributeResponse_productCodes' - A list of product codes.
--
-- 'instanceInitiatedShutdownBehavior', 'describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- 'sriovNetSupport', 'describeInstanceAttributeResponse_sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'blockDeviceMappings', 'describeInstanceAttributeResponse_blockDeviceMappings' - The block device mapping of the instance.
--
-- 'enclaveOptions', 'describeInstanceAttributeResponse_enclaveOptions' - To enable the instance for AWS Nitro Enclaves, set this parameter to
-- @true@; otherwise, set it to @false@.
--
-- 'kernelId', 'describeInstanceAttributeResponse_kernelId' - The kernel ID.
--
-- 'disableApiTermination', 'describeInstanceAttributeResponse_disableApiTermination' - If the value is @true@, you can\'t terminate the instance through the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
--
-- 'enaSupport', 'describeInstanceAttributeResponse_enaSupport' - Indicates whether enhanced networking with ENA is enabled.
--
-- 'httpStatus', 'describeInstanceAttributeResponse_httpStatus' - The response's http status code.
newDescribeInstanceAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstanceAttributeResponse
newDescribeInstanceAttributeResponse pHttpStatus_ =
  DescribeInstanceAttributeResponse'
    { groups =
        Core.Nothing,
      instanceId = Core.Nothing,
      instanceType = Core.Nothing,
      rootDeviceName = Core.Nothing,
      ebsOptimized = Core.Nothing,
      userData = Core.Nothing,
      ramdiskId = Core.Nothing,
      sourceDestCheck = Core.Nothing,
      productCodes = Core.Nothing,
      instanceInitiatedShutdownBehavior =
        Core.Nothing,
      sriovNetSupport = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      enclaveOptions = Core.Nothing,
      kernelId = Core.Nothing,
      disableApiTermination = Core.Nothing,
      enaSupport = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The security groups associated with the instance.
describeInstanceAttributeResponse_groups :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe [GroupIdentifier])
describeInstanceAttributeResponse_groups = Lens.lens (\DescribeInstanceAttributeResponse' {groups} -> groups) (\s@DescribeInstanceAttributeResponse' {} a -> s {groups = a} :: DescribeInstanceAttributeResponse) Core.. Lens.mapping Lens._Coerce

-- | The ID of the instance.
describeInstanceAttributeResponse_instanceId :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Core.Text)
describeInstanceAttributeResponse_instanceId = Lens.lens (\DescribeInstanceAttributeResponse' {instanceId} -> instanceId) (\s@DescribeInstanceAttributeResponse' {} a -> s {instanceId = a} :: DescribeInstanceAttributeResponse)

-- | The instance type.
describeInstanceAttributeResponse_instanceType :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeValue)
describeInstanceAttributeResponse_instanceType = Lens.lens (\DescribeInstanceAttributeResponse' {instanceType} -> instanceType) (\s@DescribeInstanceAttributeResponse' {} a -> s {instanceType = a} :: DescribeInstanceAttributeResponse)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
describeInstanceAttributeResponse_rootDeviceName :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeValue)
describeInstanceAttributeResponse_rootDeviceName = Lens.lens (\DescribeInstanceAttributeResponse' {rootDeviceName} -> rootDeviceName) (\s@DescribeInstanceAttributeResponse' {} a -> s {rootDeviceName = a} :: DescribeInstanceAttributeResponse)

-- | Indicates whether the instance is optimized for Amazon EBS I\/O.
describeInstanceAttributeResponse_ebsOptimized :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_ebsOptimized = Lens.lens (\DescribeInstanceAttributeResponse' {ebsOptimized} -> ebsOptimized) (\s@DescribeInstanceAttributeResponse' {} a -> s {ebsOptimized = a} :: DescribeInstanceAttributeResponse)

-- | The user data.
describeInstanceAttributeResponse_userData :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeValue)
describeInstanceAttributeResponse_userData = Lens.lens (\DescribeInstanceAttributeResponse' {userData} -> userData) (\s@DescribeInstanceAttributeResponse' {} a -> s {userData = a} :: DescribeInstanceAttributeResponse)

-- | The RAM disk ID.
describeInstanceAttributeResponse_ramdiskId :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeValue)
describeInstanceAttributeResponse_ramdiskId = Lens.lens (\DescribeInstanceAttributeResponse' {ramdiskId} -> ramdiskId) (\s@DescribeInstanceAttributeResponse' {} a -> s {ramdiskId = a} :: DescribeInstanceAttributeResponse)

-- | Indicates whether source\/destination checking is enabled. A value of
-- @true@ means that checking is enabled, and @false@ means that checking
-- is disabled. This value must be @false@ for a NAT instance to perform
-- NAT.
describeInstanceAttributeResponse_sourceDestCheck :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_sourceDestCheck = Lens.lens (\DescribeInstanceAttributeResponse' {sourceDestCheck} -> sourceDestCheck) (\s@DescribeInstanceAttributeResponse' {} a -> s {sourceDestCheck = a} :: DescribeInstanceAttributeResponse)

-- | A list of product codes.
describeInstanceAttributeResponse_productCodes :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe [ProductCode])
describeInstanceAttributeResponse_productCodes = Lens.lens (\DescribeInstanceAttributeResponse' {productCodes} -> productCodes) (\s@DescribeInstanceAttributeResponse' {} a -> s {productCodes = a} :: DescribeInstanceAttributeResponse) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeValue)
describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior = Lens.lens (\DescribeInstanceAttributeResponse' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@DescribeInstanceAttributeResponse' {} a -> s {instanceInitiatedShutdownBehavior = a} :: DescribeInstanceAttributeResponse)

-- | Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
describeInstanceAttributeResponse_sriovNetSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeValue)
describeInstanceAttributeResponse_sriovNetSupport = Lens.lens (\DescribeInstanceAttributeResponse' {sriovNetSupport} -> sriovNetSupport) (\s@DescribeInstanceAttributeResponse' {} a -> s {sriovNetSupport = a} :: DescribeInstanceAttributeResponse)

-- | The block device mapping of the instance.
describeInstanceAttributeResponse_blockDeviceMappings :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe [InstanceBlockDeviceMapping])
describeInstanceAttributeResponse_blockDeviceMappings = Lens.lens (\DescribeInstanceAttributeResponse' {blockDeviceMappings} -> blockDeviceMappings) (\s@DescribeInstanceAttributeResponse' {} a -> s {blockDeviceMappings = a} :: DescribeInstanceAttributeResponse) Core.. Lens.mapping Lens._Coerce

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to
-- @true@; otherwise, set it to @false@.
describeInstanceAttributeResponse_enclaveOptions :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe EnclaveOptions)
describeInstanceAttributeResponse_enclaveOptions = Lens.lens (\DescribeInstanceAttributeResponse' {enclaveOptions} -> enclaveOptions) (\s@DescribeInstanceAttributeResponse' {} a -> s {enclaveOptions = a} :: DescribeInstanceAttributeResponse)

-- | The kernel ID.
describeInstanceAttributeResponse_kernelId :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeValue)
describeInstanceAttributeResponse_kernelId = Lens.lens (\DescribeInstanceAttributeResponse' {kernelId} -> kernelId) (\s@DescribeInstanceAttributeResponse' {} a -> s {kernelId = a} :: DescribeInstanceAttributeResponse)

-- | If the value is @true@, you can\'t terminate the instance through the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
describeInstanceAttributeResponse_disableApiTermination :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_disableApiTermination = Lens.lens (\DescribeInstanceAttributeResponse' {disableApiTermination} -> disableApiTermination) (\s@DescribeInstanceAttributeResponse' {} a -> s {disableApiTermination = a} :: DescribeInstanceAttributeResponse)

-- | Indicates whether enhanced networking with ENA is enabled.
describeInstanceAttributeResponse_enaSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_enaSupport = Lens.lens (\DescribeInstanceAttributeResponse' {enaSupport} -> enaSupport) (\s@DescribeInstanceAttributeResponse' {} a -> s {enaSupport = a} :: DescribeInstanceAttributeResponse)

-- | The response's http status code.
describeInstanceAttributeResponse_httpStatus :: Lens.Lens' DescribeInstanceAttributeResponse Core.Int
describeInstanceAttributeResponse_httpStatus = Lens.lens (\DescribeInstanceAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceAttributeResponse' {} a -> s {httpStatus = a} :: DescribeInstanceAttributeResponse)

instance
  Core.NFData
    DescribeInstanceAttributeResponse
