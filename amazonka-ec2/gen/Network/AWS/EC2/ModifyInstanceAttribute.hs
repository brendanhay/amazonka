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
-- Module      : Network.AWS.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified instance. You can
-- specify only one attribute at a time.
--
-- __Note:__ Using this action to change the security groups associated
-- with an elastic network interface (ENI) attached to an instance in a VPC
-- can result in an error if the instance has more than one ENI. To change
-- the security groups associated with an ENI attached to an instance that
-- has multiple ENIs, we recommend that you use the
-- ModifyNetworkInterfaceAttribute action.
--
-- To modify some attributes, the instance must be stopped. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ChangingAttributesWhileInstanceStopped.html Modifying attributes of a stopped instance>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.ModifyInstanceAttribute
  ( -- * Creating a Request
    ModifyInstanceAttribute (..),
    newModifyInstanceAttribute,

    -- * Request Lenses
    modifyInstanceAttribute_groups,
    modifyInstanceAttribute_instanceType,
    modifyInstanceAttribute_ebsOptimized,
    modifyInstanceAttribute_userData,
    modifyInstanceAttribute_dryRun,
    modifyInstanceAttribute_kernel,
    modifyInstanceAttribute_sourceDestCheck,
    modifyInstanceAttribute_instanceInitiatedShutdownBehavior,
    modifyInstanceAttribute_attribute,
    modifyInstanceAttribute_sriovNetSupport,
    modifyInstanceAttribute_blockDeviceMappings,
    modifyInstanceAttribute_value,
    modifyInstanceAttribute_ramdisk,
    modifyInstanceAttribute_disableApiTermination,
    modifyInstanceAttribute_enaSupport,
    modifyInstanceAttribute_instanceId,

    -- * Destructuring the Response
    ModifyInstanceAttributeResponse (..),
    newModifyInstanceAttributeResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstanceAttribute' smart constructor.
data ModifyInstanceAttribute = ModifyInstanceAttribute'
  { -- | [EC2-VPC] Changes the security groups of the instance. You must specify
    -- at least one security group, even if it\'s just the default security
    -- group for the VPC. You must specify the security group ID, not the
    -- security group name.
    groups :: Core.Maybe [Core.Text],
    -- | Changes the instance type to the specified value. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
    -- error returned is @InvalidInstanceAttributeValue@.
    instanceType :: Core.Maybe AttributeValue,
    -- | Specifies whether the instance is optimized for Amazon EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    ebsOptimized :: Core.Maybe AttributeBooleanValue,
    -- | Changes the instance\'s user data to the specified value. If you are
    -- using an AWS SDK or command line tool, base64-encoding is performed for
    -- you, and you can load the text from a file. Otherwise, you must provide
    -- base64-encoded text.
    userData :: Core.Maybe BlobAttributeValue,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Changes the instance\'s kernel to the specified value. We recommend that
    -- you use PV-GRUB instead of kernels and RAM disks. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
    kernel :: Core.Maybe AttributeValue,
    -- | Specifies whether source\/destination checking is enabled. A value of
    -- @true@ means that checking is enabled, and @false@ means that checking
    -- is disabled. This value must be @false@ for a NAT instance to perform
    -- NAT.
    sourceDestCheck :: Core.Maybe AttributeBooleanValue,
    -- | Specifies whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Core.Maybe AttributeValue,
    -- | The name of the attribute.
    attribute :: Core.Maybe InstanceAttributeName,
    -- | Set to @simple@ to enable enhanced networking with the Intel 82599
    -- Virtual Function interface for the instance.
    --
    -- There is no way to disable enhanced networking with the Intel 82599
    -- Virtual Function interface at this time.
    --
    -- This option is supported only for HVM instances. Specifying this option
    -- with a PV instance can make it unreachable.
    sriovNetSupport :: Core.Maybe AttributeValue,
    -- | Modifies the @DeleteOnTermination@ attribute for volumes that are
    -- currently attached. The volume must be owned by the caller. If no value
    -- is specified for @DeleteOnTermination@, the default is @true@ and the
    -- volume is deleted when the instance is terminated.
    --
    -- To add instance store volumes to an Amazon EBS-backed instance, you must
    -- add them when you launch the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance>
    -- in the /Amazon EC2 User Guide/.
    blockDeviceMappings :: Core.Maybe [InstanceBlockDeviceMappingSpecification],
    -- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
    -- @userData@, @disableApiTermination@, or
    -- @instanceInitiatedShutdownBehavior@ attribute.
    value :: Core.Maybe Core.Text,
    -- | Changes the instance\'s RAM disk to the specified value. We recommend
    -- that you use PV-GRUB instead of kernels and RAM disks. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
    ramdisk :: Core.Maybe AttributeValue,
    -- | If the value is @true@, you can\'t terminate the instance using the
    -- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
    -- parameter for Spot Instances.
    disableApiTermination :: Core.Maybe AttributeBooleanValue,
    -- | Set to @true@ to enable enhanced networking with ENA for the instance.
    --
    -- This option is supported only for HVM instances. Specifying this option
    -- with a PV instance can make it unreachable.
    enaSupport :: Core.Maybe AttributeBooleanValue,
    -- | The ID of the instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'modifyInstanceAttribute_groups' - [EC2-VPC] Changes the security groups of the instance. You must specify
-- at least one security group, even if it\'s just the default security
-- group for the VPC. You must specify the security group ID, not the
-- security group name.
--
-- 'instanceType', 'modifyInstanceAttribute_instanceType' - Changes the instance type to the specified value. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
-- error returned is @InvalidInstanceAttributeValue@.
--
-- 'ebsOptimized', 'modifyInstanceAttribute_ebsOptimized' - Specifies whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- 'userData', 'modifyInstanceAttribute_userData' - Changes the instance\'s user data to the specified value. If you are
-- using an AWS SDK or command line tool, base64-encoding is performed for
-- you, and you can load the text from a file. Otherwise, you must provide
-- base64-encoded text.
--
-- 'dryRun', 'modifyInstanceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'kernel', 'modifyInstanceAttribute_kernel' - Changes the instance\'s kernel to the specified value. We recommend that
-- you use PV-GRUB instead of kernels and RAM disks. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
--
-- 'sourceDestCheck', 'modifyInstanceAttribute_sourceDestCheck' - Specifies whether source\/destination checking is enabled. A value of
-- @true@ means that checking is enabled, and @false@ means that checking
-- is disabled. This value must be @false@ for a NAT instance to perform
-- NAT.
--
-- 'instanceInitiatedShutdownBehavior', 'modifyInstanceAttribute_instanceInitiatedShutdownBehavior' - Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- 'attribute', 'modifyInstanceAttribute_attribute' - The name of the attribute.
--
-- 'sriovNetSupport', 'modifyInstanceAttribute_sriovNetSupport' - Set to @simple@ to enable enhanced networking with the Intel 82599
-- Virtual Function interface for the instance.
--
-- There is no way to disable enhanced networking with the Intel 82599
-- Virtual Function interface at this time.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
--
-- 'blockDeviceMappings', 'modifyInstanceAttribute_blockDeviceMappings' - Modifies the @DeleteOnTermination@ attribute for volumes that are
-- currently attached. The volume must be owned by the caller. If no value
-- is specified for @DeleteOnTermination@, the default is @true@ and the
-- volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must
-- add them when you launch the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance>
-- in the /Amazon EC2 User Guide/.
--
-- 'value', 'modifyInstanceAttribute_value' - A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @instanceInitiatedShutdownBehavior@ attribute.
--
-- 'ramdisk', 'modifyInstanceAttribute_ramdisk' - Changes the instance\'s RAM disk to the specified value. We recommend
-- that you use PV-GRUB instead of kernels and RAM disks. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
--
-- 'disableApiTermination', 'modifyInstanceAttribute_disableApiTermination' - If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
-- parameter for Spot Instances.
--
-- 'enaSupport', 'modifyInstanceAttribute_enaSupport' - Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
--
-- 'instanceId', 'modifyInstanceAttribute_instanceId' - The ID of the instance.
newModifyInstanceAttribute ::
  -- | 'instanceId'
  Core.Text ->
  ModifyInstanceAttribute
newModifyInstanceAttribute pInstanceId_ =
  ModifyInstanceAttribute'
    { groups = Core.Nothing,
      instanceType = Core.Nothing,
      ebsOptimized = Core.Nothing,
      userData = Core.Nothing,
      dryRun = Core.Nothing,
      kernel = Core.Nothing,
      sourceDestCheck = Core.Nothing,
      instanceInitiatedShutdownBehavior = Core.Nothing,
      attribute = Core.Nothing,
      sriovNetSupport = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      value = Core.Nothing,
      ramdisk = Core.Nothing,
      disableApiTermination = Core.Nothing,
      enaSupport = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | [EC2-VPC] Changes the security groups of the instance. You must specify
-- at least one security group, even if it\'s just the default security
-- group for the VPC. You must specify the security group ID, not the
-- security group name.
modifyInstanceAttribute_groups :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe [Core.Text])
modifyInstanceAttribute_groups = Lens.lens (\ModifyInstanceAttribute' {groups} -> groups) (\s@ModifyInstanceAttribute' {} a -> s {groups = a} :: ModifyInstanceAttribute) Core.. Lens.mapping Lens._Coerce

-- | Changes the instance type to the specified value. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
-- error returned is @InvalidInstanceAttributeValue@.
modifyInstanceAttribute_instanceType :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeValue)
modifyInstanceAttribute_instanceType = Lens.lens (\ModifyInstanceAttribute' {instanceType} -> instanceType) (\s@ModifyInstanceAttribute' {} a -> s {instanceType = a} :: ModifyInstanceAttribute)

-- | Specifies whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
modifyInstanceAttribute_ebsOptimized :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeBooleanValue)
modifyInstanceAttribute_ebsOptimized = Lens.lens (\ModifyInstanceAttribute' {ebsOptimized} -> ebsOptimized) (\s@ModifyInstanceAttribute' {} a -> s {ebsOptimized = a} :: ModifyInstanceAttribute)

-- | Changes the instance\'s user data to the specified value. If you are
-- using an AWS SDK or command line tool, base64-encoding is performed for
-- you, and you can load the text from a file. Otherwise, you must provide
-- base64-encoded text.
modifyInstanceAttribute_userData :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe BlobAttributeValue)
modifyInstanceAttribute_userData = Lens.lens (\ModifyInstanceAttribute' {userData} -> userData) (\s@ModifyInstanceAttribute' {} a -> s {userData = a} :: ModifyInstanceAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceAttribute_dryRun :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Core.Bool)
modifyInstanceAttribute_dryRun = Lens.lens (\ModifyInstanceAttribute' {dryRun} -> dryRun) (\s@ModifyInstanceAttribute' {} a -> s {dryRun = a} :: ModifyInstanceAttribute)

-- | Changes the instance\'s kernel to the specified value. We recommend that
-- you use PV-GRUB instead of kernels and RAM disks. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
modifyInstanceAttribute_kernel :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeValue)
modifyInstanceAttribute_kernel = Lens.lens (\ModifyInstanceAttribute' {kernel} -> kernel) (\s@ModifyInstanceAttribute' {} a -> s {kernel = a} :: ModifyInstanceAttribute)

-- | Specifies whether source\/destination checking is enabled. A value of
-- @true@ means that checking is enabled, and @false@ means that checking
-- is disabled. This value must be @false@ for a NAT instance to perform
-- NAT.
modifyInstanceAttribute_sourceDestCheck :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeBooleanValue)
modifyInstanceAttribute_sourceDestCheck = Lens.lens (\ModifyInstanceAttribute' {sourceDestCheck} -> sourceDestCheck) (\s@ModifyInstanceAttribute' {} a -> s {sourceDestCheck = a} :: ModifyInstanceAttribute)

-- | Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
modifyInstanceAttribute_instanceInitiatedShutdownBehavior :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeValue)
modifyInstanceAttribute_instanceInitiatedShutdownBehavior = Lens.lens (\ModifyInstanceAttribute' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@ModifyInstanceAttribute' {} a -> s {instanceInitiatedShutdownBehavior = a} :: ModifyInstanceAttribute)

-- | The name of the attribute.
modifyInstanceAttribute_attribute :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe InstanceAttributeName)
modifyInstanceAttribute_attribute = Lens.lens (\ModifyInstanceAttribute' {attribute} -> attribute) (\s@ModifyInstanceAttribute' {} a -> s {attribute = a} :: ModifyInstanceAttribute)

-- | Set to @simple@ to enable enhanced networking with the Intel 82599
-- Virtual Function interface for the instance.
--
-- There is no way to disable enhanced networking with the Intel 82599
-- Virtual Function interface at this time.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
modifyInstanceAttribute_sriovNetSupport :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeValue)
modifyInstanceAttribute_sriovNetSupport = Lens.lens (\ModifyInstanceAttribute' {sriovNetSupport} -> sriovNetSupport) (\s@ModifyInstanceAttribute' {} a -> s {sriovNetSupport = a} :: ModifyInstanceAttribute)

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are
-- currently attached. The volume must be owned by the caller. If no value
-- is specified for @DeleteOnTermination@, the default is @true@ and the
-- volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must
-- add them when you launch the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance>
-- in the /Amazon EC2 User Guide/.
modifyInstanceAttribute_blockDeviceMappings :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe [InstanceBlockDeviceMappingSpecification])
modifyInstanceAttribute_blockDeviceMappings = Lens.lens (\ModifyInstanceAttribute' {blockDeviceMappings} -> blockDeviceMappings) (\s@ModifyInstanceAttribute' {} a -> s {blockDeviceMappings = a} :: ModifyInstanceAttribute) Core.. Lens.mapping Lens._Coerce

-- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @instanceInitiatedShutdownBehavior@ attribute.
modifyInstanceAttribute_value :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Core.Text)
modifyInstanceAttribute_value = Lens.lens (\ModifyInstanceAttribute' {value} -> value) (\s@ModifyInstanceAttribute' {} a -> s {value = a} :: ModifyInstanceAttribute)

-- | Changes the instance\'s RAM disk to the specified value. We recommend
-- that you use PV-GRUB instead of kernels and RAM disks. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
modifyInstanceAttribute_ramdisk :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeValue)
modifyInstanceAttribute_ramdisk = Lens.lens (\ModifyInstanceAttribute' {ramdisk} -> ramdisk) (\s@ModifyInstanceAttribute' {} a -> s {ramdisk = a} :: ModifyInstanceAttribute)

-- | If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
-- parameter for Spot Instances.
modifyInstanceAttribute_disableApiTermination :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeBooleanValue)
modifyInstanceAttribute_disableApiTermination = Lens.lens (\ModifyInstanceAttribute' {disableApiTermination} -> disableApiTermination) (\s@ModifyInstanceAttribute' {} a -> s {disableApiTermination = a} :: ModifyInstanceAttribute)

-- | Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
modifyInstanceAttribute_enaSupport :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe AttributeBooleanValue)
modifyInstanceAttribute_enaSupport = Lens.lens (\ModifyInstanceAttribute' {enaSupport} -> enaSupport) (\s@ModifyInstanceAttribute' {} a -> s {enaSupport = a} :: ModifyInstanceAttribute)

-- | The ID of the instance.
modifyInstanceAttribute_instanceId :: Lens.Lens' ModifyInstanceAttribute Core.Text
modifyInstanceAttribute_instanceId = Lens.lens (\ModifyInstanceAttribute' {instanceId} -> instanceId) (\s@ModifyInstanceAttribute' {} a -> s {instanceId = a} :: ModifyInstanceAttribute)

instance Core.AWSRequest ModifyInstanceAttribute where
  type
    AWSResponse ModifyInstanceAttribute =
      ModifyInstanceAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ModifyInstanceAttributeResponse'

instance Core.Hashable ModifyInstanceAttribute

instance Core.NFData ModifyInstanceAttribute

instance Core.ToHeaders ModifyInstanceAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyInstanceAttribute where
  toPath = Core.const "/"

instance Core.ToQuery ModifyInstanceAttribute where
  toQuery ModifyInstanceAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyInstanceAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          (Core.toQueryList "GroupId" Core.<$> groups),
        "InstanceType" Core.=: instanceType,
        "EbsOptimized" Core.=: ebsOptimized,
        "UserData" Core.=: userData,
        "DryRun" Core.=: dryRun,
        "Kernel" Core.=: kernel,
        "SourceDestCheck" Core.=: sourceDestCheck,
        "InstanceInitiatedShutdownBehavior"
          Core.=: instanceInitiatedShutdownBehavior,
        "Attribute" Core.=: attribute,
        "SriovNetSupport" Core.=: sriovNetSupport,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Core.<$> blockDeviceMappings
          ),
        "Value" Core.=: value,
        "Ramdisk" Core.=: ramdisk,
        "DisableApiTermination"
          Core.=: disableApiTermination,
        "EnaSupport" Core.=: enaSupport,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newModifyInstanceAttributeResponse' smart constructor.
data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyInstanceAttributeResponse ::
  ModifyInstanceAttributeResponse
newModifyInstanceAttributeResponse =
  ModifyInstanceAttributeResponse'

instance Core.NFData ModifyInstanceAttributeResponse
