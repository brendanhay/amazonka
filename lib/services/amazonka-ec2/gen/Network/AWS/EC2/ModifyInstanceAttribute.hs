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
    modifyInstanceAttribute_attribute,
    modifyInstanceAttribute_enaSupport,
    modifyInstanceAttribute_sourceDestCheck,
    modifyInstanceAttribute_disableApiTermination,
    modifyInstanceAttribute_kernel,
    modifyInstanceAttribute_ramdisk,
    modifyInstanceAttribute_value,
    modifyInstanceAttribute_instanceType,
    modifyInstanceAttribute_sriovNetSupport,
    modifyInstanceAttribute_ebsOptimized,
    modifyInstanceAttribute_userData,
    modifyInstanceAttribute_instanceInitiatedShutdownBehavior,
    modifyInstanceAttribute_blockDeviceMappings,
    modifyInstanceAttribute_dryRun,
    modifyInstanceAttribute_instanceId,

    -- * Destructuring the Response
    ModifyInstanceAttributeResponse (..),
    newModifyInstanceAttributeResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstanceAttribute' smart constructor.
data ModifyInstanceAttribute = ModifyInstanceAttribute'
  { -- | [EC2-VPC] Replaces the security groups of the instance with the
    -- specified security groups. You must specify at least one security group,
    -- even if it\'s just the default security group for the VPC. You must
    -- specify the security group ID, not the security group name.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The name of the attribute.
    attribute :: Prelude.Maybe InstanceAttributeName,
    -- | Set to @true@ to enable enhanced networking with ENA for the instance.
    --
    -- This option is supported only for HVM instances. Specifying this option
    -- with a PV instance can make it unreachable.
    enaSupport :: Prelude.Maybe AttributeBooleanValue,
    -- | Enable or disable source\/destination checks, which ensure that the
    -- instance is either the source or the destination of any traffic that it
    -- receives. If the value is @true@, source\/destination checks are
    -- enabled; otherwise, they are disabled. The default value is @true@. You
    -- must disable source\/destination checks if the instance runs services
    -- such as network address translation, routing, or firewalls.
    sourceDestCheck :: Prelude.Maybe AttributeBooleanValue,
    -- | If the value is @true@, you can\'t terminate the instance using the
    -- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
    -- parameter for Spot Instances.
    disableApiTermination :: Prelude.Maybe AttributeBooleanValue,
    -- | Changes the instance\'s kernel to the specified value. We recommend that
    -- you use PV-GRUB instead of kernels and RAM disks. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
    kernel :: Prelude.Maybe AttributeValue,
    -- | Changes the instance\'s RAM disk to the specified value. We recommend
    -- that you use PV-GRUB instead of kernels and RAM disks. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
    ramdisk :: Prelude.Maybe AttributeValue,
    -- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
    -- @userData@, @disableApiTermination@, or
    -- @instanceInitiatedShutdownBehavior@ attribute.
    value :: Prelude.Maybe Prelude.Text,
    -- | Changes the instance type to the specified value. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
    -- error returned is @InvalidInstanceAttributeValue@.
    instanceType :: Prelude.Maybe AttributeValue,
    -- | Set to @simple@ to enable enhanced networking with the Intel 82599
    -- Virtual Function interface for the instance.
    --
    -- There is no way to disable enhanced networking with the Intel 82599
    -- Virtual Function interface at this time.
    --
    -- This option is supported only for HVM instances. Specifying this option
    -- with a PV instance can make it unreachable.
    sriovNetSupport :: Prelude.Maybe AttributeValue,
    -- | Specifies whether the instance is optimized for Amazon EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    ebsOptimized :: Prelude.Maybe AttributeBooleanValue,
    -- | Changes the instance\'s user data to the specified value. If you are
    -- using an Amazon Web Services SDK or command line tool, base64-encoding
    -- is performed for you, and you can load the text from a file. Otherwise,
    -- you must provide base64-encoded text.
    userData :: Prelude.Maybe BlobAttributeValue,
    -- | Specifies whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Prelude.Maybe AttributeValue,
    -- | Modifies the @DeleteOnTermination@ attribute for volumes that are
    -- currently attached. The volume must be owned by the caller. If no value
    -- is specified for @DeleteOnTermination@, the default is @true@ and the
    -- volume is deleted when the instance is terminated.
    --
    -- To add instance store volumes to an Amazon EBS-backed instance, you must
    -- add them when you launch the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance>
    -- in the /Amazon EC2 User Guide/.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMappingSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'modifyInstanceAttribute_groups' - [EC2-VPC] Replaces the security groups of the instance with the
-- specified security groups. You must specify at least one security group,
-- even if it\'s just the default security group for the VPC. You must
-- specify the security group ID, not the security group name.
--
-- 'attribute', 'modifyInstanceAttribute_attribute' - The name of the attribute.
--
-- 'enaSupport', 'modifyInstanceAttribute_enaSupport' - Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
--
-- 'sourceDestCheck', 'modifyInstanceAttribute_sourceDestCheck' - Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
--
-- 'disableApiTermination', 'modifyInstanceAttribute_disableApiTermination' - If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
-- parameter for Spot Instances.
--
-- 'kernel', 'modifyInstanceAttribute_kernel' - Changes the instance\'s kernel to the specified value. We recommend that
-- you use PV-GRUB instead of kernels and RAM disks. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
--
-- 'ramdisk', 'modifyInstanceAttribute_ramdisk' - Changes the instance\'s RAM disk to the specified value. We recommend
-- that you use PV-GRUB instead of kernels and RAM disks. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
--
-- 'value', 'modifyInstanceAttribute_value' - A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @instanceInitiatedShutdownBehavior@ attribute.
--
-- 'instanceType', 'modifyInstanceAttribute_instanceType' - Changes the instance type to the specified value. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
-- error returned is @InvalidInstanceAttributeValue@.
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
-- 'ebsOptimized', 'modifyInstanceAttribute_ebsOptimized' - Specifies whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- 'userData', 'modifyInstanceAttribute_userData' - Changes the instance\'s user data to the specified value. If you are
-- using an Amazon Web Services SDK or command line tool, base64-encoding
-- is performed for you, and you can load the text from a file. Otherwise,
-- you must provide base64-encoded text.
--
-- 'instanceInitiatedShutdownBehavior', 'modifyInstanceAttribute_instanceInitiatedShutdownBehavior' - Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
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
-- 'dryRun', 'modifyInstanceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'modifyInstanceAttribute_instanceId' - The ID of the instance.
newModifyInstanceAttribute ::
  -- | 'instanceId'
  Prelude.Text ->
  ModifyInstanceAttribute
newModifyInstanceAttribute pInstanceId_ =
  ModifyInstanceAttribute'
    { groups = Prelude.Nothing,
      attribute = Prelude.Nothing,
      enaSupport = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      kernel = Prelude.Nothing,
      ramdisk = Prelude.Nothing,
      value = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      sriovNetSupport = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | [EC2-VPC] Replaces the security groups of the instance with the
-- specified security groups. You must specify at least one security group,
-- even if it\'s just the default security group for the VPC. You must
-- specify the security group ID, not the security group name.
modifyInstanceAttribute_groups :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe [Prelude.Text])
modifyInstanceAttribute_groups = Lens.lens (\ModifyInstanceAttribute' {groups} -> groups) (\s@ModifyInstanceAttribute' {} a -> s {groups = a} :: ModifyInstanceAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The name of the attribute.
modifyInstanceAttribute_attribute :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe InstanceAttributeName)
modifyInstanceAttribute_attribute = Lens.lens (\ModifyInstanceAttribute' {attribute} -> attribute) (\s@ModifyInstanceAttribute' {} a -> s {attribute = a} :: ModifyInstanceAttribute)

-- | Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
modifyInstanceAttribute_enaSupport :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_enaSupport = Lens.lens (\ModifyInstanceAttribute' {enaSupport} -> enaSupport) (\s@ModifyInstanceAttribute' {} a -> s {enaSupport = a} :: ModifyInstanceAttribute)

-- | Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
modifyInstanceAttribute_sourceDestCheck :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_sourceDestCheck = Lens.lens (\ModifyInstanceAttribute' {sourceDestCheck} -> sourceDestCheck) (\s@ModifyInstanceAttribute' {} a -> s {sourceDestCheck = a} :: ModifyInstanceAttribute)

-- | If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
-- parameter for Spot Instances.
modifyInstanceAttribute_disableApiTermination :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_disableApiTermination = Lens.lens (\ModifyInstanceAttribute' {disableApiTermination} -> disableApiTermination) (\s@ModifyInstanceAttribute' {} a -> s {disableApiTermination = a} :: ModifyInstanceAttribute)

-- | Changes the instance\'s kernel to the specified value. We recommend that
-- you use PV-GRUB instead of kernels and RAM disks. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
modifyInstanceAttribute_kernel :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeValue)
modifyInstanceAttribute_kernel = Lens.lens (\ModifyInstanceAttribute' {kernel} -> kernel) (\s@ModifyInstanceAttribute' {} a -> s {kernel = a} :: ModifyInstanceAttribute)

-- | Changes the instance\'s RAM disk to the specified value. We recommend
-- that you use PV-GRUB instead of kernels and RAM disks. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB>.
modifyInstanceAttribute_ramdisk :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeValue)
modifyInstanceAttribute_ramdisk = Lens.lens (\ModifyInstanceAttribute' {ramdisk} -> ramdisk) (\s@ModifyInstanceAttribute' {} a -> s {ramdisk = a} :: ModifyInstanceAttribute)

-- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @instanceInitiatedShutdownBehavior@ attribute.
modifyInstanceAttribute_value :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe Prelude.Text)
modifyInstanceAttribute_value = Lens.lens (\ModifyInstanceAttribute' {value} -> value) (\s@ModifyInstanceAttribute' {} a -> s {value = a} :: ModifyInstanceAttribute)

-- | Changes the instance type to the specified value. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
-- error returned is @InvalidInstanceAttributeValue@.
modifyInstanceAttribute_instanceType :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeValue)
modifyInstanceAttribute_instanceType = Lens.lens (\ModifyInstanceAttribute' {instanceType} -> instanceType) (\s@ModifyInstanceAttribute' {} a -> s {instanceType = a} :: ModifyInstanceAttribute)

-- | Set to @simple@ to enable enhanced networking with the Intel 82599
-- Virtual Function interface for the instance.
--
-- There is no way to disable enhanced networking with the Intel 82599
-- Virtual Function interface at this time.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
modifyInstanceAttribute_sriovNetSupport :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeValue)
modifyInstanceAttribute_sriovNetSupport = Lens.lens (\ModifyInstanceAttribute' {sriovNetSupport} -> sriovNetSupport) (\s@ModifyInstanceAttribute' {} a -> s {sriovNetSupport = a} :: ModifyInstanceAttribute)

-- | Specifies whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
modifyInstanceAttribute_ebsOptimized :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_ebsOptimized = Lens.lens (\ModifyInstanceAttribute' {ebsOptimized} -> ebsOptimized) (\s@ModifyInstanceAttribute' {} a -> s {ebsOptimized = a} :: ModifyInstanceAttribute)

-- | Changes the instance\'s user data to the specified value. If you are
-- using an Amazon Web Services SDK or command line tool, base64-encoding
-- is performed for you, and you can load the text from a file. Otherwise,
-- you must provide base64-encoded text.
modifyInstanceAttribute_userData :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe BlobAttributeValue)
modifyInstanceAttribute_userData = Lens.lens (\ModifyInstanceAttribute' {userData} -> userData) (\s@ModifyInstanceAttribute' {} a -> s {userData = a} :: ModifyInstanceAttribute)

-- | Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
modifyInstanceAttribute_instanceInitiatedShutdownBehavior :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeValue)
modifyInstanceAttribute_instanceInitiatedShutdownBehavior = Lens.lens (\ModifyInstanceAttribute' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@ModifyInstanceAttribute' {} a -> s {instanceInitiatedShutdownBehavior = a} :: ModifyInstanceAttribute)

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are
-- currently attached. The volume must be owned by the caller. If no value
-- is specified for @DeleteOnTermination@, the default is @true@ and the
-- volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must
-- add them when you launch the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance>
-- in the /Amazon EC2 User Guide/.
modifyInstanceAttribute_blockDeviceMappings :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe [InstanceBlockDeviceMappingSpecification])
modifyInstanceAttribute_blockDeviceMappings = Lens.lens (\ModifyInstanceAttribute' {blockDeviceMappings} -> blockDeviceMappings) (\s@ModifyInstanceAttribute' {} a -> s {blockDeviceMappings = a} :: ModifyInstanceAttribute) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceAttribute_dryRun :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe Prelude.Bool)
modifyInstanceAttribute_dryRun = Lens.lens (\ModifyInstanceAttribute' {dryRun} -> dryRun) (\s@ModifyInstanceAttribute' {} a -> s {dryRun = a} :: ModifyInstanceAttribute)

-- | The ID of the instance.
modifyInstanceAttribute_instanceId :: Lens.Lens' ModifyInstanceAttribute Prelude.Text
modifyInstanceAttribute_instanceId = Lens.lens (\ModifyInstanceAttribute' {instanceId} -> instanceId) (\s@ModifyInstanceAttribute' {} a -> s {instanceId = a} :: ModifyInstanceAttribute)

instance Core.AWSRequest ModifyInstanceAttribute where
  type
    AWSResponse ModifyInstanceAttribute =
      ModifyInstanceAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ModifyInstanceAttributeResponse'

instance Prelude.Hashable ModifyInstanceAttribute

instance Prelude.NFData ModifyInstanceAttribute

instance Core.ToHeaders ModifyInstanceAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyInstanceAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyInstanceAttribute where
  toQuery ModifyInstanceAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyInstanceAttribute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "GroupId" Prelude.<$> groups),
        "Attribute" Core.=: attribute,
        "EnaSupport" Core.=: enaSupport,
        "SourceDestCheck" Core.=: sourceDestCheck,
        "DisableApiTermination"
          Core.=: disableApiTermination,
        "Kernel" Core.=: kernel,
        "Ramdisk" Core.=: ramdisk,
        "Value" Core.=: value,
        "InstanceType" Core.=: instanceType,
        "SriovNetSupport" Core.=: sriovNetSupport,
        "EbsOptimized" Core.=: ebsOptimized,
        "UserData" Core.=: userData,
        "InstanceInitiatedShutdownBehavior"
          Core.=: instanceInitiatedShutdownBehavior,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "DryRun" Core.=: dryRun,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newModifyInstanceAttributeResponse' smart constructor.
data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyInstanceAttributeResponse ::
  ModifyInstanceAttributeResponse
newModifyInstanceAttributeResponse =
  ModifyInstanceAttributeResponse'

instance
  Prelude.NFData
    ModifyInstanceAttributeResponse
