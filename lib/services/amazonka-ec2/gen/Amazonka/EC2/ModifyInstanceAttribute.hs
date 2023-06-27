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
-- Module      : Amazonka.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified instance. You can
-- specify only one attribute at a time.
--
-- __Note:__ Using this action to change the security groups associated
-- with an elastic network interface (ENI) attached to an instance can
-- result in an error if the instance has more than one ENI. To change the
-- security groups associated with an ENI attached to an instance that has
-- multiple ENIs, we recommend that you use the
-- ModifyNetworkInterfaceAttribute action.
--
-- To modify some attributes, the instance must be stopped. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ChangingAttributesWhileInstanceStopped.html Modify a stopped instance>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.ModifyInstanceAttribute
  ( -- * Creating a Request
    ModifyInstanceAttribute (..),
    newModifyInstanceAttribute,

    -- * Request Lenses
    modifyInstanceAttribute_attribute,
    modifyInstanceAttribute_blockDeviceMappings,
    modifyInstanceAttribute_disableApiStop,
    modifyInstanceAttribute_disableApiTermination,
    modifyInstanceAttribute_dryRun,
    modifyInstanceAttribute_ebsOptimized,
    modifyInstanceAttribute_enaSupport,
    modifyInstanceAttribute_groups,
    modifyInstanceAttribute_instanceInitiatedShutdownBehavior,
    modifyInstanceAttribute_instanceType,
    modifyInstanceAttribute_kernel,
    modifyInstanceAttribute_ramdisk,
    modifyInstanceAttribute_sourceDestCheck,
    modifyInstanceAttribute_sriovNetSupport,
    modifyInstanceAttribute_userData,
    modifyInstanceAttribute_value,
    modifyInstanceAttribute_instanceId,

    -- * Destructuring the Response
    ModifyInstanceAttributeResponse (..),
    newModifyInstanceAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyInstanceAttribute' smart constructor.
data ModifyInstanceAttribute = ModifyInstanceAttribute'
  { -- | The name of the attribute to modify.
    --
    -- You can modify the following attributes only: @disableApiTermination@ |
    -- @instanceType@ | @kernel@ | @ramdisk@ |
    -- @instanceInitiatedShutdownBehavior@ | @blockDeviceMapping@ | @userData@
    -- | @sourceDestCheck@ | @groupSet@ | @ebsOptimized@ | @sriovNetSupport@ |
    -- @enaSupport@ | @nvmeSupport@ | @disableApiStop@ | @enclaveOptions@
    attribute :: Prelude.Maybe InstanceAttributeName,
    -- | Modifies the @DeleteOnTermination@ attribute for volumes that are
    -- currently attached. The volume must be owned by the caller. If no value
    -- is specified for @DeleteOnTermination@, the default is @true@ and the
    -- volume is deleted when the instance is terminated.
    --
    -- To add instance store volumes to an Amazon EBS-backed instance, you must
    -- add them when you launch the instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Update the block device mapping when launching an instance>
    -- in the /Amazon EC2 User Guide/.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMappingSpecification],
    -- | Indicates whether an instance is enabled for stop protection. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Stop Protection>.
    disableApiStop :: Prelude.Maybe AttributeBooleanValue,
    -- | If the value is @true@, you can\'t terminate the instance using the
    -- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
    -- parameter for Spot Instances.
    disableApiTermination :: Prelude.Maybe AttributeBooleanValue,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the instance is optimized for Amazon EBS I\/O. This
    -- optimization provides dedicated throughput to Amazon EBS and an
    -- optimized configuration stack to provide optimal EBS I\/O performance.
    -- This optimization isn\'t available with all instance types. Additional
    -- usage charges apply when using an EBS Optimized instance.
    ebsOptimized :: Prelude.Maybe AttributeBooleanValue,
    -- | Set to @true@ to enable enhanced networking with ENA for the instance.
    --
    -- This option is supported only for HVM instances. Specifying this option
    -- with a PV instance can make it unreachable.
    enaSupport :: Prelude.Maybe AttributeBooleanValue,
    -- | Replaces the security groups of the instance with the specified security
    -- groups. You must specify the ID of at least one security group, even if
    -- it\'s just the default security group for the VPC.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Prelude.Maybe AttributeValue,
    -- | Changes the instance type to the specified value. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
    -- error returned is @InvalidInstanceAttributeValue@.
    instanceType :: Prelude.Maybe AttributeValue,
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
    -- | Enable or disable source\/destination checks, which ensure that the
    -- instance is either the source or the destination of any traffic that it
    -- receives. If the value is @true@, source\/destination checks are
    -- enabled; otherwise, they are disabled. The default value is @true@. You
    -- must disable source\/destination checks if the instance runs services
    -- such as network address translation, routing, or firewalls.
    sourceDestCheck :: Prelude.Maybe AttributeBooleanValue,
    -- | Set to @simple@ to enable enhanced networking with the Intel 82599
    -- Virtual Function interface for the instance.
    --
    -- There is no way to disable enhanced networking with the Intel 82599
    -- Virtual Function interface at this time.
    --
    -- This option is supported only for HVM instances. Specifying this option
    -- with a PV instance can make it unreachable.
    sriovNetSupport :: Prelude.Maybe AttributeValue,
    -- | Changes the instance\'s user data to the specified value. If you are
    -- using an Amazon Web Services SDK or command line tool, base64-encoding
    -- is performed for you, and you can load the text from a file. Otherwise,
    -- you must provide base64-encoded text.
    userData :: Prelude.Maybe BlobAttributeValue,
    -- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
    -- @userData@, @disableApiTermination@, or
    -- @instanceInitiatedShutdownBehavior@ attribute.
    value :: Prelude.Maybe Prelude.Text,
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
-- 'attribute', 'modifyInstanceAttribute_attribute' - The name of the attribute to modify.
--
-- You can modify the following attributes only: @disableApiTermination@ |
-- @instanceType@ | @kernel@ | @ramdisk@ |
-- @instanceInitiatedShutdownBehavior@ | @blockDeviceMapping@ | @userData@
-- | @sourceDestCheck@ | @groupSet@ | @ebsOptimized@ | @sriovNetSupport@ |
-- @enaSupport@ | @nvmeSupport@ | @disableApiStop@ | @enclaveOptions@
--
-- 'blockDeviceMappings', 'modifyInstanceAttribute_blockDeviceMappings' - Modifies the @DeleteOnTermination@ attribute for volumes that are
-- currently attached. The volume must be owned by the caller. If no value
-- is specified for @DeleteOnTermination@, the default is @true@ and the
-- volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must
-- add them when you launch the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Update the block device mapping when launching an instance>
-- in the /Amazon EC2 User Guide/.
--
-- 'disableApiStop', 'modifyInstanceAttribute_disableApiStop' - Indicates whether an instance is enabled for stop protection. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Stop Protection>.
--
-- 'disableApiTermination', 'modifyInstanceAttribute_disableApiTermination' - If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
-- parameter for Spot Instances.
--
-- 'dryRun', 'modifyInstanceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'ebsOptimized', 'modifyInstanceAttribute_ebsOptimized' - Specifies whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
--
-- 'enaSupport', 'modifyInstanceAttribute_enaSupport' - Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
--
-- 'groups', 'modifyInstanceAttribute_groups' - Replaces the security groups of the instance with the specified security
-- groups. You must specify the ID of at least one security group, even if
-- it\'s just the default security group for the VPC.
--
-- 'instanceInitiatedShutdownBehavior', 'modifyInstanceAttribute_instanceInitiatedShutdownBehavior' - Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- 'instanceType', 'modifyInstanceAttribute_instanceType' - Changes the instance type to the specified value. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
-- error returned is @InvalidInstanceAttributeValue@.
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
-- 'sourceDestCheck', 'modifyInstanceAttribute_sourceDestCheck' - Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
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
-- 'userData', 'modifyInstanceAttribute_userData' - Changes the instance\'s user data to the specified value. If you are
-- using an Amazon Web Services SDK or command line tool, base64-encoding
-- is performed for you, and you can load the text from a file. Otherwise,
-- you must provide base64-encoded text.
--
-- 'value', 'modifyInstanceAttribute_value' - A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @instanceInitiatedShutdownBehavior@ attribute.
--
-- 'instanceId', 'modifyInstanceAttribute_instanceId' - The ID of the instance.
newModifyInstanceAttribute ::
  -- | 'instanceId'
  Prelude.Text ->
  ModifyInstanceAttribute
newModifyInstanceAttribute pInstanceId_ =
  ModifyInstanceAttribute'
    { attribute =
        Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      disableApiStop = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      enaSupport = Prelude.Nothing,
      groups = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      kernel = Prelude.Nothing,
      ramdisk = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      sriovNetSupport = Prelude.Nothing,
      userData = Prelude.Nothing,
      value = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The name of the attribute to modify.
--
-- You can modify the following attributes only: @disableApiTermination@ |
-- @instanceType@ | @kernel@ | @ramdisk@ |
-- @instanceInitiatedShutdownBehavior@ | @blockDeviceMapping@ | @userData@
-- | @sourceDestCheck@ | @groupSet@ | @ebsOptimized@ | @sriovNetSupport@ |
-- @enaSupport@ | @nvmeSupport@ | @disableApiStop@ | @enclaveOptions@
modifyInstanceAttribute_attribute :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe InstanceAttributeName)
modifyInstanceAttribute_attribute = Lens.lens (\ModifyInstanceAttribute' {attribute} -> attribute) (\s@ModifyInstanceAttribute' {} a -> s {attribute = a} :: ModifyInstanceAttribute)

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are
-- currently attached. The volume must be owned by the caller. If no value
-- is specified for @DeleteOnTermination@, the default is @true@ and the
-- volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must
-- add them when you launch the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Update the block device mapping when launching an instance>
-- in the /Amazon EC2 User Guide/.
modifyInstanceAttribute_blockDeviceMappings :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe [InstanceBlockDeviceMappingSpecification])
modifyInstanceAttribute_blockDeviceMappings = Lens.lens (\ModifyInstanceAttribute' {blockDeviceMappings} -> blockDeviceMappings) (\s@ModifyInstanceAttribute' {} a -> s {blockDeviceMappings = a} :: ModifyInstanceAttribute) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether an instance is enabled for stop protection. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Stop_Start.html#Using_StopProtection Stop Protection>.
modifyInstanceAttribute_disableApiStop :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_disableApiStop = Lens.lens (\ModifyInstanceAttribute' {disableApiStop} -> disableApiStop) (\s@ModifyInstanceAttribute' {} a -> s {disableApiStop = a} :: ModifyInstanceAttribute)

-- | If the value is @true@, you can\'t terminate the instance using the
-- Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this
-- parameter for Spot Instances.
modifyInstanceAttribute_disableApiTermination :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_disableApiTermination = Lens.lens (\ModifyInstanceAttribute' {disableApiTermination} -> disableApiTermination) (\s@ModifyInstanceAttribute' {} a -> s {disableApiTermination = a} :: ModifyInstanceAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceAttribute_dryRun :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe Prelude.Bool)
modifyInstanceAttribute_dryRun = Lens.lens (\ModifyInstanceAttribute' {dryRun} -> dryRun) (\s@ModifyInstanceAttribute' {} a -> s {dryRun = a} :: ModifyInstanceAttribute)

-- | Specifies whether the instance is optimized for Amazon EBS I\/O. This
-- optimization provides dedicated throughput to Amazon EBS and an
-- optimized configuration stack to provide optimal EBS I\/O performance.
-- This optimization isn\'t available with all instance types. Additional
-- usage charges apply when using an EBS Optimized instance.
modifyInstanceAttribute_ebsOptimized :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_ebsOptimized = Lens.lens (\ModifyInstanceAttribute' {ebsOptimized} -> ebsOptimized) (\s@ModifyInstanceAttribute' {} a -> s {ebsOptimized = a} :: ModifyInstanceAttribute)

-- | Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option
-- with a PV instance can make it unreachable.
modifyInstanceAttribute_enaSupport :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_enaSupport = Lens.lens (\ModifyInstanceAttribute' {enaSupport} -> enaSupport) (\s@ModifyInstanceAttribute' {} a -> s {enaSupport = a} :: ModifyInstanceAttribute)

-- | Replaces the security groups of the instance with the specified security
-- groups. You must specify the ID of at least one security group, even if
-- it\'s just the default security group for the VPC.
modifyInstanceAttribute_groups :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe [Prelude.Text])
modifyInstanceAttribute_groups = Lens.lens (\ModifyInstanceAttribute' {groups} -> groups) (\s@ModifyInstanceAttribute' {} a -> s {groups = a} :: ModifyInstanceAttribute) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
modifyInstanceAttribute_instanceInitiatedShutdownBehavior :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeValue)
modifyInstanceAttribute_instanceInitiatedShutdownBehavior = Lens.lens (\ModifyInstanceAttribute' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@ModifyInstanceAttribute' {} a -> s {instanceInitiatedShutdownBehavior = a} :: ModifyInstanceAttribute)

-- | Changes the instance type to the specified value. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/. If the instance type is not valid, the
-- error returned is @InvalidInstanceAttributeValue@.
modifyInstanceAttribute_instanceType :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeValue)
modifyInstanceAttribute_instanceType = Lens.lens (\ModifyInstanceAttribute' {instanceType} -> instanceType) (\s@ModifyInstanceAttribute' {} a -> s {instanceType = a} :: ModifyInstanceAttribute)

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

-- | Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
modifyInstanceAttribute_sourceDestCheck :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyInstanceAttribute_sourceDestCheck = Lens.lens (\ModifyInstanceAttribute' {sourceDestCheck} -> sourceDestCheck) (\s@ModifyInstanceAttribute' {} a -> s {sourceDestCheck = a} :: ModifyInstanceAttribute)

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

-- | Changes the instance\'s user data to the specified value. If you are
-- using an Amazon Web Services SDK or command line tool, base64-encoding
-- is performed for you, and you can load the text from a file. Otherwise,
-- you must provide base64-encoded text.
modifyInstanceAttribute_userData :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe BlobAttributeValue)
modifyInstanceAttribute_userData = Lens.lens (\ModifyInstanceAttribute' {userData} -> userData) (\s@ModifyInstanceAttribute' {} a -> s {userData = a} :: ModifyInstanceAttribute)

-- | A new value for the attribute. Use only with the @kernel@, @ramdisk@,
-- @userData@, @disableApiTermination@, or
-- @instanceInitiatedShutdownBehavior@ attribute.
modifyInstanceAttribute_value :: Lens.Lens' ModifyInstanceAttribute (Prelude.Maybe Prelude.Text)
modifyInstanceAttribute_value = Lens.lens (\ModifyInstanceAttribute' {value} -> value) (\s@ModifyInstanceAttribute' {} a -> s {value = a} :: ModifyInstanceAttribute)

-- | The ID of the instance.
modifyInstanceAttribute_instanceId :: Lens.Lens' ModifyInstanceAttribute Prelude.Text
modifyInstanceAttribute_instanceId = Lens.lens (\ModifyInstanceAttribute' {instanceId} -> instanceId) (\s@ModifyInstanceAttribute' {} a -> s {instanceId = a} :: ModifyInstanceAttribute)

instance Core.AWSRequest ModifyInstanceAttribute where
  type
    AWSResponse ModifyInstanceAttribute =
      ModifyInstanceAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      ModifyInstanceAttributeResponse'

instance Prelude.Hashable ModifyInstanceAttribute where
  hashWithSalt _salt ModifyInstanceAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` disableApiStop
      `Prelude.hashWithSalt` disableApiTermination
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` enaSupport
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` instanceInitiatedShutdownBehavior
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` kernel
      `Prelude.hashWithSalt` ramdisk
      `Prelude.hashWithSalt` sourceDestCheck
      `Prelude.hashWithSalt` sriovNetSupport
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ModifyInstanceAttribute where
  rnf ModifyInstanceAttribute' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf disableApiStop
      `Prelude.seq` Prelude.rnf disableApiTermination
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf enaSupport
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf instanceInitiatedShutdownBehavior
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kernel
      `Prelude.seq` Prelude.rnf ramdisk
      `Prelude.seq` Prelude.rnf sourceDestCheck
      `Prelude.seq` Prelude.rnf sriovNetSupport
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ModifyInstanceAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyInstanceAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyInstanceAttribute where
  toQuery ModifyInstanceAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyInstanceAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Attribute" Data.=: attribute,
        Data.toQuery
          ( Data.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "DisableApiStop" Data.=: disableApiStop,
        "DisableApiTermination"
          Data.=: disableApiTermination,
        "DryRun" Data.=: dryRun,
        "EbsOptimized" Data.=: ebsOptimized,
        "EnaSupport" Data.=: enaSupport,
        Data.toQuery
          (Data.toQueryList "GroupId" Prelude.<$> groups),
        "InstanceInitiatedShutdownBehavior"
          Data.=: instanceInitiatedShutdownBehavior,
        "InstanceType" Data.=: instanceType,
        "Kernel" Data.=: kernel,
        "Ramdisk" Data.=: ramdisk,
        "SourceDestCheck" Data.=: sourceDestCheck,
        "SriovNetSupport" Data.=: sriovNetSupport,
        "UserData" Data.=: userData,
        "Value" Data.=: value,
        "InstanceId" Data.=: instanceId
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
  where
  rnf _ = ()
