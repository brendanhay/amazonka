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
-- Module      : Amazonka.EC2.DescribeInstanceAttribute
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
module Amazonka.EC2.DescribeInstanceAttribute
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
    describeInstanceAttributeResponse_instanceId,
    describeInstanceAttributeResponse_groups,
    describeInstanceAttributeResponse_enaSupport,
    describeInstanceAttributeResponse_sourceDestCheck,
    describeInstanceAttributeResponse_disableApiTermination,
    describeInstanceAttributeResponse_enclaveOptions,
    describeInstanceAttributeResponse_ramdiskId,
    describeInstanceAttributeResponse_kernelId,
    describeInstanceAttributeResponse_rootDeviceName,
    describeInstanceAttributeResponse_instanceType,
    describeInstanceAttributeResponse_sriovNetSupport,
    describeInstanceAttributeResponse_ebsOptimized,
    describeInstanceAttributeResponse_userData,
    describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior,
    describeInstanceAttributeResponse_productCodes,
    describeInstanceAttributeResponse_blockDeviceMappings,
    describeInstanceAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The instance attribute.
    --
    -- Note: The @enaSupport@ attribute is not supported at this time.
    attribute :: InstanceAttributeName,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeInstanceAttribute
newDescribeInstanceAttribute pAttribute_ pInstanceId_ =
  DescribeInstanceAttribute'
    { dryRun =
        Prelude.Nothing,
      attribute = pAttribute_,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceAttribute_dryRun :: Lens.Lens' DescribeInstanceAttribute (Prelude.Maybe Prelude.Bool)
describeInstanceAttribute_dryRun = Lens.lens (\DescribeInstanceAttribute' {dryRun} -> dryRun) (\s@DescribeInstanceAttribute' {} a -> s {dryRun = a} :: DescribeInstanceAttribute)

-- | The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
describeInstanceAttribute_attribute :: Lens.Lens' DescribeInstanceAttribute InstanceAttributeName
describeInstanceAttribute_attribute = Lens.lens (\DescribeInstanceAttribute' {attribute} -> attribute) (\s@DescribeInstanceAttribute' {} a -> s {attribute = a} :: DescribeInstanceAttribute)

-- | The ID of the instance.
describeInstanceAttribute_instanceId :: Lens.Lens' DescribeInstanceAttribute Prelude.Text
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
            Prelude.<$> (x Core..@? "instanceId")
            Prelude.<*> ( x Core..@? "groupSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "enaSupport")
            Prelude.<*> (x Core..@? "sourceDestCheck")
            Prelude.<*> (x Core..@? "disableApiTermination")
            Prelude.<*> (x Core..@? "enclaveOptions")
            Prelude.<*> (x Core..@? "ramdisk")
            Prelude.<*> (x Core..@? "kernel")
            Prelude.<*> (x Core..@? "rootDeviceName")
            Prelude.<*> (x Core..@? "instanceType")
            Prelude.<*> (x Core..@? "sriovNetSupport")
            Prelude.<*> (x Core..@? "ebsOptimized")
            Prelude.<*> (x Core..@? "userData")
            Prelude.<*> (x Core..@? "instanceInitiatedShutdownBehavior")
            Prelude.<*> ( x Core..@? "productCodes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "blockDeviceMapping"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceAttribute where
  hashWithSalt salt' DescribeInstanceAttribute' {..} =
    salt' `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` dryRun

instance Prelude.NFData DescribeInstanceAttribute where
  rnf DescribeInstanceAttribute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf attribute

instance Core.ToHeaders DescribeInstanceAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInstanceAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstanceAttribute where
  toQuery DescribeInstanceAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceAttribute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Attribute" Core.=: attribute,
        "InstanceId" Core.=: instanceId
      ]

-- | Describes an instance attribute.
--
-- /See:/ 'newDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The security groups associated with the instance.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | Indicates whether enhanced networking with ENA is enabled.
    enaSupport :: Prelude.Maybe AttributeBooleanValue,
    -- | Enable or disable source\/destination checks, which ensure that the
    -- instance is either the source or the destination of any traffic that it
    -- receives. If the value is @true@, source\/destination checks are
    -- enabled; otherwise, they are disabled. The default value is @true@. You
    -- must disable source\/destination checks if the instance runs services
    -- such as network address translation, routing, or firewalls.
    sourceDestCheck :: Prelude.Maybe AttributeBooleanValue,
    -- | If the value is @true@, you can\'t terminate the instance through the
    -- Amazon EC2 console, CLI, or API; otherwise, you can.
    disableApiTermination :: Prelude.Maybe AttributeBooleanValue,
    -- | To enable the instance for Amazon Web Services Nitro Enclaves, set this
    -- parameter to @true@; otherwise, set it to @false@.
    enclaveOptions :: Prelude.Maybe EnclaveOptions,
    -- | The RAM disk ID.
    ramdiskId :: Prelude.Maybe AttributeValue,
    -- | The kernel ID.
    kernelId :: Prelude.Maybe AttributeValue,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe AttributeValue,
    -- | The instance type.
    instanceType :: Prelude.Maybe AttributeValue,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Prelude.Maybe AttributeValue,
    -- | Indicates whether the instance is optimized for Amazon EBS I\/O.
    ebsOptimized :: Prelude.Maybe AttributeBooleanValue,
    -- | The user data.
    userData :: Prelude.Maybe AttributeValue,
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Prelude.Maybe AttributeValue,
    -- | A list of product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The block device mapping of the instance.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeInstanceAttributeResponse_instanceId' - The ID of the instance.
--
-- 'groups', 'describeInstanceAttributeResponse_groups' - The security groups associated with the instance.
--
-- 'enaSupport', 'describeInstanceAttributeResponse_enaSupport' - Indicates whether enhanced networking with ENA is enabled.
--
-- 'sourceDestCheck', 'describeInstanceAttributeResponse_sourceDestCheck' - Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
--
-- 'disableApiTermination', 'describeInstanceAttributeResponse_disableApiTermination' - If the value is @true@, you can\'t terminate the instance through the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
--
-- 'enclaveOptions', 'describeInstanceAttributeResponse_enclaveOptions' - To enable the instance for Amazon Web Services Nitro Enclaves, set this
-- parameter to @true@; otherwise, set it to @false@.
--
-- 'ramdiskId', 'describeInstanceAttributeResponse_ramdiskId' - The RAM disk ID.
--
-- 'kernelId', 'describeInstanceAttributeResponse_kernelId' - The kernel ID.
--
-- 'rootDeviceName', 'describeInstanceAttributeResponse_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'instanceType', 'describeInstanceAttributeResponse_instanceType' - The instance type.
--
-- 'sriovNetSupport', 'describeInstanceAttributeResponse_sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'ebsOptimized', 'describeInstanceAttributeResponse_ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I\/O.
--
-- 'userData', 'describeInstanceAttributeResponse_userData' - The user data.
--
-- 'instanceInitiatedShutdownBehavior', 'describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- 'productCodes', 'describeInstanceAttributeResponse_productCodes' - A list of product codes.
--
-- 'blockDeviceMappings', 'describeInstanceAttributeResponse_blockDeviceMappings' - The block device mapping of the instance.
--
-- 'httpStatus', 'describeInstanceAttributeResponse_httpStatus' - The response's http status code.
newDescribeInstanceAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceAttributeResponse
newDescribeInstanceAttributeResponse pHttpStatus_ =
  DescribeInstanceAttributeResponse'
    { instanceId =
        Prelude.Nothing,
      groups = Prelude.Nothing,
      enaSupport = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      disableApiTermination = Prelude.Nothing,
      enclaveOptions = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      rootDeviceName = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      sriovNetSupport = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      productCodes = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the instance.
describeInstanceAttributeResponse_instanceId :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe Prelude.Text)
describeInstanceAttributeResponse_instanceId = Lens.lens (\DescribeInstanceAttributeResponse' {instanceId} -> instanceId) (\s@DescribeInstanceAttributeResponse' {} a -> s {instanceId = a} :: DescribeInstanceAttributeResponse)

-- | The security groups associated with the instance.
describeInstanceAttributeResponse_groups :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe [GroupIdentifier])
describeInstanceAttributeResponse_groups = Lens.lens (\DescribeInstanceAttributeResponse' {groups} -> groups) (\s@DescribeInstanceAttributeResponse' {} a -> s {groups = a} :: DescribeInstanceAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether enhanced networking with ENA is enabled.
describeInstanceAttributeResponse_enaSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_enaSupport = Lens.lens (\DescribeInstanceAttributeResponse' {enaSupport} -> enaSupport) (\s@DescribeInstanceAttributeResponse' {} a -> s {enaSupport = a} :: DescribeInstanceAttributeResponse)

-- | Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
describeInstanceAttributeResponse_sourceDestCheck :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_sourceDestCheck = Lens.lens (\DescribeInstanceAttributeResponse' {sourceDestCheck} -> sourceDestCheck) (\s@DescribeInstanceAttributeResponse' {} a -> s {sourceDestCheck = a} :: DescribeInstanceAttributeResponse)

-- | If the value is @true@, you can\'t terminate the instance through the
-- Amazon EC2 console, CLI, or API; otherwise, you can.
describeInstanceAttributeResponse_disableApiTermination :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_disableApiTermination = Lens.lens (\DescribeInstanceAttributeResponse' {disableApiTermination} -> disableApiTermination) (\s@DescribeInstanceAttributeResponse' {} a -> s {disableApiTermination = a} :: DescribeInstanceAttributeResponse)

-- | To enable the instance for Amazon Web Services Nitro Enclaves, set this
-- parameter to @true@; otherwise, set it to @false@.
describeInstanceAttributeResponse_enclaveOptions :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe EnclaveOptions)
describeInstanceAttributeResponse_enclaveOptions = Lens.lens (\DescribeInstanceAttributeResponse' {enclaveOptions} -> enclaveOptions) (\s@DescribeInstanceAttributeResponse' {} a -> s {enclaveOptions = a} :: DescribeInstanceAttributeResponse)

-- | The RAM disk ID.
describeInstanceAttributeResponse_ramdiskId :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeValue)
describeInstanceAttributeResponse_ramdiskId = Lens.lens (\DescribeInstanceAttributeResponse' {ramdiskId} -> ramdiskId) (\s@DescribeInstanceAttributeResponse' {} a -> s {ramdiskId = a} :: DescribeInstanceAttributeResponse)

-- | The kernel ID.
describeInstanceAttributeResponse_kernelId :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeValue)
describeInstanceAttributeResponse_kernelId = Lens.lens (\DescribeInstanceAttributeResponse' {kernelId} -> kernelId) (\s@DescribeInstanceAttributeResponse' {} a -> s {kernelId = a} :: DescribeInstanceAttributeResponse)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
describeInstanceAttributeResponse_rootDeviceName :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeValue)
describeInstanceAttributeResponse_rootDeviceName = Lens.lens (\DescribeInstanceAttributeResponse' {rootDeviceName} -> rootDeviceName) (\s@DescribeInstanceAttributeResponse' {} a -> s {rootDeviceName = a} :: DescribeInstanceAttributeResponse)

-- | The instance type.
describeInstanceAttributeResponse_instanceType :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeValue)
describeInstanceAttributeResponse_instanceType = Lens.lens (\DescribeInstanceAttributeResponse' {instanceType} -> instanceType) (\s@DescribeInstanceAttributeResponse' {} a -> s {instanceType = a} :: DescribeInstanceAttributeResponse)

-- | Indicates whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
describeInstanceAttributeResponse_sriovNetSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeValue)
describeInstanceAttributeResponse_sriovNetSupport = Lens.lens (\DescribeInstanceAttributeResponse' {sriovNetSupport} -> sriovNetSupport) (\s@DescribeInstanceAttributeResponse' {} a -> s {sriovNetSupport = a} :: DescribeInstanceAttributeResponse)

-- | Indicates whether the instance is optimized for Amazon EBS I\/O.
describeInstanceAttributeResponse_ebsOptimized :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeBooleanValue)
describeInstanceAttributeResponse_ebsOptimized = Lens.lens (\DescribeInstanceAttributeResponse' {ebsOptimized} -> ebsOptimized) (\s@DescribeInstanceAttributeResponse' {} a -> s {ebsOptimized = a} :: DescribeInstanceAttributeResponse)

-- | The user data.
describeInstanceAttributeResponse_userData :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeValue)
describeInstanceAttributeResponse_userData = Lens.lens (\DescribeInstanceAttributeResponse' {userData} -> userData) (\s@DescribeInstanceAttributeResponse' {} a -> s {userData = a} :: DescribeInstanceAttributeResponse)

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe AttributeValue)
describeInstanceAttributeResponse_instanceInitiatedShutdownBehavior = Lens.lens (\DescribeInstanceAttributeResponse' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@DescribeInstanceAttributeResponse' {} a -> s {instanceInitiatedShutdownBehavior = a} :: DescribeInstanceAttributeResponse)

-- | A list of product codes.
describeInstanceAttributeResponse_productCodes :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe [ProductCode])
describeInstanceAttributeResponse_productCodes = Lens.lens (\DescribeInstanceAttributeResponse' {productCodes} -> productCodes) (\s@DescribeInstanceAttributeResponse' {} a -> s {productCodes = a} :: DescribeInstanceAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The block device mapping of the instance.
describeInstanceAttributeResponse_blockDeviceMappings :: Lens.Lens' DescribeInstanceAttributeResponse (Prelude.Maybe [InstanceBlockDeviceMapping])
describeInstanceAttributeResponse_blockDeviceMappings = Lens.lens (\DescribeInstanceAttributeResponse' {blockDeviceMappings} -> blockDeviceMappings) (\s@DescribeInstanceAttributeResponse' {} a -> s {blockDeviceMappings = a} :: DescribeInstanceAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstanceAttributeResponse_httpStatus :: Lens.Lens' DescribeInstanceAttributeResponse Prelude.Int
describeInstanceAttributeResponse_httpStatus = Lens.lens (\DescribeInstanceAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceAttributeResponse' {} a -> s {httpStatus = a} :: DescribeInstanceAttributeResponse)

instance
  Prelude.NFData
    DescribeInstanceAttributeResponse
  where
  rnf DescribeInstanceAttributeResponse' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf instanceInitiatedShutdownBehavior
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf sriovNetSupport
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf rootDeviceName
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf enclaveOptions
      `Prelude.seq` Prelude.rnf disableApiTermination
      `Prelude.seq` Prelude.rnf sourceDestCheck
      `Prelude.seq` Prelude.rnf enaSupport
      `Prelude.seq` Prelude.rnf groups
