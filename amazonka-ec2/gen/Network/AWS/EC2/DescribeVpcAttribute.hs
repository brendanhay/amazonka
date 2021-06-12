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
-- Module      : Network.AWS.EC2.DescribeVpcAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified VPC. You can specify
-- only one attribute at a time.
module Network.AWS.EC2.DescribeVpcAttribute
  ( -- * Creating a Request
    DescribeVpcAttribute (..),
    newDescribeVpcAttribute,

    -- * Request Lenses
    describeVpcAttribute_dryRun,
    describeVpcAttribute_attribute,
    describeVpcAttribute_vpcId,

    -- * Destructuring the Response
    DescribeVpcAttributeResponse (..),
    newDescribeVpcAttributeResponse,

    -- * Response Lenses
    describeVpcAttributeResponse_enableDnsSupport,
    describeVpcAttributeResponse_enableDnsHostnames,
    describeVpcAttributeResponse_vpcId,
    describeVpcAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcAttribute' smart constructor.
data DescribeVpcAttribute = DescribeVpcAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The VPC attribute.
    attribute :: VpcAttributeName,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpcAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'describeVpcAttribute_attribute' - The VPC attribute.
--
-- 'vpcId', 'describeVpcAttribute_vpcId' - The ID of the VPC.
newDescribeVpcAttribute ::
  -- | 'attribute'
  VpcAttributeName ->
  -- | 'vpcId'
  Core.Text ->
  DescribeVpcAttribute
newDescribeVpcAttribute pAttribute_ pVpcId_ =
  DescribeVpcAttribute'
    { dryRun = Core.Nothing,
      attribute = pAttribute_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcAttribute_dryRun :: Lens.Lens' DescribeVpcAttribute (Core.Maybe Core.Bool)
describeVpcAttribute_dryRun = Lens.lens (\DescribeVpcAttribute' {dryRun} -> dryRun) (\s@DescribeVpcAttribute' {} a -> s {dryRun = a} :: DescribeVpcAttribute)

-- | The VPC attribute.
describeVpcAttribute_attribute :: Lens.Lens' DescribeVpcAttribute VpcAttributeName
describeVpcAttribute_attribute = Lens.lens (\DescribeVpcAttribute' {attribute} -> attribute) (\s@DescribeVpcAttribute' {} a -> s {attribute = a} :: DescribeVpcAttribute)

-- | The ID of the VPC.
describeVpcAttribute_vpcId :: Lens.Lens' DescribeVpcAttribute Core.Text
describeVpcAttribute_vpcId = Lens.lens (\DescribeVpcAttribute' {vpcId} -> vpcId) (\s@DescribeVpcAttribute' {} a -> s {vpcId = a} :: DescribeVpcAttribute)

instance Core.AWSRequest DescribeVpcAttribute where
  type
    AWSResponse DescribeVpcAttribute =
      DescribeVpcAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcAttributeResponse'
            Core.<$> (x Core..@? "enableDnsSupport")
            Core.<*> (x Core..@? "enableDnsHostnames")
            Core.<*> (x Core..@? "vpcId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVpcAttribute

instance Core.NFData DescribeVpcAttribute

instance Core.ToHeaders DescribeVpcAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVpcAttribute where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVpcAttribute where
  toQuery DescribeVpcAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeVpcAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Attribute" Core.=: attribute,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newDescribeVpcAttributeResponse' smart constructor.
data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse'
  { -- | Indicates whether DNS resolution is enabled for the VPC. If this
    -- attribute is @true@, the Amazon DNS server resolves DNS hostnames for
    -- your instances to their corresponding IP addresses; otherwise, it does
    -- not.
    enableDnsSupport :: Core.Maybe AttributeBooleanValue,
    -- | Indicates whether the instances launched in the VPC get DNS hostnames.
    -- If this attribute is @true@, instances in the VPC get DNS hostnames;
    -- otherwise, they do not.
    enableDnsHostnames :: Core.Maybe AttributeBooleanValue,
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableDnsSupport', 'describeVpcAttributeResponse_enableDnsSupport' - Indicates whether DNS resolution is enabled for the VPC. If this
-- attribute is @true@, the Amazon DNS server resolves DNS hostnames for
-- your instances to their corresponding IP addresses; otherwise, it does
-- not.
--
-- 'enableDnsHostnames', 'describeVpcAttributeResponse_enableDnsHostnames' - Indicates whether the instances launched in the VPC get DNS hostnames.
-- If this attribute is @true@, instances in the VPC get DNS hostnames;
-- otherwise, they do not.
--
-- 'vpcId', 'describeVpcAttributeResponse_vpcId' - The ID of the VPC.
--
-- 'httpStatus', 'describeVpcAttributeResponse_httpStatus' - The response's http status code.
newDescribeVpcAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcAttributeResponse
newDescribeVpcAttributeResponse pHttpStatus_ =
  DescribeVpcAttributeResponse'
    { enableDnsSupport =
        Core.Nothing,
      enableDnsHostnames = Core.Nothing,
      vpcId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether DNS resolution is enabled for the VPC. If this
-- attribute is @true@, the Amazon DNS server resolves DNS hostnames for
-- your instances to their corresponding IP addresses; otherwise, it does
-- not.
describeVpcAttributeResponse_enableDnsSupport :: Lens.Lens' DescribeVpcAttributeResponse (Core.Maybe AttributeBooleanValue)
describeVpcAttributeResponse_enableDnsSupport = Lens.lens (\DescribeVpcAttributeResponse' {enableDnsSupport} -> enableDnsSupport) (\s@DescribeVpcAttributeResponse' {} a -> s {enableDnsSupport = a} :: DescribeVpcAttributeResponse)

-- | Indicates whether the instances launched in the VPC get DNS hostnames.
-- If this attribute is @true@, instances in the VPC get DNS hostnames;
-- otherwise, they do not.
describeVpcAttributeResponse_enableDnsHostnames :: Lens.Lens' DescribeVpcAttributeResponse (Core.Maybe AttributeBooleanValue)
describeVpcAttributeResponse_enableDnsHostnames = Lens.lens (\DescribeVpcAttributeResponse' {enableDnsHostnames} -> enableDnsHostnames) (\s@DescribeVpcAttributeResponse' {} a -> s {enableDnsHostnames = a} :: DescribeVpcAttributeResponse)

-- | The ID of the VPC.
describeVpcAttributeResponse_vpcId :: Lens.Lens' DescribeVpcAttributeResponse (Core.Maybe Core.Text)
describeVpcAttributeResponse_vpcId = Lens.lens (\DescribeVpcAttributeResponse' {vpcId} -> vpcId) (\s@DescribeVpcAttributeResponse' {} a -> s {vpcId = a} :: DescribeVpcAttributeResponse)

-- | The response's http status code.
describeVpcAttributeResponse_httpStatus :: Lens.Lens' DescribeVpcAttributeResponse Core.Int
describeVpcAttributeResponse_httpStatus = Lens.lens (\DescribeVpcAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcAttributeResponse' {} a -> s {httpStatus = a} :: DescribeVpcAttributeResponse)

instance Core.NFData DescribeVpcAttributeResponse
