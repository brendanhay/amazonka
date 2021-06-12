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
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one
-- attribute at a time.
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
  ( -- * Creating a Request
    DescribeNetworkInterfaceAttribute (..),
    newDescribeNetworkInterfaceAttribute,

    -- * Request Lenses
    describeNetworkInterfaceAttribute_dryRun,
    describeNetworkInterfaceAttribute_attribute,
    describeNetworkInterfaceAttribute_networkInterfaceId,

    -- * Destructuring the Response
    DescribeNetworkInterfaceAttributeResponse (..),
    newDescribeNetworkInterfaceAttributeResponse,

    -- * Response Lenses
    describeNetworkInterfaceAttributeResponse_groups,
    describeNetworkInterfaceAttributeResponse_attachment,
    describeNetworkInterfaceAttributeResponse_sourceDestCheck,
    describeNetworkInterfaceAttributeResponse_networkInterfaceId,
    describeNetworkInterfaceAttributeResponse_description,
    describeNetworkInterfaceAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'newDescribeNetworkInterfaceAttribute' smart constructor.
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The attribute of the network interface. This parameter is required.
    attribute :: Core.Maybe NetworkInterfaceAttribute,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfaceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeNetworkInterfaceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'describeNetworkInterfaceAttribute_attribute' - The attribute of the network interface. This parameter is required.
--
-- 'networkInterfaceId', 'describeNetworkInterfaceAttribute_networkInterfaceId' - The ID of the network interface.
newDescribeNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Core.Text ->
  DescribeNetworkInterfaceAttribute
newDescribeNetworkInterfaceAttribute
  pNetworkInterfaceId_ =
    DescribeNetworkInterfaceAttribute'
      { dryRun =
          Core.Nothing,
        attribute = Core.Nothing,
        networkInterfaceId =
          pNetworkInterfaceId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInterfaceAttribute_dryRun :: Lens.Lens' DescribeNetworkInterfaceAttribute (Core.Maybe Core.Bool)
describeNetworkInterfaceAttribute_dryRun = Lens.lens (\DescribeNetworkInterfaceAttribute' {dryRun} -> dryRun) (\s@DescribeNetworkInterfaceAttribute' {} a -> s {dryRun = a} :: DescribeNetworkInterfaceAttribute)

-- | The attribute of the network interface. This parameter is required.
describeNetworkInterfaceAttribute_attribute :: Lens.Lens' DescribeNetworkInterfaceAttribute (Core.Maybe NetworkInterfaceAttribute)
describeNetworkInterfaceAttribute_attribute = Lens.lens (\DescribeNetworkInterfaceAttribute' {attribute} -> attribute) (\s@DescribeNetworkInterfaceAttribute' {} a -> s {attribute = a} :: DescribeNetworkInterfaceAttribute)

-- | The ID of the network interface.
describeNetworkInterfaceAttribute_networkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttribute Core.Text
describeNetworkInterfaceAttribute_networkInterfaceId = Lens.lens (\DescribeNetworkInterfaceAttribute' {networkInterfaceId} -> networkInterfaceId) (\s@DescribeNetworkInterfaceAttribute' {} a -> s {networkInterfaceId = a} :: DescribeNetworkInterfaceAttribute)

instance
  Core.AWSRequest
    DescribeNetworkInterfaceAttribute
  where
  type
    AWSResponse DescribeNetworkInterfaceAttribute =
      DescribeNetworkInterfaceAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInterfaceAttributeResponse'
            Core.<$> ( x Core..@? "groupSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "attachment")
            Core.<*> (x Core..@? "sourceDestCheck")
            Core.<*> (x Core..@? "networkInterfaceId")
            Core.<*> (x Core..@? "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeNetworkInterfaceAttribute

instance
  Core.NFData
    DescribeNetworkInterfaceAttribute

instance
  Core.ToHeaders
    DescribeNetworkInterfaceAttribute
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeNetworkInterfaceAttribute
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeNetworkInterfaceAttribute
  where
  toQuery DescribeNetworkInterfaceAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeNetworkInterfaceAttribute" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Attribute" Core.=: attribute,
        "NetworkInterfaceId" Core.=: networkInterfaceId
      ]

-- | Contains the output of DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'newDescribeNetworkInterfaceAttributeResponse' smart constructor.
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
  { -- | The security groups associated with the network interface.
    groups :: Core.Maybe [GroupIdentifier],
    -- | The attachment (if any) of the network interface.
    attachment :: Core.Maybe NetworkInterfaceAttachment,
    -- | Indicates whether source\/destination checking is enabled.
    sourceDestCheck :: Core.Maybe AttributeBooleanValue,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The description of the network interface.
    description :: Core.Maybe AttributeValue,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfaceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'describeNetworkInterfaceAttributeResponse_groups' - The security groups associated with the network interface.
--
-- 'attachment', 'describeNetworkInterfaceAttributeResponse_attachment' - The attachment (if any) of the network interface.
--
-- 'sourceDestCheck', 'describeNetworkInterfaceAttributeResponse_sourceDestCheck' - Indicates whether source\/destination checking is enabled.
--
-- 'networkInterfaceId', 'describeNetworkInterfaceAttributeResponse_networkInterfaceId' - The ID of the network interface.
--
-- 'description', 'describeNetworkInterfaceAttributeResponse_description' - The description of the network interface.
--
-- 'httpStatus', 'describeNetworkInterfaceAttributeResponse_httpStatus' - The response's http status code.
newDescribeNetworkInterfaceAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNetworkInterfaceAttributeResponse
newDescribeNetworkInterfaceAttributeResponse
  pHttpStatus_ =
    DescribeNetworkInterfaceAttributeResponse'
      { groups =
          Core.Nothing,
        attachment = Core.Nothing,
        sourceDestCheck = Core.Nothing,
        networkInterfaceId =
          Core.Nothing,
        description = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The security groups associated with the network interface.
describeNetworkInterfaceAttributeResponse_groups :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe [GroupIdentifier])
describeNetworkInterfaceAttributeResponse_groups = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {groups} -> groups) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {groups = a} :: DescribeNetworkInterfaceAttributeResponse) Core.. Lens.mapping Lens._Coerce

-- | The attachment (if any) of the network interface.
describeNetworkInterfaceAttributeResponse_attachment :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe NetworkInterfaceAttachment)
describeNetworkInterfaceAttributeResponse_attachment = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {attachment} -> attachment) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {attachment = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | Indicates whether source\/destination checking is enabled.
describeNetworkInterfaceAttributeResponse_sourceDestCheck :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe AttributeBooleanValue)
describeNetworkInterfaceAttributeResponse_sourceDestCheck = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {sourceDestCheck} -> sourceDestCheck) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {sourceDestCheck = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | The ID of the network interface.
describeNetworkInterfaceAttributeResponse_networkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe Core.Text)
describeNetworkInterfaceAttributeResponse_networkInterfaceId = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {networkInterfaceId} -> networkInterfaceId) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {networkInterfaceId = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | The description of the network interface.
describeNetworkInterfaceAttributeResponse_description :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe AttributeValue)
describeNetworkInterfaceAttributeResponse_description = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {description} -> description) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {description = a} :: DescribeNetworkInterfaceAttributeResponse)

-- | The response's http status code.
describeNetworkInterfaceAttributeResponse_httpStatus :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse Core.Int
describeNetworkInterfaceAttributeResponse_httpStatus = Lens.lens (\DescribeNetworkInterfaceAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInterfaceAttributeResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInterfaceAttributeResponse)

instance
  Core.NFData
    DescribeNetworkInterfaceAttributeResponse
