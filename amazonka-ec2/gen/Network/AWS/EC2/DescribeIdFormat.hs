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
-- Module      : Network.AWS.EC2.DescribeIdFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for your resources on a per-Region
-- basis, for example, to view which resource types are enabled for longer
-- IDs. This request only returns information about resource types whose ID
-- formats can be modified; it does not return information about other
-- resource types.
--
-- The following resource types support longer IDs: @bundle@ |
-- @conversion-task@ | @customer-gateway@ | @dhcp-options@ |
-- @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ |
-- @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ |
-- @network-acl@ | @network-acl-association@ | @network-interface@ |
-- @network-interface-attachment@ | @prefix-list@ | @reservation@ |
-- @route-table@ | @route-table-association@ | @security-group@ |
-- @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ |
-- @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ |
-- @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@.
--
-- These settings apply to the IAM user who makes the request; they do not
-- apply to the entire AWS account. By default, an IAM user defaults to the
-- same settings as the root user, unless they explicitly override the
-- settings by running the ModifyIdFormat command. Resources created with
-- longer IDs are visible to all IAM users, regardless of these settings
-- and provided that they have permission to use the relevant @Describe@
-- command for the resource type.
module Network.AWS.EC2.DescribeIdFormat
  ( -- * Creating a Request
    DescribeIdFormat (..),
    newDescribeIdFormat,

    -- * Request Lenses
    describeIdFormat_resource,

    -- * Destructuring the Response
    DescribeIdFormatResponse (..),
    newDescribeIdFormatResponse,

    -- * Response Lenses
    describeIdFormatResponse_statuses,
    describeIdFormatResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIdFormat' smart constructor.
data DescribeIdFormat = DescribeIdFormat'
  { -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
    -- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
    -- @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ |
    -- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
    -- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
    -- @reservation@ | @route-table@ | @route-table-association@ |
    -- @security-group@ | @snapshot@ | @subnet@ |
    -- @subnet-cidr-block-association@ | @volume@ | @vpc@ |
    -- @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@
    -- | @vpn-connection@ | @vpn-gateway@
    resource :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'describeIdFormat_resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @reservation@ | @route-table@ | @route-table-association@ |
-- @security-group@ | @snapshot@ | @subnet@ |
-- @subnet-cidr-block-association@ | @volume@ | @vpc@ |
-- @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@
-- | @vpn-connection@ | @vpn-gateway@
newDescribeIdFormat ::
  DescribeIdFormat
newDescribeIdFormat =
  DescribeIdFormat' {resource = Core.Nothing}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @reservation@ | @route-table@ | @route-table-association@ |
-- @security-group@ | @snapshot@ | @subnet@ |
-- @subnet-cidr-block-association@ | @volume@ | @vpc@ |
-- @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@
-- | @vpn-connection@ | @vpn-gateway@
describeIdFormat_resource :: Lens.Lens' DescribeIdFormat (Core.Maybe Core.Text)
describeIdFormat_resource = Lens.lens (\DescribeIdFormat' {resource} -> resource) (\s@DescribeIdFormat' {} a -> s {resource = a} :: DescribeIdFormat)

instance Core.AWSRequest DescribeIdFormat where
  type
    AWSResponse DescribeIdFormat =
      DescribeIdFormatResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIdFormatResponse'
            Core.<$> ( x Core..@? "statusSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeIdFormat

instance Core.NFData DescribeIdFormat

instance Core.ToHeaders DescribeIdFormat where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeIdFormat where
  toPath = Core.const "/"

instance Core.ToQuery DescribeIdFormat where
  toQuery DescribeIdFormat' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeIdFormat" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "Resource" Core.=: resource
      ]

-- | /See:/ 'newDescribeIdFormatResponse' smart constructor.
data DescribeIdFormatResponse = DescribeIdFormatResponse'
  { -- | Information about the ID format for the resource.
    statuses :: Core.Maybe [IdFormat],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIdFormatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statuses', 'describeIdFormatResponse_statuses' - Information about the ID format for the resource.
--
-- 'httpStatus', 'describeIdFormatResponse_httpStatus' - The response's http status code.
newDescribeIdFormatResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeIdFormatResponse
newDescribeIdFormatResponse pHttpStatus_ =
  DescribeIdFormatResponse'
    { statuses = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the ID format for the resource.
describeIdFormatResponse_statuses :: Lens.Lens' DescribeIdFormatResponse (Core.Maybe [IdFormat])
describeIdFormatResponse_statuses = Lens.lens (\DescribeIdFormatResponse' {statuses} -> statuses) (\s@DescribeIdFormatResponse' {} a -> s {statuses = a} :: DescribeIdFormatResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeIdFormatResponse_httpStatus :: Lens.Lens' DescribeIdFormatResponse Core.Int
describeIdFormatResponse_httpStatus = Lens.lens (\DescribeIdFormatResponse' {httpStatus} -> httpStatus) (\s@DescribeIdFormatResponse' {} a -> s {httpStatus = a} :: DescribeIdFormatResponse)

instance Core.NFData DescribeIdFormatResponse
