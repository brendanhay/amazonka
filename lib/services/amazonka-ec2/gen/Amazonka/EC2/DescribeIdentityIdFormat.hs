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
-- Module      : Amazonka.EC2.DescribeIdentityIdFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for resources for the specified IAM
-- user, IAM role, or root user. For example, you can view the resource
-- types that are enabled for longer IDs. This request only returns
-- information about resource types whose ID formats can be modified; it
-- does not return information about other resource types. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
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
-- These settings apply to the principal specified in the request. They do
-- not apply to the principal that makes the request.
module Amazonka.EC2.DescribeIdentityIdFormat
  ( -- * Creating a Request
    DescribeIdentityIdFormat (..),
    newDescribeIdentityIdFormat,

    -- * Request Lenses
    describeIdentityIdFormat_resource,
    describeIdentityIdFormat_principalArn,

    -- * Destructuring the Response
    DescribeIdentityIdFormatResponse (..),
    newDescribeIdentityIdFormatResponse,

    -- * Response Lenses
    describeIdentityIdFormatResponse_statuses,
    describeIdentityIdFormatResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIdentityIdFormat' smart constructor.
data DescribeIdentityIdFormat = DescribeIdentityIdFormat'
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
    resource :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the principal, which can be an IAM role, IAM user, or the
    -- root user.
    principalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityIdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'describeIdentityIdFormat_resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @reservation@ | @route-table@ | @route-table-association@ |
-- @security-group@ | @snapshot@ | @subnet@ |
-- @subnet-cidr-block-association@ | @volume@ | @vpc@ |
-- @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@
-- | @vpn-connection@ | @vpn-gateway@
--
-- 'principalArn', 'describeIdentityIdFormat_principalArn' - The ARN of the principal, which can be an IAM role, IAM user, or the
-- root user.
newDescribeIdentityIdFormat ::
  -- | 'principalArn'
  Prelude.Text ->
  DescribeIdentityIdFormat
newDescribeIdentityIdFormat pPrincipalArn_ =
  DescribeIdentityIdFormat'
    { resource =
        Prelude.Nothing,
      principalArn = pPrincipalArn_
    }

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
describeIdentityIdFormat_resource :: Lens.Lens' DescribeIdentityIdFormat (Prelude.Maybe Prelude.Text)
describeIdentityIdFormat_resource = Lens.lens (\DescribeIdentityIdFormat' {resource} -> resource) (\s@DescribeIdentityIdFormat' {} a -> s {resource = a} :: DescribeIdentityIdFormat)

-- | The ARN of the principal, which can be an IAM role, IAM user, or the
-- root user.
describeIdentityIdFormat_principalArn :: Lens.Lens' DescribeIdentityIdFormat Prelude.Text
describeIdentityIdFormat_principalArn = Lens.lens (\DescribeIdentityIdFormat' {principalArn} -> principalArn) (\s@DescribeIdentityIdFormat' {} a -> s {principalArn = a} :: DescribeIdentityIdFormat)

instance Core.AWSRequest DescribeIdentityIdFormat where
  type
    AWSResponse DescribeIdentityIdFormat =
      DescribeIdentityIdFormatResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIdentityIdFormatResponse'
            Prelude.<$> ( x Data..@? "statusSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIdentityIdFormat where
  hashWithSalt _salt DescribeIdentityIdFormat' {..} =
    _salt `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` principalArn

instance Prelude.NFData DescribeIdentityIdFormat where
  rnf DescribeIdentityIdFormat' {..} =
    Prelude.rnf resource
      `Prelude.seq` Prelude.rnf principalArn

instance Data.ToHeaders DescribeIdentityIdFormat where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIdentityIdFormat where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIdentityIdFormat where
  toQuery DescribeIdentityIdFormat' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeIdentityIdFormat" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Resource" Data.=: resource,
        "PrincipalArn" Data.=: principalArn
      ]

-- | /See:/ 'newDescribeIdentityIdFormatResponse' smart constructor.
data DescribeIdentityIdFormatResponse = DescribeIdentityIdFormatResponse'
  { -- | Information about the ID format for the resources.
    statuses :: Prelude.Maybe [IdFormat],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityIdFormatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statuses', 'describeIdentityIdFormatResponse_statuses' - Information about the ID format for the resources.
--
-- 'httpStatus', 'describeIdentityIdFormatResponse_httpStatus' - The response's http status code.
newDescribeIdentityIdFormatResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIdentityIdFormatResponse
newDescribeIdentityIdFormatResponse pHttpStatus_ =
  DescribeIdentityIdFormatResponse'
    { statuses =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the ID format for the resources.
describeIdentityIdFormatResponse_statuses :: Lens.Lens' DescribeIdentityIdFormatResponse (Prelude.Maybe [IdFormat])
describeIdentityIdFormatResponse_statuses = Lens.lens (\DescribeIdentityIdFormatResponse' {statuses} -> statuses) (\s@DescribeIdentityIdFormatResponse' {} a -> s {statuses = a} :: DescribeIdentityIdFormatResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeIdentityIdFormatResponse_httpStatus :: Lens.Lens' DescribeIdentityIdFormatResponse Prelude.Int
describeIdentityIdFormatResponse_httpStatus = Lens.lens (\DescribeIdentityIdFormatResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityIdFormatResponse' {} a -> s {httpStatus = a} :: DescribeIdentityIdFormatResponse)

instance
  Prelude.NFData
    DescribeIdentityIdFormatResponse
  where
  rnf DescribeIdentityIdFormatResponse' {..} =
    Prelude.rnf statuses
      `Prelude.seq` Prelude.rnf httpStatus
