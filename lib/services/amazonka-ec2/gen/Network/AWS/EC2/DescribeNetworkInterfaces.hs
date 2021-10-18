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
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network interfaces.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkInterfaces
  ( -- * Creating a Request
    DescribeNetworkInterfaces (..),
    newDescribeNetworkInterfaces,

    -- * Request Lenses
    describeNetworkInterfaces_nextToken,
    describeNetworkInterfaces_maxResults,
    describeNetworkInterfaces_dryRun,
    describeNetworkInterfaces_networkInterfaceIds,
    describeNetworkInterfaces_filters,

    -- * Destructuring the Response
    DescribeNetworkInterfacesResponse (..),
    newDescribeNetworkInterfacesResponse,

    -- * Response Lenses
    describeNetworkInterfacesResponse_nextToken,
    describeNetworkInterfacesResponse_networkInterfaces,
    describeNetworkInterfacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeNetworkInterfaces.
--
-- /See:/ 'newDescribeNetworkInterfaces' smart constructor.
data DescribeNetworkInterfaces = DescribeNetworkInterfaces'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this request. The request
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results. You cannot specify this parameter and the network
    -- interface IDs parameter in the same request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more network interface IDs.
    --
    -- Default: Describes all your network interfaces.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters.
    --
    -- -   @addresses.private-ip-address@ - The private IPv4 addresses
    --     associated with the network interface.
    --
    -- -   @addresses.primary@ - Whether the private IPv4 address is the
    --     primary IP address associated with the network interface.
    --
    -- -   @addresses.association.public-ip@ - The association ID returned when
    --     the network interface was associated with the Elastic IP address
    --     (IPv4).
    --
    -- -   @addresses.association.owner-id@ - The owner ID of the addresses
    --     associated with the network interface.
    --
    -- -   @association.association-id@ - The association ID returned when the
    --     network interface was associated with an IPv4 address.
    --
    -- -   @association.allocation-id@ - The allocation ID returned when you
    --     allocated the Elastic IP address (IPv4) for your network interface.
    --
    -- -   @association.ip-owner-id@ - The owner of the Elastic IP address
    --     (IPv4) associated with the network interface.
    --
    -- -   @association.public-ip@ - The address of the Elastic IP address
    --     (IPv4) bound to the network interface.
    --
    -- -   @association.public-dns-name@ - The public DNS name for the network
    --     interface (IPv4).
    --
    -- -   @attachment.attachment-id@ - The ID of the interface attachment.
    --
    -- -   @attachment.attach-time@ - The time that the network interface was
    --     attached to an instance.
    --
    -- -   @attachment.delete-on-termination@ - Indicates whether the
    --     attachment is deleted when an instance is terminated.
    --
    -- -   @attachment.device-index@ - The device index to which the network
    --     interface is attached.
    --
    -- -   @attachment.instance-id@ - The ID of the instance to which the
    --     network interface is attached.
    --
    -- -   @attachment.instance-owner-id@ - The owner ID of the instance to
    --     which the network interface is attached.
    --
    -- -   @attachment.status@ - The status of the attachment (@attaching@ |
    --     @attached@ | @detaching@ | @detached@).
    --
    -- -   @availability-zone@ - The Availability Zone of the network
    --     interface.
    --
    -- -   @description@ - The description of the network interface.
    --
    -- -   @group-id@ - The ID of a security group associated with the network
    --     interface.
    --
    -- -   @group-name@ - The name of a security group associated with the
    --     network interface.
    --
    -- -   @ipv6-addresses.ipv6-address@ - An IPv6 address associated with the
    --     network interface.
    --
    -- -   @mac-address@ - The MAC address of the network interface.
    --
    -- -   @network-interface-id@ - The ID of the network interface.
    --
    -- -   @owner-id@ - The Amazon Web Services account ID of the network
    --     interface owner.
    --
    -- -   @private-ip-address@ - The private IPv4 address or addresses of the
    --     network interface.
    --
    -- -   @private-dns-name@ - The private DNS name of the network interface
    --     (IPv4).
    --
    -- -   @requester-id@ - The alias or Amazon Web Services account ID of the
    --     principal or service that created the network interface.
    --
    -- -   @requester-managed@ - Indicates whether the network interface is
    --     being managed by an Amazon Web Service (for example, Amazon Web
    --     Services Management Console, Auto Scaling, and so on).
    --
    -- -   @source-dest-check@ - Indicates whether the network interface
    --     performs source\/destination checking. A value of @true@ means
    --     checking is enabled, and @false@ means checking is disabled. The
    --     value must be @false@ for the network interface to perform network
    --     address translation (NAT) in your VPC.
    --
    -- -   @status@ - The status of the network interface. If the network
    --     interface is not attached to an instance, the status is @available@;
    --     if a network interface is attached to an instance the status is
    --     @in-use@.
    --
    -- -   @subnet-id@ - The ID of the subnet for the network interface.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @vpc-id@ - The ID of the VPC for the network interface.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkInterfaces_nextToken' - The token to retrieve the next page of results.
--
-- 'maxResults', 'describeNetworkInterfaces_maxResults' - The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results. You cannot specify this parameter and the network
-- interface IDs parameter in the same request.
--
-- 'dryRun', 'describeNetworkInterfaces_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInterfaceIds', 'describeNetworkInterfaces_networkInterfaceIds' - One or more network interface IDs.
--
-- Default: Describes all your network interfaces.
--
-- 'filters', 'describeNetworkInterfaces_filters' - One or more filters.
--
-- -   @addresses.private-ip-address@ - The private IPv4 addresses
--     associated with the network interface.
--
-- -   @addresses.primary@ - Whether the private IPv4 address is the
--     primary IP address associated with the network interface.
--
-- -   @addresses.association.public-ip@ - The association ID returned when
--     the network interface was associated with the Elastic IP address
--     (IPv4).
--
-- -   @addresses.association.owner-id@ - The owner ID of the addresses
--     associated with the network interface.
--
-- -   @association.association-id@ - The association ID returned when the
--     network interface was associated with an IPv4 address.
--
-- -   @association.allocation-id@ - The allocation ID returned when you
--     allocated the Elastic IP address (IPv4) for your network interface.
--
-- -   @association.ip-owner-id@ - The owner of the Elastic IP address
--     (IPv4) associated with the network interface.
--
-- -   @association.public-ip@ - The address of the Elastic IP address
--     (IPv4) bound to the network interface.
--
-- -   @association.public-dns-name@ - The public DNS name for the network
--     interface (IPv4).
--
-- -   @attachment.attachment-id@ - The ID of the interface attachment.
--
-- -   @attachment.attach-time@ - The time that the network interface was
--     attached to an instance.
--
-- -   @attachment.delete-on-termination@ - Indicates whether the
--     attachment is deleted when an instance is terminated.
--
-- -   @attachment.device-index@ - The device index to which the network
--     interface is attached.
--
-- -   @attachment.instance-id@ - The ID of the instance to which the
--     network interface is attached.
--
-- -   @attachment.instance-owner-id@ - The owner ID of the instance to
--     which the network interface is attached.
--
-- -   @attachment.status@ - The status of the attachment (@attaching@ |
--     @attached@ | @detaching@ | @detached@).
--
-- -   @availability-zone@ - The Availability Zone of the network
--     interface.
--
-- -   @description@ - The description of the network interface.
--
-- -   @group-id@ - The ID of a security group associated with the network
--     interface.
--
-- -   @group-name@ - The name of a security group associated with the
--     network interface.
--
-- -   @ipv6-addresses.ipv6-address@ - An IPv6 address associated with the
--     network interface.
--
-- -   @mac-address@ - The MAC address of the network interface.
--
-- -   @network-interface-id@ - The ID of the network interface.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the network
--     interface owner.
--
-- -   @private-ip-address@ - The private IPv4 address or addresses of the
--     network interface.
--
-- -   @private-dns-name@ - The private DNS name of the network interface
--     (IPv4).
--
-- -   @requester-id@ - The alias or Amazon Web Services account ID of the
--     principal or service that created the network interface.
--
-- -   @requester-managed@ - Indicates whether the network interface is
--     being managed by an Amazon Web Service (for example, Amazon Web
--     Services Management Console, Auto Scaling, and so on).
--
-- -   @source-dest-check@ - Indicates whether the network interface
--     performs source\/destination checking. A value of @true@ means
--     checking is enabled, and @false@ means checking is disabled. The
--     value must be @false@ for the network interface to perform network
--     address translation (NAT) in your VPC.
--
-- -   @status@ - The status of the network interface. If the network
--     interface is not attached to an instance, the status is @available@;
--     if a network interface is attached to an instance the status is
--     @in-use@.
--
-- -   @subnet-id@ - The ID of the subnet for the network interface.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC for the network interface.
newDescribeNetworkInterfaces ::
  DescribeNetworkInterfaces
newDescribeNetworkInterfaces =
  DescribeNetworkInterfaces'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      networkInterfaceIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
describeNetworkInterfaces_nextToken :: Lens.Lens' DescribeNetworkInterfaces (Prelude.Maybe Prelude.Text)
describeNetworkInterfaces_nextToken = Lens.lens (\DescribeNetworkInterfaces' {nextToken} -> nextToken) (\s@DescribeNetworkInterfaces' {} a -> s {nextToken = a} :: DescribeNetworkInterfaces)

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results. You cannot specify this parameter and the network
-- interface IDs parameter in the same request.
describeNetworkInterfaces_maxResults :: Lens.Lens' DescribeNetworkInterfaces (Prelude.Maybe Prelude.Natural)
describeNetworkInterfaces_maxResults = Lens.lens (\DescribeNetworkInterfaces' {maxResults} -> maxResults) (\s@DescribeNetworkInterfaces' {} a -> s {maxResults = a} :: DescribeNetworkInterfaces)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkInterfaces_dryRun :: Lens.Lens' DescribeNetworkInterfaces (Prelude.Maybe Prelude.Bool)
describeNetworkInterfaces_dryRun = Lens.lens (\DescribeNetworkInterfaces' {dryRun} -> dryRun) (\s@DescribeNetworkInterfaces' {} a -> s {dryRun = a} :: DescribeNetworkInterfaces)

-- | One or more network interface IDs.
--
-- Default: Describes all your network interfaces.
describeNetworkInterfaces_networkInterfaceIds :: Lens.Lens' DescribeNetworkInterfaces (Prelude.Maybe [Prelude.Text])
describeNetworkInterfaces_networkInterfaceIds = Lens.lens (\DescribeNetworkInterfaces' {networkInterfaceIds} -> networkInterfaceIds) (\s@DescribeNetworkInterfaces' {} a -> s {networkInterfaceIds = a} :: DescribeNetworkInterfaces) Prelude.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @addresses.private-ip-address@ - The private IPv4 addresses
--     associated with the network interface.
--
-- -   @addresses.primary@ - Whether the private IPv4 address is the
--     primary IP address associated with the network interface.
--
-- -   @addresses.association.public-ip@ - The association ID returned when
--     the network interface was associated with the Elastic IP address
--     (IPv4).
--
-- -   @addresses.association.owner-id@ - The owner ID of the addresses
--     associated with the network interface.
--
-- -   @association.association-id@ - The association ID returned when the
--     network interface was associated with an IPv4 address.
--
-- -   @association.allocation-id@ - The allocation ID returned when you
--     allocated the Elastic IP address (IPv4) for your network interface.
--
-- -   @association.ip-owner-id@ - The owner of the Elastic IP address
--     (IPv4) associated with the network interface.
--
-- -   @association.public-ip@ - The address of the Elastic IP address
--     (IPv4) bound to the network interface.
--
-- -   @association.public-dns-name@ - The public DNS name for the network
--     interface (IPv4).
--
-- -   @attachment.attachment-id@ - The ID of the interface attachment.
--
-- -   @attachment.attach-time@ - The time that the network interface was
--     attached to an instance.
--
-- -   @attachment.delete-on-termination@ - Indicates whether the
--     attachment is deleted when an instance is terminated.
--
-- -   @attachment.device-index@ - The device index to which the network
--     interface is attached.
--
-- -   @attachment.instance-id@ - The ID of the instance to which the
--     network interface is attached.
--
-- -   @attachment.instance-owner-id@ - The owner ID of the instance to
--     which the network interface is attached.
--
-- -   @attachment.status@ - The status of the attachment (@attaching@ |
--     @attached@ | @detaching@ | @detached@).
--
-- -   @availability-zone@ - The Availability Zone of the network
--     interface.
--
-- -   @description@ - The description of the network interface.
--
-- -   @group-id@ - The ID of a security group associated with the network
--     interface.
--
-- -   @group-name@ - The name of a security group associated with the
--     network interface.
--
-- -   @ipv6-addresses.ipv6-address@ - An IPv6 address associated with the
--     network interface.
--
-- -   @mac-address@ - The MAC address of the network interface.
--
-- -   @network-interface-id@ - The ID of the network interface.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the network
--     interface owner.
--
-- -   @private-ip-address@ - The private IPv4 address or addresses of the
--     network interface.
--
-- -   @private-dns-name@ - The private DNS name of the network interface
--     (IPv4).
--
-- -   @requester-id@ - The alias or Amazon Web Services account ID of the
--     principal or service that created the network interface.
--
-- -   @requester-managed@ - Indicates whether the network interface is
--     being managed by an Amazon Web Service (for example, Amazon Web
--     Services Management Console, Auto Scaling, and so on).
--
-- -   @source-dest-check@ - Indicates whether the network interface
--     performs source\/destination checking. A value of @true@ means
--     checking is enabled, and @false@ means checking is disabled. The
--     value must be @false@ for the network interface to perform network
--     address translation (NAT) in your VPC.
--
-- -   @status@ - The status of the network interface. If the network
--     interface is not attached to an instance, the status is @available@;
--     if a network interface is attached to an instance the status is
--     @in-use@.
--
-- -   @subnet-id@ - The ID of the subnet for the network interface.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC for the network interface.
describeNetworkInterfaces_filters :: Lens.Lens' DescribeNetworkInterfaces (Prelude.Maybe [Filter])
describeNetworkInterfaces_filters = Lens.lens (\DescribeNetworkInterfaces' {filters} -> filters) (\s@DescribeNetworkInterfaces' {} a -> s {filters = a} :: DescribeNetworkInterfaces) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeNetworkInterfaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkInterfacesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkInterfacesResponse_networkInterfaces
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNetworkInterfaces_nextToken
          Lens..~ rs
          Lens.^? describeNetworkInterfacesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeNetworkInterfaces where
  type
    AWSResponse DescribeNetworkInterfaces =
      DescribeNetworkInterfacesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkInterfacesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "networkInterfaceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeNetworkInterfaces

instance Prelude.NFData DescribeNetworkInterfaces

instance Core.ToHeaders DescribeNetworkInterfaces where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeNetworkInterfaces where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeNetworkInterfaces where
  toQuery DescribeNetworkInterfaces' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeNetworkInterfaces" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "NetworkInterfaceId"
              Prelude.<$> networkInterfaceIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | Contains the output of DescribeNetworkInterfaces.
--
-- /See:/ 'newDescribeNetworkInterfacesResponse' smart constructor.
data DescribeNetworkInterfacesResponse = DescribeNetworkInterfacesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more network interfaces.
    networkInterfaces :: Prelude.Maybe [NetworkInterface],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNetworkInterfacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkInterfacesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'networkInterfaces', 'describeNetworkInterfacesResponse_networkInterfaces' - Information about one or more network interfaces.
--
-- 'httpStatus', 'describeNetworkInterfacesResponse_httpStatus' - The response's http status code.
newDescribeNetworkInterfacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNetworkInterfacesResponse
newDescribeNetworkInterfacesResponse pHttpStatus_ =
  DescribeNetworkInterfacesResponse'
    { nextToken =
        Prelude.Nothing,
      networkInterfaces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkInterfacesResponse_nextToken :: Lens.Lens' DescribeNetworkInterfacesResponse (Prelude.Maybe Prelude.Text)
describeNetworkInterfacesResponse_nextToken = Lens.lens (\DescribeNetworkInterfacesResponse' {nextToken} -> nextToken) (\s@DescribeNetworkInterfacesResponse' {} a -> s {nextToken = a} :: DescribeNetworkInterfacesResponse)

-- | Information about one or more network interfaces.
describeNetworkInterfacesResponse_networkInterfaces :: Lens.Lens' DescribeNetworkInterfacesResponse (Prelude.Maybe [NetworkInterface])
describeNetworkInterfacesResponse_networkInterfaces = Lens.lens (\DescribeNetworkInterfacesResponse' {networkInterfaces} -> networkInterfaces) (\s@DescribeNetworkInterfacesResponse' {} a -> s {networkInterfaces = a} :: DescribeNetworkInterfacesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNetworkInterfacesResponse_httpStatus :: Lens.Lens' DescribeNetworkInterfacesResponse Prelude.Int
describeNetworkInterfacesResponse_httpStatus = Lens.lens (\DescribeNetworkInterfacesResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkInterfacesResponse' {} a -> s {httpStatus = a} :: DescribeNetworkInterfacesResponse)

instance
  Prelude.NFData
    DescribeNetworkInterfacesResponse
