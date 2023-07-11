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
-- Module      : Amazonka.EC2.AssignPrivateIpAddresses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more secondary private IP addresses to the specified
-- network interface.
--
-- You can specify one or more specific secondary IP addresses, or you can
-- specify the number of secondary IP addresses to be automatically
-- assigned within the subnet\'s CIDR block range. The number of secondary
-- IP addresses that you can assign to an instance varies by instance type.
-- For information about instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/. For more information
-- about Elastic IP addresses, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- When you move a secondary private IP address to another network
-- interface, any Elastic IP address that is associated with the IP address
-- is also moved.
--
-- Remapping an IP address is an asynchronous operation. When you move an
-- IP address from one network interface to another, check
-- @network\/interfaces\/macs\/mac\/local-ipv4s@ in the instance metadata
-- to confirm that the remapping is complete.
--
-- You must specify either the IP addresses or the IP address count in the
-- request.
--
-- You can optionally use Prefix Delegation on the network interface. You
-- must specify either the IPv4 Prefix Delegation prefixes, or the IPv4
-- Prefix Delegation count. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.AssignPrivateIpAddresses
  ( -- * Creating a Request
    AssignPrivateIpAddresses (..),
    newAssignPrivateIpAddresses,

    -- * Request Lenses
    assignPrivateIpAddresses_allowReassignment,
    assignPrivateIpAddresses_ipv4PrefixCount,
    assignPrivateIpAddresses_ipv4Prefixes,
    assignPrivateIpAddresses_privateIpAddresses,
    assignPrivateIpAddresses_secondaryPrivateIpAddressCount,
    assignPrivateIpAddresses_networkInterfaceId,

    -- * Destructuring the Response
    AssignPrivateIpAddressesResponse (..),
    newAssignPrivateIpAddressesResponse,

    -- * Response Lenses
    assignPrivateIpAddressesResponse_assignedIpv4Prefixes,
    assignPrivateIpAddressesResponse_assignedPrivateIpAddresses,
    assignPrivateIpAddressesResponse_networkInterfaceId,
    assignPrivateIpAddressesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for AssignPrivateIpAddresses.
--
-- /See:/ 'newAssignPrivateIpAddresses' smart constructor.
data AssignPrivateIpAddresses = AssignPrivateIpAddresses'
  { -- | Indicates whether to allow an IP address that is already assigned to
    -- another network interface or instance to be reassigned to the specified
    -- network interface.
    allowReassignment :: Prelude.Maybe Prelude.Bool,
    -- | The number of IPv4 prefixes that Amazon Web Services automatically
    -- assigns to the network interface. You cannot use this option if you use
    -- the @Ipv4 Prefixes@ option.
    ipv4PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv4 prefixes assigned to the network interface. You cannot
    -- use this option if you use the @Ipv4PrefixCount@ option.
    ipv4Prefixes :: Prelude.Maybe [Prelude.Text],
    -- | The IP addresses to be assigned as a secondary private IP address to the
    -- network interface. You can\'t specify this parameter when also
    -- specifying a number of secondary IP addresses.
    --
    -- If you don\'t specify an IP address, Amazon EC2 automatically selects an
    -- IP address within the subnet range.
    privateIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The number of secondary IP addresses to assign to the network interface.
    -- You can\'t specify this parameter when also specifying private IP
    -- addresses.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignPrivateIpAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowReassignment', 'assignPrivateIpAddresses_allowReassignment' - Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
--
-- 'ipv4PrefixCount', 'assignPrivateIpAddresses_ipv4PrefixCount' - The number of IPv4 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv4 Prefixes@ option.
--
-- 'ipv4Prefixes', 'assignPrivateIpAddresses_ipv4Prefixes' - One or more IPv4 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv4PrefixCount@ option.
--
-- 'privateIpAddresses', 'assignPrivateIpAddresses_privateIpAddresses' - The IP addresses to be assigned as a secondary private IP address to the
-- network interface. You can\'t specify this parameter when also
-- specifying a number of secondary IP addresses.
--
-- If you don\'t specify an IP address, Amazon EC2 automatically selects an
-- IP address within the subnet range.
--
-- 'secondaryPrivateIpAddressCount', 'assignPrivateIpAddresses_secondaryPrivateIpAddressCount' - The number of secondary IP addresses to assign to the network interface.
-- You can\'t specify this parameter when also specifying private IP
-- addresses.
--
-- 'networkInterfaceId', 'assignPrivateIpAddresses_networkInterfaceId' - The ID of the network interface.
newAssignPrivateIpAddresses ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  AssignPrivateIpAddresses
newAssignPrivateIpAddresses pNetworkInterfaceId_ =
  AssignPrivateIpAddresses'
    { allowReassignment =
        Prelude.Nothing,
      ipv4PrefixCount = Prelude.Nothing,
      ipv4Prefixes = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      secondaryPrivateIpAddressCount = Prelude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
assignPrivateIpAddresses_allowReassignment :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe Prelude.Bool)
assignPrivateIpAddresses_allowReassignment = Lens.lens (\AssignPrivateIpAddresses' {allowReassignment} -> allowReassignment) (\s@AssignPrivateIpAddresses' {} a -> s {allowReassignment = a} :: AssignPrivateIpAddresses)

-- | The number of IPv4 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv4 Prefixes@ option.
assignPrivateIpAddresses_ipv4PrefixCount :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe Prelude.Int)
assignPrivateIpAddresses_ipv4PrefixCount = Lens.lens (\AssignPrivateIpAddresses' {ipv4PrefixCount} -> ipv4PrefixCount) (\s@AssignPrivateIpAddresses' {} a -> s {ipv4PrefixCount = a} :: AssignPrivateIpAddresses)

-- | One or more IPv4 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv4PrefixCount@ option.
assignPrivateIpAddresses_ipv4Prefixes :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe [Prelude.Text])
assignPrivateIpAddresses_ipv4Prefixes = Lens.lens (\AssignPrivateIpAddresses' {ipv4Prefixes} -> ipv4Prefixes) (\s@AssignPrivateIpAddresses' {} a -> s {ipv4Prefixes = a} :: AssignPrivateIpAddresses) Prelude.. Lens.mapping Lens.coerced

-- | The IP addresses to be assigned as a secondary private IP address to the
-- network interface. You can\'t specify this parameter when also
-- specifying a number of secondary IP addresses.
--
-- If you don\'t specify an IP address, Amazon EC2 automatically selects an
-- IP address within the subnet range.
assignPrivateIpAddresses_privateIpAddresses :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe [Prelude.Text])
assignPrivateIpAddresses_privateIpAddresses = Lens.lens (\AssignPrivateIpAddresses' {privateIpAddresses} -> privateIpAddresses) (\s@AssignPrivateIpAddresses' {} a -> s {privateIpAddresses = a} :: AssignPrivateIpAddresses) Prelude.. Lens.mapping Lens.coerced

-- | The number of secondary IP addresses to assign to the network interface.
-- You can\'t specify this parameter when also specifying private IP
-- addresses.
assignPrivateIpAddresses_secondaryPrivateIpAddressCount :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe Prelude.Int)
assignPrivateIpAddresses_secondaryPrivateIpAddressCount = Lens.lens (\AssignPrivateIpAddresses' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@AssignPrivateIpAddresses' {} a -> s {secondaryPrivateIpAddressCount = a} :: AssignPrivateIpAddresses)

-- | The ID of the network interface.
assignPrivateIpAddresses_networkInterfaceId :: Lens.Lens' AssignPrivateIpAddresses Prelude.Text
assignPrivateIpAddresses_networkInterfaceId = Lens.lens (\AssignPrivateIpAddresses' {networkInterfaceId} -> networkInterfaceId) (\s@AssignPrivateIpAddresses' {} a -> s {networkInterfaceId = a} :: AssignPrivateIpAddresses)

instance Core.AWSRequest AssignPrivateIpAddresses where
  type
    AWSResponse AssignPrivateIpAddresses =
      AssignPrivateIpAddressesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssignPrivateIpAddressesResponse'
            Prelude.<$> ( x
                            Data..@? "assignedIpv4PrefixSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x
                            Data..@? "assignedPrivateIpAddressesSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "networkInterfaceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssignPrivateIpAddresses where
  hashWithSalt _salt AssignPrivateIpAddresses' {..} =
    _salt
      `Prelude.hashWithSalt` allowReassignment
      `Prelude.hashWithSalt` ipv4PrefixCount
      `Prelude.hashWithSalt` ipv4Prefixes
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` secondaryPrivateIpAddressCount
      `Prelude.hashWithSalt` networkInterfaceId

instance Prelude.NFData AssignPrivateIpAddresses where
  rnf AssignPrivateIpAddresses' {..} =
    Prelude.rnf allowReassignment
      `Prelude.seq` Prelude.rnf ipv4PrefixCount
      `Prelude.seq` Prelude.rnf ipv4Prefixes
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf secondaryPrivateIpAddressCount
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance Data.ToHeaders AssignPrivateIpAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssignPrivateIpAddresses where
  toPath = Prelude.const "/"

instance Data.ToQuery AssignPrivateIpAddresses where
  toQuery AssignPrivateIpAddresses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssignPrivateIpAddresses" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AllowReassignment" Data.=: allowReassignment,
        "Ipv4PrefixCount" Data.=: ipv4PrefixCount,
        Data.toQuery
          ( Data.toQueryList "Ipv4Prefix"
              Prelude.<$> ipv4Prefixes
          ),
        Data.toQuery
          ( Data.toQueryList "PrivateIpAddress"
              Prelude.<$> privateIpAddresses
          ),
        "SecondaryPrivateIpAddressCount"
          Data.=: secondaryPrivateIpAddressCount,
        "NetworkInterfaceId" Data.=: networkInterfaceId
      ]

-- | /See:/ 'newAssignPrivateIpAddressesResponse' smart constructor.
data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse'
  { -- | The IPv4 prefixes that are assigned to the network interface.
    assignedIpv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecification],
    -- | The private IP addresses assigned to the network interface.
    assignedPrivateIpAddresses :: Prelude.Maybe [AssignedPrivateIpAddress],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignPrivateIpAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignedIpv4Prefixes', 'assignPrivateIpAddressesResponse_assignedIpv4Prefixes' - The IPv4 prefixes that are assigned to the network interface.
--
-- 'assignedPrivateIpAddresses', 'assignPrivateIpAddressesResponse_assignedPrivateIpAddresses' - The private IP addresses assigned to the network interface.
--
-- 'networkInterfaceId', 'assignPrivateIpAddressesResponse_networkInterfaceId' - The ID of the network interface.
--
-- 'httpStatus', 'assignPrivateIpAddressesResponse_httpStatus' - The response's http status code.
newAssignPrivateIpAddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssignPrivateIpAddressesResponse
newAssignPrivateIpAddressesResponse pHttpStatus_ =
  AssignPrivateIpAddressesResponse'
    { assignedIpv4Prefixes =
        Prelude.Nothing,
      assignedPrivateIpAddresses =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IPv4 prefixes that are assigned to the network interface.
assignPrivateIpAddressesResponse_assignedIpv4Prefixes :: Lens.Lens' AssignPrivateIpAddressesResponse (Prelude.Maybe [Ipv4PrefixSpecification])
assignPrivateIpAddressesResponse_assignedIpv4Prefixes = Lens.lens (\AssignPrivateIpAddressesResponse' {assignedIpv4Prefixes} -> assignedIpv4Prefixes) (\s@AssignPrivateIpAddressesResponse' {} a -> s {assignedIpv4Prefixes = a} :: AssignPrivateIpAddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The private IP addresses assigned to the network interface.
assignPrivateIpAddressesResponse_assignedPrivateIpAddresses :: Lens.Lens' AssignPrivateIpAddressesResponse (Prelude.Maybe [AssignedPrivateIpAddress])
assignPrivateIpAddressesResponse_assignedPrivateIpAddresses = Lens.lens (\AssignPrivateIpAddressesResponse' {assignedPrivateIpAddresses} -> assignedPrivateIpAddresses) (\s@AssignPrivateIpAddressesResponse' {} a -> s {assignedPrivateIpAddresses = a} :: AssignPrivateIpAddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
assignPrivateIpAddressesResponse_networkInterfaceId :: Lens.Lens' AssignPrivateIpAddressesResponse (Prelude.Maybe Prelude.Text)
assignPrivateIpAddressesResponse_networkInterfaceId = Lens.lens (\AssignPrivateIpAddressesResponse' {networkInterfaceId} -> networkInterfaceId) (\s@AssignPrivateIpAddressesResponse' {} a -> s {networkInterfaceId = a} :: AssignPrivateIpAddressesResponse)

-- | The response's http status code.
assignPrivateIpAddressesResponse_httpStatus :: Lens.Lens' AssignPrivateIpAddressesResponse Prelude.Int
assignPrivateIpAddressesResponse_httpStatus = Lens.lens (\AssignPrivateIpAddressesResponse' {httpStatus} -> httpStatus) (\s@AssignPrivateIpAddressesResponse' {} a -> s {httpStatus = a} :: AssignPrivateIpAddressesResponse)

instance
  Prelude.NFData
    AssignPrivateIpAddressesResponse
  where
  rnf AssignPrivateIpAddressesResponse' {..} =
    Prelude.rnf assignedIpv4Prefixes
      `Prelude.seq` Prelude.rnf assignedPrivateIpAddresses
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf httpStatus
