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
-- Module      : Amazonka.EC2.CreateNetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network interface in the specified subnet.
--
-- For more information about network interfaces, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateNetworkInterface
  ( -- * Creating a Request
    CreateNetworkInterface (..),
    newCreateNetworkInterface,

    -- * Request Lenses
    createNetworkInterface_ipv4PrefixCount,
    createNetworkInterface_interfaceType,
    createNetworkInterface_clientToken,
    createNetworkInterface_privateIpAddresses,
    createNetworkInterface_description,
    createNetworkInterface_dryRun,
    createNetworkInterface_ipv6AddressCount,
    createNetworkInterface_ipv4Prefixes,
    createNetworkInterface_privateIpAddress,
    createNetworkInterface_ipv6PrefixCount,
    createNetworkInterface_secondaryPrivateIpAddressCount,
    createNetworkInterface_ipv6Prefixes,
    createNetworkInterface_tagSpecifications,
    createNetworkInterface_groups,
    createNetworkInterface_ipv6Addresses,
    createNetworkInterface_subnetId,

    -- * Destructuring the Response
    CreateNetworkInterfaceResponse (..),
    newCreateNetworkInterfaceResponse,

    -- * Response Lenses
    createNetworkInterfaceResponse_clientToken,
    createNetworkInterfaceResponse_networkInterface,
    createNetworkInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CreateNetworkInterface.
--
-- /See:/ 'newCreateNetworkInterface' smart constructor.
data CreateNetworkInterface = CreateNetworkInterface'
  { -- | The number of IPv4 prefixes that Amazon Web Services automatically
    -- assigns to the network interface. You cannot use this option if you use
    -- the @Ipv4 Prefixes@ option.
    ipv4PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates the type of network interface. To create an Elastic Fabric
    -- Adapter (EFA), specify @efa@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
    -- in the /Amazon Elastic Compute Cloud User Guide/. To create a trunk
    -- network interface, specify @efa@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/eni-trunking.html Network interface trunking>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    interfaceType :: Prelude.Maybe NetworkInterfaceCreationType,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | A description for the network interface.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The number of IPv6 addresses to assign to a network interface. Amazon
    -- EC2 automatically selects the IPv6 addresses from the subnet range. You
    -- can\'t use this option if specifying specific IPv6 addresses. If your
    -- subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@,
    -- you can specify @0@ to override this setting.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv4 prefixes assigned to the network interface. You cannot
    -- use this option if you use the @Ipv4PrefixCount@ option.
    ipv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecificationRequest],
    -- | The primary private IPv4 address of the network interface. If you don\'t
    -- specify an IPv4 address, Amazon EC2 selects one for you from the
    -- subnet\'s IPv4 CIDR range. If you specify an IP address, you cannot
    -- indicate any IP addresses specified in @privateIpAddresses@ as primary
    -- (only one IP address can be designated as primary).
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv6 prefixes that Amazon Web Services automatically
    -- assigns to the network interface. You cannot use this option if you use
    -- the @Ipv6Prefixes@ option.
    ipv6PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | The number of secondary private IPv4 addresses to assign to a network
    -- interface. When you specify a number of secondary IPv4 addresses, Amazon
    -- EC2 selects these IP addresses within the subnet\'s IPv4 CIDR range. You
    -- can\'t specify this option and specify more than one private IP address
    -- using @privateIpAddresses@.
    --
    -- The number of IP addresses you can assign to a network interface varies
    -- by instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per ENI Per Instance Type>
    -- in the /Amazon Virtual Private Cloud User Guide/.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv6 prefixes assigned to the network interface. You cannot
    -- use this option if you use the @Ipv6PrefixCount@ option.
    ipv6Prefixes :: Prelude.Maybe [Ipv6PrefixSpecificationRequest],
    -- | The tags to apply to the new network interface.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The IDs of one or more security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
    -- your subnet. You can\'t use this option if you\'re specifying a number
    -- of IPv6 addresses.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | The ID of the subnet to associate with the network interface.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4PrefixCount', 'createNetworkInterface_ipv4PrefixCount' - The number of IPv4 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv4 Prefixes@ option.
--
-- 'interfaceType', 'createNetworkInterface_interfaceType' - Indicates the type of network interface. To create an Elastic Fabric
-- Adapter (EFA), specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/. To create a trunk
-- network interface, specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/eni-trunking.html Network interface trunking>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'clientToken', 'createNetworkInterface_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'privateIpAddresses', 'createNetworkInterface_privateIpAddresses' - One or more private IPv4 addresses.
--
-- 'description', 'createNetworkInterface_description' - A description for the network interface.
--
-- 'dryRun', 'createNetworkInterface_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'ipv6AddressCount', 'createNetworkInterface_ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses. If your
-- subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@,
-- you can specify @0@ to override this setting.
--
-- 'ipv4Prefixes', 'createNetworkInterface_ipv4Prefixes' - One or more IPv4 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv4PrefixCount@ option.
--
-- 'privateIpAddress', 'createNetworkInterface_privateIpAddress' - The primary private IPv4 address of the network interface. If you don\'t
-- specify an IPv4 address, Amazon EC2 selects one for you from the
-- subnet\'s IPv4 CIDR range. If you specify an IP address, you cannot
-- indicate any IP addresses specified in @privateIpAddresses@ as primary
-- (only one IP address can be designated as primary).
--
-- 'ipv6PrefixCount', 'createNetworkInterface_ipv6PrefixCount' - The number of IPv6 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv6Prefixes@ option.
--
-- 'secondaryPrivateIpAddressCount', 'createNetworkInterface_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses to assign to a network
-- interface. When you specify a number of secondary IPv4 addresses, Amazon
-- EC2 selects these IP addresses within the subnet\'s IPv4 CIDR range. You
-- can\'t specify this option and specify more than one private IP address
-- using @privateIpAddresses@.
--
-- The number of IP addresses you can assign to a network interface varies
-- by instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per ENI Per Instance Type>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- 'ipv6Prefixes', 'createNetworkInterface_ipv6Prefixes' - One or more IPv6 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv6PrefixCount@ option.
--
-- 'tagSpecifications', 'createNetworkInterface_tagSpecifications' - The tags to apply to the new network interface.
--
-- 'groups', 'createNetworkInterface_groups' - The IDs of one or more security groups.
--
-- 'ipv6Addresses', 'createNetworkInterface_ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
--
-- 'subnetId', 'createNetworkInterface_subnetId' - The ID of the subnet to associate with the network interface.
newCreateNetworkInterface ::
  -- | 'subnetId'
  Prelude.Text ->
  CreateNetworkInterface
newCreateNetworkInterface pSubnetId_ =
  CreateNetworkInterface'
    { ipv4PrefixCount =
        Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      ipv4Prefixes = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      ipv6PrefixCount = Prelude.Nothing,
      secondaryPrivateIpAddressCount = Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      groups = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | The number of IPv4 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv4 Prefixes@ option.
createNetworkInterface_ipv4PrefixCount :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Int)
createNetworkInterface_ipv4PrefixCount = Lens.lens (\CreateNetworkInterface' {ipv4PrefixCount} -> ipv4PrefixCount) (\s@CreateNetworkInterface' {} a -> s {ipv4PrefixCount = a} :: CreateNetworkInterface)

-- | Indicates the type of network interface. To create an Elastic Fabric
-- Adapter (EFA), specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/. To create a trunk
-- network interface, specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/eni-trunking.html Network interface trunking>
-- in the /Amazon Elastic Compute Cloud User Guide/.
createNetworkInterface_interfaceType :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe NetworkInterfaceCreationType)
createNetworkInterface_interfaceType = Lens.lens (\CreateNetworkInterface' {interfaceType} -> interfaceType) (\s@CreateNetworkInterface' {} a -> s {interfaceType = a} :: CreateNetworkInterface)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createNetworkInterface_clientToken :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Text)
createNetworkInterface_clientToken = Lens.lens (\CreateNetworkInterface' {clientToken} -> clientToken) (\s@CreateNetworkInterface' {} a -> s {clientToken = a} :: CreateNetworkInterface)

-- | One or more private IPv4 addresses.
createNetworkInterface_privateIpAddresses :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [PrivateIpAddressSpecification])
createNetworkInterface_privateIpAddresses = Lens.lens (\CreateNetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@CreateNetworkInterface' {} a -> s {privateIpAddresses = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | A description for the network interface.
createNetworkInterface_description :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Text)
createNetworkInterface_description = Lens.lens (\CreateNetworkInterface' {description} -> description) (\s@CreateNetworkInterface' {} a -> s {description = a} :: CreateNetworkInterface)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkInterface_dryRun :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Bool)
createNetworkInterface_dryRun = Lens.lens (\CreateNetworkInterface' {dryRun} -> dryRun) (\s@CreateNetworkInterface' {} a -> s {dryRun = a} :: CreateNetworkInterface)

-- | The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses. If your
-- subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@,
-- you can specify @0@ to override this setting.
createNetworkInterface_ipv6AddressCount :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Int)
createNetworkInterface_ipv6AddressCount = Lens.lens (\CreateNetworkInterface' {ipv6AddressCount} -> ipv6AddressCount) (\s@CreateNetworkInterface' {} a -> s {ipv6AddressCount = a} :: CreateNetworkInterface)

-- | One or more IPv4 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv4PrefixCount@ option.
createNetworkInterface_ipv4Prefixes :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [Ipv4PrefixSpecificationRequest])
createNetworkInterface_ipv4Prefixes = Lens.lens (\CreateNetworkInterface' {ipv4Prefixes} -> ipv4Prefixes) (\s@CreateNetworkInterface' {} a -> s {ipv4Prefixes = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The primary private IPv4 address of the network interface. If you don\'t
-- specify an IPv4 address, Amazon EC2 selects one for you from the
-- subnet\'s IPv4 CIDR range. If you specify an IP address, you cannot
-- indicate any IP addresses specified in @privateIpAddresses@ as primary
-- (only one IP address can be designated as primary).
createNetworkInterface_privateIpAddress :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Text)
createNetworkInterface_privateIpAddress = Lens.lens (\CreateNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@CreateNetworkInterface' {} a -> s {privateIpAddress = a} :: CreateNetworkInterface)

-- | The number of IPv6 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv6Prefixes@ option.
createNetworkInterface_ipv6PrefixCount :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Int)
createNetworkInterface_ipv6PrefixCount = Lens.lens (\CreateNetworkInterface' {ipv6PrefixCount} -> ipv6PrefixCount) (\s@CreateNetworkInterface' {} a -> s {ipv6PrefixCount = a} :: CreateNetworkInterface)

-- | The number of secondary private IPv4 addresses to assign to a network
-- interface. When you specify a number of secondary IPv4 addresses, Amazon
-- EC2 selects these IP addresses within the subnet\'s IPv4 CIDR range. You
-- can\'t specify this option and specify more than one private IP address
-- using @privateIpAddresses@.
--
-- The number of IP addresses you can assign to a network interface varies
-- by instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per ENI Per Instance Type>
-- in the /Amazon Virtual Private Cloud User Guide/.
createNetworkInterface_secondaryPrivateIpAddressCount :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Int)
createNetworkInterface_secondaryPrivateIpAddressCount = Lens.lens (\CreateNetworkInterface' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@CreateNetworkInterface' {} a -> s {secondaryPrivateIpAddressCount = a} :: CreateNetworkInterface)

-- | One or more IPv6 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv6PrefixCount@ option.
createNetworkInterface_ipv6Prefixes :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [Ipv6PrefixSpecificationRequest])
createNetworkInterface_ipv6Prefixes = Lens.lens (\CreateNetworkInterface' {ipv6Prefixes} -> ipv6Prefixes) (\s@CreateNetworkInterface' {} a -> s {ipv6Prefixes = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply to the new network interface.
createNetworkInterface_tagSpecifications :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [TagSpecification])
createNetworkInterface_tagSpecifications = Lens.lens (\CreateNetworkInterface' {tagSpecifications} -> tagSpecifications) (\s@CreateNetworkInterface' {} a -> s {tagSpecifications = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of one or more security groups.
createNetworkInterface_groups :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [Prelude.Text])
createNetworkInterface_groups = Lens.lens (\CreateNetworkInterface' {groups} -> groups) (\s@CreateNetworkInterface' {} a -> s {groups = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
createNetworkInterface_ipv6Addresses :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [InstanceIpv6Address])
createNetworkInterface_ipv6Addresses = Lens.lens (\CreateNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@CreateNetworkInterface' {} a -> s {ipv6Addresses = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subnet to associate with the network interface.
createNetworkInterface_subnetId :: Lens.Lens' CreateNetworkInterface Prelude.Text
createNetworkInterface_subnetId = Lens.lens (\CreateNetworkInterface' {subnetId} -> subnetId) (\s@CreateNetworkInterface' {} a -> s {subnetId = a} :: CreateNetworkInterface)

instance Core.AWSRequest CreateNetworkInterface where
  type
    AWSResponse CreateNetworkInterface =
      CreateNetworkInterfaceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInterfaceResponse'
            Prelude.<$> (x Core..@? "clientToken")
            Prelude.<*> (x Core..@? "networkInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkInterface where
  hashWithSalt _salt CreateNetworkInterface' {..} =
    _salt `Prelude.hashWithSalt` ipv4PrefixCount
      `Prelude.hashWithSalt` interfaceType
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ipv6AddressCount
      `Prelude.hashWithSalt` ipv4Prefixes
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` ipv6PrefixCount
      `Prelude.hashWithSalt` secondaryPrivateIpAddressCount
      `Prelude.hashWithSalt` ipv6Prefixes
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData CreateNetworkInterface where
  rnf CreateNetworkInterface' {..} =
    Prelude.rnf ipv4PrefixCount
      `Prelude.seq` Prelude.rnf interfaceType
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ipv6AddressCount
      `Prelude.seq` Prelude.rnf ipv4Prefixes
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf ipv6PrefixCount
      `Prelude.seq` Prelude.rnf secondaryPrivateIpAddressCount
      `Prelude.seq` Prelude.rnf ipv6Prefixes
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf subnetId

instance Core.ToHeaders CreateNetworkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateNetworkInterface where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateNetworkInterface where
  toQuery CreateNetworkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateNetworkInterface" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "Ipv4PrefixCount" Core.=: ipv4PrefixCount,
        "InterfaceType" Core.=: interfaceType,
        "ClientToken" Core.=: clientToken,
        Core.toQuery
          ( Core.toQueryList "PrivateIpAddresses"
              Prelude.<$> privateIpAddresses
          ),
        "Description" Core.=: description,
        "DryRun" Core.=: dryRun,
        "Ipv6AddressCount" Core.=: ipv6AddressCount,
        Core.toQuery
          ( Core.toQueryList "Ipv4Prefix"
              Prelude.<$> ipv4Prefixes
          ),
        "PrivateIpAddress" Core.=: privateIpAddress,
        "Ipv6PrefixCount" Core.=: ipv6PrefixCount,
        "SecondaryPrivateIpAddressCount"
          Core.=: secondaryPrivateIpAddressCount,
        Core.toQuery
          ( Core.toQueryList "Ipv6Prefix"
              Prelude.<$> ipv6Prefixes
          ),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        Core.toQuery
          ( Core.toQueryList "SecurityGroupId"
              Prelude.<$> groups
          ),
        Core.toQuery
          ( Core.toQueryList "Ipv6Addresses"
              Prelude.<$> ipv6Addresses
          ),
        "SubnetId" Core.=: subnetId
      ]

-- | Contains the output of CreateNetworkInterface.
--
-- /See:/ 'newCreateNetworkInterfaceResponse' smart constructor.
data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the network interface.
    networkInterface :: Prelude.Maybe NetworkInterface,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createNetworkInterfaceResponse_clientToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'networkInterface', 'createNetworkInterfaceResponse_networkInterface' - Information about the network interface.
--
-- 'httpStatus', 'createNetworkInterfaceResponse_httpStatus' - The response's http status code.
newCreateNetworkInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNetworkInterfaceResponse
newCreateNetworkInterfaceResponse pHttpStatus_ =
  CreateNetworkInterfaceResponse'
    { clientToken =
        Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
createNetworkInterfaceResponse_clientToken :: Lens.Lens' CreateNetworkInterfaceResponse (Prelude.Maybe Prelude.Text)
createNetworkInterfaceResponse_clientToken = Lens.lens (\CreateNetworkInterfaceResponse' {clientToken} -> clientToken) (\s@CreateNetworkInterfaceResponse' {} a -> s {clientToken = a} :: CreateNetworkInterfaceResponse)

-- | Information about the network interface.
createNetworkInterfaceResponse_networkInterface :: Lens.Lens' CreateNetworkInterfaceResponse (Prelude.Maybe NetworkInterface)
createNetworkInterfaceResponse_networkInterface = Lens.lens (\CreateNetworkInterfaceResponse' {networkInterface} -> networkInterface) (\s@CreateNetworkInterfaceResponse' {} a -> s {networkInterface = a} :: CreateNetworkInterfaceResponse)

-- | The response's http status code.
createNetworkInterfaceResponse_httpStatus :: Lens.Lens' CreateNetworkInterfaceResponse Prelude.Int
createNetworkInterfaceResponse_httpStatus = Lens.lens (\CreateNetworkInterfaceResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkInterfaceResponse' {} a -> s {httpStatus = a} :: CreateNetworkInterfaceResponse)

instance
  Prelude.NFData
    CreateNetworkInterfaceResponse
  where
  rnf CreateNetworkInterfaceResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf networkInterface
      `Prelude.seq` Prelude.rnf httpStatus
