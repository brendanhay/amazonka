{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.CreateNetworkInterface
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
module Network.AWS.EC2.CreateNetworkInterface
  ( -- * Creating a Request
    CreateNetworkInterface (..),
    newCreateNetworkInterface,

    -- * Request Lenses
    createNetworkInterface_groups,
    createNetworkInterface_tagSpecifications,
    createNetworkInterface_privateIpAddresses,
    createNetworkInterface_ipv6Addresses,
    createNetworkInterface_dryRun,
    createNetworkInterface_interfaceType,
    createNetworkInterface_ipv6AddressCount,
    createNetworkInterface_description,
    createNetworkInterface_secondaryPrivateIpAddressCount,
    createNetworkInterface_privateIpAddress,
    createNetworkInterface_subnetId,

    -- * Destructuring the Response
    CreateNetworkInterfaceResponse (..),
    newCreateNetworkInterfaceResponse,

    -- * Response Lenses
    createNetworkInterfaceResponse_networkInterface,
    createNetworkInterfaceResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateNetworkInterface.
--
-- /See:/ 'newCreateNetworkInterface' smart constructor.
data CreateNetworkInterface = CreateNetworkInterface'
  { -- | The IDs of one or more security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The tags to apply to the new network interface.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
    -- your subnet. You can\'t use this option if you\'re specifying a number
    -- of IPv6 addresses.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the type of network interface. To create an Elastic Fabric
    -- Adapter (EFA), specify @efa@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    interfaceType :: Prelude.Maybe NetworkInterfaceCreationType,
    -- | The number of IPv6 addresses to assign to a network interface. Amazon
    -- EC2 automatically selects the IPv6 addresses from the subnet range. You
    -- can\'t use this option if specifying specific IPv6 addresses. If your
    -- subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@,
    -- you can specify @0@ to override this setting.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | A description for the network interface.
    description :: Prelude.Maybe Prelude.Text,
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
    -- | The primary private IPv4 address of the network interface. If you don\'t
    -- specify an IPv4 address, Amazon EC2 selects one for you from the
    -- subnet\'s IPv4 CIDR range. If you specify an IP address, you cannot
    -- indicate any IP addresses specified in @privateIpAddresses@ as primary
    -- (only one IP address can be designated as primary).
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet to associate with the network interface.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'createNetworkInterface_groups' - The IDs of one or more security groups.
--
-- 'tagSpecifications', 'createNetworkInterface_tagSpecifications' - The tags to apply to the new network interface.
--
-- 'privateIpAddresses', 'createNetworkInterface_privateIpAddresses' - One or more private IPv4 addresses.
--
-- 'ipv6Addresses', 'createNetworkInterface_ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
--
-- 'dryRun', 'createNetworkInterface_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'interfaceType', 'createNetworkInterface_interfaceType' - Indicates the type of network interface. To create an Elastic Fabric
-- Adapter (EFA), specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'ipv6AddressCount', 'createNetworkInterface_ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses. If your
-- subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@,
-- you can specify @0@ to override this setting.
--
-- 'description', 'createNetworkInterface_description' - A description for the network interface.
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
-- 'privateIpAddress', 'createNetworkInterface_privateIpAddress' - The primary private IPv4 address of the network interface. If you don\'t
-- specify an IPv4 address, Amazon EC2 selects one for you from the
-- subnet\'s IPv4 CIDR range. If you specify an IP address, you cannot
-- indicate any IP addresses specified in @privateIpAddresses@ as primary
-- (only one IP address can be designated as primary).
--
-- 'subnetId', 'createNetworkInterface_subnetId' - The ID of the subnet to associate with the network interface.
newCreateNetworkInterface ::
  -- | 'subnetId'
  Prelude.Text ->
  CreateNetworkInterface
newCreateNetworkInterface pSubnetId_ =
  CreateNetworkInterface'
    { groups = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      description = Prelude.Nothing,
      secondaryPrivateIpAddressCount = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | The IDs of one or more security groups.
createNetworkInterface_groups :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [Prelude.Text])
createNetworkInterface_groups = Lens.lens (\CreateNetworkInterface' {groups} -> groups) (\s@CreateNetworkInterface' {} a -> s {groups = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | The tags to apply to the new network interface.
createNetworkInterface_tagSpecifications :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [TagSpecification])
createNetworkInterface_tagSpecifications = Lens.lens (\CreateNetworkInterface' {tagSpecifications} -> tagSpecifications) (\s@CreateNetworkInterface' {} a -> s {tagSpecifications = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more private IPv4 addresses.
createNetworkInterface_privateIpAddresses :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [PrivateIpAddressSpecification])
createNetworkInterface_privateIpAddresses = Lens.lens (\CreateNetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@CreateNetworkInterface' {} a -> s {privateIpAddresses = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
createNetworkInterface_ipv6Addresses :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe [InstanceIpv6Address])
createNetworkInterface_ipv6Addresses = Lens.lens (\CreateNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@CreateNetworkInterface' {} a -> s {ipv6Addresses = a} :: CreateNetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkInterface_dryRun :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Bool)
createNetworkInterface_dryRun = Lens.lens (\CreateNetworkInterface' {dryRun} -> dryRun) (\s@CreateNetworkInterface' {} a -> s {dryRun = a} :: CreateNetworkInterface)

-- | Indicates the type of network interface. To create an Elastic Fabric
-- Adapter (EFA), specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/.
createNetworkInterface_interfaceType :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe NetworkInterfaceCreationType)
createNetworkInterface_interfaceType = Lens.lens (\CreateNetworkInterface' {interfaceType} -> interfaceType) (\s@CreateNetworkInterface' {} a -> s {interfaceType = a} :: CreateNetworkInterface)

-- | The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses. If your
-- subnet has the @AssignIpv6AddressOnCreation@ attribute set to @true@,
-- you can specify @0@ to override this setting.
createNetworkInterface_ipv6AddressCount :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Int)
createNetworkInterface_ipv6AddressCount = Lens.lens (\CreateNetworkInterface' {ipv6AddressCount} -> ipv6AddressCount) (\s@CreateNetworkInterface' {} a -> s {ipv6AddressCount = a} :: CreateNetworkInterface)

-- | A description for the network interface.
createNetworkInterface_description :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Text)
createNetworkInterface_description = Lens.lens (\CreateNetworkInterface' {description} -> description) (\s@CreateNetworkInterface' {} a -> s {description = a} :: CreateNetworkInterface)

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

-- | The primary private IPv4 address of the network interface. If you don\'t
-- specify an IPv4 address, Amazon EC2 selects one for you from the
-- subnet\'s IPv4 CIDR range. If you specify an IP address, you cannot
-- indicate any IP addresses specified in @privateIpAddresses@ as primary
-- (only one IP address can be designated as primary).
createNetworkInterface_privateIpAddress :: Lens.Lens' CreateNetworkInterface (Prelude.Maybe Prelude.Text)
createNetworkInterface_privateIpAddress = Lens.lens (\CreateNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@CreateNetworkInterface' {} a -> s {privateIpAddress = a} :: CreateNetworkInterface)

-- | The ID of the subnet to associate with the network interface.
createNetworkInterface_subnetId :: Lens.Lens' CreateNetworkInterface Prelude.Text
createNetworkInterface_subnetId = Lens.lens (\CreateNetworkInterface' {subnetId} -> subnetId) (\s@CreateNetworkInterface' {} a -> s {subnetId = a} :: CreateNetworkInterface)

instance Prelude.AWSRequest CreateNetworkInterface where
  type
    Rs CreateNetworkInterface =
      CreateNetworkInterfaceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkInterfaceResponse'
            Prelude.<$> (x Prelude..@? "networkInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNetworkInterface

instance Prelude.NFData CreateNetworkInterface

instance Prelude.ToHeaders CreateNetworkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateNetworkInterface where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateNetworkInterface where
  toQuery CreateNetworkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateNetworkInterface" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "SecurityGroupId"
              Prelude.<$> groups
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "PrivateIpAddresses"
              Prelude.<$> privateIpAddresses
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "Ipv6Addresses"
              Prelude.<$> ipv6Addresses
          ),
        "DryRun" Prelude.=: dryRun,
        "InterfaceType" Prelude.=: interfaceType,
        "Ipv6AddressCount" Prelude.=: ipv6AddressCount,
        "Description" Prelude.=: description,
        "SecondaryPrivateIpAddressCount"
          Prelude.=: secondaryPrivateIpAddressCount,
        "PrivateIpAddress" Prelude.=: privateIpAddress,
        "SubnetId" Prelude.=: subnetId
      ]

-- | Contains the output of CreateNetworkInterface.
--
-- /See:/ 'newCreateNetworkInterfaceResponse' smart constructor.
data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse'
  { -- | Information about the network interface.
    networkInterface :: Prelude.Maybe NetworkInterface,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
    { networkInterface =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the network interface.
createNetworkInterfaceResponse_networkInterface :: Lens.Lens' CreateNetworkInterfaceResponse (Prelude.Maybe NetworkInterface)
createNetworkInterfaceResponse_networkInterface = Lens.lens (\CreateNetworkInterfaceResponse' {networkInterface} -> networkInterface) (\s@CreateNetworkInterfaceResponse' {} a -> s {networkInterface = a} :: CreateNetworkInterfaceResponse)

-- | The response's http status code.
createNetworkInterfaceResponse_httpStatus :: Lens.Lens' CreateNetworkInterfaceResponse Prelude.Int
createNetworkInterfaceResponse_httpStatus = Lens.lens (\CreateNetworkInterfaceResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkInterfaceResponse' {} a -> s {httpStatus = a} :: CreateNetworkInterfaceResponse)

instance
  Prelude.NFData
    CreateNetworkInterfaceResponse
