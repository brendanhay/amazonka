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
-- Module      : Network.AWS.EC2.AssignPrivateIpAddresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.EC2.AssignPrivateIpAddresses
  ( -- * Creating a Request
    AssignPrivateIpAddresses (..),
    newAssignPrivateIpAddresses,

    -- * Request Lenses
    assignPrivateIpAddresses_privateIpAddresses,
    assignPrivateIpAddresses_secondaryPrivateIpAddressCount,
    assignPrivateIpAddresses_allowReassignment,
    assignPrivateIpAddresses_networkInterfaceId,

    -- * Destructuring the Response
    AssignPrivateIpAddressesResponse (..),
    newAssignPrivateIpAddressesResponse,

    -- * Response Lenses
    assignPrivateIpAddressesResponse_assignedPrivateIpAddresses,
    assignPrivateIpAddressesResponse_networkInterfaceId,
    assignPrivateIpAddressesResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AssignPrivateIpAddresses.
--
-- /See:/ 'newAssignPrivateIpAddresses' smart constructor.
data AssignPrivateIpAddresses = AssignPrivateIpAddresses'
  { -- | One or more IP addresses to be assigned as a secondary private IP
    -- address to the network interface. You can\'t specify this parameter when
    -- also specifying a number of secondary IP addresses.
    --
    -- If you don\'t specify an IP address, Amazon EC2 automatically selects an
    -- IP address within the subnet range.
    privateIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The number of secondary IP addresses to assign to the network interface.
    -- You can\'t specify this parameter when also specifying private IP
    -- addresses.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to allow an IP address that is already assigned to
    -- another network interface or instance to be reassigned to the specified
    -- network interface.
    allowReassignment :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignPrivateIpAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpAddresses', 'assignPrivateIpAddresses_privateIpAddresses' - One or more IP addresses to be assigned as a secondary private IP
-- address to the network interface. You can\'t specify this parameter when
-- also specifying a number of secondary IP addresses.
--
-- If you don\'t specify an IP address, Amazon EC2 automatically selects an
-- IP address within the subnet range.
--
-- 'secondaryPrivateIpAddressCount', 'assignPrivateIpAddresses_secondaryPrivateIpAddressCount' - The number of secondary IP addresses to assign to the network interface.
-- You can\'t specify this parameter when also specifying private IP
-- addresses.
--
-- 'allowReassignment', 'assignPrivateIpAddresses_allowReassignment' - Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
--
-- 'networkInterfaceId', 'assignPrivateIpAddresses_networkInterfaceId' - The ID of the network interface.
newAssignPrivateIpAddresses ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  AssignPrivateIpAddresses
newAssignPrivateIpAddresses pNetworkInterfaceId_ =
  AssignPrivateIpAddresses'
    { privateIpAddresses =
        Prelude.Nothing,
      secondaryPrivateIpAddressCount = Prelude.Nothing,
      allowReassignment = Prelude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | One or more IP addresses to be assigned as a secondary private IP
-- address to the network interface. You can\'t specify this parameter when
-- also specifying a number of secondary IP addresses.
--
-- If you don\'t specify an IP address, Amazon EC2 automatically selects an
-- IP address within the subnet range.
assignPrivateIpAddresses_privateIpAddresses :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe [Prelude.Text])
assignPrivateIpAddresses_privateIpAddresses = Lens.lens (\AssignPrivateIpAddresses' {privateIpAddresses} -> privateIpAddresses) (\s@AssignPrivateIpAddresses' {} a -> s {privateIpAddresses = a} :: AssignPrivateIpAddresses) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of secondary IP addresses to assign to the network interface.
-- You can\'t specify this parameter when also specifying private IP
-- addresses.
assignPrivateIpAddresses_secondaryPrivateIpAddressCount :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe Prelude.Int)
assignPrivateIpAddresses_secondaryPrivateIpAddressCount = Lens.lens (\AssignPrivateIpAddresses' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@AssignPrivateIpAddresses' {} a -> s {secondaryPrivateIpAddressCount = a} :: AssignPrivateIpAddresses)

-- | Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
assignPrivateIpAddresses_allowReassignment :: Lens.Lens' AssignPrivateIpAddresses (Prelude.Maybe Prelude.Bool)
assignPrivateIpAddresses_allowReassignment = Lens.lens (\AssignPrivateIpAddresses' {allowReassignment} -> allowReassignment) (\s@AssignPrivateIpAddresses' {} a -> s {allowReassignment = a} :: AssignPrivateIpAddresses)

-- | The ID of the network interface.
assignPrivateIpAddresses_networkInterfaceId :: Lens.Lens' AssignPrivateIpAddresses Prelude.Text
assignPrivateIpAddresses_networkInterfaceId = Lens.lens (\AssignPrivateIpAddresses' {networkInterfaceId} -> networkInterfaceId) (\s@AssignPrivateIpAddresses' {} a -> s {networkInterfaceId = a} :: AssignPrivateIpAddresses)

instance Prelude.AWSRequest AssignPrivateIpAddresses where
  type
    Rs AssignPrivateIpAddresses =
      AssignPrivateIpAddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssignPrivateIpAddressesResponse'
            Prelude.<$> ( x Prelude..@? "assignedPrivateIpAddressesSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (x Prelude..@? "networkInterfaceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssignPrivateIpAddresses

instance Prelude.NFData AssignPrivateIpAddresses

instance Prelude.ToHeaders AssignPrivateIpAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AssignPrivateIpAddresses where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssignPrivateIpAddresses where
  toQuery AssignPrivateIpAddresses' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AssignPrivateIpAddresses" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "PrivateIpAddress"
              Prelude.<$> privateIpAddresses
          ),
        "SecondaryPrivateIpAddressCount"
          Prelude.=: secondaryPrivateIpAddressCount,
        "AllowReassignment" Prelude.=: allowReassignment,
        "NetworkInterfaceId" Prelude.=: networkInterfaceId
      ]

-- | /See:/ 'newAssignPrivateIpAddressesResponse' smart constructor.
data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse'
  { -- | The private IP addresses assigned to the network interface.
    assignedPrivateIpAddresses :: Prelude.Maybe [AssignedPrivateIpAddress],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignPrivateIpAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
    { assignedPrivateIpAddresses =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The private IP addresses assigned to the network interface.
assignPrivateIpAddressesResponse_assignedPrivateIpAddresses :: Lens.Lens' AssignPrivateIpAddressesResponse (Prelude.Maybe [AssignedPrivateIpAddress])
assignPrivateIpAddressesResponse_assignedPrivateIpAddresses = Lens.lens (\AssignPrivateIpAddressesResponse' {assignedPrivateIpAddresses} -> assignedPrivateIpAddresses) (\s@AssignPrivateIpAddressesResponse' {} a -> s {assignedPrivateIpAddresses = a} :: AssignPrivateIpAddressesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the network interface.
assignPrivateIpAddressesResponse_networkInterfaceId :: Lens.Lens' AssignPrivateIpAddressesResponse (Prelude.Maybe Prelude.Text)
assignPrivateIpAddressesResponse_networkInterfaceId = Lens.lens (\AssignPrivateIpAddressesResponse' {networkInterfaceId} -> networkInterfaceId) (\s@AssignPrivateIpAddressesResponse' {} a -> s {networkInterfaceId = a} :: AssignPrivateIpAddressesResponse)

-- | The response's http status code.
assignPrivateIpAddressesResponse_httpStatus :: Lens.Lens' AssignPrivateIpAddressesResponse Prelude.Int
assignPrivateIpAddressesResponse_httpStatus = Lens.lens (\AssignPrivateIpAddressesResponse' {httpStatus} -> httpStatus) (\s@AssignPrivateIpAddressesResponse' {} a -> s {httpStatus = a} :: AssignPrivateIpAddressesResponse)

instance
  Prelude.NFData
    AssignPrivateIpAddressesResponse
