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
-- Module      : Network.AWS.EC2.UnassignPrivateIpAddresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more secondary private IP addresses from a network
-- interface.
module Network.AWS.EC2.UnassignPrivateIpAddresses
  ( -- * Creating a Request
    UnassignPrivateIpAddresses (..),
    newUnassignPrivateIpAddresses,

    -- * Request Lenses
    unassignPrivateIpAddresses_networkInterfaceId,
    unassignPrivateIpAddresses_privateIpAddresses,

    -- * Destructuring the Response
    UnassignPrivateIpAddressesResponse (..),
    newUnassignPrivateIpAddressesResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for UnassignPrivateIpAddresses.
--
-- /See:/ 'newUnassignPrivateIpAddresses' smart constructor.
data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses'
  { -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text,
    -- | The secondary private IP addresses to unassign from the network
    -- interface. You can specify this option multiple times to unassign more
    -- than one IP address.
    privateIpAddresses :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnassignPrivateIpAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaceId', 'unassignPrivateIpAddresses_networkInterfaceId' - The ID of the network interface.
--
-- 'privateIpAddresses', 'unassignPrivateIpAddresses_privateIpAddresses' - The secondary private IP addresses to unassign from the network
-- interface. You can specify this option multiple times to unassign more
-- than one IP address.
newUnassignPrivateIpAddresses ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  UnassignPrivateIpAddresses
newUnassignPrivateIpAddresses pNetworkInterfaceId_ =
  UnassignPrivateIpAddresses'
    { networkInterfaceId =
        pNetworkInterfaceId_,
      privateIpAddresses = Prelude.mempty
    }

-- | The ID of the network interface.
unassignPrivateIpAddresses_networkInterfaceId :: Lens.Lens' UnassignPrivateIpAddresses Prelude.Text
unassignPrivateIpAddresses_networkInterfaceId = Lens.lens (\UnassignPrivateIpAddresses' {networkInterfaceId} -> networkInterfaceId) (\s@UnassignPrivateIpAddresses' {} a -> s {networkInterfaceId = a} :: UnassignPrivateIpAddresses)

-- | The secondary private IP addresses to unassign from the network
-- interface. You can specify this option multiple times to unassign more
-- than one IP address.
unassignPrivateIpAddresses_privateIpAddresses :: Lens.Lens' UnassignPrivateIpAddresses [Prelude.Text]
unassignPrivateIpAddresses_privateIpAddresses = Lens.lens (\UnassignPrivateIpAddresses' {privateIpAddresses} -> privateIpAddresses) (\s@UnassignPrivateIpAddresses' {} a -> s {privateIpAddresses = a} :: UnassignPrivateIpAddresses) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    UnassignPrivateIpAddresses
  where
  type
    Rs UnassignPrivateIpAddresses =
      UnassignPrivateIpAddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UnassignPrivateIpAddressesResponse'

instance Prelude.Hashable UnassignPrivateIpAddresses

instance Prelude.NFData UnassignPrivateIpAddresses

instance Prelude.ToHeaders UnassignPrivateIpAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UnassignPrivateIpAddresses where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UnassignPrivateIpAddresses where
  toQuery UnassignPrivateIpAddresses' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UnassignPrivateIpAddresses" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NetworkInterfaceId" Prelude.=: networkInterfaceId,
        Prelude.toQueryList
          "PrivateIpAddress"
          privateIpAddresses
      ]

-- | /See:/ 'newUnassignPrivateIpAddressesResponse' smart constructor.
data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnassignPrivateIpAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnassignPrivateIpAddressesResponse ::
  UnassignPrivateIpAddressesResponse
newUnassignPrivateIpAddressesResponse =
  UnassignPrivateIpAddressesResponse'

instance
  Prelude.NFData
    UnassignPrivateIpAddressesResponse
