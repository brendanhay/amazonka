{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.LocalGatewayRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGatewayRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LocalGatewayRouteState
import Amazonka.EC2.Types.LocalGatewayRouteType
import qualified Amazonka.Prelude as Prelude

-- | Describes a route for a local gateway route table.
--
-- /See:/ 'newLocalGatewayRoute' smart constructor.
data LocalGatewayRoute = LocalGatewayRoute'
  { -- | The ID of the customer-owned address pool.
    coipPoolId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR block used for destination matches.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix list.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table.
    localGatewayRouteTableArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the local gateway
    -- route.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the route.
    state :: Prelude.Maybe LocalGatewayRouteState,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The route type.
    type' :: Prelude.Maybe LocalGatewayRouteType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coipPoolId', 'localGatewayRoute_coipPoolId' - The ID of the customer-owned address pool.
--
-- 'destinationCidrBlock', 'localGatewayRoute_destinationCidrBlock' - The CIDR block used for destination matches.
--
-- 'destinationPrefixListId', 'localGatewayRoute_destinationPrefixListId' - The ID of the prefix list.
--
-- 'localGatewayRouteTableArn', 'localGatewayRoute_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table.
--
-- 'localGatewayRouteTableId', 'localGatewayRoute_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'localGatewayVirtualInterfaceGroupId', 'localGatewayRoute_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'networkInterfaceId', 'localGatewayRoute_networkInterfaceId' - The ID of the network interface.
--
-- 'ownerId', 'localGatewayRoute_ownerId' - The ID of the Amazon Web Services account that owns the local gateway
-- route.
--
-- 'state', 'localGatewayRoute_state' - The state of the route.
--
-- 'subnetId', 'localGatewayRoute_subnetId' - The ID of the subnet.
--
-- 'type'', 'localGatewayRoute_type' - The route type.
newLocalGatewayRoute ::
  LocalGatewayRoute
newLocalGatewayRoute =
  LocalGatewayRoute'
    { coipPoolId = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      destinationPrefixListId = Prelude.Nothing,
      localGatewayRouteTableArn = Prelude.Nothing,
      localGatewayRouteTableId = Prelude.Nothing,
      localGatewayVirtualInterfaceGroupId =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ID of the customer-owned address pool.
localGatewayRoute_coipPoolId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_coipPoolId = Lens.lens (\LocalGatewayRoute' {coipPoolId} -> coipPoolId) (\s@LocalGatewayRoute' {} a -> s {coipPoolId = a} :: LocalGatewayRoute)

-- | The CIDR block used for destination matches.
localGatewayRoute_destinationCidrBlock :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_destinationCidrBlock = Lens.lens (\LocalGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@LocalGatewayRoute' {} a -> s {destinationCidrBlock = a} :: LocalGatewayRoute)

-- | The ID of the prefix list.
localGatewayRoute_destinationPrefixListId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_destinationPrefixListId = Lens.lens (\LocalGatewayRoute' {destinationPrefixListId} -> destinationPrefixListId) (\s@LocalGatewayRoute' {} a -> s {destinationPrefixListId = a} :: LocalGatewayRoute)

-- | The Amazon Resource Name (ARN) of the local gateway route table.
localGatewayRoute_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRoute' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRoute' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRoute)

-- | The ID of the local gateway route table.
localGatewayRoute_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_localGatewayRouteTableId = Lens.lens (\LocalGatewayRoute' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRoute' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRoute)

-- | The ID of the virtual interface group.
localGatewayRoute_localGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_localGatewayVirtualInterfaceGroupId = Lens.lens (\LocalGatewayRoute' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@LocalGatewayRoute' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRoute)

-- | The ID of the network interface.
localGatewayRoute_networkInterfaceId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_networkInterfaceId = Lens.lens (\LocalGatewayRoute' {networkInterfaceId} -> networkInterfaceId) (\s@LocalGatewayRoute' {} a -> s {networkInterfaceId = a} :: LocalGatewayRoute)

-- | The ID of the Amazon Web Services account that owns the local gateway
-- route.
localGatewayRoute_ownerId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_ownerId = Lens.lens (\LocalGatewayRoute' {ownerId} -> ownerId) (\s@LocalGatewayRoute' {} a -> s {ownerId = a} :: LocalGatewayRoute)

-- | The state of the route.
localGatewayRoute_state :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe LocalGatewayRouteState)
localGatewayRoute_state = Lens.lens (\LocalGatewayRoute' {state} -> state) (\s@LocalGatewayRoute' {} a -> s {state = a} :: LocalGatewayRoute)

-- | The ID of the subnet.
localGatewayRoute_subnetId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_subnetId = Lens.lens (\LocalGatewayRoute' {subnetId} -> subnetId) (\s@LocalGatewayRoute' {} a -> s {subnetId = a} :: LocalGatewayRoute)

-- | The route type.
localGatewayRoute_type :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe LocalGatewayRouteType)
localGatewayRoute_type = Lens.lens (\LocalGatewayRoute' {type'} -> type') (\s@LocalGatewayRoute' {} a -> s {type' = a} :: LocalGatewayRoute)

instance Data.FromXML LocalGatewayRoute where
  parseXML x =
    LocalGatewayRoute'
      Prelude.<$> (x Data..@? "coipPoolId")
      Prelude.<*> (x Data..@? "destinationCidrBlock")
      Prelude.<*> (x Data..@? "destinationPrefixListId")
      Prelude.<*> (x Data..@? "localGatewayRouteTableArn")
      Prelude.<*> (x Data..@? "localGatewayRouteTableId")
      Prelude.<*> (x Data..@? "localGatewayVirtualInterfaceGroupId")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> (x Data..@? "type")

instance Prelude.Hashable LocalGatewayRoute where
  hashWithSalt _salt LocalGatewayRoute' {..} =
    _salt
      `Prelude.hashWithSalt` coipPoolId
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` destinationPrefixListId
      `Prelude.hashWithSalt` localGatewayRouteTableArn
      `Prelude.hashWithSalt` localGatewayRouteTableId
      `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupId
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData LocalGatewayRoute where
  rnf LocalGatewayRoute' {..} =
    Prelude.rnf coipPoolId
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf destinationPrefixListId
      `Prelude.seq` Prelude.rnf localGatewayRouteTableArn
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf type'
