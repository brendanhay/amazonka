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
-- Module      : Network.AWS.EC2.Types.LocalGatewayRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRoute where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LocalGatewayRouteState
import Network.AWS.EC2.Types.LocalGatewayRouteType
import qualified Network.AWS.Lens as Lens

-- | Describes a route for a local gateway route table.
--
-- /See:/ 'newLocalGatewayRoute' smart constructor.
data LocalGatewayRoute = LocalGatewayRoute'
  { -- | The AWS account ID that owns the local gateway route.
    ownerId :: Core.Maybe Core.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table.
    localGatewayRouteTableArn :: Core.Maybe Core.Text,
    -- | The state of the route.
    state :: Core.Maybe LocalGatewayRouteState,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Core.Maybe Core.Text,
    -- | The CIDR block used for destination matches.
    destinationCidrBlock :: Core.Maybe Core.Text,
    -- | The route type.
    type' :: Core.Maybe LocalGatewayRouteType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LocalGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'localGatewayRoute_ownerId' - The AWS account ID that owns the local gateway route.
--
-- 'localGatewayVirtualInterfaceGroupId', 'localGatewayRoute_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'localGatewayRouteTableArn', 'localGatewayRoute_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table.
--
-- 'state', 'localGatewayRoute_state' - The state of the route.
--
-- 'localGatewayRouteTableId', 'localGatewayRoute_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'destinationCidrBlock', 'localGatewayRoute_destinationCidrBlock' - The CIDR block used for destination matches.
--
-- 'type'', 'localGatewayRoute_type' - The route type.
newLocalGatewayRoute ::
  LocalGatewayRoute
newLocalGatewayRoute =
  LocalGatewayRoute'
    { ownerId = Core.Nothing,
      localGatewayVirtualInterfaceGroupId = Core.Nothing,
      localGatewayRouteTableArn = Core.Nothing,
      state = Core.Nothing,
      localGatewayRouteTableId = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      type' = Core.Nothing
    }

-- | The AWS account ID that owns the local gateway route.
localGatewayRoute_ownerId :: Lens.Lens' LocalGatewayRoute (Core.Maybe Core.Text)
localGatewayRoute_ownerId = Lens.lens (\LocalGatewayRoute' {ownerId} -> ownerId) (\s@LocalGatewayRoute' {} a -> s {ownerId = a} :: LocalGatewayRoute)

-- | The ID of the virtual interface group.
localGatewayRoute_localGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRoute (Core.Maybe Core.Text)
localGatewayRoute_localGatewayVirtualInterfaceGroupId = Lens.lens (\LocalGatewayRoute' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@LocalGatewayRoute' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRoute)

-- | The Amazon Resource Name (ARN) of the local gateway route table.
localGatewayRoute_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRoute (Core.Maybe Core.Text)
localGatewayRoute_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRoute' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRoute' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRoute)

-- | The state of the route.
localGatewayRoute_state :: Lens.Lens' LocalGatewayRoute (Core.Maybe LocalGatewayRouteState)
localGatewayRoute_state = Lens.lens (\LocalGatewayRoute' {state} -> state) (\s@LocalGatewayRoute' {} a -> s {state = a} :: LocalGatewayRoute)

-- | The ID of the local gateway route table.
localGatewayRoute_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRoute (Core.Maybe Core.Text)
localGatewayRoute_localGatewayRouteTableId = Lens.lens (\LocalGatewayRoute' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRoute' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRoute)

-- | The CIDR block used for destination matches.
localGatewayRoute_destinationCidrBlock :: Lens.Lens' LocalGatewayRoute (Core.Maybe Core.Text)
localGatewayRoute_destinationCidrBlock = Lens.lens (\LocalGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@LocalGatewayRoute' {} a -> s {destinationCidrBlock = a} :: LocalGatewayRoute)

-- | The route type.
localGatewayRoute_type :: Lens.Lens' LocalGatewayRoute (Core.Maybe LocalGatewayRouteType)
localGatewayRoute_type = Lens.lens (\LocalGatewayRoute' {type'} -> type') (\s@LocalGatewayRoute' {} a -> s {type' = a} :: LocalGatewayRoute)

instance Core.FromXML LocalGatewayRoute where
  parseXML x =
    LocalGatewayRoute'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "localGatewayVirtualInterfaceGroupId")
      Core.<*> (x Core..@? "localGatewayRouteTableArn")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "localGatewayRouteTableId")
      Core.<*> (x Core..@? "destinationCidrBlock")
      Core.<*> (x Core..@? "type")

instance Core.Hashable LocalGatewayRoute

instance Core.NFData LocalGatewayRoute
