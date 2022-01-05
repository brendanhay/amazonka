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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGatewayRoute where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LocalGatewayRouteState
import Amazonka.EC2.Types.LocalGatewayRouteType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a route for a local gateway route table.
--
-- /See:/ 'newLocalGatewayRoute' smart constructor.
data LocalGatewayRoute = LocalGatewayRoute'
  { -- | The state of the route.
    state :: Prelude.Maybe LocalGatewayRouteState,
    -- | The Amazon Resource Name (ARN) of the local gateway route table.
    localGatewayRouteTableArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the local gateway
    -- route.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The route type.
    type' :: Prelude.Maybe LocalGatewayRouteType,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR block used for destination matches.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text
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
-- 'state', 'localGatewayRoute_state' - The state of the route.
--
-- 'localGatewayRouteTableArn', 'localGatewayRoute_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table.
--
-- 'ownerId', 'localGatewayRoute_ownerId' - The ID of the Amazon Web Services account that owns the local gateway
-- route.
--
-- 'localGatewayRouteTableId', 'localGatewayRoute_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'type'', 'localGatewayRoute_type' - The route type.
--
-- 'localGatewayVirtualInterfaceGroupId', 'localGatewayRoute_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'destinationCidrBlock', 'localGatewayRoute_destinationCidrBlock' - The CIDR block used for destination matches.
newLocalGatewayRoute ::
  LocalGatewayRoute
newLocalGatewayRoute =
  LocalGatewayRoute'
    { state = Prelude.Nothing,
      localGatewayRouteTableArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      localGatewayRouteTableId = Prelude.Nothing,
      type' = Prelude.Nothing,
      localGatewayVirtualInterfaceGroupId =
        Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing
    }

-- | The state of the route.
localGatewayRoute_state :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe LocalGatewayRouteState)
localGatewayRoute_state = Lens.lens (\LocalGatewayRoute' {state} -> state) (\s@LocalGatewayRoute' {} a -> s {state = a} :: LocalGatewayRoute)

-- | The Amazon Resource Name (ARN) of the local gateway route table.
localGatewayRoute_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRoute' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRoute' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRoute)

-- | The ID of the Amazon Web Services account that owns the local gateway
-- route.
localGatewayRoute_ownerId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_ownerId = Lens.lens (\LocalGatewayRoute' {ownerId} -> ownerId) (\s@LocalGatewayRoute' {} a -> s {ownerId = a} :: LocalGatewayRoute)

-- | The ID of the local gateway route table.
localGatewayRoute_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_localGatewayRouteTableId = Lens.lens (\LocalGatewayRoute' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRoute' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRoute)

-- | The route type.
localGatewayRoute_type :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe LocalGatewayRouteType)
localGatewayRoute_type = Lens.lens (\LocalGatewayRoute' {type'} -> type') (\s@LocalGatewayRoute' {} a -> s {type' = a} :: LocalGatewayRoute)

-- | The ID of the virtual interface group.
localGatewayRoute_localGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_localGatewayVirtualInterfaceGroupId = Lens.lens (\LocalGatewayRoute' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@LocalGatewayRoute' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRoute)

-- | The CIDR block used for destination matches.
localGatewayRoute_destinationCidrBlock :: Lens.Lens' LocalGatewayRoute (Prelude.Maybe Prelude.Text)
localGatewayRoute_destinationCidrBlock = Lens.lens (\LocalGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@LocalGatewayRoute' {} a -> s {destinationCidrBlock = a} :: LocalGatewayRoute)

instance Core.FromXML LocalGatewayRoute where
  parseXML x =
    LocalGatewayRoute'
      Prelude.<$> (x Core..@? "state")
      Prelude.<*> (x Core..@? "localGatewayRouteTableArn")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "localGatewayRouteTableId")
      Prelude.<*> (x Core..@? "type")
      Prelude.<*> (x Core..@? "localGatewayVirtualInterfaceGroupId")
      Prelude.<*> (x Core..@? "destinationCidrBlock")

instance Prelude.Hashable LocalGatewayRoute where
  hashWithSalt _salt LocalGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` localGatewayRouteTableArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` localGatewayRouteTableId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupId
      `Prelude.hashWithSalt` destinationCidrBlock

instance Prelude.NFData LocalGatewayRoute where
  rnf LocalGatewayRoute' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf localGatewayRouteTableArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupId
      `Prelude.seq` Prelude.rnf destinationCidrBlock
