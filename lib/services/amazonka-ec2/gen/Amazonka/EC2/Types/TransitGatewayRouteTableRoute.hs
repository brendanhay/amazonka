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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteTableRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteTableRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a route in a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRouteTableRoute' smart constructor.
data TransitGatewayRouteTableRoute = TransitGatewayRouteTableRoute'
  { -- | The ID of the route attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The CIDR block used for destination matches.
    destinationCidr :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource for the route attachment.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type for the route attachment.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The route origin. The following are the possible values:
    --
    -- -   static
    --
    -- -   propagated
    routeOrigin :: Prelude.Maybe Prelude.Text,
    -- | The state of the route.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRouteTableRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'transitGatewayRouteTableRoute_attachmentId' - The ID of the route attachment.
--
-- 'destinationCidr', 'transitGatewayRouteTableRoute_destinationCidr' - The CIDR block used for destination matches.
--
-- 'prefixListId', 'transitGatewayRouteTableRoute_prefixListId' - The ID of the prefix list.
--
-- 'resourceId', 'transitGatewayRouteTableRoute_resourceId' - The ID of the resource for the route attachment.
--
-- 'resourceType', 'transitGatewayRouteTableRoute_resourceType' - The resource type for the route attachment.
--
-- 'routeOrigin', 'transitGatewayRouteTableRoute_routeOrigin' - The route origin. The following are the possible values:
--
-- -   static
--
-- -   propagated
--
-- 'state', 'transitGatewayRouteTableRoute_state' - The state of the route.
newTransitGatewayRouteTableRoute ::
  TransitGatewayRouteTableRoute
newTransitGatewayRouteTableRoute =
  TransitGatewayRouteTableRoute'
    { attachmentId =
        Prelude.Nothing,
      destinationCidr = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      routeOrigin = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The ID of the route attachment.
transitGatewayRouteTableRoute_attachmentId :: Lens.Lens' TransitGatewayRouteTableRoute (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableRoute_attachmentId = Lens.lens (\TransitGatewayRouteTableRoute' {attachmentId} -> attachmentId) (\s@TransitGatewayRouteTableRoute' {} a -> s {attachmentId = a} :: TransitGatewayRouteTableRoute)

-- | The CIDR block used for destination matches.
transitGatewayRouteTableRoute_destinationCidr :: Lens.Lens' TransitGatewayRouteTableRoute (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableRoute_destinationCidr = Lens.lens (\TransitGatewayRouteTableRoute' {destinationCidr} -> destinationCidr) (\s@TransitGatewayRouteTableRoute' {} a -> s {destinationCidr = a} :: TransitGatewayRouteTableRoute)

-- | The ID of the prefix list.
transitGatewayRouteTableRoute_prefixListId :: Lens.Lens' TransitGatewayRouteTableRoute (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableRoute_prefixListId = Lens.lens (\TransitGatewayRouteTableRoute' {prefixListId} -> prefixListId) (\s@TransitGatewayRouteTableRoute' {} a -> s {prefixListId = a} :: TransitGatewayRouteTableRoute)

-- | The ID of the resource for the route attachment.
transitGatewayRouteTableRoute_resourceId :: Lens.Lens' TransitGatewayRouteTableRoute (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableRoute_resourceId = Lens.lens (\TransitGatewayRouteTableRoute' {resourceId} -> resourceId) (\s@TransitGatewayRouteTableRoute' {} a -> s {resourceId = a} :: TransitGatewayRouteTableRoute)

-- | The resource type for the route attachment.
transitGatewayRouteTableRoute_resourceType :: Lens.Lens' TransitGatewayRouteTableRoute (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableRoute_resourceType = Lens.lens (\TransitGatewayRouteTableRoute' {resourceType} -> resourceType) (\s@TransitGatewayRouteTableRoute' {} a -> s {resourceType = a} :: TransitGatewayRouteTableRoute)

-- | The route origin. The following are the possible values:
--
-- -   static
--
-- -   propagated
transitGatewayRouteTableRoute_routeOrigin :: Lens.Lens' TransitGatewayRouteTableRoute (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableRoute_routeOrigin = Lens.lens (\TransitGatewayRouteTableRoute' {routeOrigin} -> routeOrigin) (\s@TransitGatewayRouteTableRoute' {} a -> s {routeOrigin = a} :: TransitGatewayRouteTableRoute)

-- | The state of the route.
transitGatewayRouteTableRoute_state :: Lens.Lens' TransitGatewayRouteTableRoute (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableRoute_state = Lens.lens (\TransitGatewayRouteTableRoute' {state} -> state) (\s@TransitGatewayRouteTableRoute' {} a -> s {state = a} :: TransitGatewayRouteTableRoute)

instance Data.FromXML TransitGatewayRouteTableRoute where
  parseXML x =
    TransitGatewayRouteTableRoute'
      Prelude.<$> (x Data..@? "attachmentId")
      Prelude.<*> (x Data..@? "destinationCidr")
      Prelude.<*> (x Data..@? "prefixListId")
      Prelude.<*> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "routeOrigin")
      Prelude.<*> (x Data..@? "state")

instance
  Prelude.Hashable
    TransitGatewayRouteTableRoute
  where
  hashWithSalt _salt TransitGatewayRouteTableRoute' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` destinationCidr
      `Prelude.hashWithSalt` prefixListId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` routeOrigin
      `Prelude.hashWithSalt` state

instance Prelude.NFData TransitGatewayRouteTableRoute where
  rnf TransitGatewayRouteTableRoute' {..} =
    Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf destinationCidr
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf routeOrigin
      `Prelude.seq` Prelude.rnf state
