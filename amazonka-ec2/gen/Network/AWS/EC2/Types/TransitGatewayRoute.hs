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
-- Module      : Network.AWS.EC2.Types.TransitGatewayRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRoute where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayRouteAttachment
import Network.AWS.EC2.Types.TransitGatewayRouteState
import Network.AWS.EC2.Types.TransitGatewayRouteType
import qualified Network.AWS.Lens as Lens

-- | Describes a route for a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRoute' smart constructor.
data TransitGatewayRoute = TransitGatewayRoute'
  { -- | The ID of the prefix list used for destination matches.
    prefixListId :: Core.Maybe Core.Text,
    -- | The state of the route.
    state :: Core.Maybe TransitGatewayRouteState,
    -- | The CIDR block used for destination matches.
    destinationCidrBlock :: Core.Maybe Core.Text,
    -- | The route type.
    type' :: Core.Maybe TransitGatewayRouteType,
    -- | The attachments.
    transitGatewayAttachments :: Core.Maybe [TransitGatewayRouteAttachment]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixListId', 'transitGatewayRoute_prefixListId' - The ID of the prefix list used for destination matches.
--
-- 'state', 'transitGatewayRoute_state' - The state of the route.
--
-- 'destinationCidrBlock', 'transitGatewayRoute_destinationCidrBlock' - The CIDR block used for destination matches.
--
-- 'type'', 'transitGatewayRoute_type' - The route type.
--
-- 'transitGatewayAttachments', 'transitGatewayRoute_transitGatewayAttachments' - The attachments.
newTransitGatewayRoute ::
  TransitGatewayRoute
newTransitGatewayRoute =
  TransitGatewayRoute'
    { prefixListId = Core.Nothing,
      state = Core.Nothing,
      destinationCidrBlock = Core.Nothing,
      type' = Core.Nothing,
      transitGatewayAttachments = Core.Nothing
    }

-- | The ID of the prefix list used for destination matches.
transitGatewayRoute_prefixListId :: Lens.Lens' TransitGatewayRoute (Core.Maybe Core.Text)
transitGatewayRoute_prefixListId = Lens.lens (\TransitGatewayRoute' {prefixListId} -> prefixListId) (\s@TransitGatewayRoute' {} a -> s {prefixListId = a} :: TransitGatewayRoute)

-- | The state of the route.
transitGatewayRoute_state :: Lens.Lens' TransitGatewayRoute (Core.Maybe TransitGatewayRouteState)
transitGatewayRoute_state = Lens.lens (\TransitGatewayRoute' {state} -> state) (\s@TransitGatewayRoute' {} a -> s {state = a} :: TransitGatewayRoute)

-- | The CIDR block used for destination matches.
transitGatewayRoute_destinationCidrBlock :: Lens.Lens' TransitGatewayRoute (Core.Maybe Core.Text)
transitGatewayRoute_destinationCidrBlock = Lens.lens (\TransitGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@TransitGatewayRoute' {} a -> s {destinationCidrBlock = a} :: TransitGatewayRoute)

-- | The route type.
transitGatewayRoute_type :: Lens.Lens' TransitGatewayRoute (Core.Maybe TransitGatewayRouteType)
transitGatewayRoute_type = Lens.lens (\TransitGatewayRoute' {type'} -> type') (\s@TransitGatewayRoute' {} a -> s {type' = a} :: TransitGatewayRoute)

-- | The attachments.
transitGatewayRoute_transitGatewayAttachments :: Lens.Lens' TransitGatewayRoute (Core.Maybe [TransitGatewayRouteAttachment])
transitGatewayRoute_transitGatewayAttachments = Lens.lens (\TransitGatewayRoute' {transitGatewayAttachments} -> transitGatewayAttachments) (\s@TransitGatewayRoute' {} a -> s {transitGatewayAttachments = a} :: TransitGatewayRoute) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML TransitGatewayRoute where
  parseXML x =
    TransitGatewayRoute'
      Core.<$> (x Core..@? "prefixListId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "destinationCidrBlock")
      Core.<*> (x Core..@? "type")
      Core.<*> ( x Core..@? "transitGatewayAttachments"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable TransitGatewayRoute

instance Core.NFData TransitGatewayRoute
