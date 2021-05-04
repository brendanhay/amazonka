{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayRouteAttachment
import Network.AWS.EC2.Types.TransitGatewayRouteState
import Network.AWS.EC2.Types.TransitGatewayRouteType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a route for a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRoute' smart constructor.
data TransitGatewayRoute = TransitGatewayRoute'
  { -- | The ID of the prefix list used for destination matches.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The state of the route.
    state :: Prelude.Maybe TransitGatewayRouteState,
    -- | The CIDR block used for destination matches.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The route type.
    type' :: Prelude.Maybe TransitGatewayRouteType,
    -- | The attachments.
    transitGatewayAttachments :: Prelude.Maybe [TransitGatewayRouteAttachment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { prefixListId =
        Prelude.Nothing,
      state = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      type' = Prelude.Nothing,
      transitGatewayAttachments = Prelude.Nothing
    }

-- | The ID of the prefix list used for destination matches.
transitGatewayRoute_prefixListId :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe Prelude.Text)
transitGatewayRoute_prefixListId = Lens.lens (\TransitGatewayRoute' {prefixListId} -> prefixListId) (\s@TransitGatewayRoute' {} a -> s {prefixListId = a} :: TransitGatewayRoute)

-- | The state of the route.
transitGatewayRoute_state :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe TransitGatewayRouteState)
transitGatewayRoute_state = Lens.lens (\TransitGatewayRoute' {state} -> state) (\s@TransitGatewayRoute' {} a -> s {state = a} :: TransitGatewayRoute)

-- | The CIDR block used for destination matches.
transitGatewayRoute_destinationCidrBlock :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe Prelude.Text)
transitGatewayRoute_destinationCidrBlock = Lens.lens (\TransitGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@TransitGatewayRoute' {} a -> s {destinationCidrBlock = a} :: TransitGatewayRoute)

-- | The route type.
transitGatewayRoute_type :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe TransitGatewayRouteType)
transitGatewayRoute_type = Lens.lens (\TransitGatewayRoute' {type'} -> type') (\s@TransitGatewayRoute' {} a -> s {type' = a} :: TransitGatewayRoute)

-- | The attachments.
transitGatewayRoute_transitGatewayAttachments :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe [TransitGatewayRouteAttachment])
transitGatewayRoute_transitGatewayAttachments = Lens.lens (\TransitGatewayRoute' {transitGatewayAttachments} -> transitGatewayAttachments) (\s@TransitGatewayRoute' {} a -> s {transitGatewayAttachments = a} :: TransitGatewayRoute) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML TransitGatewayRoute where
  parseXML x =
    TransitGatewayRoute'
      Prelude.<$> (x Prelude..@? "prefixListId")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "destinationCidrBlock")
      Prelude.<*> (x Prelude..@? "type")
      Prelude.<*> ( x Prelude..@? "transitGatewayAttachments"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable TransitGatewayRoute

instance Prelude.NFData TransitGatewayRoute
