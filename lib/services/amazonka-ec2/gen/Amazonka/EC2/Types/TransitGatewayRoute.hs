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
-- Module      : Amazonka.EC2.Types.TransitGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayRouteAttachment
import Amazonka.EC2.Types.TransitGatewayRouteState
import Amazonka.EC2.Types.TransitGatewayRouteType
import qualified Amazonka.Prelude as Prelude

-- | Describes a route for a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRoute' smart constructor.
data TransitGatewayRoute = TransitGatewayRoute'
  { -- | The CIDR block used for destination matches.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix list used for destination matches.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The state of the route.
    state :: Prelude.Maybe TransitGatewayRouteState,
    -- | The attachments.
    transitGatewayAttachments :: Prelude.Maybe [TransitGatewayRouteAttachment],
    -- | The ID of the transit gateway route table announcement.
    transitGatewayRouteTableAnnouncementId :: Prelude.Maybe Prelude.Text,
    -- | The route type.
    type' :: Prelude.Maybe TransitGatewayRouteType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCidrBlock', 'transitGatewayRoute_destinationCidrBlock' - The CIDR block used for destination matches.
--
-- 'prefixListId', 'transitGatewayRoute_prefixListId' - The ID of the prefix list used for destination matches.
--
-- 'state', 'transitGatewayRoute_state' - The state of the route.
--
-- 'transitGatewayAttachments', 'transitGatewayRoute_transitGatewayAttachments' - The attachments.
--
-- 'transitGatewayRouteTableAnnouncementId', 'transitGatewayRoute_transitGatewayRouteTableAnnouncementId' - The ID of the transit gateway route table announcement.
--
-- 'type'', 'transitGatewayRoute_type' - The route type.
newTransitGatewayRoute ::
  TransitGatewayRoute
newTransitGatewayRoute =
  TransitGatewayRoute'
    { destinationCidrBlock =
        Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachments = Prelude.Nothing,
      transitGatewayRouteTableAnnouncementId =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The CIDR block used for destination matches.
transitGatewayRoute_destinationCidrBlock :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe Prelude.Text)
transitGatewayRoute_destinationCidrBlock = Lens.lens (\TransitGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@TransitGatewayRoute' {} a -> s {destinationCidrBlock = a} :: TransitGatewayRoute)

-- | The ID of the prefix list used for destination matches.
transitGatewayRoute_prefixListId :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe Prelude.Text)
transitGatewayRoute_prefixListId = Lens.lens (\TransitGatewayRoute' {prefixListId} -> prefixListId) (\s@TransitGatewayRoute' {} a -> s {prefixListId = a} :: TransitGatewayRoute)

-- | The state of the route.
transitGatewayRoute_state :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe TransitGatewayRouteState)
transitGatewayRoute_state = Lens.lens (\TransitGatewayRoute' {state} -> state) (\s@TransitGatewayRoute' {} a -> s {state = a} :: TransitGatewayRoute)

-- | The attachments.
transitGatewayRoute_transitGatewayAttachments :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe [TransitGatewayRouteAttachment])
transitGatewayRoute_transitGatewayAttachments = Lens.lens (\TransitGatewayRoute' {transitGatewayAttachments} -> transitGatewayAttachments) (\s@TransitGatewayRoute' {} a -> s {transitGatewayAttachments = a} :: TransitGatewayRoute) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway route table announcement.
transitGatewayRoute_transitGatewayRouteTableAnnouncementId :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe Prelude.Text)
transitGatewayRoute_transitGatewayRouteTableAnnouncementId = Lens.lens (\TransitGatewayRoute' {transitGatewayRouteTableAnnouncementId} -> transitGatewayRouteTableAnnouncementId) (\s@TransitGatewayRoute' {} a -> s {transitGatewayRouteTableAnnouncementId = a} :: TransitGatewayRoute)

-- | The route type.
transitGatewayRoute_type :: Lens.Lens' TransitGatewayRoute (Prelude.Maybe TransitGatewayRouteType)
transitGatewayRoute_type = Lens.lens (\TransitGatewayRoute' {type'} -> type') (\s@TransitGatewayRoute' {} a -> s {type' = a} :: TransitGatewayRoute)

instance Data.FromXML TransitGatewayRoute where
  parseXML x =
    TransitGatewayRoute'
      Prelude.<$> (x Data..@? "destinationCidrBlock")
      Prelude.<*> (x Data..@? "prefixListId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "transitGatewayAttachments"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "transitGatewayRouteTableAnnouncementId")
      Prelude.<*> (x Data..@? "type")

instance Prelude.Hashable TransitGatewayRoute where
  hashWithSalt _salt TransitGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` prefixListId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` transitGatewayAttachments
      `Prelude.hashWithSalt` transitGatewayRouteTableAnnouncementId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TransitGatewayRoute where
  rnf TransitGatewayRoute' {..} =
    Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf transitGatewayAttachments
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableAnnouncementId
      `Prelude.seq` Prelude.rnf type'
