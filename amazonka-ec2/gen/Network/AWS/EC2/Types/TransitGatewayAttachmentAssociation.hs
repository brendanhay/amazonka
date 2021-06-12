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
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAssociationState
import qualified Network.AWS.Lens as Lens

-- | Describes an association.
--
-- /See:/ 'newTransitGatewayAttachmentAssociation' smart constructor.
data TransitGatewayAttachmentAssociation = TransitGatewayAttachmentAssociation'
  { -- | The state of the association.
    state :: Core.Maybe TransitGatewayAssociationState,
    -- | The ID of the route table for the transit gateway.
    transitGatewayRouteTableId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayAttachmentAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'transitGatewayAttachmentAssociation_state' - The state of the association.
--
-- 'transitGatewayRouteTableId', 'transitGatewayAttachmentAssociation_transitGatewayRouteTableId' - The ID of the route table for the transit gateway.
newTransitGatewayAttachmentAssociation ::
  TransitGatewayAttachmentAssociation
newTransitGatewayAttachmentAssociation =
  TransitGatewayAttachmentAssociation'
    { state =
        Core.Nothing,
      transitGatewayRouteTableId =
        Core.Nothing
    }

-- | The state of the association.
transitGatewayAttachmentAssociation_state :: Lens.Lens' TransitGatewayAttachmentAssociation (Core.Maybe TransitGatewayAssociationState)
transitGatewayAttachmentAssociation_state = Lens.lens (\TransitGatewayAttachmentAssociation' {state} -> state) (\s@TransitGatewayAttachmentAssociation' {} a -> s {state = a} :: TransitGatewayAttachmentAssociation)

-- | The ID of the route table for the transit gateway.
transitGatewayAttachmentAssociation_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayAttachmentAssociation (Core.Maybe Core.Text)
transitGatewayAttachmentAssociation_transitGatewayRouteTableId = Lens.lens (\TransitGatewayAttachmentAssociation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayAttachmentAssociation' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayAttachmentAssociation)

instance
  Core.FromXML
    TransitGatewayAttachmentAssociation
  where
  parseXML x =
    TransitGatewayAttachmentAssociation'
      Core.<$> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayRouteTableId")

instance
  Core.Hashable
    TransitGatewayAttachmentAssociation

instance
  Core.NFData
    TransitGatewayAttachmentAssociation
