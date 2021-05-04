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
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAssociationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an association.
--
-- /See:/ 'newTransitGatewayAttachmentAssociation' smart constructor.
data TransitGatewayAttachmentAssociation = TransitGatewayAttachmentAssociation'
  { -- | The state of the association.
    state :: Prelude.Maybe TransitGatewayAssociationState,
    -- | The ID of the route table for the transit gateway.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      transitGatewayRouteTableId =
        Prelude.Nothing
    }

-- | The state of the association.
transitGatewayAttachmentAssociation_state :: Lens.Lens' TransitGatewayAttachmentAssociation (Prelude.Maybe TransitGatewayAssociationState)
transitGatewayAttachmentAssociation_state = Lens.lens (\TransitGatewayAttachmentAssociation' {state} -> state) (\s@TransitGatewayAttachmentAssociation' {} a -> s {state = a} :: TransitGatewayAttachmentAssociation)

-- | The ID of the route table for the transit gateway.
transitGatewayAttachmentAssociation_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayAttachmentAssociation (Prelude.Maybe Prelude.Text)
transitGatewayAttachmentAssociation_transitGatewayRouteTableId = Lens.lens (\TransitGatewayAttachmentAssociation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayAttachmentAssociation' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayAttachmentAssociation)

instance
  Prelude.FromXML
    TransitGatewayAttachmentAssociation
  where
  parseXML x =
    TransitGatewayAttachmentAssociation'
      Prelude.<$> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "transitGatewayRouteTableId")

instance
  Prelude.Hashable
    TransitGatewayAttachmentAssociation

instance
  Prelude.NFData
    TransitGatewayAttachmentAssociation
