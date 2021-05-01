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
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a propagation route table.
--
-- /See:/ 'newTransitGatewayAttachmentPropagation' smart constructor.
data TransitGatewayAttachmentPropagation = TransitGatewayAttachmentPropagation'
  { -- | The state of the propagation route table.
    state :: Prelude.Maybe TransitGatewayPropagationState,
    -- | The ID of the propagation route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayAttachmentPropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'transitGatewayAttachmentPropagation_state' - The state of the propagation route table.
--
-- 'transitGatewayRouteTableId', 'transitGatewayAttachmentPropagation_transitGatewayRouteTableId' - The ID of the propagation route table.
newTransitGatewayAttachmentPropagation ::
  TransitGatewayAttachmentPropagation
newTransitGatewayAttachmentPropagation =
  TransitGatewayAttachmentPropagation'
    { state =
        Prelude.Nothing,
      transitGatewayRouteTableId =
        Prelude.Nothing
    }

-- | The state of the propagation route table.
transitGatewayAttachmentPropagation_state :: Lens.Lens' TransitGatewayAttachmentPropagation (Prelude.Maybe TransitGatewayPropagationState)
transitGatewayAttachmentPropagation_state = Lens.lens (\TransitGatewayAttachmentPropagation' {state} -> state) (\s@TransitGatewayAttachmentPropagation' {} a -> s {state = a} :: TransitGatewayAttachmentPropagation)

-- | The ID of the propagation route table.
transitGatewayAttachmentPropagation_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayAttachmentPropagation (Prelude.Maybe Prelude.Text)
transitGatewayAttachmentPropagation_transitGatewayRouteTableId = Lens.lens (\TransitGatewayAttachmentPropagation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayAttachmentPropagation' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayAttachmentPropagation)

instance
  Prelude.FromXML
    TransitGatewayAttachmentPropagation
  where
  parseXML x =
    TransitGatewayAttachmentPropagation'
      Prelude.<$> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "transitGatewayRouteTableId")

instance
  Prelude.Hashable
    TransitGatewayAttachmentPropagation

instance
  Prelude.NFData
    TransitGatewayAttachmentPropagation
