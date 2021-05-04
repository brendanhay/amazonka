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
-- Module      : Network.AWS.EC2.Types.TransitGatewayPropagation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPropagation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes route propagation.
--
-- /See:/ 'newTransitGatewayPropagation' smart constructor.
data TransitGatewayPropagation = TransitGatewayPropagation'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The state.
    state :: Prelude.Maybe TransitGatewayPropagationState,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayPropagation_resourceId' - The ID of the resource.
--
-- 'resourceType', 'transitGatewayPropagation_resourceType' - The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'state', 'transitGatewayPropagation_state' - The state.
--
-- 'transitGatewayAttachmentId', 'transitGatewayPropagation_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'transitGatewayRouteTableId', 'transitGatewayPropagation_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newTransitGatewayPropagation ::
  TransitGatewayPropagation
newTransitGatewayPropagation =
  TransitGatewayPropagation'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      transitGatewayRouteTableId = Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayPropagation_resourceId :: Lens.Lens' TransitGatewayPropagation (Prelude.Maybe Prelude.Text)
transitGatewayPropagation_resourceId = Lens.lens (\TransitGatewayPropagation' {resourceId} -> resourceId) (\s@TransitGatewayPropagation' {} a -> s {resourceId = a} :: TransitGatewayPropagation)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayPropagation_resourceType :: Lens.Lens' TransitGatewayPropagation (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayPropagation_resourceType = Lens.lens (\TransitGatewayPropagation' {resourceType} -> resourceType) (\s@TransitGatewayPropagation' {} a -> s {resourceType = a} :: TransitGatewayPropagation)

-- | The state.
transitGatewayPropagation_state :: Lens.Lens' TransitGatewayPropagation (Prelude.Maybe TransitGatewayPropagationState)
transitGatewayPropagation_state = Lens.lens (\TransitGatewayPropagation' {state} -> state) (\s@TransitGatewayPropagation' {} a -> s {state = a} :: TransitGatewayPropagation)

-- | The ID of the attachment.
transitGatewayPropagation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayPropagation (Prelude.Maybe Prelude.Text)
transitGatewayPropagation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayPropagation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayPropagation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPropagation)

-- | The ID of the transit gateway route table.
transitGatewayPropagation_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayPropagation (Prelude.Maybe Prelude.Text)
transitGatewayPropagation_transitGatewayRouteTableId = Lens.lens (\TransitGatewayPropagation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayPropagation' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayPropagation)

instance Prelude.FromXML TransitGatewayPropagation where
  parseXML x =
    TransitGatewayPropagation'
      Prelude.<$> (x Prelude..@? "resourceId")
      Prelude.<*> (x Prelude..@? "resourceType")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Prelude..@? "transitGatewayRouteTableId")

instance Prelude.Hashable TransitGatewayPropagation

instance Prelude.NFData TransitGatewayPropagation
