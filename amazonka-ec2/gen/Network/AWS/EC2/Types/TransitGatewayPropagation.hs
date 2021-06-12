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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import qualified Network.AWS.Lens as Lens

-- | Describes route propagation.
--
-- /See:/ 'newTransitGatewayPropagation' smart constructor.
data TransitGatewayPropagation = TransitGatewayPropagation'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Core.Text,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Core.Maybe TransitGatewayAttachmentResourceType,
    -- | The state.
    state :: Core.Maybe TransitGatewayPropagationState,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      resourceType = Core.Nothing,
      state = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      transitGatewayRouteTableId = Core.Nothing
    }

-- | The ID of the resource.
transitGatewayPropagation_resourceId :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Core.Text)
transitGatewayPropagation_resourceId = Lens.lens (\TransitGatewayPropagation' {resourceId} -> resourceId) (\s@TransitGatewayPropagation' {} a -> s {resourceId = a} :: TransitGatewayPropagation)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayPropagation_resourceType :: Lens.Lens' TransitGatewayPropagation (Core.Maybe TransitGatewayAttachmentResourceType)
transitGatewayPropagation_resourceType = Lens.lens (\TransitGatewayPropagation' {resourceType} -> resourceType) (\s@TransitGatewayPropagation' {} a -> s {resourceType = a} :: TransitGatewayPropagation)

-- | The state.
transitGatewayPropagation_state :: Lens.Lens' TransitGatewayPropagation (Core.Maybe TransitGatewayPropagationState)
transitGatewayPropagation_state = Lens.lens (\TransitGatewayPropagation' {state} -> state) (\s@TransitGatewayPropagation' {} a -> s {state = a} :: TransitGatewayPropagation)

-- | The ID of the attachment.
transitGatewayPropagation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Core.Text)
transitGatewayPropagation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayPropagation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayPropagation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPropagation)

-- | The ID of the transit gateway route table.
transitGatewayPropagation_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayPropagation (Core.Maybe Core.Text)
transitGatewayPropagation_transitGatewayRouteTableId = Lens.lens (\TransitGatewayPropagation' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayPropagation' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayPropagation)

instance Core.FromXML TransitGatewayPropagation where
  parseXML x =
    TransitGatewayPropagation'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
      Core.<*> (x Core..@? "transitGatewayRouteTableId")

instance Core.Hashable TransitGatewayPropagation

instance Core.NFData TransitGatewayPropagation
