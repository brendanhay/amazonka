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
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteTablePropagation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayPropagationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a route table propagation.
--
-- /See:/ 'newTransitGatewayRouteTablePropagation' smart constructor.
data TransitGatewayRouteTablePropagation = TransitGatewayRouteTablePropagation'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The state of the resource.
    state :: Prelude.Maybe TransitGatewayPropagationState,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRouteTablePropagation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayRouteTablePropagation_resourceId' - The ID of the resource.
--
-- 'resourceType', 'transitGatewayRouteTablePropagation_resourceType' - The type of resource. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'state', 'transitGatewayRouteTablePropagation_state' - The state of the resource.
--
-- 'transitGatewayAttachmentId', 'transitGatewayRouteTablePropagation_transitGatewayAttachmentId' - The ID of the attachment.
newTransitGatewayRouteTablePropagation ::
  TransitGatewayRouteTablePropagation
newTransitGatewayRouteTablePropagation =
  TransitGatewayRouteTablePropagation'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayRouteTablePropagation_resourceId :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Text)
transitGatewayRouteTablePropagation_resourceId = Lens.lens (\TransitGatewayRouteTablePropagation' {resourceId} -> resourceId) (\s@TransitGatewayRouteTablePropagation' {} a -> s {resourceId = a} :: TransitGatewayRouteTablePropagation)

-- | The type of resource. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayRouteTablePropagation_resourceType :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayRouteTablePropagation_resourceType = Lens.lens (\TransitGatewayRouteTablePropagation' {resourceType} -> resourceType) (\s@TransitGatewayRouteTablePropagation' {} a -> s {resourceType = a} :: TransitGatewayRouteTablePropagation)

-- | The state of the resource.
transitGatewayRouteTablePropagation_state :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe TransitGatewayPropagationState)
transitGatewayRouteTablePropagation_state = Lens.lens (\TransitGatewayRouteTablePropagation' {state} -> state) (\s@TransitGatewayRouteTablePropagation' {} a -> s {state = a} :: TransitGatewayRouteTablePropagation)

-- | The ID of the attachment.
transitGatewayRouteTablePropagation_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteTablePropagation (Prelude.Maybe Prelude.Text)
transitGatewayRouteTablePropagation_transitGatewayAttachmentId = Lens.lens (\TransitGatewayRouteTablePropagation' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayRouteTablePropagation' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayRouteTablePropagation)

instance
  Prelude.FromXML
    TransitGatewayRouteTablePropagation
  where
  parseXML x =
    TransitGatewayRouteTablePropagation'
      Prelude.<$> (x Prelude..@? "resourceId")
      Prelude.<*> (x Prelude..@? "resourceType")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "transitGatewayAttachmentId")

instance
  Prelude.Hashable
    TransitGatewayRouteTablePropagation

instance
  Prelude.NFData
    TransitGatewayRouteTablePropagation
