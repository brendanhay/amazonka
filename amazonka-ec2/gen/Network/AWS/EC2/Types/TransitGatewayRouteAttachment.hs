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
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayRouteAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens

-- | Describes a route attachment.
--
-- /See:/ 'newTransitGatewayRouteAttachment' smart constructor.
data TransitGatewayRouteAttachment = TransitGatewayRouteAttachment'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Core.Text,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Core.Maybe TransitGatewayAttachmentResourceType,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayRouteAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayRouteAttachment_resourceId' - The ID of the resource.
--
-- 'resourceType', 'transitGatewayRouteAttachment_resourceType' - The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'transitGatewayAttachmentId', 'transitGatewayRouteAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newTransitGatewayRouteAttachment ::
  TransitGatewayRouteAttachment
newTransitGatewayRouteAttachment =
  TransitGatewayRouteAttachment'
    { resourceId =
        Core.Nothing,
      resourceType = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing
    }

-- | The ID of the resource.
transitGatewayRouteAttachment_resourceId :: Lens.Lens' TransitGatewayRouteAttachment (Core.Maybe Core.Text)
transitGatewayRouteAttachment_resourceId = Lens.lens (\TransitGatewayRouteAttachment' {resourceId} -> resourceId) (\s@TransitGatewayRouteAttachment' {} a -> s {resourceId = a} :: TransitGatewayRouteAttachment)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayRouteAttachment_resourceType :: Lens.Lens' TransitGatewayRouteAttachment (Core.Maybe TransitGatewayAttachmentResourceType)
transitGatewayRouteAttachment_resourceType = Lens.lens (\TransitGatewayRouteAttachment' {resourceType} -> resourceType) (\s@TransitGatewayRouteAttachment' {} a -> s {resourceType = a} :: TransitGatewayRouteAttachment)

-- | The ID of the attachment.
transitGatewayRouteAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayRouteAttachment (Core.Maybe Core.Text)
transitGatewayRouteAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayRouteAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayRouteAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayRouteAttachment)

instance Core.FromXML TransitGatewayRouteAttachment where
  parseXML x =
    TransitGatewayRouteAttachment'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")

instance Core.Hashable TransitGatewayRouteAttachment

instance Core.NFData TransitGatewayRouteAttachment
