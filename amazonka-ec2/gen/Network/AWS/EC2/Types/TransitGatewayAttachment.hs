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
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import qualified Network.AWS.Lens as Lens

-- | Describes an attachment between a resource and a transit gateway.
--
-- /See:/ 'newTransitGatewayAttachment' smart constructor.
data TransitGatewayAttachment = TransitGatewayAttachment'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Core.Text,
    -- | The creation time.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | The association.
    association :: Core.Maybe TransitGatewayAttachmentAssociation,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Core.Maybe TransitGatewayAttachmentResourceType,
    -- | The attachment state. Note that the @initiating@ state has been
    -- deprecated.
    state :: Core.Maybe TransitGatewayAttachmentState,
    -- | The tags for the attachment.
    tags :: Core.Maybe [Tag],
    -- | The ID of the AWS account that owns the resource.
    resourceOwnerId :: Core.Maybe Core.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text,
    -- | The ID of the AWS account that owns the transit gateway.
    transitGatewayOwnerId :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayAttachment_resourceId' - The ID of the resource.
--
-- 'creationTime', 'transitGatewayAttachment_creationTime' - The creation time.
--
-- 'association', 'transitGatewayAttachment_association' - The association.
--
-- 'resourceType', 'transitGatewayAttachment_resourceType' - The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'state', 'transitGatewayAttachment_state' - The attachment state. Note that the @initiating@ state has been
-- deprecated.
--
-- 'tags', 'transitGatewayAttachment_tags' - The tags for the attachment.
--
-- 'resourceOwnerId', 'transitGatewayAttachment_resourceOwnerId' - The ID of the AWS account that owns the resource.
--
-- 'transitGatewayAttachmentId', 'transitGatewayAttachment_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'transitGatewayOwnerId', 'transitGatewayAttachment_transitGatewayOwnerId' - The ID of the AWS account that owns the transit gateway.
--
-- 'transitGatewayId', 'transitGatewayAttachment_transitGatewayId' - The ID of the transit gateway.
newTransitGatewayAttachment ::
  TransitGatewayAttachment
newTransitGatewayAttachment =
  TransitGatewayAttachment'
    { resourceId =
        Core.Nothing,
      creationTime = Core.Nothing,
      association = Core.Nothing,
      resourceType = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      resourceOwnerId = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      transitGatewayOwnerId = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The ID of the resource.
transitGatewayAttachment_resourceId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
transitGatewayAttachment_resourceId = Lens.lens (\TransitGatewayAttachment' {resourceId} -> resourceId) (\s@TransitGatewayAttachment' {} a -> s {resourceId = a} :: TransitGatewayAttachment)

-- | The creation time.
transitGatewayAttachment_creationTime :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.UTCTime)
transitGatewayAttachment_creationTime = Lens.lens (\TransitGatewayAttachment' {creationTime} -> creationTime) (\s@TransitGatewayAttachment' {} a -> s {creationTime = a} :: TransitGatewayAttachment) Core.. Lens.mapping Core._Time

-- | The association.
transitGatewayAttachment_association :: Lens.Lens' TransitGatewayAttachment (Core.Maybe TransitGatewayAttachmentAssociation)
transitGatewayAttachment_association = Lens.lens (\TransitGatewayAttachment' {association} -> association) (\s@TransitGatewayAttachment' {} a -> s {association = a} :: TransitGatewayAttachment)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayAttachment_resourceType :: Lens.Lens' TransitGatewayAttachment (Core.Maybe TransitGatewayAttachmentResourceType)
transitGatewayAttachment_resourceType = Lens.lens (\TransitGatewayAttachment' {resourceType} -> resourceType) (\s@TransitGatewayAttachment' {} a -> s {resourceType = a} :: TransitGatewayAttachment)

-- | The attachment state. Note that the @initiating@ state has been
-- deprecated.
transitGatewayAttachment_state :: Lens.Lens' TransitGatewayAttachment (Core.Maybe TransitGatewayAttachmentState)
transitGatewayAttachment_state = Lens.lens (\TransitGatewayAttachment' {state} -> state) (\s@TransitGatewayAttachment' {} a -> s {state = a} :: TransitGatewayAttachment)

-- | The tags for the attachment.
transitGatewayAttachment_tags :: Lens.Lens' TransitGatewayAttachment (Core.Maybe [Tag])
transitGatewayAttachment_tags = Lens.lens (\TransitGatewayAttachment' {tags} -> tags) (\s@TransitGatewayAttachment' {} a -> s {tags = a} :: TransitGatewayAttachment) Core.. Lens.mapping Lens._Coerce

-- | The ID of the AWS account that owns the resource.
transitGatewayAttachment_resourceOwnerId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
transitGatewayAttachment_resourceOwnerId = Lens.lens (\TransitGatewayAttachment' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayAttachment' {} a -> s {resourceOwnerId = a} :: TransitGatewayAttachment)

-- | The ID of the attachment.
transitGatewayAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
transitGatewayAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayAttachment)

-- | The ID of the AWS account that owns the transit gateway.
transitGatewayAttachment_transitGatewayOwnerId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
transitGatewayAttachment_transitGatewayOwnerId = Lens.lens (\TransitGatewayAttachment' {transitGatewayOwnerId} -> transitGatewayOwnerId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayOwnerId = a} :: TransitGatewayAttachment)

-- | The ID of the transit gateway.
transitGatewayAttachment_transitGatewayId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
transitGatewayAttachment_transitGatewayId = Lens.lens (\TransitGatewayAttachment' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayId = a} :: TransitGatewayAttachment)

instance Core.FromXML TransitGatewayAttachment where
  parseXML x =
    TransitGatewayAttachment'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "association")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "resourceOwnerId")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
      Core.<*> (x Core..@? "transitGatewayOwnerId")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable TransitGatewayAttachment

instance Core.NFData TransitGatewayAttachment
