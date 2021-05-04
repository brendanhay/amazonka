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
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an attachment between a resource and a transit gateway.
--
-- /See:/ 'newTransitGatewayAttachment' smart constructor.
data TransitGatewayAttachment = TransitGatewayAttachment'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The association.
    association :: Prelude.Maybe TransitGatewayAttachmentAssociation,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The attachment state. Note that the @initiating@ state has been
    -- deprecated.
    state :: Prelude.Maybe TransitGatewayAttachmentState,
    -- | The tags for the attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the AWS account that owns the resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS account that owns the transit gateway.
    transitGatewayOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      association = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      transitGatewayOwnerId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayAttachment_resourceId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_resourceId = Lens.lens (\TransitGatewayAttachment' {resourceId} -> resourceId) (\s@TransitGatewayAttachment' {} a -> s {resourceId = a} :: TransitGatewayAttachment)

-- | The creation time.
transitGatewayAttachment_creationTime :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.UTCTime)
transitGatewayAttachment_creationTime = Lens.lens (\TransitGatewayAttachment' {creationTime} -> creationTime) (\s@TransitGatewayAttachment' {} a -> s {creationTime = a} :: TransitGatewayAttachment) Prelude.. Lens.mapping Prelude._Time

-- | The association.
transitGatewayAttachment_association :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe TransitGatewayAttachmentAssociation)
transitGatewayAttachment_association = Lens.lens (\TransitGatewayAttachment' {association} -> association) (\s@TransitGatewayAttachment' {} a -> s {association = a} :: TransitGatewayAttachment)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayAttachment_resourceType :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayAttachment_resourceType = Lens.lens (\TransitGatewayAttachment' {resourceType} -> resourceType) (\s@TransitGatewayAttachment' {} a -> s {resourceType = a} :: TransitGatewayAttachment)

-- | The attachment state. Note that the @initiating@ state has been
-- deprecated.
transitGatewayAttachment_state :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe TransitGatewayAttachmentState)
transitGatewayAttachment_state = Lens.lens (\TransitGatewayAttachment' {state} -> state) (\s@TransitGatewayAttachment' {} a -> s {state = a} :: TransitGatewayAttachment)

-- | The tags for the attachment.
transitGatewayAttachment_tags :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe [Tag])
transitGatewayAttachment_tags = Lens.lens (\TransitGatewayAttachment' {tags} -> tags) (\s@TransitGatewayAttachment' {} a -> s {tags = a} :: TransitGatewayAttachment) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the AWS account that owns the resource.
transitGatewayAttachment_resourceOwnerId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_resourceOwnerId = Lens.lens (\TransitGatewayAttachment' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayAttachment' {} a -> s {resourceOwnerId = a} :: TransitGatewayAttachment)

-- | The ID of the attachment.
transitGatewayAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayAttachment)

-- | The ID of the AWS account that owns the transit gateway.
transitGatewayAttachment_transitGatewayOwnerId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_transitGatewayOwnerId = Lens.lens (\TransitGatewayAttachment' {transitGatewayOwnerId} -> transitGatewayOwnerId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayOwnerId = a} :: TransitGatewayAttachment)

-- | The ID of the transit gateway.
transitGatewayAttachment_transitGatewayId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_transitGatewayId = Lens.lens (\TransitGatewayAttachment' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayId = a} :: TransitGatewayAttachment)

instance Prelude.FromXML TransitGatewayAttachment where
  parseXML x =
    TransitGatewayAttachment'
      Prelude.<$> (x Prelude..@? "resourceId")
      Prelude.<*> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "association")
      Prelude.<*> (x Prelude..@? "resourceType")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "resourceOwnerId")
      Prelude.<*> (x Prelude..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Prelude..@? "transitGatewayOwnerId")
      Prelude.<*> (x Prelude..@? "transitGatewayId")

instance Prelude.Hashable TransitGatewayAttachment

instance Prelude.NFData TransitGatewayAttachment
