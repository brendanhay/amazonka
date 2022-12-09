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
-- Module      : Amazonka.EC2.Types.TransitGatewayAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayAttachmentAssociation
import Amazonka.EC2.Types.TransitGatewayAttachmentResourceType
import Amazonka.EC2.Types.TransitGatewayAttachmentState
import qualified Amazonka.Prelude as Prelude

-- | Describes an attachment between a resource and a transit gateway.
--
-- /See:/ 'newTransitGatewayAttachment' smart constructor.
data TransitGatewayAttachment = TransitGatewayAttachment'
  { -- | The association.
    association :: Prelude.Maybe TransitGatewayAttachmentAssociation,
    -- | The creation time.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The resource type. Note that the @tgw-peering@ resource type has been
    -- deprecated.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The attachment state. Note that the @initiating@ state has been
    -- deprecated.
    state :: Prelude.Maybe TransitGatewayAttachmentState,
    -- | The tags for the attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the transit gateway.
    transitGatewayOwnerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'transitGatewayAttachment_association' - The association.
--
-- 'creationTime', 'transitGatewayAttachment_creationTime' - The creation time.
--
-- 'resourceId', 'transitGatewayAttachment_resourceId' - The ID of the resource.
--
-- 'resourceOwnerId', 'transitGatewayAttachment_resourceOwnerId' - The ID of the Amazon Web Services account that owns the resource.
--
-- 'resourceType', 'transitGatewayAttachment_resourceType' - The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'state', 'transitGatewayAttachment_state' - The attachment state. Note that the @initiating@ state has been
-- deprecated.
--
-- 'tags', 'transitGatewayAttachment_tags' - The tags for the attachment.
--
-- 'transitGatewayAttachmentId', 'transitGatewayAttachment_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'transitGatewayId', 'transitGatewayAttachment_transitGatewayId' - The ID of the transit gateway.
--
-- 'transitGatewayOwnerId', 'transitGatewayAttachment_transitGatewayOwnerId' - The ID of the Amazon Web Services account that owns the transit gateway.
newTransitGatewayAttachment ::
  TransitGatewayAttachment
newTransitGatewayAttachment =
  TransitGatewayAttachment'
    { association =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      transitGatewayOwnerId = Prelude.Nothing
    }

-- | The association.
transitGatewayAttachment_association :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe TransitGatewayAttachmentAssociation)
transitGatewayAttachment_association = Lens.lens (\TransitGatewayAttachment' {association} -> association) (\s@TransitGatewayAttachment' {} a -> s {association = a} :: TransitGatewayAttachment)

-- | The creation time.
transitGatewayAttachment_creationTime :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.UTCTime)
transitGatewayAttachment_creationTime = Lens.lens (\TransitGatewayAttachment' {creationTime} -> creationTime) (\s@TransitGatewayAttachment' {} a -> s {creationTime = a} :: TransitGatewayAttachment) Prelude.. Lens.mapping Data._Time

-- | The ID of the resource.
transitGatewayAttachment_resourceId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_resourceId = Lens.lens (\TransitGatewayAttachment' {resourceId} -> resourceId) (\s@TransitGatewayAttachment' {} a -> s {resourceId = a} :: TransitGatewayAttachment)

-- | The ID of the Amazon Web Services account that owns the resource.
transitGatewayAttachment_resourceOwnerId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_resourceOwnerId = Lens.lens (\TransitGatewayAttachment' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayAttachment' {} a -> s {resourceOwnerId = a} :: TransitGatewayAttachment)

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
transitGatewayAttachment_tags = Lens.lens (\TransitGatewayAttachment' {tags} -> tags) (\s@TransitGatewayAttachment' {} a -> s {tags = a} :: TransitGatewayAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the attachment.
transitGatewayAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayAttachment)

-- | The ID of the transit gateway.
transitGatewayAttachment_transitGatewayId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_transitGatewayId = Lens.lens (\TransitGatewayAttachment' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayId = a} :: TransitGatewayAttachment)

-- | The ID of the Amazon Web Services account that owns the transit gateway.
transitGatewayAttachment_transitGatewayOwnerId :: Lens.Lens' TransitGatewayAttachment (Prelude.Maybe Prelude.Text)
transitGatewayAttachment_transitGatewayOwnerId = Lens.lens (\TransitGatewayAttachment' {transitGatewayOwnerId} -> transitGatewayOwnerId) (\s@TransitGatewayAttachment' {} a -> s {transitGatewayOwnerId = a} :: TransitGatewayAttachment)

instance Data.FromXML TransitGatewayAttachment where
  parseXML x =
    TransitGatewayAttachment'
      Prelude.<$> (x Data..@? "association")
      Prelude.<*> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceOwnerId")
      Prelude.<*> (x Data..@? "resourceType")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Data..@? "transitGatewayId")
      Prelude.<*> (x Data..@? "transitGatewayOwnerId")

instance Prelude.Hashable TransitGatewayAttachment where
  hashWithSalt _salt TransitGatewayAttachment' {..} =
    _salt `Prelude.hashWithSalt` association
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceOwnerId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transitGatewayAttachmentId
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` transitGatewayOwnerId

instance Prelude.NFData TransitGatewayAttachment where
  rnf TransitGatewayAttachment' {..} =
    Prelude.rnf association
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf transitGatewayOwnerId
