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
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens

-- | Describes a transit gateway prefix list attachment.
--
-- /See:/ 'newTransitGatewayPrefixListAttachment' smart constructor.
data TransitGatewayPrefixListAttachment = TransitGatewayPrefixListAttachment'
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
-- Create a value of 'TransitGatewayPrefixListAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayPrefixListAttachment_resourceId' - The ID of the resource.
--
-- 'resourceType', 'transitGatewayPrefixListAttachment_resourceType' - The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
--
-- 'transitGatewayAttachmentId', 'transitGatewayPrefixListAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newTransitGatewayPrefixListAttachment ::
  TransitGatewayPrefixListAttachment
newTransitGatewayPrefixListAttachment =
  TransitGatewayPrefixListAttachment'
    { resourceId =
        Core.Nothing,
      resourceType = Core.Nothing,
      transitGatewayAttachmentId =
        Core.Nothing
    }

-- | The ID of the resource.
transitGatewayPrefixListAttachment_resourceId :: Lens.Lens' TransitGatewayPrefixListAttachment (Core.Maybe Core.Text)
transitGatewayPrefixListAttachment_resourceId = Lens.lens (\TransitGatewayPrefixListAttachment' {resourceId} -> resourceId) (\s@TransitGatewayPrefixListAttachment' {} a -> s {resourceId = a} :: TransitGatewayPrefixListAttachment)

-- | The resource type. Note that the @tgw-peering@ resource type has been
-- deprecated.
transitGatewayPrefixListAttachment_resourceType :: Lens.Lens' TransitGatewayPrefixListAttachment (Core.Maybe TransitGatewayAttachmentResourceType)
transitGatewayPrefixListAttachment_resourceType = Lens.lens (\TransitGatewayPrefixListAttachment' {resourceType} -> resourceType) (\s@TransitGatewayPrefixListAttachment' {} a -> s {resourceType = a} :: TransitGatewayPrefixListAttachment)

-- | The ID of the attachment.
transitGatewayPrefixListAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayPrefixListAttachment (Core.Maybe Core.Text)
transitGatewayPrefixListAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayPrefixListAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayPrefixListAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPrefixListAttachment)

instance
  Core.FromXML
    TransitGatewayPrefixListAttachment
  where
  parseXML x =
    TransitGatewayPrefixListAttachment'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")

instance
  Core.Hashable
    TransitGatewayPrefixListAttachment

instance
  Core.NFData
    TransitGatewayPrefixListAttachment
