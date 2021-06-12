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
-- Module      : Network.AWS.EC2.Types.TransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayVpcAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions
import qualified Network.AWS.Lens as Lens

-- | Describes a VPC attachment.
--
-- /See:/ 'newTransitGatewayVpcAttachment' smart constructor.
data TransitGatewayVpcAttachment = TransitGatewayVpcAttachment'
  { -- | The creation time.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | The VPC attachment options.
    options :: Core.Maybe TransitGatewayVpcAttachmentOptions,
    -- | The IDs of the subnets.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The state of the VPC attachment. Note that the @initiating@ state has
    -- been deprecated.
    state :: Core.Maybe TransitGatewayAttachmentState,
    -- | The ID of the AWS account that owns the VPC.
    vpcOwnerId :: Core.Maybe Core.Text,
    -- | The tags for the VPC attachment.
    tags :: Core.Maybe [Tag],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text,
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'transitGatewayVpcAttachment_creationTime' - The creation time.
--
-- 'options', 'transitGatewayVpcAttachment_options' - The VPC attachment options.
--
-- 'subnetIds', 'transitGatewayVpcAttachment_subnetIds' - The IDs of the subnets.
--
-- 'state', 'transitGatewayVpcAttachment_state' - The state of the VPC attachment. Note that the @initiating@ state has
-- been deprecated.
--
-- 'vpcOwnerId', 'transitGatewayVpcAttachment_vpcOwnerId' - The ID of the AWS account that owns the VPC.
--
-- 'tags', 'transitGatewayVpcAttachment_tags' - The tags for the VPC attachment.
--
-- 'transitGatewayAttachmentId', 'transitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'vpcId', 'transitGatewayVpcAttachment_vpcId' - The ID of the VPC.
--
-- 'transitGatewayId', 'transitGatewayVpcAttachment_transitGatewayId' - The ID of the transit gateway.
newTransitGatewayVpcAttachment ::
  TransitGatewayVpcAttachment
newTransitGatewayVpcAttachment =
  TransitGatewayVpcAttachment'
    { creationTime =
        Core.Nothing,
      options = Core.Nothing,
      subnetIds = Core.Nothing,
      state = Core.Nothing,
      vpcOwnerId = Core.Nothing,
      tags = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      vpcId = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The creation time.
transitGatewayVpcAttachment_creationTime :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.UTCTime)
transitGatewayVpcAttachment_creationTime = Lens.lens (\TransitGatewayVpcAttachment' {creationTime} -> creationTime) (\s@TransitGatewayVpcAttachment' {} a -> s {creationTime = a} :: TransitGatewayVpcAttachment) Core.. Lens.mapping Core._Time

-- | The VPC attachment options.
transitGatewayVpcAttachment_options :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe TransitGatewayVpcAttachmentOptions)
transitGatewayVpcAttachment_options = Lens.lens (\TransitGatewayVpcAttachment' {options} -> options) (\s@TransitGatewayVpcAttachment' {} a -> s {options = a} :: TransitGatewayVpcAttachment)

-- | The IDs of the subnets.
transitGatewayVpcAttachment_subnetIds :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe [Core.Text])
transitGatewayVpcAttachment_subnetIds = Lens.lens (\TransitGatewayVpcAttachment' {subnetIds} -> subnetIds) (\s@TransitGatewayVpcAttachment' {} a -> s {subnetIds = a} :: TransitGatewayVpcAttachment) Core.. Lens.mapping Lens._Coerce

-- | The state of the VPC attachment. Note that the @initiating@ state has
-- been deprecated.
transitGatewayVpcAttachment_state :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe TransitGatewayAttachmentState)
transitGatewayVpcAttachment_state = Lens.lens (\TransitGatewayVpcAttachment' {state} -> state) (\s@TransitGatewayVpcAttachment' {} a -> s {state = a} :: TransitGatewayVpcAttachment)

-- | The ID of the AWS account that owns the VPC.
transitGatewayVpcAttachment_vpcOwnerId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
transitGatewayVpcAttachment_vpcOwnerId = Lens.lens (\TransitGatewayVpcAttachment' {vpcOwnerId} -> vpcOwnerId) (\s@TransitGatewayVpcAttachment' {} a -> s {vpcOwnerId = a} :: TransitGatewayVpcAttachment)

-- | The tags for the VPC attachment.
transitGatewayVpcAttachment_tags :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe [Tag])
transitGatewayVpcAttachment_tags = Lens.lens (\TransitGatewayVpcAttachment' {tags} -> tags) (\s@TransitGatewayVpcAttachment' {} a -> s {tags = a} :: TransitGatewayVpcAttachment) Core.. Lens.mapping Lens._Coerce

-- | The ID of the attachment.
transitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
transitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayVpcAttachment)

-- | The ID of the VPC.
transitGatewayVpcAttachment_vpcId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
transitGatewayVpcAttachment_vpcId = Lens.lens (\TransitGatewayVpcAttachment' {vpcId} -> vpcId) (\s@TransitGatewayVpcAttachment' {} a -> s {vpcId = a} :: TransitGatewayVpcAttachment)

-- | The ID of the transit gateway.
transitGatewayVpcAttachment_transitGatewayId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
transitGatewayVpcAttachment_transitGatewayId = Lens.lens (\TransitGatewayVpcAttachment' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayVpcAttachment' {} a -> s {transitGatewayId = a} :: TransitGatewayVpcAttachment)

instance Core.FromXML TransitGatewayVpcAttachment where
  parseXML x =
    TransitGatewayVpcAttachment'
      Core.<$> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "options")
      Core.<*> ( x Core..@? "subnetIds" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "vpcOwnerId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
      Core.<*> (x Core..@? "vpcId")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable TransitGatewayVpcAttachment

instance Core.NFData TransitGatewayVpcAttachment
