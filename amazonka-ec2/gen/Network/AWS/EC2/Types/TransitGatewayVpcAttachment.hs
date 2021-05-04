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
-- Module      : Network.AWS.EC2.Types.TransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayVpcAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPC attachment.
--
-- /See:/ 'newTransitGatewayVpcAttachment' smart constructor.
data TransitGatewayVpcAttachment = TransitGatewayVpcAttachment'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The VPC attachment options.
    options :: Prelude.Maybe TransitGatewayVpcAttachmentOptions,
    -- | The IDs of the subnets.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The state of the VPC attachment. Note that the @initiating@ state has
    -- been deprecated.
    state :: Prelude.Maybe TransitGatewayAttachmentState,
    -- | The ID of the AWS account that owns the VPC.
    vpcOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the VPC attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      options = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      state = Prelude.Nothing,
      vpcOwnerId = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing
    }

-- | The creation time.
transitGatewayVpcAttachment_creationTime :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.UTCTime)
transitGatewayVpcAttachment_creationTime = Lens.lens (\TransitGatewayVpcAttachment' {creationTime} -> creationTime) (\s@TransitGatewayVpcAttachment' {} a -> s {creationTime = a} :: TransitGatewayVpcAttachment) Prelude.. Lens.mapping Prelude._Time

-- | The VPC attachment options.
transitGatewayVpcAttachment_options :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe TransitGatewayVpcAttachmentOptions)
transitGatewayVpcAttachment_options = Lens.lens (\TransitGatewayVpcAttachment' {options} -> options) (\s@TransitGatewayVpcAttachment' {} a -> s {options = a} :: TransitGatewayVpcAttachment)

-- | The IDs of the subnets.
transitGatewayVpcAttachment_subnetIds :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe [Prelude.Text])
transitGatewayVpcAttachment_subnetIds = Lens.lens (\TransitGatewayVpcAttachment' {subnetIds} -> subnetIds) (\s@TransitGatewayVpcAttachment' {} a -> s {subnetIds = a} :: TransitGatewayVpcAttachment) Prelude.. Lens.mapping Prelude._Coerce

-- | The state of the VPC attachment. Note that the @initiating@ state has
-- been deprecated.
transitGatewayVpcAttachment_state :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe TransitGatewayAttachmentState)
transitGatewayVpcAttachment_state = Lens.lens (\TransitGatewayVpcAttachment' {state} -> state) (\s@TransitGatewayVpcAttachment' {} a -> s {state = a} :: TransitGatewayVpcAttachment)

-- | The ID of the AWS account that owns the VPC.
transitGatewayVpcAttachment_vpcOwnerId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_vpcOwnerId = Lens.lens (\TransitGatewayVpcAttachment' {vpcOwnerId} -> vpcOwnerId) (\s@TransitGatewayVpcAttachment' {} a -> s {vpcOwnerId = a} :: TransitGatewayVpcAttachment)

-- | The tags for the VPC attachment.
transitGatewayVpcAttachment_tags :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe [Tag])
transitGatewayVpcAttachment_tags = Lens.lens (\TransitGatewayVpcAttachment' {tags} -> tags) (\s@TransitGatewayVpcAttachment' {} a -> s {tags = a} :: TransitGatewayVpcAttachment) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the attachment.
transitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayVpcAttachment)

-- | The ID of the VPC.
transitGatewayVpcAttachment_vpcId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_vpcId = Lens.lens (\TransitGatewayVpcAttachment' {vpcId} -> vpcId) (\s@TransitGatewayVpcAttachment' {} a -> s {vpcId = a} :: TransitGatewayVpcAttachment)

-- | The ID of the transit gateway.
transitGatewayVpcAttachment_transitGatewayId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_transitGatewayId = Lens.lens (\TransitGatewayVpcAttachment' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayVpcAttachment' {} a -> s {transitGatewayId = a} :: TransitGatewayVpcAttachment)

instance Prelude.FromXML TransitGatewayVpcAttachment where
  parseXML x =
    TransitGatewayVpcAttachment'
      Prelude.<$> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "options")
      Prelude.<*> ( x Prelude..@? "subnetIds" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "vpcOwnerId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Prelude..@? "vpcId")
      Prelude.<*> (x Prelude..@? "transitGatewayId")

instance Prelude.Hashable TransitGatewayVpcAttachment

instance Prelude.NFData TransitGatewayVpcAttachment
