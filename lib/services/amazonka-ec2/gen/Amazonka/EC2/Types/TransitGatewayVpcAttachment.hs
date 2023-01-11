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
-- Module      : Amazonka.EC2.Types.TransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayVpcAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayAttachmentState
import Amazonka.EC2.Types.TransitGatewayVpcAttachmentOptions
import qualified Amazonka.Prelude as Prelude

-- | Describes a VPC attachment.
--
-- /See:/ 'newTransitGatewayVpcAttachment' smart constructor.
data TransitGatewayVpcAttachment = TransitGatewayVpcAttachment'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The VPC attachment options.
    options :: Prelude.Maybe TransitGatewayVpcAttachmentOptions,
    -- | The state of the VPC attachment. Note that the @initiating@ state has
    -- been deprecated.
    state :: Prelude.Maybe TransitGatewayAttachmentState,
    -- | The IDs of the subnets.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags for the VPC attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the VPC.
    vpcOwnerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'state', 'transitGatewayVpcAttachment_state' - The state of the VPC attachment. Note that the @initiating@ state has
-- been deprecated.
--
-- 'subnetIds', 'transitGatewayVpcAttachment_subnetIds' - The IDs of the subnets.
--
-- 'tags', 'transitGatewayVpcAttachment_tags' - The tags for the VPC attachment.
--
-- 'transitGatewayAttachmentId', 'transitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
--
-- 'transitGatewayId', 'transitGatewayVpcAttachment_transitGatewayId' - The ID of the transit gateway.
--
-- 'vpcId', 'transitGatewayVpcAttachment_vpcId' - The ID of the VPC.
--
-- 'vpcOwnerId', 'transitGatewayVpcAttachment_vpcOwnerId' - The ID of the Amazon Web Services account that owns the VPC.
newTransitGatewayVpcAttachment ::
  TransitGatewayVpcAttachment
newTransitGatewayVpcAttachment =
  TransitGatewayVpcAttachment'
    { creationTime =
        Prelude.Nothing,
      options = Prelude.Nothing,
      state = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcOwnerId = Prelude.Nothing
    }

-- | The creation time.
transitGatewayVpcAttachment_creationTime :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.UTCTime)
transitGatewayVpcAttachment_creationTime = Lens.lens (\TransitGatewayVpcAttachment' {creationTime} -> creationTime) (\s@TransitGatewayVpcAttachment' {} a -> s {creationTime = a} :: TransitGatewayVpcAttachment) Prelude.. Lens.mapping Data._Time

-- | The VPC attachment options.
transitGatewayVpcAttachment_options :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe TransitGatewayVpcAttachmentOptions)
transitGatewayVpcAttachment_options = Lens.lens (\TransitGatewayVpcAttachment' {options} -> options) (\s@TransitGatewayVpcAttachment' {} a -> s {options = a} :: TransitGatewayVpcAttachment)

-- | The state of the VPC attachment. Note that the @initiating@ state has
-- been deprecated.
transitGatewayVpcAttachment_state :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe TransitGatewayAttachmentState)
transitGatewayVpcAttachment_state = Lens.lens (\TransitGatewayVpcAttachment' {state} -> state) (\s@TransitGatewayVpcAttachment' {} a -> s {state = a} :: TransitGatewayVpcAttachment)

-- | The IDs of the subnets.
transitGatewayVpcAttachment_subnetIds :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe [Prelude.Text])
transitGatewayVpcAttachment_subnetIds = Lens.lens (\TransitGatewayVpcAttachment' {subnetIds} -> subnetIds) (\s@TransitGatewayVpcAttachment' {} a -> s {subnetIds = a} :: TransitGatewayVpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the VPC attachment.
transitGatewayVpcAttachment_tags :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe [Tag])
transitGatewayVpcAttachment_tags = Lens.lens (\TransitGatewayVpcAttachment' {tags} -> tags) (\s@TransitGatewayVpcAttachment' {} a -> s {tags = a} :: TransitGatewayVpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the attachment.
transitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayVpcAttachment)

-- | The ID of the transit gateway.
transitGatewayVpcAttachment_transitGatewayId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_transitGatewayId = Lens.lens (\TransitGatewayVpcAttachment' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayVpcAttachment' {} a -> s {transitGatewayId = a} :: TransitGatewayVpcAttachment)

-- | The ID of the VPC.
transitGatewayVpcAttachment_vpcId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_vpcId = Lens.lens (\TransitGatewayVpcAttachment' {vpcId} -> vpcId) (\s@TransitGatewayVpcAttachment' {} a -> s {vpcId = a} :: TransitGatewayVpcAttachment)

-- | The ID of the Amazon Web Services account that owns the VPC.
transitGatewayVpcAttachment_vpcOwnerId :: Lens.Lens' TransitGatewayVpcAttachment (Prelude.Maybe Prelude.Text)
transitGatewayVpcAttachment_vpcOwnerId = Lens.lens (\TransitGatewayVpcAttachment' {vpcOwnerId} -> vpcOwnerId) (\s@TransitGatewayVpcAttachment' {} a -> s {vpcOwnerId = a} :: TransitGatewayVpcAttachment)

instance Data.FromXML TransitGatewayVpcAttachment where
  parseXML x =
    TransitGatewayVpcAttachment'
      Prelude.<$> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "options")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "subnetIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Data..@? "transitGatewayId")
      Prelude.<*> (x Data..@? "vpcId")
      Prelude.<*> (x Data..@? "vpcOwnerId")

instance Prelude.Hashable TransitGatewayVpcAttachment where
  hashWithSalt _salt TransitGatewayVpcAttachment' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transitGatewayAttachmentId
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcOwnerId

instance Prelude.NFData TransitGatewayVpcAttachment where
  rnf TransitGatewayVpcAttachment' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcOwnerId
