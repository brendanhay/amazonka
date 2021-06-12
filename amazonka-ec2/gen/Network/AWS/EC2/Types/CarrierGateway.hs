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
-- Module      : Network.AWS.EC2.Types.CarrierGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CarrierGatewayState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a carrier gateway.
--
-- /See:/ 'newCarrierGateway' smart constructor.
data CarrierGateway = CarrierGateway'
  { -- | The AWS account ID of the owner of the carrier gateway.
    ownerId :: Core.Maybe Core.Text,
    -- | The state of the carrier gateway.
    state :: Core.Maybe CarrierGatewayState,
    -- | The tags assigned to the carrier gateway.
    tags :: Core.Maybe [Tag],
    -- | The ID of the carrier gateway.
    carrierGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the VPC associated with the carrier gateway.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CarrierGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'carrierGateway_ownerId' - The AWS account ID of the owner of the carrier gateway.
--
-- 'state', 'carrierGateway_state' - The state of the carrier gateway.
--
-- 'tags', 'carrierGateway_tags' - The tags assigned to the carrier gateway.
--
-- 'carrierGatewayId', 'carrierGateway_carrierGatewayId' - The ID of the carrier gateway.
--
-- 'vpcId', 'carrierGateway_vpcId' - The ID of the VPC associated with the carrier gateway.
newCarrierGateway ::
  CarrierGateway
newCarrierGateway =
  CarrierGateway'
    { ownerId = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      carrierGatewayId = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The AWS account ID of the owner of the carrier gateway.
carrierGateway_ownerId :: Lens.Lens' CarrierGateway (Core.Maybe Core.Text)
carrierGateway_ownerId = Lens.lens (\CarrierGateway' {ownerId} -> ownerId) (\s@CarrierGateway' {} a -> s {ownerId = a} :: CarrierGateway)

-- | The state of the carrier gateway.
carrierGateway_state :: Lens.Lens' CarrierGateway (Core.Maybe CarrierGatewayState)
carrierGateway_state = Lens.lens (\CarrierGateway' {state} -> state) (\s@CarrierGateway' {} a -> s {state = a} :: CarrierGateway)

-- | The tags assigned to the carrier gateway.
carrierGateway_tags :: Lens.Lens' CarrierGateway (Core.Maybe [Tag])
carrierGateway_tags = Lens.lens (\CarrierGateway' {tags} -> tags) (\s@CarrierGateway' {} a -> s {tags = a} :: CarrierGateway) Core.. Lens.mapping Lens._Coerce

-- | The ID of the carrier gateway.
carrierGateway_carrierGatewayId :: Lens.Lens' CarrierGateway (Core.Maybe Core.Text)
carrierGateway_carrierGatewayId = Lens.lens (\CarrierGateway' {carrierGatewayId} -> carrierGatewayId) (\s@CarrierGateway' {} a -> s {carrierGatewayId = a} :: CarrierGateway)

-- | The ID of the VPC associated with the carrier gateway.
carrierGateway_vpcId :: Lens.Lens' CarrierGateway (Core.Maybe Core.Text)
carrierGateway_vpcId = Lens.lens (\CarrierGateway' {vpcId} -> vpcId) (\s@CarrierGateway' {} a -> s {vpcId = a} :: CarrierGateway)

instance Core.FromXML CarrierGateway where
  parseXML x =
    CarrierGateway'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "carrierGatewayId")
      Core.<*> (x Core..@? "vpcId")

instance Core.Hashable CarrierGateway

instance Core.NFData CarrierGateway
