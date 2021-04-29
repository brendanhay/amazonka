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
-- Module      : Network.AWS.EC2.Types.CarrierGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CarrierGatewayState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a carrier gateway.
--
-- /See:/ 'newCarrierGateway' smart constructor.
data CarrierGateway = CarrierGateway'
  { -- | The AWS account ID of the owner of the carrier gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the carrier gateway.
    state :: Prelude.Maybe CarrierGatewayState,
    -- | The tags assigned to the carrier gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the carrier gateway.
    carrierGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC associated with the carrier gateway.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      carrierGatewayId = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The AWS account ID of the owner of the carrier gateway.
carrierGateway_ownerId :: Lens.Lens' CarrierGateway (Prelude.Maybe Prelude.Text)
carrierGateway_ownerId = Lens.lens (\CarrierGateway' {ownerId} -> ownerId) (\s@CarrierGateway' {} a -> s {ownerId = a} :: CarrierGateway)

-- | The state of the carrier gateway.
carrierGateway_state :: Lens.Lens' CarrierGateway (Prelude.Maybe CarrierGatewayState)
carrierGateway_state = Lens.lens (\CarrierGateway' {state} -> state) (\s@CarrierGateway' {} a -> s {state = a} :: CarrierGateway)

-- | The tags assigned to the carrier gateway.
carrierGateway_tags :: Lens.Lens' CarrierGateway (Prelude.Maybe [Tag])
carrierGateway_tags = Lens.lens (\CarrierGateway' {tags} -> tags) (\s@CarrierGateway' {} a -> s {tags = a} :: CarrierGateway) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the carrier gateway.
carrierGateway_carrierGatewayId :: Lens.Lens' CarrierGateway (Prelude.Maybe Prelude.Text)
carrierGateway_carrierGatewayId = Lens.lens (\CarrierGateway' {carrierGatewayId} -> carrierGatewayId) (\s@CarrierGateway' {} a -> s {carrierGatewayId = a} :: CarrierGateway)

-- | The ID of the VPC associated with the carrier gateway.
carrierGateway_vpcId :: Lens.Lens' CarrierGateway (Prelude.Maybe Prelude.Text)
carrierGateway_vpcId = Lens.lens (\CarrierGateway' {vpcId} -> vpcId) (\s@CarrierGateway' {} a -> s {vpcId = a} :: CarrierGateway)

instance Prelude.FromXML CarrierGateway where
  parseXML x =
    CarrierGateway'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "carrierGatewayId")
      Prelude.<*> (x Prelude..@? "vpcId")

instance Prelude.Hashable CarrierGateway

instance Prelude.NFData CarrierGateway
