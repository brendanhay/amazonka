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
-- Module      : Amazonka.EC2.Types.CarrierGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CarrierGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CarrierGatewayState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a carrier gateway.
--
-- /See:/ 'newCarrierGateway' smart constructor.
data CarrierGateway = CarrierGateway'
  { -- | The ID of the carrier gateway.
    carrierGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the carrier gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the carrier gateway.
    state :: Prelude.Maybe CarrierGatewayState,
    -- | The tags assigned to the carrier gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC associated with the carrier gateway.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CarrierGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGatewayId', 'carrierGateway_carrierGatewayId' - The ID of the carrier gateway.
--
-- 'ownerId', 'carrierGateway_ownerId' - The Amazon Web Services account ID of the owner of the carrier gateway.
--
-- 'state', 'carrierGateway_state' - The state of the carrier gateway.
--
-- 'tags', 'carrierGateway_tags' - The tags assigned to the carrier gateway.
--
-- 'vpcId', 'carrierGateway_vpcId' - The ID of the VPC associated with the carrier gateway.
newCarrierGateway ::
  CarrierGateway
newCarrierGateway =
  CarrierGateway'
    { carrierGatewayId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ID of the carrier gateway.
carrierGateway_carrierGatewayId :: Lens.Lens' CarrierGateway (Prelude.Maybe Prelude.Text)
carrierGateway_carrierGatewayId = Lens.lens (\CarrierGateway' {carrierGatewayId} -> carrierGatewayId) (\s@CarrierGateway' {} a -> s {carrierGatewayId = a} :: CarrierGateway)

-- | The Amazon Web Services account ID of the owner of the carrier gateway.
carrierGateway_ownerId :: Lens.Lens' CarrierGateway (Prelude.Maybe Prelude.Text)
carrierGateway_ownerId = Lens.lens (\CarrierGateway' {ownerId} -> ownerId) (\s@CarrierGateway' {} a -> s {ownerId = a} :: CarrierGateway)

-- | The state of the carrier gateway.
carrierGateway_state :: Lens.Lens' CarrierGateway (Prelude.Maybe CarrierGatewayState)
carrierGateway_state = Lens.lens (\CarrierGateway' {state} -> state) (\s@CarrierGateway' {} a -> s {state = a} :: CarrierGateway)

-- | The tags assigned to the carrier gateway.
carrierGateway_tags :: Lens.Lens' CarrierGateway (Prelude.Maybe [Tag])
carrierGateway_tags = Lens.lens (\CarrierGateway' {tags} -> tags) (\s@CarrierGateway' {} a -> s {tags = a} :: CarrierGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC associated with the carrier gateway.
carrierGateway_vpcId :: Lens.Lens' CarrierGateway (Prelude.Maybe Prelude.Text)
carrierGateway_vpcId = Lens.lens (\CarrierGateway' {vpcId} -> vpcId) (\s@CarrierGateway' {} a -> s {vpcId = a} :: CarrierGateway)

instance Data.FromXML CarrierGateway where
  parseXML x =
    CarrierGateway'
      Prelude.<$> (x Data..@? "carrierGatewayId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable CarrierGateway where
  hashWithSalt _salt CarrierGateway' {..} =
    _salt `Prelude.hashWithSalt` carrierGatewayId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData CarrierGateway where
  rnf CarrierGateway' {..} =
    Prelude.rnf carrierGatewayId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
