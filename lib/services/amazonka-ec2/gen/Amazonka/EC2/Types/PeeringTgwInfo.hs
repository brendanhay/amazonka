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
-- Module      : Amazonka.EC2.Types.PeeringTgwInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PeeringTgwInfo where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the transit gateway in the peering attachment.
--
-- /See:/ 'newPeeringTgwInfo' smart constructor.
data PeeringTgwInfo = PeeringTgwInfo'
  { -- | The ID of the Amazon Web Services account that owns the transit gateway.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Region of the transit gateway.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PeeringTgwInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'peeringTgwInfo_ownerId' - The ID of the Amazon Web Services account that owns the transit gateway.
--
-- 'transitGatewayId', 'peeringTgwInfo_transitGatewayId' - The ID of the transit gateway.
--
-- 'region', 'peeringTgwInfo_region' - The Region of the transit gateway.
newPeeringTgwInfo ::
  PeeringTgwInfo
newPeeringTgwInfo =
  PeeringTgwInfo'
    { ownerId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services account that owns the transit gateway.
peeringTgwInfo_ownerId :: Lens.Lens' PeeringTgwInfo (Prelude.Maybe Prelude.Text)
peeringTgwInfo_ownerId = Lens.lens (\PeeringTgwInfo' {ownerId} -> ownerId) (\s@PeeringTgwInfo' {} a -> s {ownerId = a} :: PeeringTgwInfo)

-- | The ID of the transit gateway.
peeringTgwInfo_transitGatewayId :: Lens.Lens' PeeringTgwInfo (Prelude.Maybe Prelude.Text)
peeringTgwInfo_transitGatewayId = Lens.lens (\PeeringTgwInfo' {transitGatewayId} -> transitGatewayId) (\s@PeeringTgwInfo' {} a -> s {transitGatewayId = a} :: PeeringTgwInfo)

-- | The Region of the transit gateway.
peeringTgwInfo_region :: Lens.Lens' PeeringTgwInfo (Prelude.Maybe Prelude.Text)
peeringTgwInfo_region = Lens.lens (\PeeringTgwInfo' {region} -> region) (\s@PeeringTgwInfo' {} a -> s {region = a} :: PeeringTgwInfo)

instance Core.FromXML PeeringTgwInfo where
  parseXML x =
    PeeringTgwInfo'
      Prelude.<$> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "transitGatewayId")
      Prelude.<*> (x Core..@? "region")

instance Prelude.Hashable PeeringTgwInfo where
  hashWithSalt salt' PeeringTgwInfo' {..} =
    salt' `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` ownerId

instance Prelude.NFData PeeringTgwInfo where
  rnf PeeringTgwInfo' {..} =
    Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf transitGatewayId
