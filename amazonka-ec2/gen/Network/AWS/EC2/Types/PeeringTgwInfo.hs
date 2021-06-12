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
-- Module      : Network.AWS.EC2.Types.PeeringTgwInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringTgwInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the transit gateway in the peering attachment.
--
-- /See:/ 'newPeeringTgwInfo' smart constructor.
data PeeringTgwInfo = PeeringTgwInfo'
  { -- | The AWS account ID of the owner of the transit gateway.
    ownerId :: Core.Maybe Core.Text,
    -- | The Region of the transit gateway.
    region :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PeeringTgwInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'peeringTgwInfo_ownerId' - The AWS account ID of the owner of the transit gateway.
--
-- 'region', 'peeringTgwInfo_region' - The Region of the transit gateway.
--
-- 'transitGatewayId', 'peeringTgwInfo_transitGatewayId' - The ID of the transit gateway.
newPeeringTgwInfo ::
  PeeringTgwInfo
newPeeringTgwInfo =
  PeeringTgwInfo'
    { ownerId = Core.Nothing,
      region = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The AWS account ID of the owner of the transit gateway.
peeringTgwInfo_ownerId :: Lens.Lens' PeeringTgwInfo (Core.Maybe Core.Text)
peeringTgwInfo_ownerId = Lens.lens (\PeeringTgwInfo' {ownerId} -> ownerId) (\s@PeeringTgwInfo' {} a -> s {ownerId = a} :: PeeringTgwInfo)

-- | The Region of the transit gateway.
peeringTgwInfo_region :: Lens.Lens' PeeringTgwInfo (Core.Maybe Core.Text)
peeringTgwInfo_region = Lens.lens (\PeeringTgwInfo' {region} -> region) (\s@PeeringTgwInfo' {} a -> s {region = a} :: PeeringTgwInfo)

-- | The ID of the transit gateway.
peeringTgwInfo_transitGatewayId :: Lens.Lens' PeeringTgwInfo (Core.Maybe Core.Text)
peeringTgwInfo_transitGatewayId = Lens.lens (\PeeringTgwInfo' {transitGatewayId} -> transitGatewayId) (\s@PeeringTgwInfo' {} a -> s {transitGatewayId = a} :: PeeringTgwInfo)

instance Core.FromXML PeeringTgwInfo where
  parseXML x =
    PeeringTgwInfo'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "region")
      Core.<*> (x Core..@? "transitGatewayId")

instance Core.Hashable PeeringTgwInfo

instance Core.NFData PeeringTgwInfo
