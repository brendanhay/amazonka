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
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListReference where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
import Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
import qualified Network.AWS.Lens as Lens

-- | Describes a prefix list reference.
--
-- /See:/ 'newTransitGatewayPrefixListReference' smart constructor.
data TransitGatewayPrefixListReference = TransitGatewayPrefixListReference'
  { -- | Information about the transit gateway attachment.
    transitGatewayAttachment :: Core.Maybe TransitGatewayPrefixListAttachment,
    -- | The ID of the prefix list owner.
    prefixListOwnerId :: Core.Maybe Core.Text,
    -- | The ID of the prefix list.
    prefixListId :: Core.Maybe Core.Text,
    -- | The state of the prefix list reference.
    state :: Core.Maybe TransitGatewayPrefixListReferenceState,
    -- | Indicates whether traffic that matches this route is dropped.
    blackhole :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayPrefixListReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayAttachment', 'transitGatewayPrefixListReference_transitGatewayAttachment' - Information about the transit gateway attachment.
--
-- 'prefixListOwnerId', 'transitGatewayPrefixListReference_prefixListOwnerId' - The ID of the prefix list owner.
--
-- 'prefixListId', 'transitGatewayPrefixListReference_prefixListId' - The ID of the prefix list.
--
-- 'state', 'transitGatewayPrefixListReference_state' - The state of the prefix list reference.
--
-- 'blackhole', 'transitGatewayPrefixListReference_blackhole' - Indicates whether traffic that matches this route is dropped.
--
-- 'transitGatewayRouteTableId', 'transitGatewayPrefixListReference_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newTransitGatewayPrefixListReference ::
  TransitGatewayPrefixListReference
newTransitGatewayPrefixListReference =
  TransitGatewayPrefixListReference'
    { transitGatewayAttachment =
        Core.Nothing,
      prefixListOwnerId = Core.Nothing,
      prefixListId = Core.Nothing,
      state = Core.Nothing,
      blackhole = Core.Nothing,
      transitGatewayRouteTableId =
        Core.Nothing
    }

-- | Information about the transit gateway attachment.
transitGatewayPrefixListReference_transitGatewayAttachment :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe TransitGatewayPrefixListAttachment)
transitGatewayPrefixListReference_transitGatewayAttachment = Lens.lens (\TransitGatewayPrefixListReference' {transitGatewayAttachment} -> transitGatewayAttachment) (\s@TransitGatewayPrefixListReference' {} a -> s {transitGatewayAttachment = a} :: TransitGatewayPrefixListReference)

-- | The ID of the prefix list owner.
transitGatewayPrefixListReference_prefixListOwnerId :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Core.Text)
transitGatewayPrefixListReference_prefixListOwnerId = Lens.lens (\TransitGatewayPrefixListReference' {prefixListOwnerId} -> prefixListOwnerId) (\s@TransitGatewayPrefixListReference' {} a -> s {prefixListOwnerId = a} :: TransitGatewayPrefixListReference)

-- | The ID of the prefix list.
transitGatewayPrefixListReference_prefixListId :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Core.Text)
transitGatewayPrefixListReference_prefixListId = Lens.lens (\TransitGatewayPrefixListReference' {prefixListId} -> prefixListId) (\s@TransitGatewayPrefixListReference' {} a -> s {prefixListId = a} :: TransitGatewayPrefixListReference)

-- | The state of the prefix list reference.
transitGatewayPrefixListReference_state :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe TransitGatewayPrefixListReferenceState)
transitGatewayPrefixListReference_state = Lens.lens (\TransitGatewayPrefixListReference' {state} -> state) (\s@TransitGatewayPrefixListReference' {} a -> s {state = a} :: TransitGatewayPrefixListReference)

-- | Indicates whether traffic that matches this route is dropped.
transitGatewayPrefixListReference_blackhole :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Core.Bool)
transitGatewayPrefixListReference_blackhole = Lens.lens (\TransitGatewayPrefixListReference' {blackhole} -> blackhole) (\s@TransitGatewayPrefixListReference' {} a -> s {blackhole = a} :: TransitGatewayPrefixListReference)

-- | The ID of the transit gateway route table.
transitGatewayPrefixListReference_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Core.Text)
transitGatewayPrefixListReference_transitGatewayRouteTableId = Lens.lens (\TransitGatewayPrefixListReference' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayPrefixListReference' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayPrefixListReference)

instance
  Core.FromXML
    TransitGatewayPrefixListReference
  where
  parseXML x =
    TransitGatewayPrefixListReference'
      Core.<$> (x Core..@? "transitGatewayAttachment")
      Core.<*> (x Core..@? "prefixListOwnerId")
      Core.<*> (x Core..@? "prefixListId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "blackhole")
      Core.<*> (x Core..@? "transitGatewayRouteTableId")

instance
  Core.Hashable
    TransitGatewayPrefixListReference

instance
  Core.NFData
    TransitGatewayPrefixListReference
