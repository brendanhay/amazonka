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
-- Module      : Amazonka.EC2.Types.TransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPrefixListReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TransitGatewayPrefixListAttachment
import Amazonka.EC2.Types.TransitGatewayPrefixListReferenceState
import qualified Amazonka.Prelude as Prelude

-- | Describes a prefix list reference.
--
-- /See:/ 'newTransitGatewayPrefixListReference' smart constructor.
data TransitGatewayPrefixListReference = TransitGatewayPrefixListReference'
  { -- | Indicates whether traffic that matches this route is dropped.
    blackhole :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the prefix list owner.
    prefixListOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the prefix list reference.
    state :: Prelude.Maybe TransitGatewayPrefixListReferenceState,
    -- | Information about the transit gateway attachment.
    transitGatewayAttachment :: Prelude.Maybe TransitGatewayPrefixListAttachment,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPrefixListReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blackhole', 'transitGatewayPrefixListReference_blackhole' - Indicates whether traffic that matches this route is dropped.
--
-- 'prefixListId', 'transitGatewayPrefixListReference_prefixListId' - The ID of the prefix list.
--
-- 'prefixListOwnerId', 'transitGatewayPrefixListReference_prefixListOwnerId' - The ID of the prefix list owner.
--
-- 'state', 'transitGatewayPrefixListReference_state' - The state of the prefix list reference.
--
-- 'transitGatewayAttachment', 'transitGatewayPrefixListReference_transitGatewayAttachment' - Information about the transit gateway attachment.
--
-- 'transitGatewayRouteTableId', 'transitGatewayPrefixListReference_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newTransitGatewayPrefixListReference ::
  TransitGatewayPrefixListReference
newTransitGatewayPrefixListReference =
  TransitGatewayPrefixListReference'
    { blackhole =
        Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      prefixListOwnerId = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayAttachment =
        Prelude.Nothing,
      transitGatewayRouteTableId =
        Prelude.Nothing
    }

-- | Indicates whether traffic that matches this route is dropped.
transitGatewayPrefixListReference_blackhole :: Lens.Lens' TransitGatewayPrefixListReference (Prelude.Maybe Prelude.Bool)
transitGatewayPrefixListReference_blackhole = Lens.lens (\TransitGatewayPrefixListReference' {blackhole} -> blackhole) (\s@TransitGatewayPrefixListReference' {} a -> s {blackhole = a} :: TransitGatewayPrefixListReference)

-- | The ID of the prefix list.
transitGatewayPrefixListReference_prefixListId :: Lens.Lens' TransitGatewayPrefixListReference (Prelude.Maybe Prelude.Text)
transitGatewayPrefixListReference_prefixListId = Lens.lens (\TransitGatewayPrefixListReference' {prefixListId} -> prefixListId) (\s@TransitGatewayPrefixListReference' {} a -> s {prefixListId = a} :: TransitGatewayPrefixListReference)

-- | The ID of the prefix list owner.
transitGatewayPrefixListReference_prefixListOwnerId :: Lens.Lens' TransitGatewayPrefixListReference (Prelude.Maybe Prelude.Text)
transitGatewayPrefixListReference_prefixListOwnerId = Lens.lens (\TransitGatewayPrefixListReference' {prefixListOwnerId} -> prefixListOwnerId) (\s@TransitGatewayPrefixListReference' {} a -> s {prefixListOwnerId = a} :: TransitGatewayPrefixListReference)

-- | The state of the prefix list reference.
transitGatewayPrefixListReference_state :: Lens.Lens' TransitGatewayPrefixListReference (Prelude.Maybe TransitGatewayPrefixListReferenceState)
transitGatewayPrefixListReference_state = Lens.lens (\TransitGatewayPrefixListReference' {state} -> state) (\s@TransitGatewayPrefixListReference' {} a -> s {state = a} :: TransitGatewayPrefixListReference)

-- | Information about the transit gateway attachment.
transitGatewayPrefixListReference_transitGatewayAttachment :: Lens.Lens' TransitGatewayPrefixListReference (Prelude.Maybe TransitGatewayPrefixListAttachment)
transitGatewayPrefixListReference_transitGatewayAttachment = Lens.lens (\TransitGatewayPrefixListReference' {transitGatewayAttachment} -> transitGatewayAttachment) (\s@TransitGatewayPrefixListReference' {} a -> s {transitGatewayAttachment = a} :: TransitGatewayPrefixListReference)

-- | The ID of the transit gateway route table.
transitGatewayPrefixListReference_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayPrefixListReference (Prelude.Maybe Prelude.Text)
transitGatewayPrefixListReference_transitGatewayRouteTableId = Lens.lens (\TransitGatewayPrefixListReference' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayPrefixListReference' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayPrefixListReference)

instance
  Data.FromXML
    TransitGatewayPrefixListReference
  where
  parseXML x =
    TransitGatewayPrefixListReference'
      Prelude.<$> (x Data..@? "blackhole")
      Prelude.<*> (x Data..@? "prefixListId")
      Prelude.<*> (x Data..@? "prefixListOwnerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "transitGatewayAttachment")
      Prelude.<*> (x Data..@? "transitGatewayRouteTableId")

instance
  Prelude.Hashable
    TransitGatewayPrefixListReference
  where
  hashWithSalt
    _salt
    TransitGatewayPrefixListReference' {..} =
      _salt
        `Prelude.hashWithSalt` blackhole
        `Prelude.hashWithSalt` prefixListId
        `Prelude.hashWithSalt` prefixListOwnerId
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` transitGatewayAttachment
        `Prelude.hashWithSalt` transitGatewayRouteTableId

instance
  Prelude.NFData
    TransitGatewayPrefixListReference
  where
  rnf TransitGatewayPrefixListReference' {..} =
    Prelude.rnf blackhole
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf prefixListOwnerId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf transitGatewayAttachment
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
