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
-- Module      : Amazonka.EC2.Types.LocalGatewayRouteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGatewayRouteTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LocalGatewayRouteTableMode
import Amazonka.EC2.Types.StateReason
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a local gateway route table.
--
-- /See:/ 'newLocalGatewayRouteTable' smart constructor.
data LocalGatewayRouteTable = LocalGatewayRouteTable'
  { -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table.
    localGatewayRouteTableArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The mode of the local gateway route table.
    mode :: Prelude.Maybe LocalGatewayRouteTableMode,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the local gateway
    -- route table.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the local gateway route table.
    state :: Prelude.Maybe Prelude.Text,
    -- | Information about the state change.
    stateReason :: Prelude.Maybe StateReason,
    -- | The tags assigned to the local gateway route table.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayId', 'localGatewayRouteTable_localGatewayId' - The ID of the local gateway.
--
-- 'localGatewayRouteTableArn', 'localGatewayRouteTable_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table.
--
-- 'localGatewayRouteTableId', 'localGatewayRouteTable_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'mode', 'localGatewayRouteTable_mode' - The mode of the local gateway route table.
--
-- 'outpostArn', 'localGatewayRouteTable_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'ownerId', 'localGatewayRouteTable_ownerId' - The ID of the Amazon Web Services account that owns the local gateway
-- route table.
--
-- 'state', 'localGatewayRouteTable_state' - The state of the local gateway route table.
--
-- 'stateReason', 'localGatewayRouteTable_stateReason' - Information about the state change.
--
-- 'tags', 'localGatewayRouteTable_tags' - The tags assigned to the local gateway route table.
newLocalGatewayRouteTable ::
  LocalGatewayRouteTable
newLocalGatewayRouteTable =
  LocalGatewayRouteTable'
    { localGatewayId =
        Prelude.Nothing,
      localGatewayRouteTableArn = Prelude.Nothing,
      localGatewayRouteTableId = Prelude.Nothing,
      mode = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the local gateway.
localGatewayRouteTable_localGatewayId :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_localGatewayId = Lens.lens (\LocalGatewayRouteTable' {localGatewayId} -> localGatewayId) (\s@LocalGatewayRouteTable' {} a -> s {localGatewayId = a} :: LocalGatewayRouteTable)

-- | The Amazon Resource Name (ARN) of the local gateway route table.
localGatewayRouteTable_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRouteTable' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRouteTable' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRouteTable)

-- | The ID of the local gateway route table.
localGatewayRouteTable_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_localGatewayRouteTableId = Lens.lens (\LocalGatewayRouteTable' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRouteTable' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTable)

-- | The mode of the local gateway route table.
localGatewayRouteTable_mode :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe LocalGatewayRouteTableMode)
localGatewayRouteTable_mode = Lens.lens (\LocalGatewayRouteTable' {mode} -> mode) (\s@LocalGatewayRouteTable' {} a -> s {mode = a} :: LocalGatewayRouteTable)

-- | The Amazon Resource Name (ARN) of the Outpost.
localGatewayRouteTable_outpostArn :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_outpostArn = Lens.lens (\LocalGatewayRouteTable' {outpostArn} -> outpostArn) (\s@LocalGatewayRouteTable' {} a -> s {outpostArn = a} :: LocalGatewayRouteTable)

-- | The ID of the Amazon Web Services account that owns the local gateway
-- route table.
localGatewayRouteTable_ownerId :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_ownerId = Lens.lens (\LocalGatewayRouteTable' {ownerId} -> ownerId) (\s@LocalGatewayRouteTable' {} a -> s {ownerId = a} :: LocalGatewayRouteTable)

-- | The state of the local gateway route table.
localGatewayRouteTable_state :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_state = Lens.lens (\LocalGatewayRouteTable' {state} -> state) (\s@LocalGatewayRouteTable' {} a -> s {state = a} :: LocalGatewayRouteTable)

-- | Information about the state change.
localGatewayRouteTable_stateReason :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe StateReason)
localGatewayRouteTable_stateReason = Lens.lens (\LocalGatewayRouteTable' {stateReason} -> stateReason) (\s@LocalGatewayRouteTable' {} a -> s {stateReason = a} :: LocalGatewayRouteTable)

-- | The tags assigned to the local gateway route table.
localGatewayRouteTable_tags :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe [Tag])
localGatewayRouteTable_tags = Lens.lens (\LocalGatewayRouteTable' {tags} -> tags) (\s@LocalGatewayRouteTable' {} a -> s {tags = a} :: LocalGatewayRouteTable) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML LocalGatewayRouteTable where
  parseXML x =
    LocalGatewayRouteTable'
      Prelude.<$> (x Data..@? "localGatewayId")
      Prelude.<*> (x Data..@? "localGatewayRouteTableArn")
      Prelude.<*> (x Data..@? "localGatewayRouteTableId")
      Prelude.<*> (x Data..@? "mode")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "stateReason")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable LocalGatewayRouteTable where
  hashWithSalt _salt LocalGatewayRouteTable' {..} =
    _salt
      `Prelude.hashWithSalt` localGatewayId
      `Prelude.hashWithSalt` localGatewayRouteTableArn
      `Prelude.hashWithSalt` localGatewayRouteTableId
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` tags

instance Prelude.NFData LocalGatewayRouteTable where
  rnf LocalGatewayRouteTable' {..} =
    Prelude.rnf localGatewayId
      `Prelude.seq` Prelude.rnf localGatewayRouteTableArn
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf tags
