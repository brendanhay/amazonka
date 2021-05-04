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
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTable where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a local gateway route table.
--
-- /See:/ 'newLocalGatewayRouteTable' smart constructor.
data LocalGatewayRouteTable = LocalGatewayRouteTable'
  { -- | The AWS account ID that owns the local gateway route table.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table.
    localGatewayRouteTableArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the local gateway route table.
    state :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the local gateway route table.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LocalGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'localGatewayRouteTable_ownerId' - The AWS account ID that owns the local gateway route table.
--
-- 'outpostArn', 'localGatewayRouteTable_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'localGatewayId', 'localGatewayRouteTable_localGatewayId' - The ID of the local gateway.
--
-- 'localGatewayRouteTableArn', 'localGatewayRouteTable_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table.
--
-- 'state', 'localGatewayRouteTable_state' - The state of the local gateway route table.
--
-- 'localGatewayRouteTableId', 'localGatewayRouteTable_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'tags', 'localGatewayRouteTable_tags' - The tags assigned to the local gateway route table.
newLocalGatewayRouteTable ::
  LocalGatewayRouteTable
newLocalGatewayRouteTable =
  LocalGatewayRouteTable'
    { ownerId = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      localGatewayId = Prelude.Nothing,
      localGatewayRouteTableArn = Prelude.Nothing,
      state = Prelude.Nothing,
      localGatewayRouteTableId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The AWS account ID that owns the local gateway route table.
localGatewayRouteTable_ownerId :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_ownerId = Lens.lens (\LocalGatewayRouteTable' {ownerId} -> ownerId) (\s@LocalGatewayRouteTable' {} a -> s {ownerId = a} :: LocalGatewayRouteTable)

-- | The Amazon Resource Name (ARN) of the Outpost.
localGatewayRouteTable_outpostArn :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_outpostArn = Lens.lens (\LocalGatewayRouteTable' {outpostArn} -> outpostArn) (\s@LocalGatewayRouteTable' {} a -> s {outpostArn = a} :: LocalGatewayRouteTable)

-- | The ID of the local gateway.
localGatewayRouteTable_localGatewayId :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_localGatewayId = Lens.lens (\LocalGatewayRouteTable' {localGatewayId} -> localGatewayId) (\s@LocalGatewayRouteTable' {} a -> s {localGatewayId = a} :: LocalGatewayRouteTable)

-- | The Amazon Resource Name (ARN) of the local gateway route table.
localGatewayRouteTable_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRouteTable' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRouteTable' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRouteTable)

-- | The state of the local gateway route table.
localGatewayRouteTable_state :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_state = Lens.lens (\LocalGatewayRouteTable' {state} -> state) (\s@LocalGatewayRouteTable' {} a -> s {state = a} :: LocalGatewayRouteTable)

-- | The ID of the local gateway route table.
localGatewayRouteTable_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe Prelude.Text)
localGatewayRouteTable_localGatewayRouteTableId = Lens.lens (\LocalGatewayRouteTable' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRouteTable' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTable)

-- | The tags assigned to the local gateway route table.
localGatewayRouteTable_tags :: Lens.Lens' LocalGatewayRouteTable (Prelude.Maybe [Tag])
localGatewayRouteTable_tags = Lens.lens (\LocalGatewayRouteTable' {tags} -> tags) (\s@LocalGatewayRouteTable' {} a -> s {tags = a} :: LocalGatewayRouteTable) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML LocalGatewayRouteTable where
  parseXML x =
    LocalGatewayRouteTable'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "localGatewayId")
      Prelude.<*> (x Prelude..@? "localGatewayRouteTableArn")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "localGatewayRouteTableId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable LocalGatewayRouteTable

instance Prelude.NFData LocalGatewayRouteTable
