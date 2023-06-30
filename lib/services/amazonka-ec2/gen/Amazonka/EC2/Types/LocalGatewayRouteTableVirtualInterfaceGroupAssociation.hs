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
-- Module      : Amazonka.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an association between a local gateway route table and a
-- virtual interface group.
--
-- /See:/ 'newLocalGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data LocalGatewayRouteTableVirtualInterfaceGroupAssociation = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
  { -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table for the
    -- virtual interface group.
    localGatewayRouteTableArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the local gateway
    -- virtual interface group association.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    state :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the association.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalGatewayRouteTableVirtualInterfaceGroupAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId' - The ID of the local gateway.
--
-- 'localGatewayRouteTableArn', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table for the
-- virtual interface group.
--
-- 'localGatewayRouteTableId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociationId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId' - The ID of the association.
--
-- 'localGatewayVirtualInterfaceGroupId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'ownerId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId' - The ID of the Amazon Web Services account that owns the local gateway
-- virtual interface group association.
--
-- 'state', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_state' - The state of the association.
--
-- 'tags', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_tags' - The tags assigned to the association.
newLocalGatewayRouteTableVirtualInterfaceGroupAssociation ::
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation
newLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
    { localGatewayId =
        Prelude.Nothing,
      localGatewayRouteTableArn =
        Prelude.Nothing,
      localGatewayRouteTableId =
        Prelude.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationId =
        Prelude.Nothing,
      localGatewayVirtualInterfaceGroupId =
        Prelude.Nothing,
      ownerId =
        Prelude.Nothing,
      state =
        Prelude.Nothing,
      tags =
        Prelude.Nothing
    }

-- | The ID of the local gateway.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayId} -> localGatewayId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The Amazon Resource Name (ARN) of the local gateway route table for the
-- virtual interface group.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the local gateway route table.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableVirtualInterfaceGroupAssociationId} -> localGatewayRouteTableVirtualInterfaceGroupAssociationId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the virtual interface group.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the Amazon Web Services account that owns the local gateway
-- virtual interface group association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {ownerId} -> ownerId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {ownerId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The state of the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_state :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_state = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {state} -> state) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {state = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The tags assigned to the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_tags :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe [Tag])
localGatewayRouteTableVirtualInterfaceGroupAssociation_tags = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {tags} -> tags) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {tags = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromXML
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  parseXML x =
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
      Prelude.<$> (x Data..@? "localGatewayId")
      Prelude.<*> (x Data..@? "localGatewayRouteTableArn")
      Prelude.<*> (x Data..@? "localGatewayRouteTableId")
      Prelude.<*> ( x
                      Data..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationId"
                  )
      Prelude.<*> (x Data..@? "localGatewayVirtualInterfaceGroupId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  hashWithSalt
    _salt
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` localGatewayId
        `Prelude.hashWithSalt` localGatewayRouteTableArn
        `Prelude.hashWithSalt` localGatewayRouteTableId
        `Prelude.hashWithSalt` localGatewayRouteTableVirtualInterfaceGroupAssociationId
        `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupId
        `Prelude.hashWithSalt` ownerId
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  rnf
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {..} =
      Prelude.rnf localGatewayId
        `Prelude.seq` Prelude.rnf localGatewayRouteTableArn
        `Prelude.seq` Prelude.rnf localGatewayRouteTableId
        `Prelude.seq` Prelude.rnf
          localGatewayRouteTableVirtualInterfaceGroupAssociationId
        `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupId
        `Prelude.seq` Prelude.rnf ownerId
        `Prelude.seq` Prelude.rnf state
        `Prelude.seq` Prelude.rnf tags
