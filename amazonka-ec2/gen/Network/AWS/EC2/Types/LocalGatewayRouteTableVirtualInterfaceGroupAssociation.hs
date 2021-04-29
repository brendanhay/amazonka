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
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an association between a local gateway route table and a
-- virtual interface group.
--
-- /See:/ 'newLocalGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data LocalGatewayRouteTableVirtualInterfaceGroupAssociation = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
  { -- | The AWS account ID that owns the local gateway virtual interface group
    -- association.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table for the
    -- virtual interface group.
    localGatewayRouteTableArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    state :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the association.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LocalGatewayRouteTableVirtualInterfaceGroupAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId' - The AWS account ID that owns the local gateway virtual interface group
-- association.
--
-- 'localGatewayVirtualInterfaceGroupId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'localGatewayId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId' - The ID of the local gateway.
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociationId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId' - The ID of the association.
--
-- 'localGatewayRouteTableArn', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table for the
-- virtual interface group.
--
-- 'state', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_state' - The state of the association.
--
-- 'localGatewayRouteTableId', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'tags', 'localGatewayRouteTableVirtualInterfaceGroupAssociation_tags' - The tags assigned to the association.
newLocalGatewayRouteTableVirtualInterfaceGroupAssociation ::
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation
newLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
    { ownerId =
        Prelude.Nothing,
      localGatewayVirtualInterfaceGroupId =
        Prelude.Nothing,
      localGatewayId =
        Prelude.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationId =
        Prelude.Nothing,
      localGatewayRouteTableArn =
        Prelude.Nothing,
      state =
        Prelude.Nothing,
      localGatewayRouteTableId =
        Prelude.Nothing,
      tags =
        Prelude.Nothing
    }

-- | The AWS account ID that owns the local gateway virtual interface group
-- association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_ownerId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {ownerId} -> ownerId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {ownerId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the virtual interface group.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayVirtualInterfaceGroupId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the local gateway.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayId} -> localGatewayId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableVirtualInterfaceGroupAssociationId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableVirtualInterfaceGroupAssociationId} -> localGatewayRouteTableVirtualInterfaceGroupAssociationId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The Amazon Resource Name (ARN) of the local gateway route table for the
-- virtual interface group.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The state of the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_state :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_state = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {state} -> state) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {state = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The ID of the local gateway route table.
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVirtualInterfaceGroupAssociation_localGatewayRouteTableId = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)

-- | The tags assigned to the association.
localGatewayRouteTableVirtualInterfaceGroupAssociation_tags :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Prelude.Maybe [Tag])
localGatewayRouteTableVirtualInterfaceGroupAssociation_tags = Lens.lens (\LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {tags} -> tags) (\s@LocalGatewayRouteTableVirtualInterfaceGroupAssociation' {} a -> s {tags = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  parseXML x =
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
      Prelude.<$> (x Prelude..@? "ownerId")
        Prelude.<*> (x Prelude..@? "localGatewayVirtualInterfaceGroupId")
        Prelude.<*> (x Prelude..@? "localGatewayId")
        Prelude.<*> ( x
                        Prelude..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationId"
                    )
        Prelude.<*> (x Prelude..@? "localGatewayRouteTableArn")
        Prelude.<*> (x Prelude..@? "state")
        Prelude.<*> (x Prelude..@? "localGatewayRouteTableId")
        Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                        Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                    )

instance
  Prelude.Hashable
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation

instance
  Prelude.NFData
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
