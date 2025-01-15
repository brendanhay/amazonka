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
-- Module      : Amazonka.EC2.Types.LocalGatewayRouteTableVpcAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGatewayRouteTableVpcAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an association between a local gateway route table and a VPC.
--
-- /See:/ 'newLocalGatewayRouteTableVpcAssociation' smart constructor.
data LocalGatewayRouteTableVpcAssociation = LocalGatewayRouteTableVpcAssociation'
  { -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table for the
    -- association.
    localGatewayRouteTableArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    localGatewayRouteTableVpcAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the local gateway
    -- route table for the association.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    state :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the association.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalGatewayRouteTableVpcAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayId', 'localGatewayRouteTableVpcAssociation_localGatewayId' - The ID of the local gateway.
--
-- 'localGatewayRouteTableArn', 'localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn' - The Amazon Resource Name (ARN) of the local gateway route table for the
-- association.
--
-- 'localGatewayRouteTableId', 'localGatewayRouteTableVpcAssociation_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'localGatewayRouteTableVpcAssociationId', 'localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId' - The ID of the association.
--
-- 'ownerId', 'localGatewayRouteTableVpcAssociation_ownerId' - The ID of the Amazon Web Services account that owns the local gateway
-- route table for the association.
--
-- 'state', 'localGatewayRouteTableVpcAssociation_state' - The state of the association.
--
-- 'tags', 'localGatewayRouteTableVpcAssociation_tags' - The tags assigned to the association.
--
-- 'vpcId', 'localGatewayRouteTableVpcAssociation_vpcId' - The ID of the VPC.
newLocalGatewayRouteTableVpcAssociation ::
  LocalGatewayRouteTableVpcAssociation
newLocalGatewayRouteTableVpcAssociation =
  LocalGatewayRouteTableVpcAssociation'
    { localGatewayId =
        Prelude.Nothing,
      localGatewayRouteTableArn =
        Prelude.Nothing,
      localGatewayRouteTableId =
        Prelude.Nothing,
      localGatewayRouteTableVpcAssociationId =
        Prelude.Nothing,
      ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ID of the local gateway.
localGatewayRouteTableVpcAssociation_localGatewayId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVpcAssociation_localGatewayId = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {localGatewayId} -> localGatewayId) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {localGatewayId = a} :: LocalGatewayRouteTableVpcAssociation)

-- | The Amazon Resource Name (ARN) of the local gateway route table for the
-- association.
localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVpcAssociation_localGatewayRouteTableArn = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {localGatewayRouteTableArn} -> localGatewayRouteTableArn) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {localGatewayRouteTableArn = a} :: LocalGatewayRouteTableVpcAssociation)

-- | The ID of the local gateway route table.
localGatewayRouteTableVpcAssociation_localGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVpcAssociation_localGatewayRouteTableId = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTableVpcAssociation)

-- | The ID of the association.
localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVpcAssociation_localGatewayRouteTableVpcAssociationId = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {localGatewayRouteTableVpcAssociationId} -> localGatewayRouteTableVpcAssociationId) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {localGatewayRouteTableVpcAssociationId = a} :: LocalGatewayRouteTableVpcAssociation)

-- | The ID of the Amazon Web Services account that owns the local gateway
-- route table for the association.
localGatewayRouteTableVpcAssociation_ownerId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVpcAssociation_ownerId = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {ownerId} -> ownerId) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {ownerId = a} :: LocalGatewayRouteTableVpcAssociation)

-- | The state of the association.
localGatewayRouteTableVpcAssociation_state :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVpcAssociation_state = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {state} -> state) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {state = a} :: LocalGatewayRouteTableVpcAssociation)

-- | The tags assigned to the association.
localGatewayRouteTableVpcAssociation_tags :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe [Tag])
localGatewayRouteTableVpcAssociation_tags = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {tags} -> tags) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {tags = a} :: LocalGatewayRouteTableVpcAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
localGatewayRouteTableVpcAssociation_vpcId :: Lens.Lens' LocalGatewayRouteTableVpcAssociation (Prelude.Maybe Prelude.Text)
localGatewayRouteTableVpcAssociation_vpcId = Lens.lens (\LocalGatewayRouteTableVpcAssociation' {vpcId} -> vpcId) (\s@LocalGatewayRouteTableVpcAssociation' {} a -> s {vpcId = a} :: LocalGatewayRouteTableVpcAssociation)

instance
  Data.FromXML
    LocalGatewayRouteTableVpcAssociation
  where
  parseXML x =
    LocalGatewayRouteTableVpcAssociation'
      Prelude.<$> (x Data..@? "localGatewayId")
      Prelude.<*> (x Data..@? "localGatewayRouteTableArn")
      Prelude.<*> (x Data..@? "localGatewayRouteTableId")
      Prelude.<*> (x Data..@? "localGatewayRouteTableVpcAssociationId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")

instance
  Prelude.Hashable
    LocalGatewayRouteTableVpcAssociation
  where
  hashWithSalt
    _salt
    LocalGatewayRouteTableVpcAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` localGatewayId
        `Prelude.hashWithSalt` localGatewayRouteTableArn
        `Prelude.hashWithSalt` localGatewayRouteTableId
        `Prelude.hashWithSalt` localGatewayRouteTableVpcAssociationId
        `Prelude.hashWithSalt` ownerId
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    LocalGatewayRouteTableVpcAssociation
  where
  rnf LocalGatewayRouteTableVpcAssociation' {..} =
    Prelude.rnf localGatewayId `Prelude.seq`
      Prelude.rnf localGatewayRouteTableArn `Prelude.seq`
        Prelude.rnf localGatewayRouteTableId `Prelude.seq`
          Prelude.rnf localGatewayRouteTableVpcAssociationId `Prelude.seq`
            Prelude.rnf ownerId `Prelude.seq`
              Prelude.rnf state `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf vpcId
