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
-- Module      : Amazonka.EC2.Types.TrunkInterfaceAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrunkInterfaceAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InterfaceProtocolType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Currently available in __limited preview only__. If you are interested
-- in using this feature, contact your account manager.
--
-- Information about an association between a branch network interface with
-- a trunk network interface.
--
-- /See:/ 'newTrunkInterfaceAssociation' smart constructor.
data TrunkInterfaceAssociation = TrunkInterfaceAssociation'
  { -- | The tags for the trunk interface association.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the trunk network interface.
    trunkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the branch network interface.
    branchInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VLAN when you use the VLAN protocol.
    vlanId :: Prelude.Maybe Prelude.Int,
    -- | The interface protocol. Valid values are @VLAN@ and @GRE@.
    interfaceProtocol :: Prelude.Maybe InterfaceProtocolType,
    -- | The application key when you use the GRE protocol.
    greKey :: Prelude.Maybe Prelude.Int,
    -- | The ID of the association.
    associationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrunkInterfaceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'trunkInterfaceAssociation_tags' - The tags for the trunk interface association.
--
-- 'trunkInterfaceId', 'trunkInterfaceAssociation_trunkInterfaceId' - The ID of the trunk network interface.
--
-- 'branchInterfaceId', 'trunkInterfaceAssociation_branchInterfaceId' - The ID of the branch network interface.
--
-- 'vlanId', 'trunkInterfaceAssociation_vlanId' - The ID of the VLAN when you use the VLAN protocol.
--
-- 'interfaceProtocol', 'trunkInterfaceAssociation_interfaceProtocol' - The interface protocol. Valid values are @VLAN@ and @GRE@.
--
-- 'greKey', 'trunkInterfaceAssociation_greKey' - The application key when you use the GRE protocol.
--
-- 'associationId', 'trunkInterfaceAssociation_associationId' - The ID of the association.
newTrunkInterfaceAssociation ::
  TrunkInterfaceAssociation
newTrunkInterfaceAssociation =
  TrunkInterfaceAssociation'
    { tags = Prelude.Nothing,
      trunkInterfaceId = Prelude.Nothing,
      branchInterfaceId = Prelude.Nothing,
      vlanId = Prelude.Nothing,
      interfaceProtocol = Prelude.Nothing,
      greKey = Prelude.Nothing,
      associationId = Prelude.Nothing
    }

-- | The tags for the trunk interface association.
trunkInterfaceAssociation_tags :: Lens.Lens' TrunkInterfaceAssociation (Prelude.Maybe [Tag])
trunkInterfaceAssociation_tags = Lens.lens (\TrunkInterfaceAssociation' {tags} -> tags) (\s@TrunkInterfaceAssociation' {} a -> s {tags = a} :: TrunkInterfaceAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the trunk network interface.
trunkInterfaceAssociation_trunkInterfaceId :: Lens.Lens' TrunkInterfaceAssociation (Prelude.Maybe Prelude.Text)
trunkInterfaceAssociation_trunkInterfaceId = Lens.lens (\TrunkInterfaceAssociation' {trunkInterfaceId} -> trunkInterfaceId) (\s@TrunkInterfaceAssociation' {} a -> s {trunkInterfaceId = a} :: TrunkInterfaceAssociation)

-- | The ID of the branch network interface.
trunkInterfaceAssociation_branchInterfaceId :: Lens.Lens' TrunkInterfaceAssociation (Prelude.Maybe Prelude.Text)
trunkInterfaceAssociation_branchInterfaceId = Lens.lens (\TrunkInterfaceAssociation' {branchInterfaceId} -> branchInterfaceId) (\s@TrunkInterfaceAssociation' {} a -> s {branchInterfaceId = a} :: TrunkInterfaceAssociation)

-- | The ID of the VLAN when you use the VLAN protocol.
trunkInterfaceAssociation_vlanId :: Lens.Lens' TrunkInterfaceAssociation (Prelude.Maybe Prelude.Int)
trunkInterfaceAssociation_vlanId = Lens.lens (\TrunkInterfaceAssociation' {vlanId} -> vlanId) (\s@TrunkInterfaceAssociation' {} a -> s {vlanId = a} :: TrunkInterfaceAssociation)

-- | The interface protocol. Valid values are @VLAN@ and @GRE@.
trunkInterfaceAssociation_interfaceProtocol :: Lens.Lens' TrunkInterfaceAssociation (Prelude.Maybe InterfaceProtocolType)
trunkInterfaceAssociation_interfaceProtocol = Lens.lens (\TrunkInterfaceAssociation' {interfaceProtocol} -> interfaceProtocol) (\s@TrunkInterfaceAssociation' {} a -> s {interfaceProtocol = a} :: TrunkInterfaceAssociation)

-- | The application key when you use the GRE protocol.
trunkInterfaceAssociation_greKey :: Lens.Lens' TrunkInterfaceAssociation (Prelude.Maybe Prelude.Int)
trunkInterfaceAssociation_greKey = Lens.lens (\TrunkInterfaceAssociation' {greKey} -> greKey) (\s@TrunkInterfaceAssociation' {} a -> s {greKey = a} :: TrunkInterfaceAssociation)

-- | The ID of the association.
trunkInterfaceAssociation_associationId :: Lens.Lens' TrunkInterfaceAssociation (Prelude.Maybe Prelude.Text)
trunkInterfaceAssociation_associationId = Lens.lens (\TrunkInterfaceAssociation' {associationId} -> associationId) (\s@TrunkInterfaceAssociation' {} a -> s {associationId = a} :: TrunkInterfaceAssociation)

instance Core.FromXML TrunkInterfaceAssociation where
  parseXML x =
    TrunkInterfaceAssociation'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "trunkInterfaceId")
      Prelude.<*> (x Core..@? "branchInterfaceId")
      Prelude.<*> (x Core..@? "vlanId")
      Prelude.<*> (x Core..@? "interfaceProtocol")
      Prelude.<*> (x Core..@? "greKey")
      Prelude.<*> (x Core..@? "associationId")

instance Prelude.Hashable TrunkInterfaceAssociation where
  hashWithSalt _salt TrunkInterfaceAssociation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trunkInterfaceId
      `Prelude.hashWithSalt` branchInterfaceId
      `Prelude.hashWithSalt` vlanId
      `Prelude.hashWithSalt` interfaceProtocol
      `Prelude.hashWithSalt` greKey
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData TrunkInterfaceAssociation where
  rnf TrunkInterfaceAssociation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trunkInterfaceId
      `Prelude.seq` Prelude.rnf branchInterfaceId
      `Prelude.seq` Prelude.rnf vlanId
      `Prelude.seq` Prelude.rnf interfaceProtocol
      `Prelude.seq` Prelude.rnf greKey
      `Prelude.seq` Prelude.rnf associationId
