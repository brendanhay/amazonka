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
-- Module      : Amazonka.EC2.Types.LocalGatewayVirtualInterfaceGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LocalGatewayVirtualInterfaceGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a local gateway virtual interface group.
--
-- /See:/ 'newLocalGatewayVirtualInterfaceGroup' smart constructor.
data LocalGatewayVirtualInterfaceGroup = LocalGatewayVirtualInterfaceGroup'
  { -- | The ID of the local gateway.
    localGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the virtual interfaces.
    localGatewayVirtualInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Amazon Web Services account that owns the local gateway
    -- virtual interface group.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the virtual interface group.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalGatewayVirtualInterfaceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayId', 'localGatewayVirtualInterfaceGroup_localGatewayId' - The ID of the local gateway.
--
-- 'localGatewayVirtualInterfaceGroupId', 'localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
--
-- 'localGatewayVirtualInterfaceIds', 'localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds' - The IDs of the virtual interfaces.
--
-- 'ownerId', 'localGatewayVirtualInterfaceGroup_ownerId' - The ID of the Amazon Web Services account that owns the local gateway
-- virtual interface group.
--
-- 'tags', 'localGatewayVirtualInterfaceGroup_tags' - The tags assigned to the virtual interface group.
newLocalGatewayVirtualInterfaceGroup ::
  LocalGatewayVirtualInterfaceGroup
newLocalGatewayVirtualInterfaceGroup =
  LocalGatewayVirtualInterfaceGroup'
    { localGatewayId =
        Prelude.Nothing,
      localGatewayVirtualInterfaceGroupId =
        Prelude.Nothing,
      localGatewayVirtualInterfaceIds =
        Prelude.Nothing,
      ownerId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the local gateway.
localGatewayVirtualInterfaceGroup_localGatewayId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterfaceGroup_localGatewayId = Lens.lens (\LocalGatewayVirtualInterfaceGroup' {localGatewayId} -> localGatewayId) (\s@LocalGatewayVirtualInterfaceGroup' {} a -> s {localGatewayId = a} :: LocalGatewayVirtualInterfaceGroup)

-- | The ID of the virtual interface group.
localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceGroupId = Lens.lens (\LocalGatewayVirtualInterfaceGroup' {localGatewayVirtualInterfaceGroupId} -> localGatewayVirtualInterfaceGroupId) (\s@LocalGatewayVirtualInterfaceGroup' {} a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayVirtualInterfaceGroup)

-- | The IDs of the virtual interfaces.
localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Prelude.Maybe [Prelude.Text])
localGatewayVirtualInterfaceGroup_localGatewayVirtualInterfaceIds = Lens.lens (\LocalGatewayVirtualInterfaceGroup' {localGatewayVirtualInterfaceIds} -> localGatewayVirtualInterfaceIds) (\s@LocalGatewayVirtualInterfaceGroup' {} a -> s {localGatewayVirtualInterfaceIds = a} :: LocalGatewayVirtualInterfaceGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the local gateway
-- virtual interface group.
localGatewayVirtualInterfaceGroup_ownerId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Prelude.Maybe Prelude.Text)
localGatewayVirtualInterfaceGroup_ownerId = Lens.lens (\LocalGatewayVirtualInterfaceGroup' {ownerId} -> ownerId) (\s@LocalGatewayVirtualInterfaceGroup' {} a -> s {ownerId = a} :: LocalGatewayVirtualInterfaceGroup)

-- | The tags assigned to the virtual interface group.
localGatewayVirtualInterfaceGroup_tags :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Prelude.Maybe [Tag])
localGatewayVirtualInterfaceGroup_tags = Lens.lens (\LocalGatewayVirtualInterfaceGroup' {tags} -> tags) (\s@LocalGatewayVirtualInterfaceGroup' {} a -> s {tags = a} :: LocalGatewayVirtualInterfaceGroup) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromXML
    LocalGatewayVirtualInterfaceGroup
  where
  parseXML x =
    LocalGatewayVirtualInterfaceGroup'
      Prelude.<$> (x Data..@? "localGatewayId")
      Prelude.<*> (x Data..@? "localGatewayVirtualInterfaceGroupId")
      Prelude.<*> ( x Data..@? "localGatewayVirtualInterfaceIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    LocalGatewayVirtualInterfaceGroup
  where
  hashWithSalt
    _salt
    LocalGatewayVirtualInterfaceGroup' {..} =
      _salt `Prelude.hashWithSalt` localGatewayId
        `Prelude.hashWithSalt` localGatewayVirtualInterfaceGroupId
        `Prelude.hashWithSalt` localGatewayVirtualInterfaceIds
        `Prelude.hashWithSalt` ownerId
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    LocalGatewayVirtualInterfaceGroup
  where
  rnf LocalGatewayVirtualInterfaceGroup' {..} =
    Prelude.rnf localGatewayId
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceGroupId
      `Prelude.seq` Prelude.rnf localGatewayVirtualInterfaceIds
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf tags
