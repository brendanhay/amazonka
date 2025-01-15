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
-- Module      : Amazonka.ElastiCache.Types.GlobalNodeGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.GlobalNodeGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates the slot configuration and global identifier for a slice
-- group.
--
-- /See:/ 'newGlobalNodeGroup' smart constructor.
data GlobalNodeGroup = GlobalNodeGroup'
  { -- | The name of the global node group
    globalNodeGroupId :: Prelude.Maybe Prelude.Text,
    -- | The keyspace for this node group
    slots :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalNodeGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNodeGroupId', 'globalNodeGroup_globalNodeGroupId' - The name of the global node group
--
-- 'slots', 'globalNodeGroup_slots' - The keyspace for this node group
newGlobalNodeGroup ::
  GlobalNodeGroup
newGlobalNodeGroup =
  GlobalNodeGroup'
    { globalNodeGroupId =
        Prelude.Nothing,
      slots = Prelude.Nothing
    }

-- | The name of the global node group
globalNodeGroup_globalNodeGroupId :: Lens.Lens' GlobalNodeGroup (Prelude.Maybe Prelude.Text)
globalNodeGroup_globalNodeGroupId = Lens.lens (\GlobalNodeGroup' {globalNodeGroupId} -> globalNodeGroupId) (\s@GlobalNodeGroup' {} a -> s {globalNodeGroupId = a} :: GlobalNodeGroup)

-- | The keyspace for this node group
globalNodeGroup_slots :: Lens.Lens' GlobalNodeGroup (Prelude.Maybe Prelude.Text)
globalNodeGroup_slots = Lens.lens (\GlobalNodeGroup' {slots} -> slots) (\s@GlobalNodeGroup' {} a -> s {slots = a} :: GlobalNodeGroup)

instance Data.FromXML GlobalNodeGroup where
  parseXML x =
    GlobalNodeGroup'
      Prelude.<$> (x Data..@? "GlobalNodeGroupId")
      Prelude.<*> (x Data..@? "Slots")

instance Prelude.Hashable GlobalNodeGroup where
  hashWithSalt _salt GlobalNodeGroup' {..} =
    _salt
      `Prelude.hashWithSalt` globalNodeGroupId
      `Prelude.hashWithSalt` slots

instance Prelude.NFData GlobalNodeGroup where
  rnf GlobalNodeGroup' {..} =
    Prelude.rnf globalNodeGroupId `Prelude.seq`
      Prelude.rnf slots
