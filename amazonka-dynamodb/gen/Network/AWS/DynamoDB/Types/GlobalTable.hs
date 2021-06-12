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
-- Module      : Network.AWS.DynamoDB.Types.GlobalTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTable where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.Replica
import qualified Network.AWS.Lens as Lens

-- | Represents the properties of a global table.
--
-- /See:/ 'newGlobalTable' smart constructor.
data GlobalTable = GlobalTable'
  { -- | The global table name.
    globalTableName :: Core.Maybe Core.Text,
    -- | The Regions where the global table has replicas.
    replicationGroup :: Core.Maybe [Replica]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GlobalTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableName', 'globalTable_globalTableName' - The global table name.
--
-- 'replicationGroup', 'globalTable_replicationGroup' - The Regions where the global table has replicas.
newGlobalTable ::
  GlobalTable
newGlobalTable =
  GlobalTable'
    { globalTableName = Core.Nothing,
      replicationGroup = Core.Nothing
    }

-- | The global table name.
globalTable_globalTableName :: Lens.Lens' GlobalTable (Core.Maybe Core.Text)
globalTable_globalTableName = Lens.lens (\GlobalTable' {globalTableName} -> globalTableName) (\s@GlobalTable' {} a -> s {globalTableName = a} :: GlobalTable)

-- | The Regions where the global table has replicas.
globalTable_replicationGroup :: Lens.Lens' GlobalTable (Core.Maybe [Replica])
globalTable_replicationGroup = Lens.lens (\GlobalTable' {replicationGroup} -> replicationGroup) (\s@GlobalTable' {} a -> s {replicationGroup = a} :: GlobalTable) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON GlobalTable where
  parseJSON =
    Core.withObject
      "GlobalTable"
      ( \x ->
          GlobalTable'
            Core.<$> (x Core..:? "GlobalTableName")
            Core.<*> (x Core..:? "ReplicationGroup" Core..!= Core.mempty)
      )

instance Core.Hashable GlobalTable

instance Core.NFData GlobalTable
