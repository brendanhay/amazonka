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
-- Module      : Network.AWS.DynamoDB.Types.TableAutoScalingDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TableAutoScalingDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
import Network.AWS.DynamoDB.Types.TableStatus
import qualified Network.AWS.Lens as Lens

-- | Represents the auto scaling configuration for a global table.
--
-- /See:/ 'newTableAutoScalingDescription' smart constructor.
data TableAutoScalingDescription = TableAutoScalingDescription'
  { -- | The name of the table.
    tableName :: Core.Maybe Core.Text,
    -- | Represents replicas of the global table.
    replicas :: Core.Maybe [ReplicaAutoScalingDescription],
    -- | The current state of the table:
    --
    -- -   @CREATING@ - The table is being created.
    --
    -- -   @UPDATING@ - The table is being updated.
    --
    -- -   @DELETING@ - The table is being deleted.
    --
    -- -   @ACTIVE@ - The table is ready for use.
    tableStatus :: Core.Maybe TableStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TableAutoScalingDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'tableAutoScalingDescription_tableName' - The name of the table.
--
-- 'replicas', 'tableAutoScalingDescription_replicas' - Represents replicas of the global table.
--
-- 'tableStatus', 'tableAutoScalingDescription_tableStatus' - The current state of the table:
--
-- -   @CREATING@ - The table is being created.
--
-- -   @UPDATING@ - The table is being updated.
--
-- -   @DELETING@ - The table is being deleted.
--
-- -   @ACTIVE@ - The table is ready for use.
newTableAutoScalingDescription ::
  TableAutoScalingDescription
newTableAutoScalingDescription =
  TableAutoScalingDescription'
    { tableName =
        Core.Nothing,
      replicas = Core.Nothing,
      tableStatus = Core.Nothing
    }

-- | The name of the table.
tableAutoScalingDescription_tableName :: Lens.Lens' TableAutoScalingDescription (Core.Maybe Core.Text)
tableAutoScalingDescription_tableName = Lens.lens (\TableAutoScalingDescription' {tableName} -> tableName) (\s@TableAutoScalingDescription' {} a -> s {tableName = a} :: TableAutoScalingDescription)

-- | Represents replicas of the global table.
tableAutoScalingDescription_replicas :: Lens.Lens' TableAutoScalingDescription (Core.Maybe [ReplicaAutoScalingDescription])
tableAutoScalingDescription_replicas = Lens.lens (\TableAutoScalingDescription' {replicas} -> replicas) (\s@TableAutoScalingDescription' {} a -> s {replicas = a} :: TableAutoScalingDescription) Core.. Lens.mapping Lens._Coerce

-- | The current state of the table:
--
-- -   @CREATING@ - The table is being created.
--
-- -   @UPDATING@ - The table is being updated.
--
-- -   @DELETING@ - The table is being deleted.
--
-- -   @ACTIVE@ - The table is ready for use.
tableAutoScalingDescription_tableStatus :: Lens.Lens' TableAutoScalingDescription (Core.Maybe TableStatus)
tableAutoScalingDescription_tableStatus = Lens.lens (\TableAutoScalingDescription' {tableStatus} -> tableStatus) (\s@TableAutoScalingDescription' {} a -> s {tableStatus = a} :: TableAutoScalingDescription)

instance Core.FromJSON TableAutoScalingDescription where
  parseJSON =
    Core.withObject
      "TableAutoScalingDescription"
      ( \x ->
          TableAutoScalingDescription'
            Core.<$> (x Core..:? "TableName")
            Core.<*> (x Core..:? "Replicas" Core..!= Core.mempty)
            Core.<*> (x Core..:? "TableStatus")
      )

instance Core.Hashable TableAutoScalingDescription

instance Core.NFData TableAutoScalingDescription
