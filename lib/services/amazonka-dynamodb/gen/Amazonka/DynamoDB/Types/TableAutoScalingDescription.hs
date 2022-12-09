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
-- Module      : Amazonka.DynamoDB.Types.TableAutoScalingDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TableAutoScalingDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ReplicaAutoScalingDescription
import Amazonka.DynamoDB.Types.TableStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling configuration for a global table.
--
-- /See:/ 'newTableAutoScalingDescription' smart constructor.
data TableAutoScalingDescription = TableAutoScalingDescription'
  { -- | Represents replicas of the global table.
    replicas :: Prelude.Maybe [ReplicaAutoScalingDescription],
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the table:
    --
    -- -   @CREATING@ - The table is being created.
    --
    -- -   @UPDATING@ - The table is being updated.
    --
    -- -   @DELETING@ - The table is being deleted.
    --
    -- -   @ACTIVE@ - The table is ready for use.
    tableStatus :: Prelude.Maybe TableStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableAutoScalingDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicas', 'tableAutoScalingDescription_replicas' - Represents replicas of the global table.
--
-- 'tableName', 'tableAutoScalingDescription_tableName' - The name of the table.
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
    { replicas =
        Prelude.Nothing,
      tableName = Prelude.Nothing,
      tableStatus = Prelude.Nothing
    }

-- | Represents replicas of the global table.
tableAutoScalingDescription_replicas :: Lens.Lens' TableAutoScalingDescription (Prelude.Maybe [ReplicaAutoScalingDescription])
tableAutoScalingDescription_replicas = Lens.lens (\TableAutoScalingDescription' {replicas} -> replicas) (\s@TableAutoScalingDescription' {} a -> s {replicas = a} :: TableAutoScalingDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the table.
tableAutoScalingDescription_tableName :: Lens.Lens' TableAutoScalingDescription (Prelude.Maybe Prelude.Text)
tableAutoScalingDescription_tableName = Lens.lens (\TableAutoScalingDescription' {tableName} -> tableName) (\s@TableAutoScalingDescription' {} a -> s {tableName = a} :: TableAutoScalingDescription)

-- | The current state of the table:
--
-- -   @CREATING@ - The table is being created.
--
-- -   @UPDATING@ - The table is being updated.
--
-- -   @DELETING@ - The table is being deleted.
--
-- -   @ACTIVE@ - The table is ready for use.
tableAutoScalingDescription_tableStatus :: Lens.Lens' TableAutoScalingDescription (Prelude.Maybe TableStatus)
tableAutoScalingDescription_tableStatus = Lens.lens (\TableAutoScalingDescription' {tableStatus} -> tableStatus) (\s@TableAutoScalingDescription' {} a -> s {tableStatus = a} :: TableAutoScalingDescription)

instance Data.FromJSON TableAutoScalingDescription where
  parseJSON =
    Data.withObject
      "TableAutoScalingDescription"
      ( \x ->
          TableAutoScalingDescription'
            Prelude.<$> (x Data..:? "Replicas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "TableStatus")
      )

instance Prelude.Hashable TableAutoScalingDescription where
  hashWithSalt _salt TableAutoScalingDescription' {..} =
    _salt `Prelude.hashWithSalt` replicas
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` tableStatus

instance Prelude.NFData TableAutoScalingDescription where
  rnf TableAutoScalingDescription' {..} =
    Prelude.rnf replicas
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf tableStatus
