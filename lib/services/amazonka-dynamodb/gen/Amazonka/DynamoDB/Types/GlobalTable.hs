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
-- Module      : Amazonka.DynamoDB.Types.GlobalTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.Replica
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a global table.
--
-- /See:/ 'newGlobalTable' smart constructor.
data GlobalTable = GlobalTable'
  { -- | The global table name.
    globalTableName :: Prelude.Maybe Prelude.Text,
    -- | The Regions where the global table has replicas.
    replicationGroup :: Prelude.Maybe [Replica]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { globalTableName = Prelude.Nothing,
      replicationGroup = Prelude.Nothing
    }

-- | The global table name.
globalTable_globalTableName :: Lens.Lens' GlobalTable (Prelude.Maybe Prelude.Text)
globalTable_globalTableName = Lens.lens (\GlobalTable' {globalTableName} -> globalTableName) (\s@GlobalTable' {} a -> s {globalTableName = a} :: GlobalTable)

-- | The Regions where the global table has replicas.
globalTable_replicationGroup :: Lens.Lens' GlobalTable (Prelude.Maybe [Replica])
globalTable_replicationGroup = Lens.lens (\GlobalTable' {replicationGroup} -> replicationGroup) (\s@GlobalTable' {} a -> s {replicationGroup = a} :: GlobalTable) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GlobalTable where
  parseJSON =
    Data.withObject
      "GlobalTable"
      ( \x ->
          GlobalTable'
            Prelude.<$> (x Data..:? "GlobalTableName")
            Prelude.<*> ( x Data..:? "ReplicationGroup"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GlobalTable where
  hashWithSalt _salt GlobalTable' {..} =
    _salt `Prelude.hashWithSalt` globalTableName
      `Prelude.hashWithSalt` replicationGroup

instance Prelude.NFData GlobalTable where
  rnf GlobalTable' {..} =
    Prelude.rnf globalTableName
      `Prelude.seq` Prelude.rnf replicationGroup
