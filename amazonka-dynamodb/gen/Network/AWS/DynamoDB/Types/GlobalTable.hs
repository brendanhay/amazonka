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
-- Module      : Network.AWS.DynamoDB.Types.GlobalTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTable where

import Network.AWS.DynamoDB.Types.Replica
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of a global table.
--
-- /See:/ 'newGlobalTable' smart constructor.
data GlobalTable = GlobalTable'
  { -- | The global table name.
    globalTableName :: Prelude.Maybe Prelude.Text,
    -- | The Regions where the global table has replicas.
    replicationGroup :: Prelude.Maybe [Replica]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
globalTable_replicationGroup = Lens.lens (\GlobalTable' {replicationGroup} -> replicationGroup) (\s@GlobalTable' {} a -> s {replicationGroup = a} :: GlobalTable) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON GlobalTable where
  parseJSON =
    Prelude.withObject
      "GlobalTable"
      ( \x ->
          GlobalTable'
            Prelude.<$> (x Prelude..:? "GlobalTableName")
            Prelude.<*> ( x Prelude..:? "ReplicationGroup"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GlobalTable

instance Prelude.NFData GlobalTable
