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
-- Module      : Amazonka.DynamoDB.Types.GlobalTableDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalTableDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.GlobalTableStatus
import Amazonka.DynamoDB.Types.ReplicaDescription
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the global table.
--
-- /See:/ 'newGlobalTableDescription' smart constructor.
data GlobalTableDescription = GlobalTableDescription'
  { -- | The creation time of the global table.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the global table.
    globalTableArn :: Prelude.Maybe Prelude.Text,
    -- | The global table name.
    globalTableName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the global table:
    --
    -- -   @CREATING@ - The global table is being created.
    --
    -- -   @UPDATING@ - The global table is being updated.
    --
    -- -   @DELETING@ - The global table is being deleted.
    --
    -- -   @ACTIVE@ - The global table is ready for use.
    globalTableStatus :: Prelude.Maybe GlobalTableStatus,
    -- | The Regions where the global table has replicas.
    replicationGroup :: Prelude.Maybe [ReplicaDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalTableDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'globalTableDescription_creationDateTime' - The creation time of the global table.
--
-- 'globalTableArn', 'globalTableDescription_globalTableArn' - The unique identifier of the global table.
--
-- 'globalTableName', 'globalTableDescription_globalTableName' - The global table name.
--
-- 'globalTableStatus', 'globalTableDescription_globalTableStatus' - The current state of the global table:
--
-- -   @CREATING@ - The global table is being created.
--
-- -   @UPDATING@ - The global table is being updated.
--
-- -   @DELETING@ - The global table is being deleted.
--
-- -   @ACTIVE@ - The global table is ready for use.
--
-- 'replicationGroup', 'globalTableDescription_replicationGroup' - The Regions where the global table has replicas.
newGlobalTableDescription ::
  GlobalTableDescription
newGlobalTableDescription =
  GlobalTableDescription'
    { creationDateTime =
        Prelude.Nothing,
      globalTableArn = Prelude.Nothing,
      globalTableName = Prelude.Nothing,
      globalTableStatus = Prelude.Nothing,
      replicationGroup = Prelude.Nothing
    }

-- | The creation time of the global table.
globalTableDescription_creationDateTime :: Lens.Lens' GlobalTableDescription (Prelude.Maybe Prelude.UTCTime)
globalTableDescription_creationDateTime = Lens.lens (\GlobalTableDescription' {creationDateTime} -> creationDateTime) (\s@GlobalTableDescription' {} a -> s {creationDateTime = a} :: GlobalTableDescription) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the global table.
globalTableDescription_globalTableArn :: Lens.Lens' GlobalTableDescription (Prelude.Maybe Prelude.Text)
globalTableDescription_globalTableArn = Lens.lens (\GlobalTableDescription' {globalTableArn} -> globalTableArn) (\s@GlobalTableDescription' {} a -> s {globalTableArn = a} :: GlobalTableDescription)

-- | The global table name.
globalTableDescription_globalTableName :: Lens.Lens' GlobalTableDescription (Prelude.Maybe Prelude.Text)
globalTableDescription_globalTableName = Lens.lens (\GlobalTableDescription' {globalTableName} -> globalTableName) (\s@GlobalTableDescription' {} a -> s {globalTableName = a} :: GlobalTableDescription)

-- | The current state of the global table:
--
-- -   @CREATING@ - The global table is being created.
--
-- -   @UPDATING@ - The global table is being updated.
--
-- -   @DELETING@ - The global table is being deleted.
--
-- -   @ACTIVE@ - The global table is ready for use.
globalTableDescription_globalTableStatus :: Lens.Lens' GlobalTableDescription (Prelude.Maybe GlobalTableStatus)
globalTableDescription_globalTableStatus = Lens.lens (\GlobalTableDescription' {globalTableStatus} -> globalTableStatus) (\s@GlobalTableDescription' {} a -> s {globalTableStatus = a} :: GlobalTableDescription)

-- | The Regions where the global table has replicas.
globalTableDescription_replicationGroup :: Lens.Lens' GlobalTableDescription (Prelude.Maybe [ReplicaDescription])
globalTableDescription_replicationGroup = Lens.lens (\GlobalTableDescription' {replicationGroup} -> replicationGroup) (\s@GlobalTableDescription' {} a -> s {replicationGroup = a} :: GlobalTableDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GlobalTableDescription where
  parseJSON =
    Data.withObject
      "GlobalTableDescription"
      ( \x ->
          GlobalTableDescription'
            Prelude.<$> (x Data..:? "CreationDateTime")
            Prelude.<*> (x Data..:? "GlobalTableArn")
            Prelude.<*> (x Data..:? "GlobalTableName")
            Prelude.<*> (x Data..:? "GlobalTableStatus")
            Prelude.<*> ( x
                            Data..:? "ReplicationGroup"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable GlobalTableDescription where
  hashWithSalt _salt GlobalTableDescription' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` globalTableArn
      `Prelude.hashWithSalt` globalTableName
      `Prelude.hashWithSalt` globalTableStatus
      `Prelude.hashWithSalt` replicationGroup

instance Prelude.NFData GlobalTableDescription where
  rnf GlobalTableDescription' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf globalTableArn `Prelude.seq`
        Prelude.rnf globalTableName `Prelude.seq`
          Prelude.rnf globalTableStatus `Prelude.seq`
            Prelude.rnf replicationGroup
