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
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableDescription where

import Network.AWS.DynamoDB.Types.GlobalTableStatus
import Network.AWS.DynamoDB.Types.ReplicaDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about the global table.
--
-- /See:/ 'newGlobalTableDescription' smart constructor.
data GlobalTableDescription = GlobalTableDescription'
  { -- | The global table name.
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
    replicationGroup :: Prelude.Maybe [ReplicaDescription],
    -- | The creation time of the global table.
    creationDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The unique identifier of the global table.
    globalTableArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GlobalTableDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'creationDateTime', 'globalTableDescription_creationDateTime' - The creation time of the global table.
--
-- 'globalTableArn', 'globalTableDescription_globalTableArn' - The unique identifier of the global table.
newGlobalTableDescription ::
  GlobalTableDescription
newGlobalTableDescription =
  GlobalTableDescription'
    { globalTableName =
        Prelude.Nothing,
      globalTableStatus = Prelude.Nothing,
      replicationGroup = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      globalTableArn = Prelude.Nothing
    }

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
globalTableDescription_replicationGroup = Lens.lens (\GlobalTableDescription' {replicationGroup} -> replicationGroup) (\s@GlobalTableDescription' {} a -> s {replicationGroup = a} :: GlobalTableDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The creation time of the global table.
globalTableDescription_creationDateTime :: Lens.Lens' GlobalTableDescription (Prelude.Maybe Prelude.UTCTime)
globalTableDescription_creationDateTime = Lens.lens (\GlobalTableDescription' {creationDateTime} -> creationDateTime) (\s@GlobalTableDescription' {} a -> s {creationDateTime = a} :: GlobalTableDescription) Prelude.. Lens.mapping Prelude._Time

-- | The unique identifier of the global table.
globalTableDescription_globalTableArn :: Lens.Lens' GlobalTableDescription (Prelude.Maybe Prelude.Text)
globalTableDescription_globalTableArn = Lens.lens (\GlobalTableDescription' {globalTableArn} -> globalTableArn) (\s@GlobalTableDescription' {} a -> s {globalTableArn = a} :: GlobalTableDescription)

instance Prelude.FromJSON GlobalTableDescription where
  parseJSON =
    Prelude.withObject
      "GlobalTableDescription"
      ( \x ->
          GlobalTableDescription'
            Prelude.<$> (x Prelude..:? "GlobalTableName")
            Prelude.<*> (x Prelude..:? "GlobalTableStatus")
            Prelude.<*> ( x Prelude..:? "ReplicationGroup"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CreationDateTime")
            Prelude.<*> (x Prelude..:? "GlobalTableArn")
      )

instance Prelude.Hashable GlobalTableDescription

instance Prelude.NFData GlobalTableDescription
