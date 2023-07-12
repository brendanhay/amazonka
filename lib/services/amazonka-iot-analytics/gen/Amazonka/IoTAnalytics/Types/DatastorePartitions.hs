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
-- Module      : Amazonka.IoTAnalytics.Types.DatastorePartitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastorePartitions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.DatastorePartition
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the partition dimensions in a data store.
--
-- /See:/ 'newDatastorePartitions' smart constructor.
data DatastorePartitions = DatastorePartitions'
  { -- | A list of partition dimensions in a data store.
    partitions :: Prelude.Maybe [DatastorePartition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastorePartitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitions', 'datastorePartitions_partitions' - A list of partition dimensions in a data store.
newDatastorePartitions ::
  DatastorePartitions
newDatastorePartitions =
  DatastorePartitions' {partitions = Prelude.Nothing}

-- | A list of partition dimensions in a data store.
datastorePartitions_partitions :: Lens.Lens' DatastorePartitions (Prelude.Maybe [DatastorePartition])
datastorePartitions_partitions = Lens.lens (\DatastorePartitions' {partitions} -> partitions) (\s@DatastorePartitions' {} a -> s {partitions = a} :: DatastorePartitions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DatastorePartitions where
  parseJSON =
    Data.withObject
      "DatastorePartitions"
      ( \x ->
          DatastorePartitions'
            Prelude.<$> (x Data..:? "partitions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DatastorePartitions where
  hashWithSalt _salt DatastorePartitions' {..} =
    _salt `Prelude.hashWithSalt` partitions

instance Prelude.NFData DatastorePartitions where
  rnf DatastorePartitions' {..} = Prelude.rnf partitions

instance Data.ToJSON DatastorePartitions where
  toJSON DatastorePartitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("partitions" Data..=) Prelude.<$> partitions]
      )
