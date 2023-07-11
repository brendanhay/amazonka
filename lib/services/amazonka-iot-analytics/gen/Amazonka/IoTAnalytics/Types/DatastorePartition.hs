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
-- Module      : Amazonka.IoTAnalytics.Types.DatastorePartition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastorePartition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.Partition
import Amazonka.IoTAnalytics.Types.TimestampPartition
import qualified Amazonka.Prelude as Prelude

-- | A single dimension to partition a data store. The dimension must be an
-- @AttributePartition@ or a @TimestampPartition@.
--
-- /See:/ 'newDatastorePartition' smart constructor.
data DatastorePartition = DatastorePartition'
  { -- | A partition dimension defined by an @attributeName@.
    attributePartition :: Prelude.Maybe Partition,
    -- | A partition dimension defined by a timestamp attribute.
    timestampPartition :: Prelude.Maybe TimestampPartition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastorePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributePartition', 'datastorePartition_attributePartition' - A partition dimension defined by an @attributeName@.
--
-- 'timestampPartition', 'datastorePartition_timestampPartition' - A partition dimension defined by a timestamp attribute.
newDatastorePartition ::
  DatastorePartition
newDatastorePartition =
  DatastorePartition'
    { attributePartition =
        Prelude.Nothing,
      timestampPartition = Prelude.Nothing
    }

-- | A partition dimension defined by an @attributeName@.
datastorePartition_attributePartition :: Lens.Lens' DatastorePartition (Prelude.Maybe Partition)
datastorePartition_attributePartition = Lens.lens (\DatastorePartition' {attributePartition} -> attributePartition) (\s@DatastorePartition' {} a -> s {attributePartition = a} :: DatastorePartition)

-- | A partition dimension defined by a timestamp attribute.
datastorePartition_timestampPartition :: Lens.Lens' DatastorePartition (Prelude.Maybe TimestampPartition)
datastorePartition_timestampPartition = Lens.lens (\DatastorePartition' {timestampPartition} -> timestampPartition) (\s@DatastorePartition' {} a -> s {timestampPartition = a} :: DatastorePartition)

instance Data.FromJSON DatastorePartition where
  parseJSON =
    Data.withObject
      "DatastorePartition"
      ( \x ->
          DatastorePartition'
            Prelude.<$> (x Data..:? "attributePartition")
            Prelude.<*> (x Data..:? "timestampPartition")
      )

instance Prelude.Hashable DatastorePartition where
  hashWithSalt _salt DatastorePartition' {..} =
    _salt
      `Prelude.hashWithSalt` attributePartition
      `Prelude.hashWithSalt` timestampPartition

instance Prelude.NFData DatastorePartition where
  rnf DatastorePartition' {..} =
    Prelude.rnf attributePartition
      `Prelude.seq` Prelude.rnf timestampPartition

instance Data.ToJSON DatastorePartition where
  toJSON DatastorePartition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributePartition" Data..=)
              Prelude.<$> attributePartition,
            ("timestampPartition" Data..=)
              Prelude.<$> timestampPartition
          ]
      )
