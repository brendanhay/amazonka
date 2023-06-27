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
-- Module      : Amazonka.TimeStreamWrite.Types.PartitionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.PartitionKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.PartitionKeyEnforcementLevel
import Amazonka.TimeStreamWrite.Types.PartitionKeyType

-- | An attribute used in partitioning data in a table. A dimension key
-- partitions data using the values of the dimension specified by the
-- dimension-name as partition key, while a measure key partitions data
-- using measure names (values of the \'measure_name\' column).
--
-- /See:/ 'newPartitionKey' smart constructor.
data PartitionKey = PartitionKey'
  { -- | The level of enforcement for the specification of a dimension key in
    -- ingested records. Options are REQUIRED (dimension key must be specified)
    -- and OPTIONAL (dimension key does not have to be specified).
    enforcementInRecord :: Prelude.Maybe PartitionKeyEnforcementLevel,
    -- | The name of the attribute used for a dimension key.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the partition key. Options are DIMENSION (dimension key) and
    -- MEASURE (measure key).
    type' :: PartitionKeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enforcementInRecord', 'partitionKey_enforcementInRecord' - The level of enforcement for the specification of a dimension key in
-- ingested records. Options are REQUIRED (dimension key must be specified)
-- and OPTIONAL (dimension key does not have to be specified).
--
-- 'name', 'partitionKey_name' - The name of the attribute used for a dimension key.
--
-- 'type'', 'partitionKey_type' - The type of the partition key. Options are DIMENSION (dimension key) and
-- MEASURE (measure key).
newPartitionKey ::
  -- | 'type''
  PartitionKeyType ->
  PartitionKey
newPartitionKey pType_ =
  PartitionKey'
    { enforcementInRecord =
        Prelude.Nothing,
      name = Prelude.Nothing,
      type' = pType_
    }

-- | The level of enforcement for the specification of a dimension key in
-- ingested records. Options are REQUIRED (dimension key must be specified)
-- and OPTIONAL (dimension key does not have to be specified).
partitionKey_enforcementInRecord :: Lens.Lens' PartitionKey (Prelude.Maybe PartitionKeyEnforcementLevel)
partitionKey_enforcementInRecord = Lens.lens (\PartitionKey' {enforcementInRecord} -> enforcementInRecord) (\s@PartitionKey' {} a -> s {enforcementInRecord = a} :: PartitionKey)

-- | The name of the attribute used for a dimension key.
partitionKey_name :: Lens.Lens' PartitionKey (Prelude.Maybe Prelude.Text)
partitionKey_name = Lens.lens (\PartitionKey' {name} -> name) (\s@PartitionKey' {} a -> s {name = a} :: PartitionKey)

-- | The type of the partition key. Options are DIMENSION (dimension key) and
-- MEASURE (measure key).
partitionKey_type :: Lens.Lens' PartitionKey PartitionKeyType
partitionKey_type = Lens.lens (\PartitionKey' {type'} -> type') (\s@PartitionKey' {} a -> s {type' = a} :: PartitionKey)

instance Data.FromJSON PartitionKey where
  parseJSON =
    Data.withObject
      "PartitionKey"
      ( \x ->
          PartitionKey'
            Prelude.<$> (x Data..:? "EnforcementInRecord")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable PartitionKey where
  hashWithSalt _salt PartitionKey' {..} =
    _salt
      `Prelude.hashWithSalt` enforcementInRecord
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PartitionKey where
  rnf PartitionKey' {..} =
    Prelude.rnf enforcementInRecord
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON PartitionKey where
  toJSON PartitionKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnforcementInRecord" Data..=)
              Prelude.<$> enforcementInRecord,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("Type" Data..= type')
          ]
      )
