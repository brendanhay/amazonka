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
-- Module      : Amazonka.TimeStreamWrite.Types.Schema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.Schema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.PartitionKey

-- | A Schema specifies the expected data model of the table.
--
-- /See:/ 'newSchema' smart constructor.
data Schema = Schema'
  { -- | A non-empty list of partition keys defining the attributes used to
    -- partition the table data. The order of the list determines the partition
    -- hierarchy. The name and type of each partition key as well as the
    -- partition key order cannot be changed after the table is created.
    -- However, the enforcement level of each partition key can be changed.
    compositePartitionKey :: Prelude.Maybe (Prelude.NonEmpty PartitionKey)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Schema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compositePartitionKey', 'schema_compositePartitionKey' - A non-empty list of partition keys defining the attributes used to
-- partition the table data. The order of the list determines the partition
-- hierarchy. The name and type of each partition key as well as the
-- partition key order cannot be changed after the table is created.
-- However, the enforcement level of each partition key can be changed.
newSchema ::
  Schema
newSchema =
  Schema' {compositePartitionKey = Prelude.Nothing}

-- | A non-empty list of partition keys defining the attributes used to
-- partition the table data. The order of the list determines the partition
-- hierarchy. The name and type of each partition key as well as the
-- partition key order cannot be changed after the table is created.
-- However, the enforcement level of each partition key can be changed.
schema_compositePartitionKey :: Lens.Lens' Schema (Prelude.Maybe (Prelude.NonEmpty PartitionKey))
schema_compositePartitionKey = Lens.lens (\Schema' {compositePartitionKey} -> compositePartitionKey) (\s@Schema' {} a -> s {compositePartitionKey = a} :: Schema) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Schema where
  parseJSON =
    Data.withObject
      "Schema"
      ( \x ->
          Schema'
            Prelude.<$> (x Data..:? "CompositePartitionKey")
      )

instance Prelude.Hashable Schema where
  hashWithSalt _salt Schema' {..} =
    _salt `Prelude.hashWithSalt` compositePartitionKey

instance Prelude.NFData Schema where
  rnf Schema' {..} = Prelude.rnf compositePartitionKey

instance Data.ToJSON Schema where
  toJSON Schema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompositePartitionKey" Data..=)
              Prelude.<$> compositePartitionKey
          ]
      )
