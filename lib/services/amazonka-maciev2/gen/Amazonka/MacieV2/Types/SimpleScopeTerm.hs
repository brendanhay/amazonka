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
-- Module      : Amazonka.MacieV2.Types.SimpleScopeTerm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SimpleScopeTerm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.JobComparator
import Amazonka.MacieV2.Types.ScopeFilterKey
import qualified Amazonka.Prelude as Prelude

-- | Specifies a property-based condition that determines whether an S3
-- object is included or excluded from a classification job.
--
-- /See:/ 'newSimpleScopeTerm' smart constructor.
data SimpleScopeTerm = SimpleScopeTerm'
  { -- | The operator to use in the condition. Valid values for each supported
    -- property (key) are:
    --
    -- -   OBJECT_EXTENSION - EQ (equals) or NE (not equals)
    --
    -- -   OBJECT_KEY - STARTS_WITH
    --
    -- -   OBJECT_LAST_MODIFIED_DATE - Any operator except CONTAINS
    --
    -- -   OBJECT_SIZE - Any operator except CONTAINS
    comparator :: Prelude.Maybe JobComparator,
    -- | The object property to use in the condition.
    key :: Prelude.Maybe ScopeFilterKey,
    -- | An array that lists the values to use in the condition. If the value for
    -- the key property is OBJECT_EXTENSION or OBJECT_KEY, this array can
    -- specify multiple values and Amazon Macie uses OR logic to join the
    -- values. Otherwise, this array can specify only one value.
    --
    -- Valid values for each supported property (key) are:
    --
    -- -   OBJECT_EXTENSION - A string that represents the file name extension
    --     of an object. For example: docx or pdf
    --
    -- -   OBJECT_KEY - A string that represents the key prefix (folder name or
    --     path) of an object. For example: logs or awslogs\/eventlogs. This
    --     value applies a condition to objects whose keys (names) begin with
    --     the specified value.
    --
    -- -   OBJECT_LAST_MODIFIED_DATE - The date and time (in UTC and extended
    --     ISO 8601 format) when an object was created or last changed,
    --     whichever is latest. For example: 2020-09-28T14:31:13Z
    --
    -- -   OBJECT_SIZE - An integer that represents the storage size (in bytes)
    --     of an object.
    --
    -- Macie doesn\'t support use of wildcard characters in these values. Also,
    -- string values are case sensitive.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimpleScopeTerm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparator', 'simpleScopeTerm_comparator' - The operator to use in the condition. Valid values for each supported
-- property (key) are:
--
-- -   OBJECT_EXTENSION - EQ (equals) or NE (not equals)
--
-- -   OBJECT_KEY - STARTS_WITH
--
-- -   OBJECT_LAST_MODIFIED_DATE - Any operator except CONTAINS
--
-- -   OBJECT_SIZE - Any operator except CONTAINS
--
-- 'key', 'simpleScopeTerm_key' - The object property to use in the condition.
--
-- 'values', 'simpleScopeTerm_values' - An array that lists the values to use in the condition. If the value for
-- the key property is OBJECT_EXTENSION or OBJECT_KEY, this array can
-- specify multiple values and Amazon Macie uses OR logic to join the
-- values. Otherwise, this array can specify only one value.
--
-- Valid values for each supported property (key) are:
--
-- -   OBJECT_EXTENSION - A string that represents the file name extension
--     of an object. For example: docx or pdf
--
-- -   OBJECT_KEY - A string that represents the key prefix (folder name or
--     path) of an object. For example: logs or awslogs\/eventlogs. This
--     value applies a condition to objects whose keys (names) begin with
--     the specified value.
--
-- -   OBJECT_LAST_MODIFIED_DATE - The date and time (in UTC and extended
--     ISO 8601 format) when an object was created or last changed,
--     whichever is latest. For example: 2020-09-28T14:31:13Z
--
-- -   OBJECT_SIZE - An integer that represents the storage size (in bytes)
--     of an object.
--
-- Macie doesn\'t support use of wildcard characters in these values. Also,
-- string values are case sensitive.
newSimpleScopeTerm ::
  SimpleScopeTerm
newSimpleScopeTerm =
  SimpleScopeTerm'
    { comparator = Prelude.Nothing,
      key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The operator to use in the condition. Valid values for each supported
-- property (key) are:
--
-- -   OBJECT_EXTENSION - EQ (equals) or NE (not equals)
--
-- -   OBJECT_KEY - STARTS_WITH
--
-- -   OBJECT_LAST_MODIFIED_DATE - Any operator except CONTAINS
--
-- -   OBJECT_SIZE - Any operator except CONTAINS
simpleScopeTerm_comparator :: Lens.Lens' SimpleScopeTerm (Prelude.Maybe JobComparator)
simpleScopeTerm_comparator = Lens.lens (\SimpleScopeTerm' {comparator} -> comparator) (\s@SimpleScopeTerm' {} a -> s {comparator = a} :: SimpleScopeTerm)

-- | The object property to use in the condition.
simpleScopeTerm_key :: Lens.Lens' SimpleScopeTerm (Prelude.Maybe ScopeFilterKey)
simpleScopeTerm_key = Lens.lens (\SimpleScopeTerm' {key} -> key) (\s@SimpleScopeTerm' {} a -> s {key = a} :: SimpleScopeTerm)

-- | An array that lists the values to use in the condition. If the value for
-- the key property is OBJECT_EXTENSION or OBJECT_KEY, this array can
-- specify multiple values and Amazon Macie uses OR logic to join the
-- values. Otherwise, this array can specify only one value.
--
-- Valid values for each supported property (key) are:
--
-- -   OBJECT_EXTENSION - A string that represents the file name extension
--     of an object. For example: docx or pdf
--
-- -   OBJECT_KEY - A string that represents the key prefix (folder name or
--     path) of an object. For example: logs or awslogs\/eventlogs. This
--     value applies a condition to objects whose keys (names) begin with
--     the specified value.
--
-- -   OBJECT_LAST_MODIFIED_DATE - The date and time (in UTC and extended
--     ISO 8601 format) when an object was created or last changed,
--     whichever is latest. For example: 2020-09-28T14:31:13Z
--
-- -   OBJECT_SIZE - An integer that represents the storage size (in bytes)
--     of an object.
--
-- Macie doesn\'t support use of wildcard characters in these values. Also,
-- string values are case sensitive.
simpleScopeTerm_values :: Lens.Lens' SimpleScopeTerm (Prelude.Maybe [Prelude.Text])
simpleScopeTerm_values = Lens.lens (\SimpleScopeTerm' {values} -> values) (\s@SimpleScopeTerm' {} a -> s {values = a} :: SimpleScopeTerm) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SimpleScopeTerm where
  parseJSON =
    Data.withObject
      "SimpleScopeTerm"
      ( \x ->
          SimpleScopeTerm'
            Prelude.<$> (x Data..:? "comparator")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SimpleScopeTerm where
  hashWithSalt _salt SimpleScopeTerm' {..} =
    _salt `Prelude.hashWithSalt` comparator
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData SimpleScopeTerm where
  rnf SimpleScopeTerm' {..} =
    Prelude.rnf comparator
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON SimpleScopeTerm where
  toJSON SimpleScopeTerm' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comparator" Data..=) Prelude.<$> comparator,
            ("key" Data..=) Prelude.<$> key,
            ("values" Data..=) Prelude.<$> values
          ]
      )
