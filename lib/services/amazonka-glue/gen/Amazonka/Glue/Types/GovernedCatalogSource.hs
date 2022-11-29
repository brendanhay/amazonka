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
-- Module      : Amazonka.Glue.Types.GovernedCatalogSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GovernedCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.S3SourceAdditionalOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies the data store in the governed Glue Data Catalog.
--
-- /See:/ 'newGovernedCatalogSource' smart constructor.
data GovernedCatalogSource = GovernedCatalogSource'
  { -- | Partitions satisfying this predicate are deleted. Files within the
    -- retention period in these partitions are not deleted. Set to @\"\"@ –
    -- empty by default.
    partitionPredicate :: Prelude.Maybe Prelude.Text,
    -- | Specifies additional connection options.
    additionalOptions :: Prelude.Maybe S3SourceAdditionalOptions,
    -- | The name of the data store.
    name :: Prelude.Text,
    -- | The database to read from.
    database :: Prelude.Text,
    -- | The database table to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GovernedCatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionPredicate', 'governedCatalogSource_partitionPredicate' - Partitions satisfying this predicate are deleted. Files within the
-- retention period in these partitions are not deleted. Set to @\"\"@ –
-- empty by default.
--
-- 'additionalOptions', 'governedCatalogSource_additionalOptions' - Specifies additional connection options.
--
-- 'name', 'governedCatalogSource_name' - The name of the data store.
--
-- 'database', 'governedCatalogSource_database' - The database to read from.
--
-- 'table', 'governedCatalogSource_table' - The database table to read from.
newGovernedCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  GovernedCatalogSource
newGovernedCatalogSource pName_ pDatabase_ pTable_ =
  GovernedCatalogSource'
    { partitionPredicate =
        Prelude.Nothing,
      additionalOptions = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | Partitions satisfying this predicate are deleted. Files within the
-- retention period in these partitions are not deleted. Set to @\"\"@ –
-- empty by default.
governedCatalogSource_partitionPredicate :: Lens.Lens' GovernedCatalogSource (Prelude.Maybe Prelude.Text)
governedCatalogSource_partitionPredicate = Lens.lens (\GovernedCatalogSource' {partitionPredicate} -> partitionPredicate) (\s@GovernedCatalogSource' {} a -> s {partitionPredicate = a} :: GovernedCatalogSource)

-- | Specifies additional connection options.
governedCatalogSource_additionalOptions :: Lens.Lens' GovernedCatalogSource (Prelude.Maybe S3SourceAdditionalOptions)
governedCatalogSource_additionalOptions = Lens.lens (\GovernedCatalogSource' {additionalOptions} -> additionalOptions) (\s@GovernedCatalogSource' {} a -> s {additionalOptions = a} :: GovernedCatalogSource)

-- | The name of the data store.
governedCatalogSource_name :: Lens.Lens' GovernedCatalogSource Prelude.Text
governedCatalogSource_name = Lens.lens (\GovernedCatalogSource' {name} -> name) (\s@GovernedCatalogSource' {} a -> s {name = a} :: GovernedCatalogSource)

-- | The database to read from.
governedCatalogSource_database :: Lens.Lens' GovernedCatalogSource Prelude.Text
governedCatalogSource_database = Lens.lens (\GovernedCatalogSource' {database} -> database) (\s@GovernedCatalogSource' {} a -> s {database = a} :: GovernedCatalogSource)

-- | The database table to read from.
governedCatalogSource_table :: Lens.Lens' GovernedCatalogSource Prelude.Text
governedCatalogSource_table = Lens.lens (\GovernedCatalogSource' {table} -> table) (\s@GovernedCatalogSource' {} a -> s {table = a} :: GovernedCatalogSource)

instance Core.FromJSON GovernedCatalogSource where
  parseJSON =
    Core.withObject
      "GovernedCatalogSource"
      ( \x ->
          GovernedCatalogSource'
            Prelude.<$> (x Core..:? "PartitionPredicate")
            Prelude.<*> (x Core..:? "AdditionalOptions")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Database")
            Prelude.<*> (x Core..: "Table")
      )

instance Prelude.Hashable GovernedCatalogSource where
  hashWithSalt _salt GovernedCatalogSource' {..} =
    _salt `Prelude.hashWithSalt` partitionPredicate
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData GovernedCatalogSource where
  rnf GovernedCatalogSource' {..} =
    Prelude.rnf partitionPredicate
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Core.ToJSON GovernedCatalogSource where
  toJSON GovernedCatalogSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PartitionPredicate" Core..=)
              Prelude.<$> partitionPredicate,
            ("AdditionalOptions" Core..=)
              Prelude.<$> additionalOptions,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Database" Core..= database),
            Prelude.Just ("Table" Core..= table)
          ]
      )
