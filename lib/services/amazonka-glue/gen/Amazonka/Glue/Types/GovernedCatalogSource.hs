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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GovernedCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.S3SourceAdditionalOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies the data store in the governed Glue Data Catalog.
--
-- /See:/ 'newGovernedCatalogSource' smart constructor.
data GovernedCatalogSource = GovernedCatalogSource'
  { -- | Specifies additional connection options.
    additionalOptions :: Prelude.Maybe S3SourceAdditionalOptions,
    -- | Partitions satisfying this predicate are deleted. Files within the
    -- retention period in these partitions are not deleted. Set to @\"\"@ –
    -- empty by default.
    partitionPredicate :: Prelude.Maybe Prelude.Text,
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
-- 'additionalOptions', 'governedCatalogSource_additionalOptions' - Specifies additional connection options.
--
-- 'partitionPredicate', 'governedCatalogSource_partitionPredicate' - Partitions satisfying this predicate are deleted. Files within the
-- retention period in these partitions are not deleted. Set to @\"\"@ –
-- empty by default.
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
    { additionalOptions =
        Prelude.Nothing,
      partitionPredicate = Prelude.Nothing,
      name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | Specifies additional connection options.
governedCatalogSource_additionalOptions :: Lens.Lens' GovernedCatalogSource (Prelude.Maybe S3SourceAdditionalOptions)
governedCatalogSource_additionalOptions = Lens.lens (\GovernedCatalogSource' {additionalOptions} -> additionalOptions) (\s@GovernedCatalogSource' {} a -> s {additionalOptions = a} :: GovernedCatalogSource)

-- | Partitions satisfying this predicate are deleted. Files within the
-- retention period in these partitions are not deleted. Set to @\"\"@ –
-- empty by default.
governedCatalogSource_partitionPredicate :: Lens.Lens' GovernedCatalogSource (Prelude.Maybe Prelude.Text)
governedCatalogSource_partitionPredicate = Lens.lens (\GovernedCatalogSource' {partitionPredicate} -> partitionPredicate) (\s@GovernedCatalogSource' {} a -> s {partitionPredicate = a} :: GovernedCatalogSource)

-- | The name of the data store.
governedCatalogSource_name :: Lens.Lens' GovernedCatalogSource Prelude.Text
governedCatalogSource_name = Lens.lens (\GovernedCatalogSource' {name} -> name) (\s@GovernedCatalogSource' {} a -> s {name = a} :: GovernedCatalogSource)

-- | The database to read from.
governedCatalogSource_database :: Lens.Lens' GovernedCatalogSource Prelude.Text
governedCatalogSource_database = Lens.lens (\GovernedCatalogSource' {database} -> database) (\s@GovernedCatalogSource' {} a -> s {database = a} :: GovernedCatalogSource)

-- | The database table to read from.
governedCatalogSource_table :: Lens.Lens' GovernedCatalogSource Prelude.Text
governedCatalogSource_table = Lens.lens (\GovernedCatalogSource' {table} -> table) (\s@GovernedCatalogSource' {} a -> s {table = a} :: GovernedCatalogSource)

instance Data.FromJSON GovernedCatalogSource where
  parseJSON =
    Data.withObject
      "GovernedCatalogSource"
      ( \x ->
          GovernedCatalogSource'
            Prelude.<$> (x Data..:? "AdditionalOptions")
            Prelude.<*> (x Data..:? "PartitionPredicate")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable GovernedCatalogSource where
  hashWithSalt _salt GovernedCatalogSource' {..} =
    _salt `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` partitionPredicate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData GovernedCatalogSource where
  rnf GovernedCatalogSource' {..} =
    Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf partitionPredicate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON GovernedCatalogSource where
  toJSON GovernedCatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("PartitionPredicate" Data..=)
              Prelude.<$> partitionPredicate,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
