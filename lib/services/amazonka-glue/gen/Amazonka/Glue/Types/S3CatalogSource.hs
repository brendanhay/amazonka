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
-- Module      : Amazonka.Glue.Types.S3CatalogSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3CatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.S3SourceAdditionalOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon S3 data store in the Glue Data Catalog.
--
-- /See:/ 'newS3CatalogSource' smart constructor.
data S3CatalogSource = S3CatalogSource'
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
-- Create a value of 'S3CatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionPredicate', 's3CatalogSource_partitionPredicate' - Partitions satisfying this predicate are deleted. Files within the
-- retention period in these partitions are not deleted. Set to @\"\"@ –
-- empty by default.
--
-- 'additionalOptions', 's3CatalogSource_additionalOptions' - Specifies additional connection options.
--
-- 'name', 's3CatalogSource_name' - The name of the data store.
--
-- 'database', 's3CatalogSource_database' - The database to read from.
--
-- 'table', 's3CatalogSource_table' - The database table to read from.
newS3CatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  S3CatalogSource
newS3CatalogSource pName_ pDatabase_ pTable_ =
  S3CatalogSource'
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
s3CatalogSource_partitionPredicate :: Lens.Lens' S3CatalogSource (Prelude.Maybe Prelude.Text)
s3CatalogSource_partitionPredicate = Lens.lens (\S3CatalogSource' {partitionPredicate} -> partitionPredicate) (\s@S3CatalogSource' {} a -> s {partitionPredicate = a} :: S3CatalogSource)

-- | Specifies additional connection options.
s3CatalogSource_additionalOptions :: Lens.Lens' S3CatalogSource (Prelude.Maybe S3SourceAdditionalOptions)
s3CatalogSource_additionalOptions = Lens.lens (\S3CatalogSource' {additionalOptions} -> additionalOptions) (\s@S3CatalogSource' {} a -> s {additionalOptions = a} :: S3CatalogSource)

-- | The name of the data store.
s3CatalogSource_name :: Lens.Lens' S3CatalogSource Prelude.Text
s3CatalogSource_name = Lens.lens (\S3CatalogSource' {name} -> name) (\s@S3CatalogSource' {} a -> s {name = a} :: S3CatalogSource)

-- | The database to read from.
s3CatalogSource_database :: Lens.Lens' S3CatalogSource Prelude.Text
s3CatalogSource_database = Lens.lens (\S3CatalogSource' {database} -> database) (\s@S3CatalogSource' {} a -> s {database = a} :: S3CatalogSource)

-- | The database table to read from.
s3CatalogSource_table :: Lens.Lens' S3CatalogSource Prelude.Text
s3CatalogSource_table = Lens.lens (\S3CatalogSource' {table} -> table) (\s@S3CatalogSource' {} a -> s {table = a} :: S3CatalogSource)

instance Data.FromJSON S3CatalogSource where
  parseJSON =
    Data.withObject
      "S3CatalogSource"
      ( \x ->
          S3CatalogSource'
            Prelude.<$> (x Data..:? "PartitionPredicate")
            Prelude.<*> (x Data..:? "AdditionalOptions")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable S3CatalogSource where
  hashWithSalt _salt S3CatalogSource' {..} =
    _salt `Prelude.hashWithSalt` partitionPredicate
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData S3CatalogSource where
  rnf S3CatalogSource' {..} =
    Prelude.rnf partitionPredicate
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON S3CatalogSource where
  toJSON S3CatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PartitionPredicate" Data..=)
              Prelude.<$> partitionPredicate,
            ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )
