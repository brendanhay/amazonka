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
-- Module      : Amazonka.Glue.Types.StorageDescriptor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StorageDescriptor where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.Column
import Amazonka.Glue.Types.Order
import Amazonka.Glue.Types.SchemaReference
import Amazonka.Glue.Types.SerDeInfo
import Amazonka.Glue.Types.SkewedInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the physical storage of table data.
--
-- /See:/ 'newStorageDescriptor' smart constructor.
data StorageDescriptor = StorageDescriptor'
  { -- | A list specifying the sort order of each bucket in the table.
    sortColumns :: Prelude.Maybe [Order],
    -- | @True@ if the data in the table is compressed, or @False@ if not.
    compressed :: Prelude.Maybe Prelude.Bool,
    -- | The physical location of the table. By default, this takes the form of
    -- the warehouse location, followed by the database location in the
    -- warehouse, followed by the table name.
    location :: Prelude.Maybe Prelude.Text,
    -- | A list of reducer grouping columns, clustering columns, and bucketing
    -- columns in the table.
    bucketColumns :: Prelude.Maybe [Prelude.Text],
    -- | The serialization\/deserialization (SerDe) information.
    serdeInfo :: Prelude.Maybe SerDeInfo,
    -- | The output format: @SequenceFileOutputFormat@ (binary), or
    -- @IgnoreKeyTextOutputFormat@, or a custom format.
    outputFormat :: Prelude.Maybe Prelude.Text,
    -- | Must be specified if the table contains any dimension columns.
    numberOfBuckets :: Prelude.Maybe Prelude.Int,
    -- | An object that references a schema stored in the Glue Schema Registry.
    --
    -- When creating a table, you can pass an empty list of columns for the
    -- schema, and instead use a schema reference.
    schemaReference :: Prelude.Maybe SchemaReference,
    -- | @True@ if the table data is stored in subdirectories, or @False@ if not.
    storedAsSubDirectories :: Prelude.Maybe Prelude.Bool,
    -- | The user-supplied properties in key-value form.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The input format: @SequenceFileInputFormat@ (binary), or
    -- @TextInputFormat@, or a custom format.
    inputFormat :: Prelude.Maybe Prelude.Text,
    -- | The information about values that appear frequently in a column (skewed
    -- values).
    skewedInfo :: Prelude.Maybe SkewedInfo,
    -- | A list of the @Columns@ in the table.
    columns :: Prelude.Maybe [Column]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortColumns', 'storageDescriptor_sortColumns' - A list specifying the sort order of each bucket in the table.
--
-- 'compressed', 'storageDescriptor_compressed' - @True@ if the data in the table is compressed, or @False@ if not.
--
-- 'location', 'storageDescriptor_location' - The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
--
-- 'bucketColumns', 'storageDescriptor_bucketColumns' - A list of reducer grouping columns, clustering columns, and bucketing
-- columns in the table.
--
-- 'serdeInfo', 'storageDescriptor_serdeInfo' - The serialization\/deserialization (SerDe) information.
--
-- 'outputFormat', 'storageDescriptor_outputFormat' - The output format: @SequenceFileOutputFormat@ (binary), or
-- @IgnoreKeyTextOutputFormat@, or a custom format.
--
-- 'numberOfBuckets', 'storageDescriptor_numberOfBuckets' - Must be specified if the table contains any dimension columns.
--
-- 'schemaReference', 'storageDescriptor_schemaReference' - An object that references a schema stored in the Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
--
-- 'storedAsSubDirectories', 'storageDescriptor_storedAsSubDirectories' - @True@ if the table data is stored in subdirectories, or @False@ if not.
--
-- 'parameters', 'storageDescriptor_parameters' - The user-supplied properties in key-value form.
--
-- 'inputFormat', 'storageDescriptor_inputFormat' - The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
--
-- 'skewedInfo', 'storageDescriptor_skewedInfo' - The information about values that appear frequently in a column (skewed
-- values).
--
-- 'columns', 'storageDescriptor_columns' - A list of the @Columns@ in the table.
newStorageDescriptor ::
  StorageDescriptor
newStorageDescriptor =
  StorageDescriptor'
    { sortColumns = Prelude.Nothing,
      compressed = Prelude.Nothing,
      location = Prelude.Nothing,
      bucketColumns = Prelude.Nothing,
      serdeInfo = Prelude.Nothing,
      outputFormat = Prelude.Nothing,
      numberOfBuckets = Prelude.Nothing,
      schemaReference = Prelude.Nothing,
      storedAsSubDirectories = Prelude.Nothing,
      parameters = Prelude.Nothing,
      inputFormat = Prelude.Nothing,
      skewedInfo = Prelude.Nothing,
      columns = Prelude.Nothing
    }

-- | A list specifying the sort order of each bucket in the table.
storageDescriptor_sortColumns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Order])
storageDescriptor_sortColumns = Lens.lens (\StorageDescriptor' {sortColumns} -> sortColumns) (\s@StorageDescriptor' {} a -> s {sortColumns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | @True@ if the data in the table is compressed, or @False@ if not.
storageDescriptor_compressed :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Bool)
storageDescriptor_compressed = Lens.lens (\StorageDescriptor' {compressed} -> compressed) (\s@StorageDescriptor' {} a -> s {compressed = a} :: StorageDescriptor)

-- | The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
storageDescriptor_location :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_location = Lens.lens (\StorageDescriptor' {location} -> location) (\s@StorageDescriptor' {} a -> s {location = a} :: StorageDescriptor)

-- | A list of reducer grouping columns, clustering columns, and bucketing
-- columns in the table.
storageDescriptor_bucketColumns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Prelude.Text])
storageDescriptor_bucketColumns = Lens.lens (\StorageDescriptor' {bucketColumns} -> bucketColumns) (\s@StorageDescriptor' {} a -> s {bucketColumns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | The serialization\/deserialization (SerDe) information.
storageDescriptor_serdeInfo :: Lens.Lens' StorageDescriptor (Prelude.Maybe SerDeInfo)
storageDescriptor_serdeInfo = Lens.lens (\StorageDescriptor' {serdeInfo} -> serdeInfo) (\s@StorageDescriptor' {} a -> s {serdeInfo = a} :: StorageDescriptor)

-- | The output format: @SequenceFileOutputFormat@ (binary), or
-- @IgnoreKeyTextOutputFormat@, or a custom format.
storageDescriptor_outputFormat :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_outputFormat = Lens.lens (\StorageDescriptor' {outputFormat} -> outputFormat) (\s@StorageDescriptor' {} a -> s {outputFormat = a} :: StorageDescriptor)

-- | Must be specified if the table contains any dimension columns.
storageDescriptor_numberOfBuckets :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Int)
storageDescriptor_numberOfBuckets = Lens.lens (\StorageDescriptor' {numberOfBuckets} -> numberOfBuckets) (\s@StorageDescriptor' {} a -> s {numberOfBuckets = a} :: StorageDescriptor)

-- | An object that references a schema stored in the Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
storageDescriptor_schemaReference :: Lens.Lens' StorageDescriptor (Prelude.Maybe SchemaReference)
storageDescriptor_schemaReference = Lens.lens (\StorageDescriptor' {schemaReference} -> schemaReference) (\s@StorageDescriptor' {} a -> s {schemaReference = a} :: StorageDescriptor)

-- | @True@ if the table data is stored in subdirectories, or @False@ if not.
storageDescriptor_storedAsSubDirectories :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Bool)
storageDescriptor_storedAsSubDirectories = Lens.lens (\StorageDescriptor' {storedAsSubDirectories} -> storedAsSubDirectories) (\s@StorageDescriptor' {} a -> s {storedAsSubDirectories = a} :: StorageDescriptor)

-- | The user-supplied properties in key-value form.
storageDescriptor_parameters :: Lens.Lens' StorageDescriptor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
storageDescriptor_parameters = Lens.lens (\StorageDescriptor' {parameters} -> parameters) (\s@StorageDescriptor' {} a -> s {parameters = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
storageDescriptor_inputFormat :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_inputFormat = Lens.lens (\StorageDescriptor' {inputFormat} -> inputFormat) (\s@StorageDescriptor' {} a -> s {inputFormat = a} :: StorageDescriptor)

-- | The information about values that appear frequently in a column (skewed
-- values).
storageDescriptor_skewedInfo :: Lens.Lens' StorageDescriptor (Prelude.Maybe SkewedInfo)
storageDescriptor_skewedInfo = Lens.lens (\StorageDescriptor' {skewedInfo} -> skewedInfo) (\s@StorageDescriptor' {} a -> s {skewedInfo = a} :: StorageDescriptor)

-- | A list of the @Columns@ in the table.
storageDescriptor_columns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Column])
storageDescriptor_columns = Lens.lens (\StorageDescriptor' {columns} -> columns) (\s@StorageDescriptor' {} a -> s {columns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON StorageDescriptor where
  parseJSON =
    Core.withObject
      "StorageDescriptor"
      ( \x ->
          StorageDescriptor'
            Prelude.<$> (x Core..:? "SortColumns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Compressed")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "BucketColumns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SerdeInfo")
            Prelude.<*> (x Core..:? "OutputFormat")
            Prelude.<*> (x Core..:? "NumberOfBuckets")
            Prelude.<*> (x Core..:? "SchemaReference")
            Prelude.<*> (x Core..:? "StoredAsSubDirectories")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "InputFormat")
            Prelude.<*> (x Core..:? "SkewedInfo")
            Prelude.<*> (x Core..:? "Columns" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable StorageDescriptor where
  hashWithSalt salt' StorageDescriptor' {..} =
    salt' `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` skewedInfo
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` storedAsSubDirectories
      `Prelude.hashWithSalt` schemaReference
      `Prelude.hashWithSalt` numberOfBuckets
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` serdeInfo
      `Prelude.hashWithSalt` bucketColumns
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` compressed
      `Prelude.hashWithSalt` sortColumns

instance Prelude.NFData StorageDescriptor where
  rnf StorageDescriptor' {..} =
    Prelude.rnf sortColumns
      `Prelude.seq` Prelude.rnf columns
      `Prelude.seq` Prelude.rnf skewedInfo
      `Prelude.seq` Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf storedAsSubDirectories
      `Prelude.seq` Prelude.rnf schemaReference
      `Prelude.seq` Prelude.rnf numberOfBuckets
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf serdeInfo
      `Prelude.seq` Prelude.rnf bucketColumns
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf compressed

instance Core.ToJSON StorageDescriptor where
  toJSON StorageDescriptor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortColumns" Core..=) Prelude.<$> sortColumns,
            ("Compressed" Core..=) Prelude.<$> compressed,
            ("Location" Core..=) Prelude.<$> location,
            ("BucketColumns" Core..=) Prelude.<$> bucketColumns,
            ("SerdeInfo" Core..=) Prelude.<$> serdeInfo,
            ("OutputFormat" Core..=) Prelude.<$> outputFormat,
            ("NumberOfBuckets" Core..=)
              Prelude.<$> numberOfBuckets,
            ("SchemaReference" Core..=)
              Prelude.<$> schemaReference,
            ("StoredAsSubDirectories" Core..=)
              Prelude.<$> storedAsSubDirectories,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("InputFormat" Core..=) Prelude.<$> inputFormat,
            ("SkewedInfo" Core..=) Prelude.<$> skewedInfo,
            ("Columns" Core..=) Prelude.<$> columns
          ]
      )
