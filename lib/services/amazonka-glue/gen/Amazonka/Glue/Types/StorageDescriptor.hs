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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StorageDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.Column
import Amazonka.Glue.Types.Order
import Amazonka.Glue.Types.SchemaReference
import Amazonka.Glue.Types.SerDeInfo
import Amazonka.Glue.Types.SkewedInfo
import qualified Amazonka.Prelude as Prelude

-- | Describes the physical storage of table data.
--
-- /See:/ 'newStorageDescriptor' smart constructor.
data StorageDescriptor = StorageDescriptor'
  { -- | A list specifying the sort order of each bucket in the table.
    sortColumns :: Prelude.Maybe [Order],
    -- | @True@ if the table data is stored in subdirectories, or @False@ if not.
    storedAsSubDirectories :: Prelude.Maybe Prelude.Bool,
    -- | The information about values that appear frequently in a column (skewed
    -- values).
    skewedInfo :: Prelude.Maybe SkewedInfo,
    -- | A list of the @Columns@ in the table.
    columns :: Prelude.Maybe [Column],
    -- | A list of reducer grouping columns, clustering columns, and bucketing
    -- columns in the table.
    bucketColumns :: Prelude.Maybe [Prelude.Text],
    -- | The serialization\/deserialization (SerDe) information.
    serdeInfo :: Prelude.Maybe SerDeInfo,
    -- | The output format: @SequenceFileOutputFormat@ (binary), or
    -- @IgnoreKeyTextOutputFormat@, or a custom format.
    outputFormat :: Prelude.Maybe Prelude.Text,
    -- | The physical location of the table. By default, this takes the form of
    -- the warehouse location, followed by the database location in the
    -- warehouse, followed by the table name.
    location :: Prelude.Maybe Prelude.Text,
    -- | @True@ if the data in the table is compressed, or @False@ if not.
    compressed :: Prelude.Maybe Prelude.Bool,
    -- | A list of locations that point to the path where a Delta table is
    -- located.
    additionalLocations :: Prelude.Maybe [Prelude.Text],
    -- | An object that references a schema stored in the Glue Schema Registry.
    --
    -- When creating a table, you can pass an empty list of columns for the
    -- schema, and instead use a schema reference.
    schemaReference :: Prelude.Maybe SchemaReference,
    -- | The input format: @SequenceFileInputFormat@ (binary), or
    -- @TextInputFormat@, or a custom format.
    inputFormat :: Prelude.Maybe Prelude.Text,
    -- | Must be specified if the table contains any dimension columns.
    numberOfBuckets :: Prelude.Maybe Prelude.Int,
    -- | The user-supplied properties in key-value form.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'storedAsSubDirectories', 'storageDescriptor_storedAsSubDirectories' - @True@ if the table data is stored in subdirectories, or @False@ if not.
--
-- 'skewedInfo', 'storageDescriptor_skewedInfo' - The information about values that appear frequently in a column (skewed
-- values).
--
-- 'columns', 'storageDescriptor_columns' - A list of the @Columns@ in the table.
--
-- 'bucketColumns', 'storageDescriptor_bucketColumns' - A list of reducer grouping columns, clustering columns, and bucketing
-- columns in the table.
--
-- 'serdeInfo', 'storageDescriptor_serdeInfo' - The serialization\/deserialization (SerDe) information.
--
-- 'outputFormat', 'storageDescriptor_outputFormat' - The output format: @SequenceFileOutputFormat@ (binary), or
-- @IgnoreKeyTextOutputFormat@, or a custom format.
--
-- 'location', 'storageDescriptor_location' - The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
--
-- 'compressed', 'storageDescriptor_compressed' - @True@ if the data in the table is compressed, or @False@ if not.
--
-- 'additionalLocations', 'storageDescriptor_additionalLocations' - A list of locations that point to the path where a Delta table is
-- located.
--
-- 'schemaReference', 'storageDescriptor_schemaReference' - An object that references a schema stored in the Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
--
-- 'inputFormat', 'storageDescriptor_inputFormat' - The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
--
-- 'numberOfBuckets', 'storageDescriptor_numberOfBuckets' - Must be specified if the table contains any dimension columns.
--
-- 'parameters', 'storageDescriptor_parameters' - The user-supplied properties in key-value form.
newStorageDescriptor ::
  StorageDescriptor
newStorageDescriptor =
  StorageDescriptor'
    { sortColumns = Prelude.Nothing,
      storedAsSubDirectories = Prelude.Nothing,
      skewedInfo = Prelude.Nothing,
      columns = Prelude.Nothing,
      bucketColumns = Prelude.Nothing,
      serdeInfo = Prelude.Nothing,
      outputFormat = Prelude.Nothing,
      location = Prelude.Nothing,
      compressed = Prelude.Nothing,
      additionalLocations = Prelude.Nothing,
      schemaReference = Prelude.Nothing,
      inputFormat = Prelude.Nothing,
      numberOfBuckets = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | A list specifying the sort order of each bucket in the table.
storageDescriptor_sortColumns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Order])
storageDescriptor_sortColumns = Lens.lens (\StorageDescriptor' {sortColumns} -> sortColumns) (\s@StorageDescriptor' {} a -> s {sortColumns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | @True@ if the table data is stored in subdirectories, or @False@ if not.
storageDescriptor_storedAsSubDirectories :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Bool)
storageDescriptor_storedAsSubDirectories = Lens.lens (\StorageDescriptor' {storedAsSubDirectories} -> storedAsSubDirectories) (\s@StorageDescriptor' {} a -> s {storedAsSubDirectories = a} :: StorageDescriptor)

-- | The information about values that appear frequently in a column (skewed
-- values).
storageDescriptor_skewedInfo :: Lens.Lens' StorageDescriptor (Prelude.Maybe SkewedInfo)
storageDescriptor_skewedInfo = Lens.lens (\StorageDescriptor' {skewedInfo} -> skewedInfo) (\s@StorageDescriptor' {} a -> s {skewedInfo = a} :: StorageDescriptor)

-- | A list of the @Columns@ in the table.
storageDescriptor_columns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Column])
storageDescriptor_columns = Lens.lens (\StorageDescriptor' {columns} -> columns) (\s@StorageDescriptor' {} a -> s {columns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

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

-- | The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
storageDescriptor_location :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_location = Lens.lens (\StorageDescriptor' {location} -> location) (\s@StorageDescriptor' {} a -> s {location = a} :: StorageDescriptor)

-- | @True@ if the data in the table is compressed, or @False@ if not.
storageDescriptor_compressed :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Bool)
storageDescriptor_compressed = Lens.lens (\StorageDescriptor' {compressed} -> compressed) (\s@StorageDescriptor' {} a -> s {compressed = a} :: StorageDescriptor)

-- | A list of locations that point to the path where a Delta table is
-- located.
storageDescriptor_additionalLocations :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Prelude.Text])
storageDescriptor_additionalLocations = Lens.lens (\StorageDescriptor' {additionalLocations} -> additionalLocations) (\s@StorageDescriptor' {} a -> s {additionalLocations = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | An object that references a schema stored in the Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
storageDescriptor_schemaReference :: Lens.Lens' StorageDescriptor (Prelude.Maybe SchemaReference)
storageDescriptor_schemaReference = Lens.lens (\StorageDescriptor' {schemaReference} -> schemaReference) (\s@StorageDescriptor' {} a -> s {schemaReference = a} :: StorageDescriptor)

-- | The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
storageDescriptor_inputFormat :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_inputFormat = Lens.lens (\StorageDescriptor' {inputFormat} -> inputFormat) (\s@StorageDescriptor' {} a -> s {inputFormat = a} :: StorageDescriptor)

-- | Must be specified if the table contains any dimension columns.
storageDescriptor_numberOfBuckets :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Int)
storageDescriptor_numberOfBuckets = Lens.lens (\StorageDescriptor' {numberOfBuckets} -> numberOfBuckets) (\s@StorageDescriptor' {} a -> s {numberOfBuckets = a} :: StorageDescriptor)

-- | The user-supplied properties in key-value form.
storageDescriptor_parameters :: Lens.Lens' StorageDescriptor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
storageDescriptor_parameters = Lens.lens (\StorageDescriptor' {parameters} -> parameters) (\s@StorageDescriptor' {} a -> s {parameters = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StorageDescriptor where
  parseJSON =
    Data.withObject
      "StorageDescriptor"
      ( \x ->
          StorageDescriptor'
            Prelude.<$> (x Data..:? "SortColumns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StoredAsSubDirectories")
            Prelude.<*> (x Data..:? "SkewedInfo")
            Prelude.<*> (x Data..:? "Columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BucketColumns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SerdeInfo")
            Prelude.<*> (x Data..:? "OutputFormat")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "Compressed")
            Prelude.<*> ( x Data..:? "AdditionalLocations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SchemaReference")
            Prelude.<*> (x Data..:? "InputFormat")
            Prelude.<*> (x Data..:? "NumberOfBuckets")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StorageDescriptor where
  hashWithSalt _salt StorageDescriptor' {..} =
    _salt `Prelude.hashWithSalt` sortColumns
      `Prelude.hashWithSalt` storedAsSubDirectories
      `Prelude.hashWithSalt` skewedInfo
      `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` bucketColumns
      `Prelude.hashWithSalt` serdeInfo
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` compressed
      `Prelude.hashWithSalt` additionalLocations
      `Prelude.hashWithSalt` schemaReference
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` numberOfBuckets
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData StorageDescriptor where
  rnf StorageDescriptor' {..} =
    Prelude.rnf sortColumns
      `Prelude.seq` Prelude.rnf storedAsSubDirectories
      `Prelude.seq` Prelude.rnf skewedInfo
      `Prelude.seq` Prelude.rnf columns
      `Prelude.seq` Prelude.rnf bucketColumns
      `Prelude.seq` Prelude.rnf serdeInfo
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf compressed
      `Prelude.seq` Prelude.rnf additionalLocations
      `Prelude.seq` Prelude.rnf schemaReference
      `Prelude.seq` Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf numberOfBuckets
      `Prelude.seq` Prelude.rnf parameters

instance Data.ToJSON StorageDescriptor where
  toJSON StorageDescriptor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortColumns" Data..=) Prelude.<$> sortColumns,
            ("StoredAsSubDirectories" Data..=)
              Prelude.<$> storedAsSubDirectories,
            ("SkewedInfo" Data..=) Prelude.<$> skewedInfo,
            ("Columns" Data..=) Prelude.<$> columns,
            ("BucketColumns" Data..=) Prelude.<$> bucketColumns,
            ("SerdeInfo" Data..=) Prelude.<$> serdeInfo,
            ("OutputFormat" Data..=) Prelude.<$> outputFormat,
            ("Location" Data..=) Prelude.<$> location,
            ("Compressed" Data..=) Prelude.<$> compressed,
            ("AdditionalLocations" Data..=)
              Prelude.<$> additionalLocations,
            ("SchemaReference" Data..=)
              Prelude.<$> schemaReference,
            ("InputFormat" Data..=) Prelude.<$> inputFormat,
            ("NumberOfBuckets" Data..=)
              Prelude.<$> numberOfBuckets,
            ("Parameters" Data..=) Prelude.<$> parameters
          ]
      )
