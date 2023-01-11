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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | A list of locations that point to the path where a Delta table is
    -- located.
    additionalLocations :: Prelude.Maybe [Prelude.Text],
    -- | A list of reducer grouping columns, clustering columns, and bucketing
    -- columns in the table.
    bucketColumns :: Prelude.Maybe [Prelude.Text],
    -- | A list of the @Columns@ in the table.
    columns :: Prelude.Maybe [Column],
    -- | @True@ if the data in the table is compressed, or @False@ if not.
    compressed :: Prelude.Maybe Prelude.Bool,
    -- | The input format: @SequenceFileInputFormat@ (binary), or
    -- @TextInputFormat@, or a custom format.
    inputFormat :: Prelude.Maybe Prelude.Text,
    -- | The physical location of the table. By default, this takes the form of
    -- the warehouse location, followed by the database location in the
    -- warehouse, followed by the table name.
    location :: Prelude.Maybe Prelude.Text,
    -- | Must be specified if the table contains any dimension columns.
    numberOfBuckets :: Prelude.Maybe Prelude.Int,
    -- | The output format: @SequenceFileOutputFormat@ (binary), or
    -- @IgnoreKeyTextOutputFormat@, or a custom format.
    outputFormat :: Prelude.Maybe Prelude.Text,
    -- | The user-supplied properties in key-value form.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An object that references a schema stored in the Glue Schema Registry.
    --
    -- When creating a table, you can pass an empty list of columns for the
    -- schema, and instead use a schema reference.
    schemaReference :: Prelude.Maybe SchemaReference,
    -- | The serialization\/deserialization (SerDe) information.
    serdeInfo :: Prelude.Maybe SerDeInfo,
    -- | The information about values that appear frequently in a column (skewed
    -- values).
    skewedInfo :: Prelude.Maybe SkewedInfo,
    -- | A list specifying the sort order of each bucket in the table.
    sortColumns :: Prelude.Maybe [Order],
    -- | @True@ if the table data is stored in subdirectories, or @False@ if not.
    storedAsSubDirectories :: Prelude.Maybe Prelude.Bool
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
-- 'additionalLocations', 'storageDescriptor_additionalLocations' - A list of locations that point to the path where a Delta table is
-- located.
--
-- 'bucketColumns', 'storageDescriptor_bucketColumns' - A list of reducer grouping columns, clustering columns, and bucketing
-- columns in the table.
--
-- 'columns', 'storageDescriptor_columns' - A list of the @Columns@ in the table.
--
-- 'compressed', 'storageDescriptor_compressed' - @True@ if the data in the table is compressed, or @False@ if not.
--
-- 'inputFormat', 'storageDescriptor_inputFormat' - The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
--
-- 'location', 'storageDescriptor_location' - The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
--
-- 'numberOfBuckets', 'storageDescriptor_numberOfBuckets' - Must be specified if the table contains any dimension columns.
--
-- 'outputFormat', 'storageDescriptor_outputFormat' - The output format: @SequenceFileOutputFormat@ (binary), or
-- @IgnoreKeyTextOutputFormat@, or a custom format.
--
-- 'parameters', 'storageDescriptor_parameters' - The user-supplied properties in key-value form.
--
-- 'schemaReference', 'storageDescriptor_schemaReference' - An object that references a schema stored in the Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
--
-- 'serdeInfo', 'storageDescriptor_serdeInfo' - The serialization\/deserialization (SerDe) information.
--
-- 'skewedInfo', 'storageDescriptor_skewedInfo' - The information about values that appear frequently in a column (skewed
-- values).
--
-- 'sortColumns', 'storageDescriptor_sortColumns' - A list specifying the sort order of each bucket in the table.
--
-- 'storedAsSubDirectories', 'storageDescriptor_storedAsSubDirectories' - @True@ if the table data is stored in subdirectories, or @False@ if not.
newStorageDescriptor ::
  StorageDescriptor
newStorageDescriptor =
  StorageDescriptor'
    { additionalLocations =
        Prelude.Nothing,
      bucketColumns = Prelude.Nothing,
      columns = Prelude.Nothing,
      compressed = Prelude.Nothing,
      inputFormat = Prelude.Nothing,
      location = Prelude.Nothing,
      numberOfBuckets = Prelude.Nothing,
      outputFormat = Prelude.Nothing,
      parameters = Prelude.Nothing,
      schemaReference = Prelude.Nothing,
      serdeInfo = Prelude.Nothing,
      skewedInfo = Prelude.Nothing,
      sortColumns = Prelude.Nothing,
      storedAsSubDirectories = Prelude.Nothing
    }

-- | A list of locations that point to the path where a Delta table is
-- located.
storageDescriptor_additionalLocations :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Prelude.Text])
storageDescriptor_additionalLocations = Lens.lens (\StorageDescriptor' {additionalLocations} -> additionalLocations) (\s@StorageDescriptor' {} a -> s {additionalLocations = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | A list of reducer grouping columns, clustering columns, and bucketing
-- columns in the table.
storageDescriptor_bucketColumns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Prelude.Text])
storageDescriptor_bucketColumns = Lens.lens (\StorageDescriptor' {bucketColumns} -> bucketColumns) (\s@StorageDescriptor' {} a -> s {bucketColumns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | A list of the @Columns@ in the table.
storageDescriptor_columns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Column])
storageDescriptor_columns = Lens.lens (\StorageDescriptor' {columns} -> columns) (\s@StorageDescriptor' {} a -> s {columns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | @True@ if the data in the table is compressed, or @False@ if not.
storageDescriptor_compressed :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Bool)
storageDescriptor_compressed = Lens.lens (\StorageDescriptor' {compressed} -> compressed) (\s@StorageDescriptor' {} a -> s {compressed = a} :: StorageDescriptor)

-- | The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
storageDescriptor_inputFormat :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_inputFormat = Lens.lens (\StorageDescriptor' {inputFormat} -> inputFormat) (\s@StorageDescriptor' {} a -> s {inputFormat = a} :: StorageDescriptor)

-- | The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
storageDescriptor_location :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_location = Lens.lens (\StorageDescriptor' {location} -> location) (\s@StorageDescriptor' {} a -> s {location = a} :: StorageDescriptor)

-- | Must be specified if the table contains any dimension columns.
storageDescriptor_numberOfBuckets :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Int)
storageDescriptor_numberOfBuckets = Lens.lens (\StorageDescriptor' {numberOfBuckets} -> numberOfBuckets) (\s@StorageDescriptor' {} a -> s {numberOfBuckets = a} :: StorageDescriptor)

-- | The output format: @SequenceFileOutputFormat@ (binary), or
-- @IgnoreKeyTextOutputFormat@, or a custom format.
storageDescriptor_outputFormat :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Text)
storageDescriptor_outputFormat = Lens.lens (\StorageDescriptor' {outputFormat} -> outputFormat) (\s@StorageDescriptor' {} a -> s {outputFormat = a} :: StorageDescriptor)

-- | The user-supplied properties in key-value form.
storageDescriptor_parameters :: Lens.Lens' StorageDescriptor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
storageDescriptor_parameters = Lens.lens (\StorageDescriptor' {parameters} -> parameters) (\s@StorageDescriptor' {} a -> s {parameters = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | An object that references a schema stored in the Glue Schema Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
storageDescriptor_schemaReference :: Lens.Lens' StorageDescriptor (Prelude.Maybe SchemaReference)
storageDescriptor_schemaReference = Lens.lens (\StorageDescriptor' {schemaReference} -> schemaReference) (\s@StorageDescriptor' {} a -> s {schemaReference = a} :: StorageDescriptor)

-- | The serialization\/deserialization (SerDe) information.
storageDescriptor_serdeInfo :: Lens.Lens' StorageDescriptor (Prelude.Maybe SerDeInfo)
storageDescriptor_serdeInfo = Lens.lens (\StorageDescriptor' {serdeInfo} -> serdeInfo) (\s@StorageDescriptor' {} a -> s {serdeInfo = a} :: StorageDescriptor)

-- | The information about values that appear frequently in a column (skewed
-- values).
storageDescriptor_skewedInfo :: Lens.Lens' StorageDescriptor (Prelude.Maybe SkewedInfo)
storageDescriptor_skewedInfo = Lens.lens (\StorageDescriptor' {skewedInfo} -> skewedInfo) (\s@StorageDescriptor' {} a -> s {skewedInfo = a} :: StorageDescriptor)

-- | A list specifying the sort order of each bucket in the table.
storageDescriptor_sortColumns :: Lens.Lens' StorageDescriptor (Prelude.Maybe [Order])
storageDescriptor_sortColumns = Lens.lens (\StorageDescriptor' {sortColumns} -> sortColumns) (\s@StorageDescriptor' {} a -> s {sortColumns = a} :: StorageDescriptor) Prelude.. Lens.mapping Lens.coerced

-- | @True@ if the table data is stored in subdirectories, or @False@ if not.
storageDescriptor_storedAsSubDirectories :: Lens.Lens' StorageDescriptor (Prelude.Maybe Prelude.Bool)
storageDescriptor_storedAsSubDirectories = Lens.lens (\StorageDescriptor' {storedAsSubDirectories} -> storedAsSubDirectories) (\s@StorageDescriptor' {} a -> s {storedAsSubDirectories = a} :: StorageDescriptor)

instance Data.FromJSON StorageDescriptor where
  parseJSON =
    Data.withObject
      "StorageDescriptor"
      ( \x ->
          StorageDescriptor'
            Prelude.<$> ( x Data..:? "AdditionalLocations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BucketColumns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Columns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Compressed")
            Prelude.<*> (x Data..:? "InputFormat")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "NumberOfBuckets")
            Prelude.<*> (x Data..:? "OutputFormat")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SchemaReference")
            Prelude.<*> (x Data..:? "SerdeInfo")
            Prelude.<*> (x Data..:? "SkewedInfo")
            Prelude.<*> (x Data..:? "SortColumns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StoredAsSubDirectories")
      )

instance Prelude.Hashable StorageDescriptor where
  hashWithSalt _salt StorageDescriptor' {..} =
    _salt `Prelude.hashWithSalt` additionalLocations
      `Prelude.hashWithSalt` bucketColumns
      `Prelude.hashWithSalt` columns
      `Prelude.hashWithSalt` compressed
      `Prelude.hashWithSalt` inputFormat
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` numberOfBuckets
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` schemaReference
      `Prelude.hashWithSalt` serdeInfo
      `Prelude.hashWithSalt` skewedInfo
      `Prelude.hashWithSalt` sortColumns
      `Prelude.hashWithSalt` storedAsSubDirectories

instance Prelude.NFData StorageDescriptor where
  rnf StorageDescriptor' {..} =
    Prelude.rnf additionalLocations
      `Prelude.seq` Prelude.rnf bucketColumns
      `Prelude.seq` Prelude.rnf columns
      `Prelude.seq` Prelude.rnf compressed
      `Prelude.seq` Prelude.rnf inputFormat
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf numberOfBuckets
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf schemaReference
      `Prelude.seq` Prelude.rnf serdeInfo
      `Prelude.seq` Prelude.rnf skewedInfo
      `Prelude.seq` Prelude.rnf sortColumns
      `Prelude.seq` Prelude.rnf storedAsSubDirectories

instance Data.ToJSON StorageDescriptor where
  toJSON StorageDescriptor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalLocations" Data..=)
              Prelude.<$> additionalLocations,
            ("BucketColumns" Data..=) Prelude.<$> bucketColumns,
            ("Columns" Data..=) Prelude.<$> columns,
            ("Compressed" Data..=) Prelude.<$> compressed,
            ("InputFormat" Data..=) Prelude.<$> inputFormat,
            ("Location" Data..=) Prelude.<$> location,
            ("NumberOfBuckets" Data..=)
              Prelude.<$> numberOfBuckets,
            ("OutputFormat" Data..=) Prelude.<$> outputFormat,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("SchemaReference" Data..=)
              Prelude.<$> schemaReference,
            ("SerdeInfo" Data..=) Prelude.<$> serdeInfo,
            ("SkewedInfo" Data..=) Prelude.<$> skewedInfo,
            ("SortColumns" Data..=) Prelude.<$> sortColumns,
            ("StoredAsSubDirectories" Data..=)
              Prelude.<$> storedAsSubDirectories
          ]
      )
