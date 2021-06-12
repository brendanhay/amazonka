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
-- Module      : Network.AWS.Glue.Types.StorageDescriptor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.StorageDescriptor where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.Order
import Network.AWS.Glue.Types.SchemaReference
import Network.AWS.Glue.Types.SerDeInfo
import Network.AWS.Glue.Types.SkewedInfo
import qualified Network.AWS.Lens as Lens

-- | Describes the physical storage of table data.
--
-- /See:/ 'newStorageDescriptor' smart constructor.
data StorageDescriptor = StorageDescriptor'
  { -- | @True@ if the data in the table is compressed, or @False@ if not.
    compressed :: Core.Maybe Core.Bool,
    -- | Must be specified if the table contains any dimension columns.
    numberOfBuckets :: Core.Maybe Core.Int,
    -- | The information about values that appear frequently in a column (skewed
    -- values).
    skewedInfo :: Core.Maybe SkewedInfo,
    -- | An object that references a schema stored in the AWS Glue Schema
    -- Registry.
    --
    -- When creating a table, you can pass an empty list of columns for the
    -- schema, and instead use a schema reference.
    schemaReference :: Core.Maybe SchemaReference,
    -- | A list specifying the sort order of each bucket in the table.
    sortColumns :: Core.Maybe [Order],
    -- | The output format: @SequenceFileOutputFormat@ (binary), or
    -- @IgnoreKeyTextOutputFormat@, or a custom format.
    outputFormat :: Core.Maybe Core.Text,
    -- | A list of reducer grouping columns, clustering columns, and bucketing
    -- columns in the table.
    bucketColumns :: Core.Maybe [Core.Text],
    -- | The serialization\/deserialization (SerDe) information.
    serdeInfo :: Core.Maybe SerDeInfo,
    -- | The physical location of the table. By default, this takes the form of
    -- the warehouse location, followed by the database location in the
    -- warehouse, followed by the table name.
    location :: Core.Maybe Core.Text,
    -- | A list of the @Columns@ in the table.
    columns :: Core.Maybe [Column],
    -- | The input format: @SequenceFileInputFormat@ (binary), or
    -- @TextInputFormat@, or a custom format.
    inputFormat :: Core.Maybe Core.Text,
    -- | The user-supplied properties in key-value form.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | @True@ if the table data is stored in subdirectories, or @False@ if not.
    storedAsSubDirectories :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StorageDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compressed', 'storageDescriptor_compressed' - @True@ if the data in the table is compressed, or @False@ if not.
--
-- 'numberOfBuckets', 'storageDescriptor_numberOfBuckets' - Must be specified if the table contains any dimension columns.
--
-- 'skewedInfo', 'storageDescriptor_skewedInfo' - The information about values that appear frequently in a column (skewed
-- values).
--
-- 'schemaReference', 'storageDescriptor_schemaReference' - An object that references a schema stored in the AWS Glue Schema
-- Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
--
-- 'sortColumns', 'storageDescriptor_sortColumns' - A list specifying the sort order of each bucket in the table.
--
-- 'outputFormat', 'storageDescriptor_outputFormat' - The output format: @SequenceFileOutputFormat@ (binary), or
-- @IgnoreKeyTextOutputFormat@, or a custom format.
--
-- 'bucketColumns', 'storageDescriptor_bucketColumns' - A list of reducer grouping columns, clustering columns, and bucketing
-- columns in the table.
--
-- 'serdeInfo', 'storageDescriptor_serdeInfo' - The serialization\/deserialization (SerDe) information.
--
-- 'location', 'storageDescriptor_location' - The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
--
-- 'columns', 'storageDescriptor_columns' - A list of the @Columns@ in the table.
--
-- 'inputFormat', 'storageDescriptor_inputFormat' - The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
--
-- 'parameters', 'storageDescriptor_parameters' - The user-supplied properties in key-value form.
--
-- 'storedAsSubDirectories', 'storageDescriptor_storedAsSubDirectories' - @True@ if the table data is stored in subdirectories, or @False@ if not.
newStorageDescriptor ::
  StorageDescriptor
newStorageDescriptor =
  StorageDescriptor'
    { compressed = Core.Nothing,
      numberOfBuckets = Core.Nothing,
      skewedInfo = Core.Nothing,
      schemaReference = Core.Nothing,
      sortColumns = Core.Nothing,
      outputFormat = Core.Nothing,
      bucketColumns = Core.Nothing,
      serdeInfo = Core.Nothing,
      location = Core.Nothing,
      columns = Core.Nothing,
      inputFormat = Core.Nothing,
      parameters = Core.Nothing,
      storedAsSubDirectories = Core.Nothing
    }

-- | @True@ if the data in the table is compressed, or @False@ if not.
storageDescriptor_compressed :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Bool)
storageDescriptor_compressed = Lens.lens (\StorageDescriptor' {compressed} -> compressed) (\s@StorageDescriptor' {} a -> s {compressed = a} :: StorageDescriptor)

-- | Must be specified if the table contains any dimension columns.
storageDescriptor_numberOfBuckets :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Int)
storageDescriptor_numberOfBuckets = Lens.lens (\StorageDescriptor' {numberOfBuckets} -> numberOfBuckets) (\s@StorageDescriptor' {} a -> s {numberOfBuckets = a} :: StorageDescriptor)

-- | The information about values that appear frequently in a column (skewed
-- values).
storageDescriptor_skewedInfo :: Lens.Lens' StorageDescriptor (Core.Maybe SkewedInfo)
storageDescriptor_skewedInfo = Lens.lens (\StorageDescriptor' {skewedInfo} -> skewedInfo) (\s@StorageDescriptor' {} a -> s {skewedInfo = a} :: StorageDescriptor)

-- | An object that references a schema stored in the AWS Glue Schema
-- Registry.
--
-- When creating a table, you can pass an empty list of columns for the
-- schema, and instead use a schema reference.
storageDescriptor_schemaReference :: Lens.Lens' StorageDescriptor (Core.Maybe SchemaReference)
storageDescriptor_schemaReference = Lens.lens (\StorageDescriptor' {schemaReference} -> schemaReference) (\s@StorageDescriptor' {} a -> s {schemaReference = a} :: StorageDescriptor)

-- | A list specifying the sort order of each bucket in the table.
storageDescriptor_sortColumns :: Lens.Lens' StorageDescriptor (Core.Maybe [Order])
storageDescriptor_sortColumns = Lens.lens (\StorageDescriptor' {sortColumns} -> sortColumns) (\s@StorageDescriptor' {} a -> s {sortColumns = a} :: StorageDescriptor) Core.. Lens.mapping Lens._Coerce

-- | The output format: @SequenceFileOutputFormat@ (binary), or
-- @IgnoreKeyTextOutputFormat@, or a custom format.
storageDescriptor_outputFormat :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Text)
storageDescriptor_outputFormat = Lens.lens (\StorageDescriptor' {outputFormat} -> outputFormat) (\s@StorageDescriptor' {} a -> s {outputFormat = a} :: StorageDescriptor)

-- | A list of reducer grouping columns, clustering columns, and bucketing
-- columns in the table.
storageDescriptor_bucketColumns :: Lens.Lens' StorageDescriptor (Core.Maybe [Core.Text])
storageDescriptor_bucketColumns = Lens.lens (\StorageDescriptor' {bucketColumns} -> bucketColumns) (\s@StorageDescriptor' {} a -> s {bucketColumns = a} :: StorageDescriptor) Core.. Lens.mapping Lens._Coerce

-- | The serialization\/deserialization (SerDe) information.
storageDescriptor_serdeInfo :: Lens.Lens' StorageDescriptor (Core.Maybe SerDeInfo)
storageDescriptor_serdeInfo = Lens.lens (\StorageDescriptor' {serdeInfo} -> serdeInfo) (\s@StorageDescriptor' {} a -> s {serdeInfo = a} :: StorageDescriptor)

-- | The physical location of the table. By default, this takes the form of
-- the warehouse location, followed by the database location in the
-- warehouse, followed by the table name.
storageDescriptor_location :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Text)
storageDescriptor_location = Lens.lens (\StorageDescriptor' {location} -> location) (\s@StorageDescriptor' {} a -> s {location = a} :: StorageDescriptor)

-- | A list of the @Columns@ in the table.
storageDescriptor_columns :: Lens.Lens' StorageDescriptor (Core.Maybe [Column])
storageDescriptor_columns = Lens.lens (\StorageDescriptor' {columns} -> columns) (\s@StorageDescriptor' {} a -> s {columns = a} :: StorageDescriptor) Core.. Lens.mapping Lens._Coerce

-- | The input format: @SequenceFileInputFormat@ (binary), or
-- @TextInputFormat@, or a custom format.
storageDescriptor_inputFormat :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Text)
storageDescriptor_inputFormat = Lens.lens (\StorageDescriptor' {inputFormat} -> inputFormat) (\s@StorageDescriptor' {} a -> s {inputFormat = a} :: StorageDescriptor)

-- | The user-supplied properties in key-value form.
storageDescriptor_parameters :: Lens.Lens' StorageDescriptor (Core.Maybe (Core.HashMap Core.Text Core.Text))
storageDescriptor_parameters = Lens.lens (\StorageDescriptor' {parameters} -> parameters) (\s@StorageDescriptor' {} a -> s {parameters = a} :: StorageDescriptor) Core.. Lens.mapping Lens._Coerce

-- | @True@ if the table data is stored in subdirectories, or @False@ if not.
storageDescriptor_storedAsSubDirectories :: Lens.Lens' StorageDescriptor (Core.Maybe Core.Bool)
storageDescriptor_storedAsSubDirectories = Lens.lens (\StorageDescriptor' {storedAsSubDirectories} -> storedAsSubDirectories) (\s@StorageDescriptor' {} a -> s {storedAsSubDirectories = a} :: StorageDescriptor)

instance Core.FromJSON StorageDescriptor where
  parseJSON =
    Core.withObject
      "StorageDescriptor"
      ( \x ->
          StorageDescriptor'
            Core.<$> (x Core..:? "Compressed")
            Core.<*> (x Core..:? "NumberOfBuckets")
            Core.<*> (x Core..:? "SkewedInfo")
            Core.<*> (x Core..:? "SchemaReference")
            Core.<*> (x Core..:? "SortColumns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "OutputFormat")
            Core.<*> (x Core..:? "BucketColumns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SerdeInfo")
            Core.<*> (x Core..:? "Location")
            Core.<*> (x Core..:? "Columns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "InputFormat")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StoredAsSubDirectories")
      )

instance Core.Hashable StorageDescriptor

instance Core.NFData StorageDescriptor

instance Core.ToJSON StorageDescriptor where
  toJSON StorageDescriptor' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Compressed" Core..=) Core.<$> compressed,
            ("NumberOfBuckets" Core..=) Core.<$> numberOfBuckets,
            ("SkewedInfo" Core..=) Core.<$> skewedInfo,
            ("SchemaReference" Core..=) Core.<$> schemaReference,
            ("SortColumns" Core..=) Core.<$> sortColumns,
            ("OutputFormat" Core..=) Core.<$> outputFormat,
            ("BucketColumns" Core..=) Core.<$> bucketColumns,
            ("SerdeInfo" Core..=) Core.<$> serdeInfo,
            ("Location" Core..=) Core.<$> location,
            ("Columns" Core..=) Core.<$> columns,
            ("InputFormat" Core..=) Core.<$> inputFormat,
            ("Parameters" Core..=) Core.<$> parameters,
            ("StoredAsSubDirectories" Core..=)
              Core.<$> storedAsSubDirectories
          ]
      )
